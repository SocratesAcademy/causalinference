cap log close
set more off

*Change working directory
cd "PATH" // Change to your fold with the data nsw_dw.dta and cps_controls.dta
local covariates "age agesq education black hispanic married nodegree re74 re75"
*Use Dehejia-Whaba (1999) experimental data and CPS control

********************************************
*Open Dehejia-Whaba(1999) experimental data
*********************************************
use nsw_dw, clear
gen agesq=age*age

*(1)do balance test
*tebalance summarize, shoud use after teffects command
*pstest is a command of psmatch2, need install first
pstest age-re75,t(treat) raw

teffects ipw (re78)(treat age - re75)
tebalance summarize age - re75, baseline

*(2)estimate the propensity score
cap drop id_nsw ps_nsw
pscore treat age-re75, pscore(ps_nsw) blockid(id_nsw) logit
g l=ln(ps_nsw/(1-ps_nsw))
*or
predict p, xb


pstest age-re75 ps_nsw l, t(treat) raw // also can use tebalance summarize, but first run a teffects commond

*histogram of ps
histogram ps_nsw,by(treat, col(1))
twoway (kdensity ps_nsw if treat==0)(kdensity ps_nsw if treat==1), ///
 legend(label(1 control) label(2 treated)) title("Propensity Score for the treated and control")

*(3)analysis:Because the covariates are balance, we estimate the treatment effects using regression
regress re78 treat, vce(robust)
est sto m1
regress re78 treat age-re74 agesq,vce(robust)
est sto m2
esttab m1 m2, b(%10.4f) se(%10.4f) ///
mtitles("unadjusted" "adjusted")  ///
star(* .10 ** .05 *** .01) nogap ar2


*******************************
*DW(1999) nonexperimental data: CPS
**********************************
use nsw_dw,clear

drop if treat==0
append using cps_controls
save nsw_cps, replace

gen agesq=age*age
gen marriedre75=married*re75
gen agere74=age*re74
gen nodegreeage=nodegree*age

estpost tabstat age-re75,by(treat) s(mean sd) column(s) ///
 listwise // 利用~tabstat~进行分组简单统计
eststo sum // 结果保存到~sum~中
esttab sum, main(mean) aux(sd) unstack nonumber nomtitle ///
 nogap noobs
 // 将统计结果列到显示器上，
 //也可以用~using~选项将结果保存到文件中

*(1)do balance test
pstest age-re75, t(treat) raw
*(2) Estimate the propensity score
cap drop id_nsw ps_nsw
pscore treat age-re75, pscore(ps_nsw) blockid(id_nsw) logit

cap drop id_nsw ps_nsw
pscore treat age education black hisp married re74 re75 ///
marriedre75 agere74, pscore(ps_nsw) blockid(id_nsw) ///
 comsup logit 

/*
drop id_nsw ps_nsw
pscore treat age education black hisp married nodegree re74 re75 marriedre75 agesq nodegreeage, pscore(ps_nsw) blockid(id_nsw) logit comsup
*/
cap drop l
g l=ln(ps_nsw/(1-ps_nsw))

pstest age-re75 ps_nsw l, t(treat) raw

*histogram of ps
histogram ps_nsw,by(treat, col(1))
twoway (kdensity ps_nsw if treat==0)(kdensity ps_nsw if treat==1), ///
 legend(label(1 control) label(2 treated)) title("Propensity Score for the treated and control")

save tmp,replace
/*(3)Matching: because it is very imbalance, we need to match sample

(1)trimmed the sample to satisfy common support and make it similar
preserve
su ps if treat==1
scalar min1=r(min)
scalar max1=r(max)
su ps if treat==0
scalar min0=r(min)
scalar max0=r(max)
drop if ps<max(min0,min1)
drop if ps>min(max0,max1)
(2)match sample
*/

/*(3)(4)Matching and Estimating Treatment Effects*/
*1) Using attnd, attnw, attr, attk, atts
atts re78 treat, pscore(ps_nsw) blockid(id) comsup detail


*2) Matching 
/*if we do not consider ties, then we choose only one
control as match. If there are more than one controls 
have same distance, that is, for ties, the psmatch2 would
choose one it first encounter. Then, the order of observation
is important, different order would give different matches, so 
the matched sample is different.

In order to replicates the same matched samples, we could 
order the samples randomnly, the following four lines does this work.
*/

use tmp,clear //tmp.dta is the data contains propensity score 'ps'
set seed 2342
gen u=runiform()
sort u

*psmatch2 treat, outcome(re78) p(ps_nsw) n(1)  norepl descending odds ties // if use option 'ties', there will be possible to matched with more than one controls

psmatch2 treat, outcome(re78) p(ps_nsw) n(1)  norepl ///
descending odds
pstest age-re75 ps_nsw l, both

drop u
save tmpcia,replace
/*Construct matched samples*/
sort _id
gen pair = _id if _treated==0 
replace pair = _n1 if _treated==1
bysort pair: egen paircount = count(pair)
drop if paircount !=2
save match, replace

*test balance for matched sample
pstest age-re75 ps_nsw l, t(treat) raw

/*analysis*/
qui reg re78 treat,vce(robust) // unadjusted model
eststo m1
qui reg re78 treat age-re74 , vce(robust) // adjusted model
eststo m2
esttab m1 m2, b(%10.2f) se(%10.2f) mtitles("unadjusted" "adjusted") keep(treat) star(* .10 ** .05 *** .01) nogap ar2

/*Test CIA*/
*1)Pseudo outcome
use tmpcia, clear
set seed 2342
gen u=runiform()
sort u
psmatch2 treat, outcome(re74) p(ps_nsw) n(1)  norepl descending odds ties
psmatch2 treat, outcome(re75) p(ps_nsw) n(1)  norepl descending odds ties

*2)Pseudo treatment
use nsw_dw,clear
gen w=1 // generate pseudo treatment
keep if treat==0 // only keep control group
append using tmpcia
replace w=0 if treat==0&w==. // keep cps_controls as the new control group
*regenerate variables
replace agesq = age*age
replace marriedre75=married*re75
replace agere74=age*re74

set seed 2342
gen u=runiform()
sort u

psmatch2 w age education black hisp married re74 re75 marriedre75 agere74, outcome(re78) n(1)  norepl descending odds ties

!del tmp*
erase match.dta
erase nsw_cps.dta

set more on
exit

