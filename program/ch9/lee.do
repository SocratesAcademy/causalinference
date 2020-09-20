*! Xiliang Zhao, 2016.12.26
*Modified in 2020.5.29
/*
https://sites.google.com/site/rdpackages/home
(1) rdrobust package:
rdrobust package include three commands: rdplot, rdbwselect, rdrobust

net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace
(2) McCrary (2008) density test:
Using DCdensity.ado provided by McCrary, Copy it to \ado\plus\d\ before you can use it:
 http://eml.berkeley.edu/~jmccrary/DCdensity/

rddensity to do McCrary (2008) Test and plots:
net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace

net install lpdensity, from(https://sites.google.com/site/nppackages/lpdensity/stata) replace

*rdrobust, rddensity, lpdensity can be download from: 
https://github.com/zhaoxiliang/causalinference

Reference: Calonico, Cattaneo, Farrell and Titiunik (2017): rdrobust: Software for Regression Discontinuity Designs, Stata Journal 17(2): 372-404.
*/
set more off

cd "PATH"  // Change the path to the location where lee.dta locates
use lee, clear
describe
summarize
/*(1)画出结果变量与参考变量关系图: vote vs. margin
* draw figure 6. in Lee and Lemieux (2010)
*/

rdplot vote margin if margin>=-.5&margin<=.5, c(0) nbins(50 50) graph_options(legend(off) title("") xlabel(-0.5(0.1)0.5) ///
 xtitle("Democratic Vote Share Margin of Victory, Election t") ///
 ytitle("Vote Share, Election t+1") note(Note: Replicate figure 6 of Lee and Lemieumx (2010))) // replicate figure 6 in Lee and Lemieux (2010)
 
*add ci, or ci with shade
rdplot vote margin if margin>=-.5&margin<=.5, c(0) nbins(50 50) ci(95) graph_options(legend(off) title("") xlabel(-0.5(0.1)0.5) ///
 xtitle("Democratic Vote Share Margin of Victory, Election t") ///
 ytitle("Vote Share, Election t+1") note(Note: Replicate figure 6 of Lee and Lemieumx (2010))) // replicate figure 6 in Lee and Lemieux (2010)
 
 
/*(2) 画出协变量与参考变量关系图：检验连续性假设：voteprev vs. margin*/
*Open original data of Lee(2008)
use group_final, clear
ren mdemshareprev voteprev
ren difdemshare margin
keep if margin >=-.5 &margin <=.5
*Draw figure 4(b) of Lee(2008), which is also figure 9.3 in CausalBOOK

rdplot voteprev margin, c(0)p(2) graph_options(xtitle("Democratic Vote Share Margin of Victory, Election t") ytitle("Vote Share, Election t-1")  yscale(r(0.30 0.70)) xlabel(-.5(.1).5) ylabel(0.3(.1)0.7) legend(off) note(Note: Replicate figure 4(b) of Lee (2008)))



/*(3) 画参考变量密度图：检验局部随机化假设 McCrary Test*/
use lee,clear

histogram margin if margin>=-.5&margin<=.5, xline(0, lc(red)) xlabel(-.5(.1).5)

* Bring the reference curve upfront

gen y = 1.5*_n/_N
gen x = 0
two histogram margin if margin>=-.5&margin<=.5|| line y x, legend(off) xlabel(-0.5(0.1)0.5) lc(black)

* or
histogram margin if margin>=-.5&margin<=.5,  xlabel(-.5(.1).5) addplot(line y x) legend(off)

*Using DCdensity.ado provided by McCrary: http://eml.berkeley.edu/~jmccrary/DCdensity/

keep if margin>=-.5&margin<=.5
DCdensity margin, breakpoint(0) b(.005) generate(Xj Yj r0 fhat se_fhat) nograph

gr twoway (scatter Yj Xj if Xj>=-.5&Xj<=.5, msymbol(circle_hollow) mcolor(gray))           ///
      (line fhat r0 if r0 < 0 , lcolor(black) lwidth(medthick))   ///
        (line fhat r0 if r0>0 , lcolor(black) lwidth(medthick)),   ///
  xline(0, lcolor(black)) legend(off) xlabel(-0.5(0.1)0.5)

drop Yj Xj r0 fhat se_fhat
  
/*(4) 选择最优带宽:h_CV=0.28, rdbwselect */
use lee,clear
keep if margin>=-.5&margin<=.5
rdbwselect vote margin
rdbwselect vote margin, all 

/*(5) 估计因果效应*/
rdrobust vote margin, c(0) p(1)
rdrobust vote margin, c(0) p(1) all
rdrobust vote margin, c(0) p(1) all h(.28)
rdrobust vote margin, c(0) p(1) all h(.28) kernel(uni)

*估计参数：LLR
keep if margin>=-0.28&margin<=0.28
gen d=margin>0
reg vote 1.d, vce(robust)
eststo m1
reg vote d##c.margin, vce(robust)
eststo m2

*estimate stats m1 m2
esttab m1 m2 using llr.rtf, star(* .10 ** .05 *** .01) nogap  replace ///
mtitle("Local Average" "LLR") se(%5.4f) ar2 keep(1.d margin 1.d#c.margin _cons)

*多项式估计，选择最优阶数p=2
use lee, clear
keep if margin<=.5&margin>=-.5
gen d=margin>0
gen x=margin
gen x2=x^2
gen x3=x^3
gen x4=x^4

reg vote d##c.x, vce(robust)
eststo m1
reg vote d##c.(x x2), vce(robust)
eststo m2
reg vote d##c.(x x2 x3), vce(robust)
eststo m3
reg vote d##c.(x x2 x3 x4), vce(robust)
eststo m4
esttab m1 m2 m3 m4 using lpr.rtf, star(* .10 ** .05 *** .01) nogap  replace ///
mtitle("p = 1" "p = 2" "p = 3" "p = 4") se(%5.4f) ar2 aic(%10.4f) bic(%10.4f) ///
drop(0.d 0.d#c.x 0.d#c.x2 0.d#c.x3 0.d#c.x4)

/*(6) 检验*/
*a)局部随机化假设检验McCrary(2008)密度检验
*Using rddensity to do the test
use lee, clear
rddensity margin, all
rddensity margin, p(1) all
rddensity margin, p(1) all plot plot_range(-.5 .5)

*b)协变量连续性检验
use group_final, clear
ren mdemshareprev voteprev
ren difdemshare margin
keep if margin >=-.5 &margin <=.5

gen x=margin
gen x2=x^2
gen x3=x^3
gen x4=x^4
gen d=margin>=0
reg voteprev d##c.(x x2 x3 x4) , vce(robust)

reg voteprev d##c.(x x2) if inrange(margin, -.28, .28), vce(robust)

rdrobust voteprev margin, p(1)
rdrobust voteprev margin, p(2) h(.28)

*c) 其它断点效应分析：安慰剂检验
*Plots: RDD plots
use lee, clear
rdplot vote margin if margin<0, c(-.25) graph_options(legend(off) title("") xlabel(-.5(.1)0))


rdplot vote margin if margin>=0, c(.25) graph_options(legend(off) title("") xlabel(0(.1).5))
*Placebo test: RDD estimation
rdrobust vote margin if margin<0, c(-.25)
rdrobust vote margin if margin>=0, c(.25)

*d)用不同带宽进行估计 

forvalues h = .5(-.1).1 {
qui rdrobust vote margin, h(`h') p(1)
di "bandwidth h= " `h',  " TE=" e(tau_bc), " S.E.=" e(se_tau_rb), " p-value=" e(pv_rb) 
}
  
tempname resmat
forvalue h = .5(-.1).1 {
qui rdrobust vote margin, h(`h') p(1)
matrix `resmat'=nullmat(`resmat')\(e(tau_bc), e(se_tau_rb), e(pv_rb))
 local names `"`names'`"`h'"'"'
}
matrix coln `resmat' = "treatment effect" "robust std err" "p-value"
matrix rown `resmat' = `names'
matlist `resmat', row("Bandwidth") format(%8.4f)
  

  
  
