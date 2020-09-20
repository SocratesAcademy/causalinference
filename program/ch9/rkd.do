*! Xiliang Zhao 2017.01.04
*Modified in 2020.05.28
/*弯折回归设计*/ 
*Generate a simulation of RKD
*Using ssc intall rdrobust, replace
*The old edition of rdrobust package
clear
set obs 10000

local x0 = 10

set seed 2345
*(1) Sharp RKD
gen e=rnormal()
gen x=rnormal(10,2)
gen d=min(0.5*x, 0.5*`x0')
gen y=2*d-0.5*x+rnormal()

*Graphs
*graph of treatment assignment
sc d x, xline(10)

rdplot y x, c(10) p(1)

*keep if x>=7&x<=13
*estimation
gen v=x-`x0'
gen v2=v^2
gen v3=v^3
gen v4=v^4

*local linear regression
reg y v   if x<10 
local b_l=_b[v]
reg y v  if x>=10  
local b_r=_b[v]
local tau=(`b_r'-`b_l')/(0-0.5)
di "Sharp RKD estimator is " `tau'

*Local quadratic
reg y v v2  if x<10 
local b_l=_b[v]
reg y v v2 if x>=10  
local b_r=_b[v]
local tau=(`b_r'-`b_l')/(0-0.5)
di "Sharp RKD estimator is " `tau'

*sharp RKD, using option scalepar(-2) to scale up the result, 
*the scale = 1/derivative change of treatment variable on running varaible between right and left.
rdrobust y x, c(10) kernel(uni) all deriv(1)
rdrobust y x, c(10) kernel(uni) all deriv(1) scalepar(-2)
*(2) Fuzzy RKD
clear
set obs 10000

local x0 = 10

set seed 2345

gen e=rnormal()
gen x=rnormal(10,2)
gen d=min(0.5*x+e, 0.5*`x0'+e)
gen y=2*d-0.5*x+rnormal()

*keep if x>=7 & x<=13
*estimation
gen v=x-`x0'
gen v2=v^2
gen v3=v^3
gen v4=v^4
reg y v v2   if x<10
gen b_l=_b[v]
reg y v v2  if x>=10
gen b_r=_b[v]

reg d v v2   if x<10
gen k_l=_b[v]
reg d v v2  if x>=10
gen k_r=_b[v]

gen tau=(b_r-b_l)/(k_r-k_l)
di "Fuzzy RKD estimator is " tau

rdplot y x if x>=3, c(10) p(1) graph_options(title("") legend(off) xtitle(X) ytitle(Y) )


rdplot d x if x>=3, c(10) p(1) graph_options(title("") legend(off) xtitle(X) ytitle(D) )


rdrobust y x, c(10) kernel(uni) all fuzzy(d) deriv(1)

