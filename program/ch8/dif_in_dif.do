*！Xiliang Zhao 2016.12.08
*Modified by X. Zhao, 2020.05.20
set more off

cd "PATH" // Change to the location where cardkrueger1994.dta locates.
use cardkrueger1994,clear
des
sum

/*Regression DID*/
*Simple DID
*Y = a + bD + dT + tDT + e
reg fte i.t##i.treated, vce(robust)
eststo m1
*control time-invariant covariates
reg fte i.t##i.treated bk kfc roys, vce(robust)
eststo m2
*PSM-DID
logit treated bk kfc roys
predict ps, pr
reg fte i.t##i.treated ps, vce(robust)
eststo m3
*quantile DID
qreg fte i.t##i.treated bk kfc roys, vce(robust) q(.2)
eststo q1
qreg fte i.t##i.treated bk kfc roys, vce(robust) q(.5)
eststo q2
qreg fte i.t##i.treated bk kfc roys, vce(robust) q(.8)
eststo q3

esttab m1 m2 m3 q1 q2 q3, star(* .10 ** .05 *** .01) nogap b(%8.4f) se(%8.4f) ar2(%8.4f) keep(1.t#1.treated 1.t 1.treated bk kfc roys ps) mtitle("simple DID" "DID-x" "PSM-DID" "Q20-DID" "Q50-DID" "Q80-DID")

/*using command diff to do DID*/
*Simple DID
diff fte, t(treated) p(t) robust
eststo m4
diff fte, t(treated) p(t) robust cov(bk kfc roys)
eststo m5
esttab m4 m5, star(* .10 ** .05 *** .01) nogap se(%8.4f) ar2(%8.4f) mtitle("simple DID" "DID-x")

*bootstrap standard error
diff fte, t(treated) p(t) bs rep(50)
eststo m6
diff fte, t(treated) p(t) cov(bk kfc roys) bs rep(50)
eststo m7
*kernel matching-DID
diff fte, t(treated) p(t) cov(bk kfc roys) kernel rcs
eststo m8
*Quantile DID
diff fte, t(treated) p(t) cov(bk kfc roys) qdid(0.2)
eststo m9
diff fte, t(treated) p(t) cov(bk kfc roys) qdid(0.5)
eststo m10
diff fte, t(treated) p(t) cov(bk kfc roys) qdid(.8)
eststo m11

esttab m6 m7 m8 m9 m10 m11, star(* .10 ** .05 *** .01) nogap b(%8.4f) se(%8.4f) ar2(%8.4f) mtitle("simple DID" "DID-x" "Kernel DID" "Q20-DID" "Q50-DID" "Q80-DID")


