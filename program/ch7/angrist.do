***********************************
****Xiliang Zhao, 2016.11.24
****Modified in 2020.05.10
***********************************
clear
cap log close
set more off
log using QOB_V.log, replace 

cd "PATH" // Change to the fold where angrist.dta locate
use angrist, clear

preserve
g year = YOB + QOB*1/4-1/4 
collapse (mean) EDUC LWKLYWGE,by(year)
*two con EDUC year, xtitle("出生季度") ytitle("教育年限") saving(educ, replace)
two (sc EDUC year if year-int(year)<0.5, xtitle("出生季度") ytitle("教育年限") m(S))(sc EDUC year if year-int(year)>=0.5, xtitle("出生季度") ytitle("教育年限") m(Sh))(line EDUC year), legend(order(1 "1、2 季度" 2 "3、4 季度") pos(11) ring(0) cols(1)) saving(educ,replace)

*two con LWKLYWGE year, xtitle("出生季度") ytitle("对数周工资") saving(lwage,replace)
two (sc LWKLYWGE year if year-int(year)<0.5, xtitle("出生季度") ytitle("对数周工资") m(S))(sc LWKLYWGE year if year-int(year)>=0.5, xtitle("出生季度") ytitle("对数周工资") m(Sh))(line LWKLYWGE year), legend(order(1 "1、2 季度" 2 "3、4 季度") pos(11) ring(0) cols(1)) ylabel(5.87(0.01)5.93) xlabel(30(1)40) saving(lwage,replace)

restore
*use sample
** Col 1 3 5 7 ***
reg  LWKLYWGE EDUC  YR20-YR28, vce(robust) 
estadd local Year  "Yes"
estadd local Region "NO"
est sto ols1
reg  LWKLYWGE EDUC  YR20-YR28 AGEQ AGEQSQ, vce(robust) 
estadd local Year  "Yes"
estadd local Region "NO"
est sto ols2
reg  LWKLYWGE EDUC  RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT YR20-YR28,  vce(robust) 
estadd local Year  "Yes"
estadd local Region "Yes"
est sto ols3
reg  LWKLYWGE EDUC  RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT YR20-YR28 AGEQ AGEQSQ, vce(robust) 
estadd local Year  "Yes"
estadd local Region "Yes"
est sto ols4

** Col 2 4 6 8 ***
ivregress 2sls LWKLYWGE YR20-YR28 (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28),vce(robust) 
estadd local Year  "Yes"
estadd local Region "NO"
est sto iv1
ivregress 2sls LWKLYWGE YR20-YR28 AGEQ AGEQSQ (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28),vce(robust) 
estadd local Year  "Yes"
estadd local Region "NO"
est sto iv2
ivregress 2sls LWKLYWGE YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT  (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28),vce(robust) 
estadd local Year  "Yes"
estadd local Region "Yes"
est sto iv3
ivregress 2sls LWKLYWGE YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT AGEQ AGEQSQ (EDUC = QTR120-QTR129 QTR220-QTR229 QTR320-QTR329 YR20-YR28),vce(robust) 
estadd local Year  "Yes"
estadd local Region "Yes"
est sto iv4

esttab ols1 iv1 ols2  iv2 ols3  iv3 ols4  iv4 using angrist.rtf, ///
drop(YR20 YR21 YR22 YR23 YR24 YR25 YR26 YR27 YR28 NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT _cons) ///
 star(* .10 ** .05 *** .01) b(%8.4f) se(%8.4f) stats(Year Region N r2_a, fmt(%3s %3s %5.0f %5.4f)) ///
 mtitle("OLS1" "2SLS1" "OLS2" "2SLS2" "OLS3" "2SLS3" "OLS4" "2SLS4") nogap replace


log close
set more on
exit



