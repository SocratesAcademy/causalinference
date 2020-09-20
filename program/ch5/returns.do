*﻿Modified 2019/9/1 by Xiliang Zhao
*Estimating returns to eduction in urban China, Using CHIP2002.
cd "PATH" // 将PATH改成数据所在目录
set more off

use chip2002, clear

*the basic Mincer's model
qui reg lwage educ exper expersq, vce(robust)
estadd local city "No"
estadd local industry "No"
eststo m1

qui reg lwage educ exper expersq male i.city, vce(robust)
estadd  local city "Yes"
estadd  local industry "No"
eststo m2

qui reg lwage educ exper expersq male i.city i.industry, vce(robust)
estadd  local city "Yes"
estadd  local industry "Yes"
eststo m3

qui reg lwage educ exper expersq male i.city i.industry, vce(cluster city)
estadd  local city "Yes"
estadd  local industry "Yes"
eststo m4


*将文件扩展名改为.rtf可以在word中打开
esttab m1 m2 m3 m4 using return.rtf, star(* .10 ** .05 *** .01) nogap /// 
keep(educ exper expersq male) b(%12.4f) se(%12.4f) ///
mtitle("OLS" "OLS" "OLS" "OLS") ///
stats(city industry N r2_a, fmt(%str3 %str3 %9.0f %9.4f)) replace

set more on
exit
