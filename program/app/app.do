*Stata 数据处理编程
/*
参考书：
Baum, Christopher. An Introduction to Stata
Programming, Stata Press, 2nd edition, 2006.

Cameron, Colin and Trivedi, Pravin. 
Microeconomietrics Using Stata, 
Stata Press, 2010.



第一节 常用命令
基本命令格式：
[prefix:] command [varlist][=exp][if]
[in][weight][using filename][,options]

第二节 do文件
1.注释
*星号：第一种方式
// 这是第二种方式
/* */ 第三种方式
*/

clear // 清除数据
cap eststo clear

sysuse ///
auto, ///
clear

sysuse /*
*/ auto, clear

#delimit ;
sysuse auto, clear;
list in 1/5; sum;
#delimit cr

/*2.两种函数：
r(), e()
return list
ereturn list
*/
sysuse auto, clear
sum mpg
ret list

reg mpg weight price, vce(robust)
eret list
/*3.两种宏:
全局宏：global, $
局部宏：local, `'
*/
global xlist "price weight"
di "$xlist"

reg mpg $xlist

local xlist2 "price weight"
di "`xlist2'"
reg mpg `xlist2'

/*4.三种循环语句
foreach
forvalues
while
*/

clear
set obs 100
set seed 1234
gen x1 = runiform()
gen x2 = runiform()
gen x3 = runiform()
gen x4 = runiform()

su

*foreach
cap drop sum
gen sum = 0
foreach var of varlist x1 x2 x3 x4 {
  di "`var'  " `var'
  replace sum = sum + `var'
}
di "The sum of x1-x4 = " sum
list sum in 1/5

replace sum = 0
local xs "x1 x2 x3 x4"
foreach var of local xs {
  qui replace sum = sum + `var'
  }
  di "The sum of x1-x4 = " sum
  
*forvalues
replace sum = 0
forvalues i=1(1)4 {
  qui replace sum = sum + x`i'
}
di "The sum of x1-x4 = " sum

forvalues i = 1 3 to 10 {
  di "`i'"
  }
  

*while
replace sum = 0
local i = 1
while `i' <= 4 {
   qui replace sum = sum + x`i'
   local i = `i' + 1
   }
di "The sum of x1-x4 = " sum

/*第三节 结果呈现利器：estout软件包
Ben Jann: 
http://repec.sowi.unibe.ch/stata/estout/
esttab, estout, eststo, estadd, estpost
安装：
ssc install estout, replace
*/

sysuse auto, clear
eststo m1: qui reg mpg weight price, vce(robust)
eststo m2: qui reg mpg weight price foreign, vce(robust)
esttab m1 m2

esttab m1 m2 using result.rtf, replace ///
b(%12.4f) se(%5.4f) ///
ar2(%12.4f) star(* .10 ** .05 *** .01) ///
mtitle("OLS1" "OLS2") nogap


*estadd
qui reg mpg weight price
estadd local Control = "No"
eststo m1

qui reg mpg weight price foreign
estadd local Control = "Yes"
eststo m2

esttab m1 m2,  b(%12.4f) se(%5.4f) ///
 ar2(%12.4f) star(* .10 ** .05 *** .01) ///
 mtitle("OLS1" "OLS2") ///
 stats(Control N r2_a, fmt(%3s %5.0f %5.4f))


esttab m1 m2 using result.rtf, replace ///
 b(%12.4f) se(%5.4f) ///
 ar2(%12.4f) star(* .10 ** .05 *** .01) ///
 mtitle("OLS1" "OLS2") ///
 stats(Control N r2_a, fmt(%3s %5.0f %5.4f))


 *estpost
 *estpost summarize
 estpost su mpg weight price foreign
 esttab ., cells("mean sd count") noobs nonumber ///
 nogap nomtitle
 *estpost tabstat
 estpost tabstat mpg weight price, by(foreign) ///
 s(mean sd) col(s)
 esttab ., main(mean) aux(sd) unstack noobs ///
 label nogap nonumber nomtitle
 
 
 
 
 
