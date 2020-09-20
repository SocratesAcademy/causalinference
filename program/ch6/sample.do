*Chapter 6 Matching
*Modified by Xiliang Zhao 2019/9/2
*Simulation data
set more off
clear
set obs 100 // 产生100个观测
set seed 1234 // 设定种子
g x=10*runiform() // 产生[0,10]上的均匀分布
g y0 = 0.5 * x + rnormal() // y0=0.5x+u, u~N(01)
g y1 =  y0 + 5 + .5*x // y1 = y0 + 5 + .5x, 因而，y1-y0 =5 + 0.5x
g p = exp(x-5)/(1+exp(x-5))

g d= rbinomial(1,p)
g y=d*y1 +(1-d)*y0
gr two (sc y0 x)(sc y1 x)(function y = 0.5* x , range(0 10)) ///
(function y=5 + x, range(0 10)), legend(order(1 2))
graph save pop.gph, replace
more
sc y x ||lfit y x if d==1||lfit y x if d==0
gr save sample.gph, replace
save data_cia, replace
more

gr combine pop.gph sample.gph



/*估计
(1) Regression adjustment
*/
teffects ra (y x,linear)(d), ate  vce(robust) // 估计ATE
teffects ra (y x)(d), atet  vce(robust) // 估计ATT

teffects ra (y x)(d), atet  vce(robust) coeflegend
nlcom _b[ATET:r1vs0.d] / _b[POmean:0.d]


*(2) inverse probability weighting
teffects ipw (y)(d x, logit), ate  vce(robust) aeq
teffects ipw (y)(d x, logit), atet  vce(robust) aeq

*(3) Double robust estimator (AIPW)
teffects aipw (y x)(d x, logit),  vce(robust) aeq

*(4) wooldridge(2007) IPWRA

teffects ipwra (y x)(d x, logit),  ate vce(robust)

*(5)nnmatch
teffects nnmatch (y x) (d), atet nn(1) biasadj(x)

*(6) Ps match

teffects psmatch (y) (d x, logit), ate nn(1)

*Common support: teffects overlap
teffects ipwra (y x)(d x)
teffects overlap, ptl(1)

*balance test: 
teffects ipwra (y x)(d x)
tebalance summarize x, base
tebalance density x

teffects psmatch (y) (d x, logit), ate nn(1)
tebalance box x

set more on
exit
