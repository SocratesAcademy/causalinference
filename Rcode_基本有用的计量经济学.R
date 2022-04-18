#=========================================================================#
#                           Author Information :                          #
# Name:  李建成 Li Jian-cheng                                             #
# Unit:  Sun Yat-Sen University                                           #
# Email: lijc53@mail2.sysu.edu.cn                                         #
# Address:  No. 135, Xingang Xi Road, Guangzhou, 510275, P. R. China      #
#                             STATEMENT:                                  #
# This code is just preparing for study and communication.                #
# It is not allowed for any commercial purposes.                          #
# There are any quesitons or problems, please cantact to author by e-mail. #
# The code is not perfect, it is welcome to argue.                        #
#=========================================================================#

setwd("E:\\EDUCATION\\STATA\\基本有用的计量经济学\\programs")
library(readstata13)                            # package for reading the data created by stata13~15
library(plyr)                                   # package for data


#-----------------------------------------------#
#-------Chapter5: Linear Model Regression-------#
#-----------------------------------------------#

chip2002 <- read.dta13("ch5\\chip2002.dta")
summary(chip2002)                                       # Statistical description; result1 in P73

chip2002$city <- as.factor(chip2002$city)
dummycity <- model.matrix(~city, data=chip2002)         # creat the dummy variables of city
chip2002$industry <- as.factor(chip2002$industry)
dummyindustry <- model.matrix(~industry, data=chip2002) # creat the dummy variables of industry

chip2002 <- cbind(chip2002,dummycity,dummyindustry)     # combine the chip2002 and the dummy variables 

linear_fit1 <- lm(lwage~educ+exper+expersq,data=chip2002) # 方程1：lwage=c+educ+exper+expersq+u1
summary(linear_fit1)       # result1 in P74
# Call:
#   lm(formula = lwage ~ educ + exper + expersq, data = chip2002)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.2345 -0.3229  0.0649  0.4098  2.7205 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.544e+00  4.259e-02  177.12  < 2e-16 ***
#   educ         8.734e-02  2.505e-03   34.86  < 2e-16 ***
#   exper        3.728e-02  2.940e-03   12.68  < 2e-16 ***
#   expersq     -3.619e-04  7.267e-05   -4.98 6.46e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6926 on 9256 degrees of freedom
# Multiple R-squared:  0.1584,	Adjusted R-squared:  0.1581 
# F-statistic: 580.7 on 3 and 9256 DF,  p-value: < 2.2e-16


#FWL定理--A simple proof

test_fit1 <- lm(lwage~exper+expersq,data=chip2002)   # 方程2：lwage=c+exper+expersq+u2
chip2002$fity <- test_fit1$residuals

test_fit2 <- lm(educ~exper+expersq,data=chip2002)    # 方程3：educ=c+exper+expersq+u3
chip2002$fitx <- test_fit2$residuals
test_fitFWL1 <- lm(fity~fitx,data=chip2002)          # 方程FWL1：u2=c+u3+u'
summary(test_fitFWL1)
# Call:
#   lm(formula = fity ~ fitx, data = chip2002)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.2345 -0.3229  0.0649  0.4098  2.7205 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.881e-16  7.196e-03    0.00        1    
# fitx         8.734e-02  2.505e-03   34.87   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6925 on 9258 degrees of freedom
# Multiple R-squared:  0.1161,	Adjusted R-squared:  0.116 
# F-statistic:  1216 on 1 and 9258 DF,  p-value: < 2.2e-16

test_fitFWL2 <- lm(lwage~fitx,data=chip2002)       # 方程FWL2：lwage=u3+u''
summary(test_fitFWL2)
# Call:
#   lm(formula = lwage ~ 0 + fitx, data = chip2002)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# 1.914  8.788  9.198  9.559 11.648 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# fitx  0.08734    0.03312   2.637  0.00838 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.156 on 9259 degrees of freedom
# Multiple R-squared:  0.0007504,	Adjusted R-squared:  0.0006425 
# F-statistic: 6.954 on 1 and 9259 DF,  p-value: 0.008379

#plot x & y
plot(chip2002$educ,chip2002$lwage)
plot(chip2002$fitx,chip2002$fity)


city <- names(chip2002)[16:76]
industry <- names(chip2002)[78:88]

linear_fit2 <- lm(lwage~educ+exper+expersq,data=chip2002)
linear_fit3 <- lm(lwage~educ+exper+expersq+male+city,data=chip2002)
linear_fit4 <- lm(lwage~educ+exper+expersq+male+city+industry,data=chip2002)  #result in P75



#--------------------------------------------------------------------------#
#-------Chapter6: Matching Method and Estimation of Treatment Effect-------#
#--------------------------------------------------------------------------#

library(Matching)
library(plm)
nsw_dw <- read.dta13("ch6\\nsw_dw.dta")
cps_controls <- read.dta13("ch6\\cps_controls.dta")
new_cps <- rbind(nsw_dw[nsw_dw$treat==1,],cps_controls)

##propensity score matching using nsw_dw
#ATT
logit_m1 <- glm(treat~age+education+black+hispanic+married+nodegree+re74+re75,data=nsw_dw,family = binomial(link = "logit"))
match_ATT1 <- Match(Y=nsw_dw$re78,Tr=nsw_dw$treat,X=logit_m1$fitted,estimand="ATT",M=1,replace=T) 
match_ATT_adj1 <- Match(Y=nsw_dw$re78,Tr=nsw_dw$treat,Z=nsw_dw$re75,X=logit_m$fitted,estimand="ATT",M=1,BiasAdjust = T,replace=T) 
summary(match_ATT1 )
summary(match_ATT_adj1 )

# kernel density graph
library(sm)
#before match
densitypscore1 <- data.frame(pscore=logit_m1$fitted,treat=nsw_dw$treat) 
sm.density.compare(densitypscore1$pscore,group = densitypscore1$treat)
# after match
logitpscore1 <- c(match_ATT1$mdata$X[1,],match_ATT1$mdata$X[2,])
xx<- vector(length=688)
for(i in 1:344){
  xx[i] <- 1
  xx[i+344] <- 0
}
densitypscore2 <- data.frame(pscore=logitpscore1,treat=xx) 
sm.density.compare(densitypscore2$pscore,group = densitypscore2$treat)


##propensity score matching using new_cps
#ATT
logit_m2 <- glm(treat~age+education+black+hispanic+married+nodegree+re74+re75,data=new_cps,family = binomial(link = "logit"))
match_ATT2 <- Match(Y=new_cps$re78,Tr=new_cps$treat,X=logit_m2$fitted,estimand="ATT",M=1,replace=T) 
logit_m3 <- glm(treat~age+education+black+hispanic+married+nodegree+re74+re75+I(married*re75)+I(age*re74),data=new_cps,family = binomial(link = "logit"))
match_ATT3 <- Match(Y=new_cps$re78,Tr=new_cps$treat,X=logit_m3$fitted,estimand="ATT",M=1,replace=T) 
summary(match_ATT2 )
summary(match_ATT3 )
# Cov Balance test
mb1  <- MatchBalance(treat~age+education+black+hispanic+married+nodegree+re74+re75, 
                    data=new_cps, match.out=match_ATT2, nboots=10)
mb2  <- MatchBalance(treat~age+education+black+hispanic+married+nodegree+re74+re75+I(married*re75)+I(age*re74), 
                    data=new_cps, match.out=match_ATT3, nboots=10)


# use package matchit to update data after match, DATA nsw_dw
library(MatchIt)
res_nsw_dw <- matchit(treat~age+education+black+black+hispanic+married+nodegree+re74+re75,
                      data=nsw_dw,method="nearest",ratio=1)                      # matching
library(Zelig)
z.out <- zelig(re78~treat+age+education+black+black+hispanic+married+nodegree+re74+re75,data=nsw_dw,model="ls")
xtreat <- setx(z.out,treat=1)
xcontrol <- setx(z.out,treat=0)
ATE <- sim(z.out,x=xcontrol,x1=xtreat)
summary(ATE)

res_nsw_dwA <- match.data(res_nsw_dw)                     # ALL DATA after match
res_nsw_dwT <- match.data(res_nsw_dw,group = "treat")     # Treated group 
res_nsw_dwC <- match.data(res_nsw_dw,group = "control")   # Controls group after match

#-------------------------------------#
#-------Chapter7: IV estimation-------#
#-------------------------------------#

library(AER)        # package created by Christian Kleiber & Achim Zeileis 
                    # the authors of 《Applied Econometrics with R》
angrist <- read.dta13("ch7\\angrist.dta")
iv_fit_QTR1 <- ivreg(LWKLYWGE~EDUC|QTR1,data=angrist)       #iv is QTR1 and result in P142
     # first stage reg by ols:
     # iv_fit_QTR1_freg <- lm(EDUC~QTR1,data=angrist)
     # summary(iv_fit_QTR1_freg)    
     # first table in the p142
iv_fit_QTR4 <- ivreg(LWKLYWGE~EDUC|QTR4,data=angrist)       #iv is QTR4 and result in P142

iv_fit_QTR123 <- ivreg(LWKLYWGE~EDUC|QTR1+QTR2+QTR3,data=angrist)    #iv is QTR1,QTR2,QTR3 
summary(iv_fit_QTR123)                                                     #result in P143

# Estimation of 7.3
angrist$year <-  angrist$YOB+(angrist$QOB-1)/4
   
plotdata <- angrist[,c(65,3,6)]                              
plotdata_new <- ddply(plotdata,.(year),colwise(mean))
plot(plotdata_new$year,plotdata_new$EDUC,type = "b")                 # figure 7.2
plot(plotdata_new$year,plotdata_new$LWKLYWGE,type = "b")             # figure 7.3
       # another method for graphing
       # library(ggplot2)
       # ggplot(data=plotdata_new,aes(year,EDUC))+geom_point(colour=year)

## REG discrebed in Table in P145
# Colume 1、3、5、7
colume1_OLS <- lm(LWKLYWGE~EDUC+YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)
colume3_OLS <- lm(LWKLYWGE~EDUC+AGEQ+AGEQSQ
                  +YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)
colume5_OLS <- lm(LWKLYWGE~EDUC+RACE+MARRIED+SMSA
                  +NEWENG+MIDATL+ESOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT
                  +YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)
colume7_OLS <- lm(LWKLYWGE~EDUC+AGEQ+AGEQSQ+RACE+MARRIED+SMSA
                  +NEWENG+MIDATL+ESOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT
                  +YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)

# Colume 2、4、6、8
colume2_iv<- ivreg(LWKLYWGE~EDUC
                   +YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28|.-EDUC    #multi-variablies,claim the edengous EDUC
                   +QTR120+QTR121+QTR122+QTR123+QTR124+QTR125+QTR126+QTR127
                   +QTR128+QTR129+QTR220+QTR221+QTR222+QTR223+QTR224+QTR225
                   +QTR226+QTR227+QTR228+QTR229+QTR320+QTR321+QTR322+QTR323
                   +QTR324+QTR325+QTR326+QTR327+QTR328+QTR329+YR20+YR21+YR22
                   +YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)
colume4_iv<- ivreg(LWKLYWGE~EDUC+AGEQ+AGEQSQ
                   +YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28|.-EDUC
                   +QTR120+QTR121+QTR122+QTR123+QTR124+QTR125+QTR126+QTR127
                   +QTR128+QTR129+QTR220+QTR221+QTR222+QTR223+QTR224+QTR225
                   +QTR226+QTR227+QTR228+QTR229+QTR320+QTR321+QTR322+QTR323
                   +QTR324+QTR325+QTR326+QTR327+QTR328+QTR329+YR20+YR21+YR22
                   +YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)
colume6_iv<- ivreg(LWKLYWGE~EDUC+RACE+MARRIED+SMSA
                   +NEWENG+MIDATL+ESOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT
                   +YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28|.-EDUC
                   +QTR120+QTR121+QTR122+QTR123+QTR124+QTR125+QTR126+QTR127
                   +QTR128+QTR129+QTR220+QTR221+QTR222+QTR223+QTR224+QTR225
                   +QTR226+QTR227+QTR228+QTR229+QTR320+QTR321+QTR322+QTR323
                   +QTR324+QTR325+QTR326+QTR327+QTR328+QTR329+YR20+YR21+YR22
                   +YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)
colume8_iv<- ivreg(LWKLYWGE~EDUC+AGEQ+AGEQSQ+RACE+MARRIED+SMSA
                   +NEWENG+MIDATL+ESOCENT+WNOCENT+SOATL+ESOCENT+WSOCENT+MT
                   +YR20+YR21+YR22+YR23+YR24+YR25+YR26+YR27+YR28|.-EDUC
                   +QTR120+QTR121+QTR122+QTR123+QTR124+QTR125+QTR126+QTR127
                   +QTR128+QTR129+QTR220+QTR221+QTR222+QTR223+QTR224+QTR225
                   +QTR226+QTR227+QTR228+QTR229+QTR320+QTR321+QTR322+QTR323
                   +QTR324+QTR325+QTR326+QTR327+QTR328+QTR329+YR20+YR21+YR22
                   +YR23+YR24+YR25+YR26+YR27+YR28,data=angrist)



#---------------------------------------------#
#-------Chapter8: Mehtods of Panel Data-------#
#---------------------------------------------#

# DiD
cardkrueger <- read.dta13("ch8\\cardkrueger1994.dta")
cardkrueger$treat <- ifelse(cardkrueger$treated=="NJ",cardkrueger$treat <- 1,cardkrueger$treat <- 0)
Did <- lm(fte~treat+t+I(treat*t),data=cardkrueger)
summary(Did)   # or Did$coefficients

# Synth
library(Synth)                                                     # Package created by Jens Hainmueller and Alexis Diamond
smoking <- read.dta13("ch8\\smoking.dta")
smoking$id <- as.numeric(smoking$state)
smoking$name <- as.character(smoking$state)
smoke_synth <- dataprep(
  foo =  smoking,
  predictors = c("lnincome","age15to24","retprice"),               # set other variables which time ranging is 1980:1988
  predictors.op = "mean",
  dependent = "cigsale",
  unit.variable = "id",
  time.variable = "year",
  special.predictors = list(
    list("cigsale", c(1975,1980,1988), "mean"),                    # set vatiables which time ranging is different from 1980:1988
    list("beer", c(1984:1988), "mean")),
  treatment.identifier = 3,                                        # id of treatment
  controls.identifier = c(1,2,4:39),
  time.predictors.prior =c(1980:1988),
  time.optimize.ssr = c(1970:1987),                                # MSPE 
  unit.names.variable = "name",
  time.plot = 1970:2000                                            # Time 
)
synth.out <- synth(data.prep.obj = smoke_synth)
synth.tables <- synth.tab(
  dataprep.res = smoke_synth,
  synth.res = synth.out
) 

# results tables:
print(synth.tables)

# plot results:
# path
path.plot(synth.res = synth.out,
          dataprep.res = smoke_synth,
          Legend = c("Basque country","synthetic Basque country")) 

## gaps
gaps.plot(synth.res = synth.out,
          dataprep.res = smoke_synth)
#####NoteS: the result is differnt from the book . I do not know the reason.

#-------------------------------------------------#
#-------Chapter9: Regression Discontinuity--------#
#-------------------------------------------------#

library(rdrobust)         
lee <- read.dta13("ch9\\lee.dta")

# Replicat the Result in Page220
Lee_1 <- lee[lee$margin >= -0.28 & lee$margin <= 0.28,]
Lee_1$dd <- ifelse(Lee_1$margin>0,Lee_1$d <- 1,Lee_1$d <- 0)   # Creat Rdd
summary(lm(vote~dd,data=Lee_1))                            #Result of Colume1
summary(lm(vote~dd+margin+I(dd*margin),data=Lee_1))        #Result in Colume2


# Replicat the Result in Page221
Lee_2 <- lee[lee$margin >= -0.5 & lee$margin <= 0.5,]
Lee_2$dd <- ifelse(Lee_2$margin>0,Lee_2$d <- 1,Lee_2$d <- 0)   # Creat Rdd
summary(lm(vote~dd+margin+I(dd*margin),data=Lee_2))            # Result of Colume1
summary(lm(vote~dd+margin+I(margin^2)+I(dd*margin)+I(dd*margin^2),data=Lee_2))  # Result of Colume2
summary(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3),data=Lee_2))  # Result of Colume3
summary(lm(vote~dd+margin+I(margin^2)+I(margin^3)+I(dd*margin)+I(dd*margin^2)+I(dd*margin^3)+I(dd*margin^4)+I(dd*margin^4),data=Lee_2))  # Result of Colume4


# Figure 9.12 using data Lee_2 in Page218
rdplot(y = Lee_2$vote, x = Lee_2$margin, nbins=50, p=2,      # Defult is p=4 
        x.lim=c(-0.5,0.5),
        y.lim=c(0.2,0.8),col.lines ="black",
        x.label = 'Democratic Vote Share Margin of Victory, Election t',
        y.label = 'Vote Share, Election t+1')

# Figure 9.13 in Page223
Lee_2_test1 <- Lee_2[Lee_2$margin < 0,]
Lee_2_test2 <- Lee_2[Lee_2$margin >= 0,]
rdplot(y = Lee_2_test1$vote, x = Lee_2_test1$margin, c=-0.25,p=4,
        x.lim=c(-0.5,0),y.lim=c(0.2,0.5),
        x.label=c("断点为-0.25"),col.lines ="black")
rdplot(y = Lee_2_test2$vote, x = Lee_2_test2$margin, c=0.25,p=4,
        x.lim=c(0,0.5),y.lim=c(0.5,0.75),
        x.label=c("断点为0.25"),col.lines ="black")

# Figure 9.8 in Page 206 and code edited by stata in Page219.
rdbwselect_2014(y = Lee_2$vote, x = Lee_2$margin, c=0, 
                kernel = "uniform", bwselect = "CV", 
                cvgrid_min = 0.1, cvgrid_max = 0.5, cvplot = TRUE)
                # use rdbwselect_2014 rather rdbwselect
                # Result shows the best bindwith is 0.28

# Result in Page226.
summary(rdbwselect(y = Lee_2$vote, x =Lee_2$margin, kernel = "uniform", c=0, all = T))
                
# Replicat the Result in Page227
summary(rdrobust(y = Lee_2$vote, x = Lee_2$margin,c=0, kernel = "uniform", all=T))

# Replicat the Result in Page228
set.seed(123)
x <- rnorm(10000,mean=10,sd=2)   
e <- rnorm(10000,mean=0,sd=1)
u <- rnorm(10000,mean=0,sd=1)
for(i in 1:10000){
  D[i] <- min(0.5*x[i]+u[i],5+u[i])
  Y[i] <- 2*D[i]-0.5*x[i]+e[i]
  }
rdplot(y = D, x = x, c=10, p=2,x.lim=c(0,20),y.lim=c(0,6),col.lines ="black",
       x.label = "x",y.label = "D")    # Figure 228 a
rdplot(y = Y, x = x, c=10, p=2,x.lim=c(0,20),y.lim=c(-2,6),col.lines ="black",
       x.label = "x",y.label = "Y")   # Figure 228 b

summary(rdrobust(Y, x, c=10, kernel = "uniform", fuzzy=D, deriv=1, p=2, all=T))

#-------------------------------------------------#
#-------Chapter10: Bunching-----------------------#
#-------------------------------------------------#

library(bunchr)


##-----------------------------------------------------Analyzing a kink
set.seed(42)
ability_vec <- 4000 * rbeta(100000, 2, 5)                           #生成模拟数
earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0, 0.1, 0, 1000)
# earning_fun(n, elas, t1, t2, Tax, zstar)
# Arguments
# 
# n	    Ability of person (earnings with zero tax)
# elas	elasticity of earnings w.r.t. net-of-tax rate
# t1	  Tax rate before notch/kink
# t2	  Tax rate after notch/kink
# Tax	  height of notch (zero for pure kink)
# zstar	place of notch/kink (critical earning point)

bunch_viewer(earning_vec, 1000, cf_start = 10, cf_end = 10, exclude_before = 2,
             exclude_after = 2, binw = 50, trimy=F)               #图示
kink_est1 <- bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                  cf_start = 10, cf_end = 10,
                  exclude_before = 2, exclude_after = 2, binw = 50, nboots = 100)   ##已自动生成反事实数据
# Usage
# 
# bunch(earnings, zstar, t1, t2, Tax = 0, cf_start = NA, cf_end = NA,
#       exclude_before = NA, exclude_after = NA, force_after = FALSE,
#       binw = 10, poly_size = 7, convergence = 0.01, max_iter = 100,
#       correct = TRUE, select = TRUE, draw = TRUE, nboots = 0, seed = NA,
#       progress = FALSE, title = "Bunching Visualization",
#       varname = "Earnings")
# Arguments
# 
# earnings	      Vector of earnings, hopefully a very large one.
# zstar	          Place of kink (critical earning point).
# t1	            Marginal tax rate before kink.
# t2	            Marginal tax rate after kink.
# Tax	            "Penalty" tax for crossing zstar.
# cf_start	      Number of bins before the kink bin where counter-factual histogram should start.
# cf_end	        Number of bins after the kink bin where counter-factual histogram should start.
# exclude_before	Number of excluded bins before the kink bin.
# exclude_after	  Number of excluded bins after the kink bin.
# force_after	    For notch analysis, should bunch be forced to use of the provided exclude_after for the end of the bunching, rather than trying to find the bin where the sum of the integral is zero? See details at notch_estimator documentation.
# binw	          Bin width.
# poly_size	      Order of polynomial used to calculate counter-factual histogram.
# convergence	    Minimal rate of change of bunching estimate to stop iterations.
# max_iter	      Maximum number of iterations for bunching estimates.
# correct	        Should the counter-factual histogram be corrected to compensate for shifting left because of the notch? See details.
# select	        Should model selection be used to find counter-factual histogram? See details.
# draw	          Should a graph be drawn?
# nboots	        how many bootstraps should be run?
# seed	          specify seed for bootstraps (earnings sampling).
# progress	      Should a progress bar be desplayed?
# title	          Title for Plot output
# varname	        Name for running variable, to be desplayed in the plot



kink_est1$e
## [1] 0.186233
quantile(kink_est1$booted_e, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##         0%         5%        10%        50%        90%        95% 
## 0.09330628 0.11666582 0.13313885 0.18567137 0.23956820 0.25130516 
##       100% 
## 0.32462423
kink_est1$Bn
## [1] 1170.216
quantile(kink_est1$booted_Bn, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##        0%        5%       10%       50%       90%       95%      100% 
##  599.0476  744.2115  851.2940 1166.8193 1481.8489 1541.6775 1939.3720

#show Counter-Factuals      
kink_est1$data$cf_counts


###调整带宽
kink_est2 <- bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                  cf_start = 500, cf_end = 500,
                  exclude_before = 10, exclude_after = 10, binw = 1,
                  nboots = 100, seed = 123, draw = F)
kink_est2$e
## [1] 0.1932915
quantile(kink_est2$booted_e, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##        0%        5%       10%       50%       90%       95%      100% 
## 0.1770582 0.1802953 0.1834727 0.1915006 0.2035105 0.2061229 0.2116499
kink_est2$Bn
## [1] 1223.928
quantile(kink_est2$booted_Bn, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##       0%       5%      10%      50%      90%      95%     100% 
## 1127.625 1143.042 1164.198 1219.175 1284.605 1304.872 1332.322

#show Counter-Factuals      
kink_est2$data$cf_counts

##-----------------------------------------------------Analyzing a notch
earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0.1, 0.1, 200, 1000)
bunch_viewer(earning_vec, 1000, 20, 50, 2, 25, binw = 20)
notch_est <- bunch(earning_vec, zstar = 1000, t1 = 0.1, t2 = 0.1, Tax = 200,
                   cf_start = 20, cf_end = 50, force_after = FALSE,
                   exclude_before = 2, exclude_after = 25, binw = 20,
                   nboots = 100, seed = 123)
notch_est$e
## [1] 0.1843316
quantile(notch_est$booted_e, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##        0%        5%       10%       50%       90%       95%      100% 
## 0.1203677 0.1407937 0.1621033 0.2074509 0.2074509 0.2074509 0.2074509

ability_vec_small <- 4000 * rbeta(10000, 2, 5)
earnings_small <- sapply(ability_vec_small, earning_fun, 0.2, 0, 0.1, 0, 1000)
kink_est <- bunch(earnings_small, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                  cf_start = 10, cf_end = 10,
                  exclude_before = 1, exclude_after = 1, binw = 50,
                  nboots = 100, seed = 123)
kink_est$e
## [1] 0.1662341
quantile(kink_est$booted_e, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##         0%         5%        10%        50%        90%        95% 
## 0.04387312 0.07718377 0.09195058 0.17941217 0.28779019 0.30870107 
##       100% 
## 0.39275768

##大样本
ability_vec_large <- 4000 * rbeta(1000000, 2, 5)
earnings_large <- sapply(ability_vec_large, earning_fun, 0.2, 0, 0.1, 0, 1000)
kink_est <- bunch(earnings_large, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                  cf_start = 50, cf_end = 50,
                  exclude_before = 5, exclude_after = 5, binw = 10,
                  nboots = 100, seed = 123)
kink_est$e
## [1] 0.183655
quantile(kink_est$booted_e, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##        0%        5%       10%       50%       90%       95%      100% 
## 0.1688440 0.1742967 0.1772704 0.1850824 0.1928552 0.1942905 0.1970645

kink_est <- bunch(earnings_large, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                    cf_start = 50, cf_end = 50,
                    exclude_before = 1, exclude_after = 1, binw = 5,
                    nboots = 100, seed = 123, draw = F)
kink_est$e
## [1] 0.1949573
quantile(kink_est$booted_e, probs=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))
##        0%        5%       10%       50%       90%       95%      100% 
## 0.1879073 0.1901380 0.1915570 0.1955756 0.1988548 0.2003031 0.2025746


#---------------------------------#
#-------Important REFERENCE-------#
#---------------------------------#

# [1]Abadie A, Diamond A, Hainmueller J. Synth: An R Package for Synthetic Control Methods in Comparative Case Studies[J]. Journal of Statistical Software, 2011, 42(13):1-17.
# [2]Calonico, S., M. D. Cattaneo, and R. Titiunik. rdrobust: An R Package for Robust Nonparametric Inference in Regression-Discontinuity Designs. R Journal, 2015b, 7(1): 38-51.