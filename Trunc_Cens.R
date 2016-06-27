setwd("~/Documents/MSE/GSE526")
library(foreign)
library(truncreg)
library(censReg)
dtr = read.dta("paeco526W16_ps2trun.dta")
dfull = read.dta("paeco526W16_ps2.dta")

#Problem 3
dtr = dtr[,-c(18,32)] #gets rid of "preterm" and "deadkids" covariates which aren't mentioned in the assignment 
fit1 = lm(dbirwt~., data = dtr)
(summary(fit1))

trfit = truncreg(dbirwt~., data = dtr, point = 2500, direction = "left")
summary(trfit)


tobacest = trfit$coefficients[18]
tobacse = coef(summary(trfit))[18,2]
(tobacest)
(tobacse)

CItobtr = c(tobacest-tobacse*1.96,tobacest+tobacse*1.96)
(CItobtr)

#Problem 4
dc = dfull[,-c(5,18,32)] #gets rid of uncensored birthweight, "preterm" and "deadkids" covariates which aren't mentioned in the assignment 

fit2 = lm(cdbirwt2500~., data = dc)
(summary(fit2))

cnsfit = censReg(cdbirwt2500~., data = dc, left = 2500, right = Inf, method = "BHHH")
(summary(cnsfit))

tobacestcns = cnsfit$estimate[18]
tobacsecns = coef(summary(cnsfit))[18,2]
(tobacestcns)
(tobacsecns)

CItobcns = c(tobacestcns-tobacsecns*1.96,tobacestcns+tobacsecns*1.96)
(CItobcns)

#Problem 5

dfull = dfull[,-c(18,32,35)]#gets rid of "preterm", "deadkids", and "cdbirwt2500" covariates which aren't mentioned in the assignment 

fullfit = lm(dbirwt~., data = dfull)
(summary(fullfit))

tobacestfull = fullfit$coefficients[18]
tobacsefull = coef(summary(fullfit))[18,2]
(tobacestfull)
(tobacsefull)

CIfull = c(tobacestfull-tobacsefull*1.96,tobacestfull+tobacsefull*1.96)
(CIfull)

efftobac = c(fit1$coefficients[18], tobacest,fit2$coefficients[18],cnsfit$estimate[18],tobacestfull)
names(efftobac)= c("truncOLS", "truncModel", "censOLS", "censModel", "fullOLS")
(efftobac)
