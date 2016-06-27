library(sampleSelection)
library(foreign)
library(quantreg)
library(aod)
setwd("~/Documents/MSE/GSE526")
d = read.dta("paeco526W16_ps2.dta")

#Problem 1
#a)
fit1 = lm(dbirwt~tobacco,data = d)
(summary(fit1))

#b)
db = d[,-c(18,32,35)]#gets rid of "preterm", "deadkids", and "cdbirwt2500" covariates 

fit2 = lm(dbirwt~., data = db)
(summary(fit2))

#e)
doc = d[-35] #gets rid of "cdbirwt2500"
colnames(doc)
# 
# outcome1 = dbirwt~alcohol+anemia+cardiac+chyper+dfage+dfeduc+diabete+disllb+
#   dlivord+dmage+dmar+dmeduc+drink+foreignb+nprevist+pre4000+mblack+motherr+tobacco+
#   mhispan+fblack+fotherr+fhispan+adequac2+adequac3+tripre2+tripre3+tripre0+first+plural+dmage2
# 
# selection = tobacco~preterm+deadkids+alcohol+anemia+cardiac+chyper+dfage+dfeduc+diabete+disllb+
#   dlivord+dmage+dmar+dmeduc+drink+foreignb+nprevist+pre4000+mblack+motherr+
#   mhispan+fblack+fotherr+fhispan+adequac2+adequac3+tripre2+tripre3+tripre0+first+plural+dmage2
# 
# 
# fit3 = selection( selection,outcome1,data = doc)
# (summary(fit3))


#Problem 2
d1 = read.dta("paeco526W16_qr_ps3.dta")
#a)
fit3 = lm(dbirwt~tobacco,data = d1)
(summary(fit3))
CI1 = c(fit3$coefficients[2]-1.96*coef(summary(fit3))[2,2],fit3$coefficients[2]+1.96*coef(summary(fit3))[2,2])
(CI1)

fit4 = lm(dbirwt~.,data = d1)
(summary(fit4))
CI2 = c(fit4$coefficients[18]-1.96*coef(summary(fit4))[18,2],fit4$coefficients[18]+1.96*coef(summary(fit4))[18,2])
(CI2)

#b)
fit5 = rq(dbirwt~tobacco,tau=0.5, data = d1)
(summary(fit5))
CI3 = c(fit5$coefficients[2]-1.96*coef(summary(fit5))[2,2],fit5$coefficients[2]+1.96*coef(summary(fit5))[2,2])
(CI3)

#c)
fit6 = rq(dbirwt~.,tau=0.5,data=d1)
(summary(fit6))
CI4 = c(fit6$coefficients[18]-1.96*coef(summary(fit6))[18,2],fit6$coefficients[18]+1.96*coef(summary(fit6))[18,2])
(CI4)

#e)
set.seed(100)
n=nrow(d1)
est = matrix(nrow = 32,ncol = 10)
for(i in 1:10) {
  sam = sample(1:n, replace = TRUE)
  ds = d1[sam,]
  fit = rq(dbirwt~.,tau=0.5,data = ds)
  est[,i] = fit$coefficients
}
setob = sd(est[18,])
vcv = var(t(est))

#f)
tobac = fit6$coefficients[18]
alc = fit6$coefficients[2]
(test = (-300-tobac-alc)/(sqrt(setob^2+sd(est[2,]^2))))

#Problem 3)
#a)
(fit7 = rq(dbirwt~tobacco,tau=0.1,data = d1))
(fit8 = rq(dbirwt~tobacco,tau=0.25,data = d1))
(fit9 = rq(dbirwt~tobacco,tau=0.5,data = d1))
(fit10 = rq(dbirwt~tobacco,tau=0.75,data = d1))
(fit11 = rq(dbirwt~tobacco,tau=0.9,data = d1))

#c)
fit12 = rq(dbirwt~.,tau=0.1,data = d1)
fit13 = rq(dbirwt~.,tau=0.25,data = d1)
fit14 = rq(dbirwt~.,tau=0.5,data = d1)
fit15 = rq(dbirwt~.,tau=0.75,data = d1)
fit16 = rq(dbirwt~.,tau=0.9,data = d1)


t = anova(fit12,fit13,fit14,fit15,fit16,joint=FALSE)






