library(foreign)
library(lmtest)
library(margins)
library(MASS)
library(plm)
library(ordinal)

options(digits = 7)
setwd("~/Documents/MSE/GSE526")
d = read.dta("fl89-91eco526W16.dta")
d = d[,-6]
d = cbind(d[,1:2], dmage2 = d[,2]^2, d[,3:11])
zeros = numeric(nrow(d)) #initiates a vector of zeros
ones = numeric(nrow(d))+1 #initiates a vector of zeros
#Below are the answers to 3a)
probit = glm(dead~alcohol+dmage+dmage2+dmar+dmeduc+foreignb+tobacco+mblack+motherr+mhispan,
              family = binomial(link = "probit"), data = d)
(summary(probit))

# From the coefficients and standard errors in the regression output, it appears that education and foreign birth 
# have a significant negative effect on infant mortality. In other words, if a mother is more educated or she is foreign born,
# there is a smaller chance of her baby dying, all else equal. On the flip side, being unmarried,
# black, neither black nor white, and consuming tobacco or alcohol during pregnancy all have a significant positive effect
# on infant mortality. All else equal, having one of these attributes will increase the chances of the mother's baby dying
# Age and whether or not the mother is hispanic do not appear to have a significant effect on infant mortality

#Below aare the answers to 3b)

waldpr = (waldtest(probit)) #requires lmtest package
  #^the wald test on this probit model gives an F statistic of 128.8 which is significant at at <0.001 significance level

betap = probit$coefficients[-1] #gets estimated betas (w/o constant)
valsp = pnorm(as.matrix(d[,1:10])%*%betap) #multiplies betas by given data
predictp = numeric(nrow(valsp))

for(i in 1:length(predictp)){ #predicts dead = 1 if x'beta is greater than 0.5
  if(valsp[i]>0.5){
    predictp[i] = 1
  }
}
boundp = cbind(d$dead,predictp) 
boundp = boundp[which(boundp[,1]==boundp[,2]),]

perccorrectp = length(boundp[,1])/length(d$dead)
(perccorrectp)
#below are answers to 3c)
pdfp = mean(dnorm(cbind(ones,as.matrix(d[,-c(11,12)]))%*%probit$coefficients)) #calculates mean of density of all x'betas
avgeffectsp = pdfp*probit$coefficients #multiplies mean by beta hat

(margins(probit,factors = "continuous", atmeans = F,type = "response")[[1]]) #uses a function to get the same result

#below are answers to 3d)
means = c(1,colMeans(d[,1:10])) #calculataes means of each independent variable
effatmean = dnorm(t(as.matrix(means))%*%probit$coefficients)*probit$coefficients #calculates effects at the mean

(margins(probit,factors = "continuous", atmeans = TRUE,type = "response")[[1]])
  #^uses built in function to get the same result

#   the effects at the mean are smaller than the average effects calculated above, but barely. Since the estimates
#   of the effect of the mean are misleading and do not differ very much from the mean effect, 
#   I would prefer to use the mean effect rather than the effect at the mean.

#below are answers to 3e)
alcp = probit$coefficients[2]*ones #creates a vector of betas for alcohol
tobacp = probit$coefficients[8]*ones #creates a vector of betas for tobacco
noalc = cbind(ones,d[,-c(1,11,12)]) #creataes a data frame that doesn't include alcohol
notob = cbind(ones,d[,-c(8,11,12)])
acteffalcp = mean(pnorm(as.matrix(noalc)%*%as.matrix(probit$coefficients[-c(2)])+alcp)-
                   pnorm(as.matrix(noalc)%*%probit$coefficients[-c(2)]))
  #^calculates actual effect of alcohol using the formula from class
actefftobacp = mean(pnorm(as.matrix(notob)%*%as.matrix(probit$coefficients[-c(8)])+tobacp)-
                     pnorm(as.matrix(notob)%*%probit$coefficients[-c(8)]))
  #^calculates actual effect of tobacco using the formula from class

(acteffalcp)
(actefftobacp)

(margins(probit,factors = "discrete", atmeans = F,type = "response")[[1]])
  #^uses a built in function to get the same results

# In this particular application, the effect of alcohol and tobacco calculated using a continuous approximation
# understates the actual effect by a only very small amount, so the continuous approximation was a good estimation of the actual effect.


#-----PROBLEM 4 CODE BELOW-----#
#4 a)

logit = glm(dead~alcohol+dmage+dmage2+dmar+dmeduc+foreignb+tobacco+mblack+tobacco+motherr+mhispan,
             family = binomial(link = "logit"), data = d)
(summary(logit))

pdfl = mean(dlogis(cbind(ones,as.matrix(d[,-c(11,12)]))%*%logit$coefficients))
avgeffectsl = pdfl*logit$coefficients
(avgeffectsl) #gives average effects with logistic assumption
(margins(logit,factors = "continuous", atmeans = F,type = "response")[[1]])
  #^calculates same effects using a function

waldl = waldtest(logit) #requires lmtest package

betal = logit$coefficients[-1] #gets estimated betas (w/o constant)
valsl = pnorm(as.matrix(d[,1:10])%*%betal) #multiplies betas by given data
predictl = numeric(nrow(valsl))

for(i in 1:length(predictl)){ #predicts dead = 1 if x'beta is greater than 0.5
  if(valsl[i]>0.5){
    predictl[i] = 1
  }
}
boundl = cbind(d$dead,predictl) 
boundl = boundl[which(boundl[,1]==boundl[,2]),]

perccorrectl = length(boundl[,1])/length(d$dead)
(perccorrectl)

alcl = logit$coefficients[2]*ones
tobacl = logit$coefficients[8]*ones

acteffalcl = mean(pnorm(as.matrix(noalc)%*%as.matrix(probit$coefficients[-c(2)])+alcl)-
                    pnorm(as.matrix(noalc)%*%probit$coefficients[-c(2)]))
actefftobacl = mean(pnorm(as.matrix(notob)%*%as.matrix(probit$coefficients[-c(8)])+tobacl)-
                      pnorm(as.matrix(notob)%*%probit$coefficients[-c(8)]))

(margins(logit,factors = "discrete", atmeans = F,type = "response")[[1]])
#4 b)
# The coefficients generated by the logit model are different from those of the probit model, owever, all logit coefficients have the same sign as well as 
# the same level of significance as their corresponding probit counterparts.

#The average effects calculated using the logit model also have the same signs as the probit average effects,
#but the logit average effects are slighlty larger in magnitude than their corresponding probit effects.

#4 c)
predicted = numeric(1)
i = min(d$dmage)
while(i <= max(d$dmage)){
  x00 = data.frame(alcohol = 0,dmage=i, dmage2=i^2,dmar=0,dmeduc=12,foreignb=0,tobacco=1,mblack=0,motherr=0,mhispan=0)
  prediction = predict(logit,x00, type = "response")
  predicted = c(predicted, prediction)
  i = i+1
}
predicted = predicted[-1]
(bestage = which(predicted == min(predicted))+min(d$dmage))

x00 = data.frame(alcohol = 0,dmage=29, dmage2=29^2,dmar=0,dmeduc=12,foreignb=0,tobacco=1,mblack=0,motherr=0,mhispan=0)
pred29 = predict(logit,x00,type="response",se.fit = T)

(CI4c = c(bestage-1.96*pred29$se.fit,bestage+1.96*pred29$se.fit))

# The method for estimating the best age for a mother to have a baby shown above makes use of a loop where the probability of infant mortality is calculated
# for each age between the min and max in the given data. The main issue with this method is that it assumes that all other covariates are equal to the ones
# given in part d). When trying to estimate this at the means of the covariates, I received errors in the predict function.
# Therefore, this result can be interpreted as the best age to have a baby if the mother has all the other given characteristics.
# There is also an isssue with the confidence interval that I calculated. This CI uses the standard errors on the probability of infant mortality given an age of 
# 29, which clearly is the wrong se to use. I was unable to back out this probability se to get one for age, so this CI is not very meaningful.


#4 d)

xo = c(1,0,25,25^2,1,12,0,1,0,0,0)

betal = logit$coefficients
probl = plogis(t(xo)%*%betal)
(probl)
x0 = data.frame(alcohol = 0,dmage=25, dmage2=25^2,dmar=1,dmeduc=12,foreignb=0,tobacco=1,mblack=0,motherr=0,mhispan=0)
pred = predict(logit,x0,type = "response",se.fit = TRUE)
(pred)
CI4d = c(probl-1.96*pred$se.fit, probl+1.96*pred$se.fit)
(CI4d)


#4 e)
effatmeanl = dlogis(t(as.matrix(means))%*%logit$coefficients)*logit$coefficients

x1 = data.frame(alcohol = 0,dmage=28, dmage2=28^2,dmar=0,dmeduc=17,foreignb=0,tobacco=0,mblack=0,motherr=0,mhispan=0)
pdfx1 = mean(dlogis(cbind(1,as.matrix(x1))%*%logit$coefficients))
avgeffectsx1 = pdfx1*logit$coefficients

(effatmeanl)
(avgeffectsx1)

# The marginal effects for a mother with covariates equal to the mean are higher than those of the mother given in the problem. In fact,
# they seem to be roughly twice as large.This is because there is more mass in the density function around the mean than there is at points that are farther away from the mean
# To get the asymptotic variance of the estimates of these effects, I would employ the delta method because we are looking for the 
# variance of a function of our parameters. This would involve taking the derivative of the pdf of a logistic distribution with respect to the parameters
# and squaring it, and then multiplying this function by 1/n.

#4 f)

lpm = lm(dead~alcohol+dmage+dmage2+dmar+dmeduc+foreignb+tobacco+mblack+motherr+mhispan, data = d )
coeftest(lpm, vcov = vcovHC(lpm, "HC1")) #computes robust standard errors
(avgeffectsp)
(avgeffectsl)

# Overall, the average effects of the covariates calculated using the linear probability model are actually very close to the marginal effects for both the probit
# and logit models. The coefficient estimates created in the linear probability model also share the same level of significance as the estimates created through probit and logit.
# It does appear that, on average, the linear probability model overstates the effects of the covariates, but only by a small amount.
# Though the difference does not seem very significant in this case, it may be that the difference between a linear probability model and probit/logit
# grows when the covariates have a larger effect on the outcome variable. Here, the covariates that we use to estimate infant mortality have very small effects because
# (1) infant mortality is very rare in the US (4790 of over 500k observations) so it is hard to find data that will predict it and (2) there are a huge range of factors
# not considered in the model that may also contribute. Had we modeled an outcome that could be boiled down to only a few contributing factors,
# we may expect that the disparity between estimates in the linear probability model and probit/logit may be greater.

lpmeffatmean = means*lpm$coefficients
lpmeffx1 = cbind(1,x1)*lpm$coefficients
(lpmeffatmean)
(effatmeanl)
(lpmeffx1)
(avgeffectsx1)

# It appears that once we try to calculate marginal effects given a certain set of characteristics, the linear probability model starts to fail a bit more.
# The LPM overstates the effects quite a bit given covariates equal to the average, and fails to create marginal effects for many covariates given the "x1" characteristics
# because some of these characteristics took a value of zero. This resulted in multiplying the estimated coefficient by zero, which ultimately suggests that 
# not drinking alcohol, smoking, not being black, being married etc. has no effect on infant mortality. This clearly is not accurate because these factors would surely have some effect,
# however small or insignificant, and in fact they do based on the estimates from our other models.

xo = c(1,0,25,25^2,1,12,0,1,0,0,0)
prob4f = sum(xo*lpm$coefficients)
(prob4f)

x0 = data.frame(alcohol = 0,dmage=25, dmage2=25^2,dmar=1,dmeduc=12,foreignb=0,tobacco=1,mblack=0,motherr=0,mhispan=0)
pred4f=predict(lpm,x0,type = "response",se.fit = T)

CI4f = c(prob4f-1.96*pred4f$se.fit,prob4f+1.96*pred4f$se.fit)
(CI4f)
(CI4d)
# The probability of infant mortality for a mother with the characteristics given in 4d) is roughly the same when predicted using a linear probability model as when
# using a logit model. The LPM slightly overstates the probability, however, and has a narrower CI because of its lower standard error estimate.

#Problem 6

#6 b)

ordered = polr(factor(adequacy, levels = c(0,1,2),ordered = T) ~alcohol+dmage+dmage2+dmar+dmeduc+foreignb+tobacco+mblack+motherr+mhispan,
              data = d, method = "probit") #requires "MASS" package, 
summary(ordered)

# Most of the coefficients from the regression output are negative, and it appears that all of the estimated coefficients are significant at some level.
# These coefficients can be interpreted as the effect on some unobserved "parental care index" given the covariate in question.
# For example, consuming alcohol during pregnancy is associated with a .238 decrease in the parental care index, which lowers the chances of being a 
# "good" or "average" parent during pregnancy, all else equal. An extra year of education is associated with a .0898 increase in the parental care index,
# which incrases the mother's chances of being a "good" parent (Y=2), all else equal.

#6 c)
xs = as.matrix(d[,-c(11,12)])
avgeffectso0 = mean(dnorm(ordered$zeta[1]-xs%*%as.matrix(ordered$coefficients)))*-(ordered$coefficients)

avgeffectso2 = mean(-dnorm(ordered$zeta[2]-xs%*%as.matrix(ordered$coefficients)))*-(ordered$coefficients)

avgeffectso1 = mean(dnorm(ordered$zeta[1]-xs%*%as.matrix(ordered$coefficients))
                    -dnorm(ordered$zeta[2]-xs%*%as.matrix(ordered$coefficients)))*ordered$coefficients
(avgeffectso0)
(avgeffectso1)
(avgeffectso2)

# The numbers provided by these calculations can be interpreted as a specific covariate's effect on the probability
# that a mother exhibited one of the three levels of parental care during pregnancy. For example, the number 0.03379
# in "avgeffectso0" tells us that a mother who drank during pregnancy had a 3% higher chance of being in the "0" category of parental care
# than a mother who did not. Overall, these effects reverse in sign as we move across adequacy categories. For example, being unmarried has a positive effect 
# on being a "bad" parent (0.06), but a large negative effect on being a "good" parent (-0.134). These make intuitive sense because
# one could assume that being married provides better parental support, so being unmarried reduces the ability to use such parental support, ultimately 
# decreasing the chances of being a "good" parent (Y =2). One more thing to note is that effects of covariates
# on being in category 1 are larger than the effects on being in category 0. This goes against intuition because one would think
# that, for example, smoking would increase the probability of being a "bad" parent more than it would increase the probability of being an "average" parent.

#6 d)

xoo = c(0,25,25^2,1,12,0,1,0,0,0)
betao = ordered$coefficients
probo = pnorm(ordered$zeta[2]-t(xoo)%*%betao)-pnorm(ordered$zeta[1]-t(xoo)%*%betao)
(probo)

predo = predict(ordered,x0,se.fit=T,type = "probs") 
(predo)

