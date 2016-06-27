library(foreign)
library(stargazer)
library(reshape2)
library(pander)
library(psych)
library(nonrandom)
library(Matching)
setwd("~/Documents/MSE/GSE526")
d = read.dta("paeco526_W16_final.dta")

#Problem 1
ols1 = lm(dbirwt~tobacco, data=d)
(summary(ols1))
ols2 = lm(dbirwt~., data = d)
(coef(summary(ols2))[18,])

#Problem 2
dtob = d[which(d$tobacco==1),]
dtob = dtob[,-18]
dno = d[which(d$tobacco==0),]
dno = dno[,-18]
mtob = colMeans(dtob)
mno = colMeans(dno)
mdiff = mtob-mno

ts = numeric(ncol(dtob))
pvs = numeric(ncol(dtob))
for(i in 1:ncol(dtob)){
  t = t.test(dtob[,i],dno[,i],mu=0,conf.level = .95)
  ts[i] = t$statistic
  pvs[i] = t$p.value
}

p2t = data.frame(cbind(mtob, mno, mdiff, ts, pvs))
p2t = p2t[-5,]
colnames(p2t) = c("Mean Tobacco","Mean No Tobacco","Difference in Means","Test Statistic","Pr>|t|")
stars = add.significance.stars(p2t[,5])
p2t[,3] = as.character(p2t[,3])
p2t[,3] = strtrim(p2t[,3],width = 7)
p2t[,3] = paste(p2t[,3],stars,sep="")

#Problem 4

lgt = glm(tobacco~. -dbirwt, family = binomial(link = "logit"), data = d)
  #logit of covariates on tobacco (excludes birthweight)
(summary(lgt))
lcfs = lgt$coefficients

#pscores can be calculated as:
ps = as.matrix(cbind(numeric(nrow(d))+1,d[,-c(5,18)]))%*%as.matrix(lcfs)
ps = as.matrix(predict(lgt,type="response"))
  #both of these methods give equivalent results


#Problem 5
pst = ps[which(d$tobacco==1)]
psnt = ps[which(d$tobacco==0)]
pwt = cbind(ps, d$tobacco)

hist(pst, breaks = 75, freq=F,col = rgb(0,0,1,1/2))
hist(psnt,breaks = 75,freq=F,col = rgb(1,0,0,1/2),add=T)


Nobst = length(pst)
Nobsnt = length(psnt)
print(paste("Number of non-smokers:",Nobsnt))
print(paste("Number of smokers:",Nobst))

#quantiles (non-smoker vs smoker)
(quantile(pst, c(.01,.05,.1,.25,.5,.75,.9,.95,.99)))
(quantile(psnt, c(.01,.05,.1,.25,.5,.75,.9,.95,.99)))

#other descriptive stats (top row is non-smoker)
(desc = describeBy(pwt,pwt[,2]))

#Problem 6
mint = min(pst)
maxnt = max(psnt)
cpst = pst[which(pst<=maxnt)]
cpsnt = psnt[which(psnt>=mint)]
cps = c(cpst,cpsnt)

#Problem 7
psov = ps[which(ps>=mint)]
psov = psov[which(psov<=maxnt)]
dov = d[which(ps>=mint),]
dov = dov[which(psov<=maxnt),]

dovtob = dov[which(dov$tobacco==1),]
psovt = psov[which(dov$tobacco==1)]
dovtob = dovtob[,-18]
dovtob = dovtob/psovt

dovno = dov[which(dov$tobacco==0),]
psovnt = psov[which(dov$tobacco==0)]
dovno = dovno[,-18]
dovno = dovno/(1-psovnt)
movtob = colMeans(dtob)
movno = colMeans(dno)
movdiff = as.matrix(mtob-mno)


tovs = numeric(ncol(dovtob))
povvs = numeric(ncol(dovtob))
for(i in 1:ncol(dovtob)){
  t = t.test(dovtob[,i],dovno[,i],mu=0,conf.level = .95)
  tovs[i] = t$statistic
  povvs[i] = t$p.value
}

p7t = data.frame(cbind(movtob, movno, movdiff, tovs, povvs))
p7t = p7t[-5,]
colnames(p7t) = c(".Mean Tobacco      .","Mean No Tobacco      .","Difference in Means      .","Test Statistic      .","Pr>|t|   ")
stars = add.significance.stars(p7t[,5])
p7t[,3] = as.character(p7t[,3])
p7t[,3] = strtrim(p7t[,3],width = 7)
p7t[,3] = paste(p7t[,3],stars,sep="")

p7t = stargazer(p7t, type = "html",title = "Summary Mean Characteristics Overlap Region",summary = F,digits = 4,column.sep.width = "50pt")

#Problem 8
pscores = pscore(data = d, formula = tobacco~. -dbirwt) #one of three methods used above to get pscores

xs = terms(tobacco~. -dbirwt, data = d)
mat = Match(Y = d$dbirwt, Tr = d$tobacco, X = ps, estimand = "ATE", M = 3, version = "fast", ties = F)
(mat$est)

#Problem 9

wols = lm(dbirwt~tobacco, data=dov, weights = ((dov$tobacco/psov)+((1-dov$tobacco)/(1-psov))))
(summary(wols))

#Prpblem 10
mwols = wols = lm(dbirwt~., data=dov, weights = ((dov$tobacco/psov)+((1-dov$tobacco)/(1-psov))))
(summary(mwols))
