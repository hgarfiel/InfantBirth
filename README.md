# InfantBirth
Analysis of live birth data in relation to predictors of infant mortality. The data sets used consist of roughly 550,000 observations on live infant births in the state of Florida from 1989-1991. Depending on the data set used, there are anywhere from 12-32 variables recorded for each live birth. These variables provide information on the mother's demographic characteristics, risk habits, and prenatal care employed, as well as similar information about the father. It also provides information about the baby, including weight among other things. Unfortunately, I am not at liberty to provide the data sets used out of consideration for the professor who gave them to me, as he still may be working with them and I'm not fully aware of the possible confidentiality issues surrounding their release (some of the sets are extremely rich).

These analyses are broken up into different sets of files that share names. .R files are the raw code used in the analysis and .docx, .Rmd, and .pdf files given are their corresponding output (with the same name).

###Prob_Log
This analysis runs Probit and Logit regressions on the variables included in the data set in order to determine significant predictors of infant mortality. Whether or not the mother smoked or drank alcohol during the pregnancy became the focus of the analysis. Some of the notable output in this analysis are average effects of smoking and drinking on infant mortality, effects at the mean of the same, and marginal effects, most of which are accompanied by confidence intervals. The analysis also includes an ordered probit model in which the predictor variables are used to determine the probability that a mother received a certain level of prenatal care during pregnancy.

###Trunc_Cens
These files contain Least Squares analysis of similar data sets that have been either truncated or censored at specific infant birth weights.

###Caus_Quant
These files contain analyses in which I tried to estimate causal effects of the given explanatory variables on infant mortality rather than the associations that were determined in the previous groups. Causality was estimated using Heckman's two-step method (in STATA), which attempts to correct for selection bias in the estimating equation. Quantile regression is also looked at in this analysis to determine the effect of the predictors on different quantiles of the live birth distribution. Since the error terms here may not necessarily be independent and identically distributed (i.i.d.), bootstrapping is used to get an estimate of the "true" standard errors in these equations (though this is not substantially different from the i.i.d. errors). 

###Prop
This set of files delves deeper into the causality question touched on in the Caus_Quant set. This time, however, propensity score methods are used in order to try to create equality in covariates between mothers who exhibited risky behavior during pregnancy and mothers who didn't. 

