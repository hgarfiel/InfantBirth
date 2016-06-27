# InfantBirth
Analysis of live birth data in relation to predictors of infant mortality. The data sets used consist of roughly 550,000 observations on live infant births in the state of Florida from 1989-1991. Depending on the data set used, there are anywhere from 12-32 variables recorded for each live birth. These variables provide information on the mother's demographic characteristics, risk habits, and prenatal care employed, as well as similar information about the father. It also provides information about the baby, including weight among other things. Unfortunately, I am not at liberty to provide the data sets used out of consideration for the professor who gave them to me, as he still may be working with them and I'm not fully aware of the possible confidentiality issues surrounding their release (some of the sets are extremely rich).

These analyses are broken up into different types of files that share names. .R files are the raw code used in the analysis and .docx, .Rmd, and .pdf files given are their corresponding output (with the same name)

###Prob_Log
This analysis runs Probit and Logit regressions on the variables included in the data set in order to determine significant predictors of infant mortality. Whether or not the mother smoked or drank alcohol during the pregnancy became the focus of the analysis. Some of the notable output in this analysis are average effects of smoking and drinking on infant mortality, effects at the mean of the same, and marginal effects, most of which are accompanied by confidence intervals. The analysis also includes an ordered probit model in which the predictor variables are used to determine the probability that a mother received a certain level of prenatal care during pregnancy.

###Trunc_Cens
These files contain Least Squares analysis of similar data sets that have been either truncated or censored at specific infant birth weights.

###

