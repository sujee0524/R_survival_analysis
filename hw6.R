install.packages(c('dplyr','survival','survminer','tableone','glmnet','timereg','selectiveInference'))
setwd("C:/Users/slee776/Desktop/2017-fall/STAT_642/HW")

library(dplyr)
library(survival)
library(survminer)
library(tableone)
library(glmnet)
library(timereg)
library(selectiveInference)

uis <- read.table(file="uis.dat", sep="", header=F, na.strings = ".", col.names=c("id", "age", "beck", "hercoc", "ivhx", "ndrugtx", "race", "treat", "site", "los", "time", "status"))
### for filtering the rows with missing data
uis.new = uis %>% filter(complete.cases(.))

# (a) Plot the K-M curve of two treatment group. 
# Then use logrank test to assess the effect of treat. 
# And then fit a Cox PH with treat only. 
# Among the three tests (LRT, score, and Wald) available in the output of Cox PH model, to which one is the logrank test closest? Interpret the results in terms of the hazard ratio between the two treatment groups.

fit_km <- survfit(Surv(time,status)~treat, data = uis.new)
plot(fit_km, col=c(1,2),lty=c(1,2), lwd=c(1,1), xlab ='Time (Days)', ylab='Survival Probability')
legend(100,.15,c("short","long"),col=c(1,2),lty=c(1,2))
title("Kaplan-Meier curve of two treatment group")


diff <- survdiff(Surv(time,status)~treat, data = uis.new)
diff

fit_cox_a <- coxph(Surv(time,status)~treat, data = uis.new, method='breslow')
summary(fit_cox_a)

# (b) Assess the treatment effect by adjusting for all other covariates.
# Compared with the unadjusted treatment effect estimator in part (a), is there a substantial difference? Why?
fit_cox_b <- coxph(Surv(time,status)~treat+age+beck+as.factor(hercoc)+as.factor(ivhx)+ndrugtx+race+site+los, data = uis.new, method='breslow')
summary(fit_cox_b)

# (c) Fit a Cox PH model to assess the treat×los interaction effect.
fit_cox_c_1 <- coxph(Surv(time,status)~treat*los, data = uis.new, method='breslow')
summary(fit_cox_c_1)
fit_cox_c_2 <- coxph(Surv(time,status)~treat+age+beck+as.factor(hercoc)+as.factor(ivhx)+ndrugtx+race+site+los+treat*los, data = uis.new, method='breslow')
summary(fit_cox_c_2)


# (d) Model diagnostics of main effect only model (mfit)
# i. Use Cox-Snell Residuals to assess the model fit of mfit
## Cox residual
coxsnellres=uis.new$status - resid(fit_cox_b,type="martingale")
## Then using NA method to estimate the cumulative hazard function for residuals;
fitres=survfit(coxph(Surv(coxsnellres,uis.new$status)~1,method='breslow'),type='aalen')
plot(fitres$time,-log(fitres$surv),type='s',xlab='Cox-Snell Residuals',
     ylab='Estimated Cumulative Hazard Function',
     main='Cox-Snell Residuals for access the fit of the data')
abline(0,1,col='red',lty=2)
## Most of the functions are 'survival' package, if you have time, please
## read their vignettes
## survConcordance(.) for concordance
## basehaz(.) for baseline hazard

# ii. Use martingale residuals (or cumulative martingale residuals) 
# to assess the functional form of any continuous variables in mfit. 
# Comment on the resultant plots. You may consider modifying your mfit based on the martingale residual plots (modification is optional).
resid(fit_cox_b, type='martingale')
plot(uis.new$age, resid(fit_cox_b, type='martingale'),
     xlab="Age", ylab="Martingale Residuals",
     main="Age vs. Martingale Residuals")
lines(lowess(uis.new$age, resid(fit_cox_b, type='martingale')),col='red')

plot(uis.new$beck, resid(fit_cox_b, type='martingale'),
     xlab="Beck Depression Score", ylab="Martingale Residuals",
     main="Beck Depression Score vs. Martingale Residuals")
lines(lowess(uis.new$beck, resid(fit_cox_b, type='martingale')),col='red')

plot(uis.new$ndrugtx, resid(fit_cox_b, type='martingale'),
     xlab="Number of Prior Drug Treatments", ylab="Martingale Residuals",
     main="Number of Prior Drug Treatments vs. Martingale Residuals")
lines(lowess(uis.new$ndrugtx, resid(fit_cox_b, type='martingale')),col='red')

plot(uis.new$los, resid(fit_cox_b, type='martingale'),
     xlab="Length of Stay in Treatment (Days)", ylab="Martingale Residuals",
     main="Length of Stay in Treatment vs. Martingale Residuals")
lines(lowess(uis.new$los, resid(fit_cox_b, type='martingale')),col='red')

fit_cox_d <- coxph(Surv(time,status)~treat+age+beck+as.factor(hercoc)+as.factor(ivhx)+ndrugtx+race+site+s(los), data = uis.new, method='breslow')
summary(fit_cox_d)

# iii. Assess the proportional hazards (PH) assumption for each covariate in mfit with Schoenfeld residuals via both the testing procedure and graphical plots.

# iv. Identify top three potential outliers that do not fit well to the model using deviance residuals.
plot(fit_cox_b$linear.predictors, resid(fit_cox_b, type='deviance'),
     xlab="Risk Score", ylab="Deviance Residuals",
     main="Risk vs. Deviance Residuals")
abline(0,0,lty=2,col='red')
#identify


# v. (3 pts) Identify the top three observations that are most influential for the coefficient estimation of 'treat', and plot the DFBETAs for all covariates.

# (e) Suppose the 'newtime' = 'time^2', fit a Cox model same to (b), does the coefficient with 'newtime' as outcome differ from that with 'time' as outcome. 
# How about the baseline survival time S0(t) from the two models? 
# Plot the two S0(t) from these two models, and comments on your result.