######## Model Diagnostic #########
library(survival)
library(KMsurv)
data(package='KMsurv')

###### Cox-Snell Residuals ##########

## Example 11.1;
## Bone marrow transplant data described in section 1.3.

data(bmt)

attach(bmt)
z1 = z1 -28;
z2 = z2- 28;
z1xz2 = z1 * z2;
g1 = as.double( group== 1 );
g2 = as.double( group== 2 );
g3 = as.double( group== 3 );
z7c = z7 / 30 - 9;

## Figure 11.1 on page 356.
## First get the Cox-Snell residuals.;
## The default residuals of coxph in R are the martingale residuals.
## resid(fit1,type=c("martingale", "deviance", "score", "schoenfeld",
##                   "dfbeta", "dfbetas", "scaledsch","partial"))

fit1=coxph(Surv(t2,d3)~z1+z2+z1*z2+g2+g3+z7c+z8+z10,method='breslow')
coxsnellres=bmt$d3-resid(fit1,type="martingale")

## Then using NA method to estimate the cumulative hazard function for residuals;
fitres=survfit(coxph(Surv(coxsnellres,bmt$d3)~1,method='breslow'),type='aalen')
plot(fitres$time,-log(fitres$surv),type='s',xlab='Cox-Snell Residuals', 
     ylab='Estimated Cumulative Hazard Function',
     main='Figure 11.1 on page 356')
abline(0,1,col='red',lty=2)

## Alternatively, one may use
fit4=survfit(Surv(coxsnellres,bmt$d3)~1)
Htilde=cumsum(fit4$n.event/fit4$n.risk)
plot(fit4$time,Htilde,type='s',col='blue')
abline(0,1,col='red',lty=2)

# The overall model fits data reasonable well.

######### Martingale Residuals ######
## This could be used to determine the functional form of a covariate.


##  Example 11.2 Figure 11.4 on page 361.

data(hodg)
fit2=coxph(Surv(time,delta)~wtime+factor(dtype)+factor(gtype)+score,data=hodg,method='breslow')
resid(fit2,type='martingale')
plot(hodg$wtime, resid(fit2),
     xlab="Waiting Time to Transplant (months)", ylab="Martingale Residuals",
     main='Figure 11.4 on page 361')
lines(lowess(hodg$wtime, resid(fit2)),col='red')

# So the waiting time should enter the proportional hazards model nonlinearly.


##### Deviance Residuals ######
## This could be used to determine the functional form of a covariate.
## Example 11.2 Figure 11.21 on page 384.
plot(fit2$linear.predictor, resid(fit2,type='deviance'),
     xlab="Risk Score",ylab="Deviance Residuals",
     main='Figure 11.21 on page 384')
abline(0,0,lty=2,col='red')

## One may use pointer to identify the outliers in R
identify

##### Graphic Check for Proportional Hazards Assumption #####

## Example 11.3 figure 11.5 on page 364.
data(alloauto)
fit3=survfit(coxph(Surv(time,delta)~strata(factor(type)),data=alloauto,method='breslow'),type='aalen')
cloglog2=function(y){
  log(-log(y))
}
plot(fit3,fun='cumhaz',log='y',
     col=c('blue','red'),lty=c(1,2),xlim=c(0,25),yaxt='n',
     xlab='Time on Study',ylab='Log Cumulative Hazard Rate',
     main='Figure 11.5 on page 364')

