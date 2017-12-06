
library(survival)
data(colon)
colon_death = colon[which(colon$etype==2),]

fit <- coxph(Surv(time,status)~as.factor(rx)+age+as.factor(sex)+as.factor(differ)+as.factor(extent), data=colon_death)
summary(fit)

plot(survfit(fit, newdata=data.frame(rx = "Obs",age=60, sex =0, differ=2, extent=2)), 
     conf.int= F, xlab = "Time (day)", ylab="Survival probability", col = 1, lty = 1) 
lines(survfit(fit, newdata=data.frame(rx = "Lev",age=60, sex =0, differ=2, extent=2)),conf.int= F,col = 2, lty = 2) 
lines(survfit(fit, newdata=data.frame(rx = "Lev+5FU",age=60, sex =0, differ=2, extent=2)), conf.int= F, col = 4, lty = 4) 
title("Survival curve for the patient with the given condition")
legend(100, .25, c("obs","Lev","Lev+5FU"), col=c(1,2,4), lty=c(1,2,4))


