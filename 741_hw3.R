#install.packages("survival")


library(survival)
data(colon)
colon_death = colon[which(colon$etype==2),]

# 3.4 (1)
fit <- survfit(Surv(time,status)~rx, data=colon_death)
summary(fit)

plot(fit, col=c(1,2,4), lty=c(1,2,4), lwd=c(1,1,1), xlab="Patient Time", ylab = "Survival probability")
legend(100, .25, c("obs","Lev","Lev+5FU"), col=c(1,2,4), lty=c(1,2,4))
title("Kaplan-Meier estimates for the 3 treatment groups")

# 3.4 (2)
diff1 <- survdiff(Surv(time,status)~rx, data=colon_death, subset=rx!='Lev+5FU')
diff1
diff2 <- survdiff(Surv(time,status)~rx, data=colon_death, subset=rx!='Lev')
diff2
