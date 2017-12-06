#library(survival)
library(cmprsk)
data(colon)
colon.new = colon
fit <- survfit(Surv(time,status)~rx, data=colon_death)
colon.new[which(colon$etype==1 && colon$status==1),fStatus] = 1 # recurrence occure
colon.new[which(colon$etype==1 && colon$status==2),fStatus] = 2 # death occure
colon.new[which(colon$etype==1 && colon$status==2),fStatus] = 2 # death occure


fit <- cuminc(ftime=colon$time, fstatus=colon$status, group=colon$rx)
plot(fit)

fit2 <- crr(ftime=colon$time, fstatus = colon$status, )