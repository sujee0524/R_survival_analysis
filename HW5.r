##### Problem 3 ###############
library(mgcv)
set.seed(200)
dat <- gamSim(1,n=200,dist= "binary")
#dat1 <- dat
#dat1$y <- dat1$y > median(dat1$y)
b<-gam(y~x0+x1+x2+x3,family = binomial(link = "logit"),data=dat)
c<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family = binomial(link = "logit"), data=dat)
d<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family = binomial(link = "probit"), data=dat)
#d<-gam(y~x0+s(x1)+s(x2)+x3,family = binomial(link = "logit"), data=dat)

#plot(b,pages=1)
gam.check(b,pch=19,cex=.3)

plot(c,pages=1)
gam.check(c,pch=19,cex=.3)

plot(d,pages=1)
gam.check(d,pch=19,cex=.3)


d2<-gam(y~x0+x1+s(x2)+x3,family = binomial(link = "probit"), data=dat)
gam.check(d2,pch=19,cex=.3)
d3<-gam(y~s(x0)+x1+x2+x3,family = binomial(link = "probit"), data=dat)
gam.check(d3,pch=19,cex=.3)
d4<-gam(y~x0+s(x1)+x2+x3,family = binomial(link = "probit"), data=dat)
gam.check(d4,pch=19,cex=.3)
d4<-gam(y~x0+x1+x2+s(x3),family = binomial(link = "probit"), data=dat)
gam.check(d4,pch=19,cex=.3)

########### Problem 4 #####################################################
install.packages("ROSE")
library(ROSE)
data(hacide)

glmmodel <- glm(cls ~ ., family=binomial, data = hacide.train)
predlabelglm <- predict(glmmodel, hacide.test[,2:3],type="response")
roc.curve(hacide.train$cls, glmmodel$fitted.values, plotit = F)
roc.curve(hacide.test$cls,predlabelglm, plotit = T)

data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 2000)$data

glmmodel_rose <- glm(cls ~ ., family=binomial, data = data.rose)
roc.curve( data.rose$cls, glmmodel_rose$fitted.values, plotit = F)
predlabelglmrose <- predict(glmmodel_rose, hacide.test[,2:3],type="response")
roc.curve(hacide.test$cls,predlabelglmrose, plotit = T)

##-------------------------------
glmmodel <- gam(cls ~ s(x1) + s(x2), family=binomial, data = hacide.train)
predlabelglm <- predict(glmmodel, hacide.test[,2:3],type="response")
roc.curve(hacide.train$cls, glmmodel$fitted.values, plotit = F)
roc.curve(hacide.test$cls,predlabelglm, plotit = T)

data.rose <- ROSE(cls ~ ., data = hacide.train, seed = 2000)$data

glmmodel_rose <- gam(cls ~ s(x1) + s(x2), family=binomial, data = data.rose)
roc.curve( data.rose$cls, glmmodel_rose$fitted.values, plotit = F)
predlabelglmrose <- predict(glmmodel_rose, hacide.test[,2:3],type="response")
roc.curve(hacide.test$cls,predlabelglmrose, plotit = T)







































































































# gammodel <- gam(cls ~ s(x1)+ s(x2), family=binomial, data = hacide.train)
# roc.curve(hacide.train$cls, gammodel$fitted.values, plotit = F)
# predlabelgam <- predict(gammodel, hacide.test[,2:3],type="response")
# roc.curve(hacide.test$cls,predlabelgam, plotit = T)
# 
# gammodel_rose <- gam(cls ~ s(x1)+ s(x2), family=binomial, data = data.rose)
# roc.curve(data.rose$cls, gammodel_rose$fitted.values, plotit = F)
# predlabelgamrose <- predict(gammodel_rose, hacide.test[,2:3],type="response")
# roc.curve(hacide.test$cls,predlabelgamrose, plotit = T)

