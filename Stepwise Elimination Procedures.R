#Exploratory Data Analysis
attach(Highway)
plot(Highway[,-12:-15])
cor(Highway[,-12:-15])

smalldata<-Highway[1:14]
fullreg.lm<-lm(RATE~., data=smalldata)
summary(fullreg.lm)

library(car)
vif(fullreg.lm)

#Forward Selection Procedure
library(Rcmdr)
forward.lm<-stepwise(fullreg.lm, direction="forward")
summary(forward.lm)

#Backward Elimination Procedure
backward.lm<-stepwise(fullreg.lm, direction="backward")
summary(backward.lm)

#Mixed Stepwise Procedure
step.lm<-stepwise(fullreg.lm, direction="backward/forward")
summary(step.lm)

#All Possible Regressions Procedure
library(leaps)
leaps.lm<-regsubsets(RATE~.,data=smalldata,nbest=1)
summary(leaps.lm)

plot(leaps.lm,scale="adjr2")

ultimatefinal.lm<-lm(RATE~LEN+SLIM+ACPT)
summary(ultimatefinal.lm)
plot(ultimatefinal.lm)
