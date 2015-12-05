attach(NES2008)

#Explanatory Data Analysis
par(mfrow=c(3,4))
for (i in 1:11){
  boxplot(NES2008[,2+i]~PresVote, ylab=names(NES2008)[i+2], names=c("McCain", "Obama"))
}

smalldata<-NES2008[,2:13]
nesall.logm<-glm(PresVote~.,family="binomial",data=smalldata)
summary(nesall.logm)

PIDnew<-PartyID-4

nesnew.logm<-glm(PresVote~PIDnew+Income+Black+Hisp+Other)
summary(nesnew.logm)

prednes<-predict(nesnew.logm, type="response")
predObamaMcCain<-rep("McCain", 1068)
for (i in 1:1068){
  if (prednes[i]>0.5){
    predObamaMcCain[i]<-"Obama"
  }
}
table(PresVote, predObamaMcCain)

nesother.logm<-glm(PresVote~Income+Black+Hisp+Other)
summary(nesother.logm)

prednesother<-predict(nesother.logm, type="response")
predObamaMcCainother<-rep("McCain", 1068)
for (i in 1:1068){
  if (prednesother[i]>0.5){
    predObamaMcCainother[i]<-"Obama"
  }
}
table(PresVote, predObamaMcCainother)

nesnewnew.logm<- glm(PresVote~Income+Black+Hisp+PIDnew)
summary(nesnewnew.logm)

prednesFinal<-predict(nesnewnew.logm, type="response")
predObamaMcCainFinal<-rep("McCain", 1068)
for (i in 1:1068){
  if (prednesFinal[i]>0.5){
    predObamaMcCainFinal[i]<-"Obama"
  }
}
table(PresVote, predObamaMcCainFinal)

NES2008$Null [1:534,]<-"0"
table(PresVote, prednes)

attach(NES2008)
library(tree)
mytree <- tree(factor(PresVote) ~ Income+PIDnew+Black+Hisp,data=NES2008)
mytree
summary(mytree)
plot(mytree)
text(mytree)
