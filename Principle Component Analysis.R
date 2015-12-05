attach(Highway)
newdata<-Highway[2:11]
newdata.pc<-princomp(newdata, cor=TRUE, scores=TRUE)
newdata.pc
sum(newdata.pc$sd^2)
summary(newdata.pc)
par(mfrow=c(1,1))
screeplot(newdata.pc, type="lines", main="Scree Plot")
print(loadings(newdata.pc),cutoff=0.3)
PC1<-newdata.pc$scores[,1]
PC2<-newdata.pc$scores[,2]
PC3<-newdata.pc$scores[,3]
Highway$PC1<-PC1
Highway$PC2<-PC2
Highway$PC3<-PC3
plot(Highway[18:20], main="Scatterplot Matrix of PCs")
cor(Highway[18:20])

par(mfrow=c(2,2))
boxplot(PC1~TYPE, main="Boxplot of PC1 by Road Type", xlab="Road Type")
boxplot(PC2~TYPE, main="Boxplot of PC2 by Road Type", xlab="Road Type")
boxplot(PC3~TYPE, main="Boxplot of PC3 by Road Type", xlab="Road Type")
pc.lm<-lm(RATE~PC1+PC2+PC3)
summary(pc.lm)
pcnew.lm<-lm(RATE~PC1+PC2)
summary(pcnew.lm)