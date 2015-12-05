attach(operational_sex_ratio)
osr<- operational_sex_ratio
require(lme4)
require(car)
osr$fruitset=(osr$imm/(osr$imm+osr$abort))
osr$count=(osr$imm+osr$abort)
print(cor(osr[,c("osr.0.f","osr.1.f","osr.2.f","osr.3.f","osr.4.f","osr.5.f", "osr.patch.f")],use="complete.obs"))
#plot(osr[,c("osr.0.f","osr.1.f","osr.2.f","osr.3.f","osr.4.f","osr.5.f","osr.patch.f")])
transformosr=osr
transformosr[,c("osr.0.f","osr.1.f","osr.2.f","osr.3.f","osr.4.f","osr.5.f", "osr.patch.f")]=(asin(sqrt(osr[,c("osr.0.f","osr.1.f","osr.2.f","osr.3.f","osr.4.f","osr.5.f","osr.patch.f")])))
#plot(transformosr[,c("osr.0.f","osr.1.f","osr.2.f","osr.3.f","osr.4.f","osr.5.f","osr.patch.f")])
model1=glmer(fruitset~osr.0.f+osr.1.f+osr.2.f+osr.3.f+osr.4.f+osr.5.f+(1|exclosure)+osr.patch.f+ (1|plant), data=transformosr[transformosr$projected==0,], family=binomial(link="logit"), weights=transformosr$count[transformosr$projected==0])
print(Anova(model1, type=3))
model2=glmer(fruitset~sex*(osr.0.f+osr.1.f+osr.2.f+osr.3.f+osr.4.f+osr.5.f+osr.patch.f)+(1|exclosure)+(1|plant), data=transformosr[transformosr$projected==0,], family=binomial(link="logit"), weights=transformosr$count[transformosr$projected==0])
print(Anova(model2, type=3))
model3=glmer(fruitset~sex+(1|exclosure)+(1|plant), data=transformosr[transformosr$projected==0,], family=binomial(link="logit"), weights=transformosr$count[transformosr$projected==0])
print(Anova(model3, type=3))
#model4=glmer(size.0.f~sex+(1|exclosure)+(1|plant), data=transformosr[transformosr$projected==0,], family="poisson")
#print(Anova(model4, type=3))
betas = as.matrix(coef(model2)$exclosure[1,])
betas.f = betas[4:8]
betas.h = betas.f + betas[11:15]
barplot(abs(betas.f))
barplot(abs(betas.h))
