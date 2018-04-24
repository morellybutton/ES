library(tidyverse)
library(gridExtra)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

#source("/Volumes/ELDS/ECOLIMITS/R_codes/HelperFunctions/summarySE.R")

dF.pov<-data.frame(read.csv(paste0(getwd(),"/HouseholdData/PovertyMeasures1.csv")),stringsAsFactors = F)
dF.pov <- dF.pov %>% rename(plot=PLOTCODE)
#dF.pov$z.Food.security<-(dF.pov$Food.security-mean(dF.pov$Food.security))/sd(dF.pov$Food.security)
quart<-dF.pov %>% group_by(Cocoa.income.quart) %>% summarise(income=max(Cocoa.Income,na.rm=T))

dF.income<-data.frame(read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv")),stringsAsFactors = F)

dF.income <- left_join(dF.income,dF.pov %>% select(plot,Cocoa.Income,Cocoa.income.quart),by="plot")
dF.income <- dF.income %>% rename(Survey.income = Cocoa.Income) %>% mutate(i.Survey.income.fert=Survey.income*i.prop.fert,
                                                                           i.Survey.income.bmass=Survey.income*i.prop.bmass,
                                                                           i.Survey.income.cpb=Survey.income*i.prop.cpb)
#remove duplicates
dF.income <- dF.income %>% distinct(plot,.keep_all=T)


#analyze poverty measures as predictors of quartiles
#basic necessities
x<-aov(glm(Electricity~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results$quarts<-paste0(" ",row.names(results))
results$category<-"Electricity"
x<-aov(glm(Sanitation~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Sanitation"
x<-aov(glm(Water~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Water"

#Health (under 5 mortality)
x<-aov(glm(Health~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Health"

#Education
x<-aov(glm(Education~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Education"

#Assets
x<-aov(glm(Cocoa.area.ha~factor(Cocoa.income.quart),data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Cocoa Land Asset"

x<-aov(glm(TV~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"TV"

#Food Security
x<-aov(glm(Food.security~factor(Cocoa.income.quart),family=poisson,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Food Security"

#save outcomes based on income quartile
write.csv(results,paste0(getwd(),"/Analysis/ES/Poverty.IncomeQuartile.anovas.csv"))

#do analysis using income continuous variable
#education
#flip value so that 1 is likelihood of child missing school
dF.pov$Education1<-1
dF.pov[dF.pov$Education==1|is.na(dF.pov$Education),"Education1"]<-0
x<-glm(Education1~Cocoa.Income,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(Education1~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of education metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

#to calculate probability of household having a child miss school at new income (fertiliser, biomass & capsids):
#fertiliser
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate original probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("orig.prob","orig.ci5","orig.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}

educ.fert<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
educ.fert$plot<-as.character(dF.income$plot)
educ.fert$Survey.income<-dF.income$Survey.income
educ.fert$New.income<-dF.income$i.pot.net.margin.fert*dF.income$land.area
educ.fert$parameter<-"Fertiliser"

#biomass
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate original probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("orig.prob","orig.ci5","orig.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}

educ.bmass<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
educ.bmass$plot<-as.character(dF.income$plot)
educ.bmass$Survey.income<-dF.income$Survey.income
educ.bmass$New.income<-dF.income$i.Survey.income.bmass
educ.bmass$parameter<-"Biomass"

#capsids
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate original probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("orig.prob","orig.ci5","orig.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}

educ.cpb<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
educ.cpb$plot<-as.character(dF.income$plot)
educ.cpb$Survey.income<-dF.income$Survey.income
educ.cpb$New.income<-dF.income$i.Survey.income.cpb
educ.cpb$parameter<-"Capsids"

educ<-bind_rows(educ.fert,educ.bmass,educ.cpb)
write.csv(educ,paste0(getwd(),"/Analysis/ES/Education.probabilities.wincome.csv"))

output<- educ %>% group_by(parameter) %>% summarise(Measure="Education",Original=mean(orig.prob,na.rm=T),Potential=mean(prob,na.rm=T))

#do for TV asset
x<-glm(TV~Cocoa.Income,family=binomial,data=dF.pov)
summary(x)
#compare to intercept only 
x.reduced<-glm(TV~1,family=binomial,data=dF.pov)
anova(x.reduced,x, test="Chisq")
#compute how the odds of TV metric improve with income
exp(coef(x))
#to create 95% confidence interval
exp(confint.default(x))

#fertiliser
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate original probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("orig.prob","orig.ci5","orig.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}

TV.fert<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
TV.fert$plot<-as.character(dF.income$plot)
TV.fert$Survey.income<-dF.income$Survey.income
TV.fert$New.income<-dF.income$i.Survey.income.fert
TV.fert$parameter<-"Fertiliser"

#biomass
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate original probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("orig.prob","orig.ci5","orig.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}

TV.bmass<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
TV.bmass$plot<-as.character(dF.income$plot)
TV.bmass$Survey.income<-dF.income$Survey.income
TV.bmass$New.income<-dF.income$i.Survey.income.bmass
TV.bmass$parameter<-"Biomass"

#capsids
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate original probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$Survey.income[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("orig.prob","orig.ci5","orig.ci95")
  y<-cbind(y,z)
  results[[i]]<-y
}

TV.cpb<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
TV.cpb$plot<-as.character(dF.income$plot)
TV.cpb$Survey.income<-dF.income$Survey.income
TV.cpb$New.income<-dF.income$i.Survey.income.cpb
TV.cpb$parameter<-"Capsid"

TV<-bind_rows(TV.fert,TV.bmass,TV.cpb)
write.csv(TV,paste0(getwd(),"/Analysis/ES/Asset.tv.probabilities.wincome.csv"))

TV<- TV %>% group_by(parameter) %>% summarise(Measure="Assets",Original=mean(orig.prob,na.rm=T),Potential=mean(prob,na.rm=T))

output<-bind_rows(output,TV)

#do for satisfaction and food security, using quartiles
dF.income <- dF.income %>% mutate(quart.fert=1) %>% mutate(quart.fert=replace(quart.fert,i.Survey.income.fert>as.numeric(quart[3,2]),4),
                                                                            quart.fert=replace(quart.fert,i.Survey.income.fert>as.numeric(quart[2,2])&i.Survey.income.fert<=as.numeric(quart[3,2]),3),
                                                                            quart.fert=replace(quart.fert,i.Survey.income.fert>as.numeric(quart[1,2])&i.Survey.income.fert<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.bmass=1) %>% mutate(quart.bmass=replace(quart.bmass,i.Survey.income.bmass>as.numeric(quart[3,2]),4),
                                   quart.bmass=replace(quart.bmass,i.Survey.income.bmass>as.numeric(quart[2,2])&i.Survey.income.bmass<=as.numeric(quart[3,2]),3),
                                   quart.bmass=replace(quart.bmass,i.Survey.income.bmass>as.numeric(quart[1,2])&i.Survey.income.bmass<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.cpb=1) %>% mutate(quart.cpb=replace(quart.cpb,i.Survey.income.cpb>as.numeric(quart[3,2]),4),
                                   quart.cpb=replace(quart.cpb,i.Survey.income.cpb>as.numeric(quart[2,2])&i.Survey.income.cpb<=as.numeric(quart[3,2]),3),
                                   quart.cpb=replace(quart.cpb,i.Survey.income.cpb>as.numeric(quart[1,2])&i.Survey.income.cpb<=as.numeric(quart[2,2]),2))
  
dF.pov$Cocoa.income.quart<-factor(dF.pov$Cocoa.income.quart)
x<-glm(Satisfaction.life.overall~Cocoa.income.quart,family=poisson,data=dF.pov)
summary(x)

#check the residual deviance/degrees of freedom, if greater than 1 data is overdispersed
1-pchisq(x$deviance,x$df.residual)

#land<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
satis<-data.frame(as.character(dF.income$plot),stringsAsFactors = F)
colnames(satis)<-"plot"
satis$likert<-dF.pov[match(dF.income$plot,dF.pov$plot),"Satisfaction.life.overall"]
satis$income.quartile<-dF.pov[match(dF.income$plot,dF.pov$plot),"Cocoa.income.quart"]
satis$quart.fert<-factor(dF.income$quart.fert)
satis$quart.bmass<-factor(dF.income$quart.bmass)
satis$quart.cpb<-factor(dF.income$quart.cpb)

mx <- dF.pov %>% group_by(Cocoa.income.quart) %>% summarise(satisfaction=mean(Satisfaction.life.overall,na.rm=T),satisfaction.se=sd(Satisfaction.life.overall,na.rm=T)/length(Cocoa.income.quart))

satis <- satis %>% mutate(Fertiliser=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert),type="response", se.fit=TRUE)$fit,
                          Biomass=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass),type="response", se.fit=TRUE)$fit,
                                                   Capsid=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb),type="response", se.fit=TRUE)$fit)

#sat <- satis %>% summarise(Measure="Satisfaction",Original=mean(likert,na.rm=T))
satis <- satis %>% select(plot,likert,Fertiliser,Biomass,Capsid) %>% gather(key="parameter",value="new.likert",c(-plot,-likert)) %>%
  group_by(parameter) %>% summarise(Measure="Satisfaction",Original=mean(likert,na.rm=T),Potential=mean(new.likert,na.rm=T))

output <- bind_rows(output,satis)

#do again for food security, and income quartile for plot farmers
x<-glm(Food.amount~Cocoa.income.quart,family=binomial,data=dF.pov)
summary(x)


#compute how not having an adequate amount of food decreases with quartile
beta=exp(coef(x))
w<-exp(beta)
#to create 95% confidence interval
exp(confint.default(x))

#check the residual deviance/degrees of freedom, if greater than 1 data is overdispersed
1-pchisq(x$deviance,x$df.residual)

#food<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
food<-data.frame(as.character(dF.income$plot),stringsAsFactors = F)
colnames(food)<-"plot"
food$income.quartile<-dF.income$Cocoa.income.quart
food$quart.fert<-factor(dF.income$quart.fert)
food$quart.bmass<-factor(dF.income$quart.bmass)
food$quart.cpb<-factor(dF.income$quart.cpb)
food$amount<-dF.pov[match(dF.income$plot,dF.pov$plot),"Food.amount"]


#food.2<-summarySE(dF.pov, measurevar="Food.security", groupvars=c("Cocoa.income.quart"))
food <- food %>% mutate(Fertiliser=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert),type="response", se.fit=TRUE)$fit,
                          Biomass=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass),type="response", se.fit=TRUE)$fit,
                          Capsid=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb),type="response", se.fit=TRUE)$fit)

food <- food %>% select(plot,amount,Fertiliser,Biomass,Capsid) %>% gather(key="parameter",value="prob",c(-plot,-amount)) %>%
  group_by(parameter) %>% summarise(Measure="Food Security",Original=mean(amount,na.rm=T),Potential=mean(prob,na.rm=T))

output=bind_rows(output,food)

write.csv(output,paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.NewMeans.csv"))
