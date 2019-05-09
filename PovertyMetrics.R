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
dF.income.2<-data.frame(read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.alt.",season,".csv")),stringsAsFactors = F)

dF.income <- left_join(dF.income,dF.pov %>% select(plot,Cocoa.Income,Cocoa.income.quart),by="plot")
dF.income <- dF.income %>% rename(Survey.income = Cocoa.Income) %>% mutate(i.Survey.income.fert=Survey.income*i.prop.fert,i.Survey.income.fert.lwr=Survey.income*i.prop.fert.lwr,i.Survey.income.fert.upr=Survey.income*i.prop.fert.upr,
                                                                           i.Survey.income.bmass=Survey.income*i.prop.bmass,i.Survey.income.bmass.lwr=Survey.income*i.prop.bmass.lwr,i.Survey.income.bmass.upr=Survey.income*i.prop.bmass.upr,
                                                                           i.Survey.income.cpb=Survey.income*i.prop.cpb,i.Survey.income.cpb.lwr=Survey.income*i.prop.cpb.lwr,i.Survey.income.cpb.upr=Survey.income*i.prop.cpb.upr,
                                                                           i.Survey.income.all=Survey.income*i.prop.all,i.Survey.income.all.lwr=Survey.income*i.prop.all.lwr,i.Survey.income.all.upr=Survey.income*i.prop.all.upr,
                                                                           i.Survey.income.noloss=Survey.income*i.prop.noloss)
dF.income.2 <- left_join(dF.income.2,dF.pov %>% select(plot,Cocoa.Income,Cocoa.income.quart),by="plot")
dF.income.2 <- dF.income.2 %>% rename(Survey.income = Cocoa.Income) %>% mutate(i.Survey.income.fert=Survey.income*i.prop.fert,i.Survey.income.fert.lwr=Survey.income*i.prop.fert.lwr,i.Survey.income.fert.upr=Survey.income*i.prop.fert.upr,
                                                                           i.Survey.income.bmass=Survey.income*i.prop.bmass,i.Survey.income.bmass.lwr=Survey.income*i.prop.bmass.lwr,i.Survey.income.bmass.upr=Survey.income*i.prop.bmass.upr,
                                                                           i.Survey.income.cpb=Survey.income*i.prop.cpb,i.Survey.income.cpb.lwr=Survey.income*i.prop.cpb.lwr,i.Survey.income.cpb.upr=Survey.income*i.prop.cpb.upr,
                                                                           i.Survey.income.all=Survey.income*i.prop.all,i.Survey.income.all.lwr=Survey.income*i.prop.all.lwr,i.Survey.income.all.upr=Survey.income*i.prop.all.upr,
                                                                           i.Survey.income.noloss=Survey.income*i.prop.noloss)

#remove duplicates
dF.income <- dF.income %>% distinct(plot,.keep_all=T)
dF.income.2 <- dF.income.2 %>% distinct(plot,.keep_all=T)

write.csv(dF.income,paste0(getwd(),"/Analysis/ES/Income.calculations.",season,"whhold.csv"))
write.csv(dF.income.2,paste0(getwd(),"/Analysis/ES/Income.calculations.alt.",season,"whhold.csv"))

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
x<-aov(glm(Food.amount~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Food Security"

#Satisfaction
x<-aov(glm(Satisfaction.life.overall~factor(Cocoa.income.quart),family=poisson,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Satisfaction"

#Access to Extension
x<-aov(glm(extension~factor(Cocoa.income.quart),family=binomial,data=dF.pov))
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-paste0(" ",results[1:6,"quarts"])
results[(nrow(results)-5):nrow(results),"category"]<-"Access to Extension"

#save outcomes based on income quartile
write.csv(results,paste0(getwd(),"/Analysis/ES/Poverty.IncomeQuartile.anovas_kakum.csv"))

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
#try with alternative income calculations
dF.income<-dF.income.2

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
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
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
educ.fert$New.income.lwr<-dF.income$i.pot.net.margin.fert.lwr*dF.income$land.area
educ.fert$New.income.upr<-dF.income$i.pot.net.margin.fert.upr*dF.income$land.area
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
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
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
educ.bmass$New.income.lwr<-dF.income$i.Survey.income.bmass.lwr
educ.bmass$New.income.upr<-dF.income$i.Survey.income.bmass.upr
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
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
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
educ.cpb$New.income.lwr<-dF.income$i.Survey.income.cpb.lwr
educ.cpb$New.income.upr<-dF.income$i.Survey.income.cpb.upr
educ.cpb$parameter<-"Capsids"

#all
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
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

educ.all<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
educ.all$plot<-as.character(dF.income$plot)
educ.all$Survey.income<-dF.income$Survey.income
educ.all$New.income<-dF.income$i.Survey.income.all
educ.all$New.income.lwr<-dF.income$i.Survey.income.all.lwr
educ.all$New.income.upr<-dF.income$i.Survey.income.all.upr
educ.all$parameter<-"All Options"

#no lbc loss
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.noloss[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.noloss[i]), se.fit=TRUE)
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

educ.noloss<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
educ.noloss$plot<-as.character(dF.income$plot)
educ.noloss$Survey.income<-dF.income$Survey.income
educ.noloss$New.income<-dF.income$i.Survey.income.noloss
educ.noloss$parameter<-"No LBC Loss"

educ<-bind_rows(educ.fert,educ.bmass,educ.cpb,educ.all,educ.noloss)
#write.csv(educ,paste0(getwd(),"/Analysis/ES/Education.probabilities.wincome.csv"))
write.csv(educ,paste0(getwd(),"/Analysis/ES/Education.probabilities.alt.wincome.csv"))

#educ<-read_csv(paste0(getwd(),"/Analysis/ES/Education.probabilities.wincome.csv"))
educ <- left_join(educ,dF.income %>% select(plot,Cocoa.income.quart),by="plot")
#group top 2 quartiles and bottom two
educ <- educ %>% mutate(quartile="top") %>% mutate(quartile=replace(quartile,Cocoa.income.quart<3,"bottom"))
output<- educ %>% group_by(parameter,quartile) %>% summarise(Measure="Education",Original=mean(orig.prob,na.rm=T),Potential=mean(prob,na.rm=T),Lower=mean(lwr.prob,na.rm=T),Upper=mean(upr.prob,na.rm=T))

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
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(pi.hat$fit,stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.fert.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
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
TV.fert$New.income.lwr<-dF.income$i.Survey.income.fert.lwr
TV.fert$New.income.upr<-dF.income$i.Survey.income.fert.upr
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
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.bmass.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
 
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
TV.bmass$New.income.lwr<-dF.income$i.Survey.income.bmass.lwr
TV.bmass$New.income.upr<-dF.income$i.Survey.income.bmass.upr
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
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.cpb.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
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
TV.cpb$New.income.lwr<-dF.income$i.Survey.income.cpb.lwr
TV.cpb$New.income.upr<-dF.income$i.Survey.income.cpb.upr
TV.cpb$parameter<-"Capsid"

#all
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  y<-data.frame(c(y,exp(ci)/(1+exp(ci))))
  colnames(y)<-c("prob","ci5","ci95")
  #calculate lower probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.lwr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.lwr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("lwr.prob","lwr.ci5","lwr.ci95")
  y<-cbind(y,z)
  #calculate upper probability
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.upr[i]),
                       type="response", se.fit=TRUE)
  z<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.all.upr[i]), se.fit=TRUE)
  ci = c(l.hat$fit - 1.96*l.hat$se.fit, l.hat$fit + 1.96*l.hat$se.fit)
  z<-data.frame(c(z,exp(ci)/(1+exp(ci))))
  colnames(z)<-c("upr.prob","upr.ci5","upr.ci95")
  y<-cbind(y,z)
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

TV.all<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
TV.all$plot<-as.character(dF.income$plot)
TV.all$Survey.income<-dF.income$Survey.income
TV.all$New.income<-dF.income$i.Survey.income.all
TV.all$New.income.lwr<-dF.income$i.Survey.income.all.lwr
TV.all$New.income.upr<-dF.income$i.Survey.income.all.upr
TV.all$parameter<-"All Options"

#no lbc loss
results<-list()
for(i in 1:nrow(dF.income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.noloss[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$i.Survey.income.noloss[i]), se.fit=TRUE)
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

TV.noloss<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
TV.noloss$plot<-as.character(dF.income$plot)
TV.noloss$Survey.income<-dF.income$Survey.income
TV.noloss$New.income<-dF.income$i.Survey.income.noloss
TV.noloss$parameter<-"No LBC Loss"

TV<-bind_rows(TV.fert,TV.bmass,TV.cpb,TV.all,TV.noloss)
#write.csv(TV,paste0(getwd(),"/Analysis/ES/Asset.tv.probabilities.wincome.csv"))
write.csv(TV,paste0(getwd(),"/Analysis/ES/Asset.tv.probabilities.alt.wincome.csv"))

#TV<-read_csv(paste0(getwd(),"/Analysis/ES/Asset.tv.probabilities.wincome.csv"))
TV <- left_join(TV,dF.income %>% select(plot,Cocoa.income.quart),by="plot")
#group top 2 quartiles and bottom two
TV <- TV %>% mutate(quartile="top") %>% mutate(quartile=replace(quartile,Cocoa.income.quart<3,"bottom"))

TV<- TV %>% group_by(parameter,quartile) %>% summarise(Measure="Assets",Original=mean(orig.prob,na.rm=T),Potential=mean(prob,na.rm=T),Lower=mean(lwr.prob,na.rm=T),Upper=mean(upr.prob,na.rm=T))

output<-bind_rows(output,TV)

#do for satisfaction and food security, using quartiles
dF.income <- dF.income %>% mutate(quart.fert=1) %>% mutate(quart.fert=replace(quart.fert,i.Survey.income.fert>as.numeric(quart[3,2]),4),
                                                                            quart.fert=replace(quart.fert,i.Survey.income.fert>as.numeric(quart[2,2])&i.Survey.income.fert<=as.numeric(quart[3,2]),3),
                                                                            quart.fert=replace(quart.fert,i.Survey.income.fert>as.numeric(quart[1,2])&i.Survey.income.fert<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.fert.lwr=1) %>% mutate(quart.fert.lwr=replace(quart.fert.lwr,i.Survey.income.fert.lwr>as.numeric(quart[3,2]),4),
                                  quart.fert.lwr=replace(quart.fert.lwr,i.Survey.income.fert.lwr>as.numeric(quart[2,2])&i.Survey.income.fert.lwr<=as.numeric(quart[3,2]),3),
                                  quart.fert.lwr=replace(quart.fert.lwr,i.Survey.income.fert.lwr>as.numeric(quart[1,2])&i.Survey.income.fert.lwr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.fert.upr=1) %>% mutate(quart.fert.upr=replace(quart.fert.upr,i.Survey.income.fert.upr>as.numeric(quart[3,2]),4),
                                      quart.fert.upr=replace(quart.fert.upr,i.Survey.income.fert.upr>as.numeric(quart[2,2])&i.Survey.income.fert.upr<=as.numeric(quart[3,2]),3),
                                      quart.fert.upr=replace(quart.fert.upr,i.Survey.income.fert.upr>as.numeric(quart[1,2])&i.Survey.income.fert.upr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.bmass=1) %>% mutate(quart.bmass=replace(quart.bmass,i.Survey.income.bmass>as.numeric(quart[3,2]),4),
                                   quart.bmass=replace(quart.bmass,i.Survey.income.bmass>as.numeric(quart[2,2])&i.Survey.income.bmass<=as.numeric(quart[3,2]),3),
                                   quart.bmass=replace(quart.bmass,i.Survey.income.bmass>as.numeric(quart[1,2])&i.Survey.income.bmass<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.bmass.lwr=1) %>% mutate(quart.bmass.lwr=replace(quart.bmass.lwr,i.Survey.income.bmass.lwr>as.numeric(quart[3,2]),4),
                                   quart.bmass.lwr=replace(quart.bmass.lwr,i.Survey.income.bmass.lwr>as.numeric(quart[2,2])&i.Survey.income.bmass.lwr<=as.numeric(quart[3,2]),3),
                                   quart.bmass.lwr=replace(quart.bmass.lwr,i.Survey.income.bmass.lwr>as.numeric(quart[1,2])&i.Survey.income.bmass.lwr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.bmass.upr=1) %>% mutate(quart.bmass.upr=replace(quart.bmass.upr,i.Survey.income.bmass.upr>as.numeric(quart[3,2]),4),
                                       quart.bmass.upr=replace(quart.bmass.upr,i.Survey.income.bmass.upr>as.numeric(quart[2,2])&i.Survey.income.bmass.upr<=as.numeric(quart[3,2]),3),
                                       quart.bmass.upr=replace(quart.bmass.upr,i.Survey.income.bmass.upr>as.numeric(quart[1,2])&i.Survey.income.bmass.upr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.cpb=1) %>% mutate(quart.cpb=replace(quart.cpb,i.Survey.income.cpb>as.numeric(quart[3,2]),4),
                                   quart.cpb=replace(quart.cpb,i.Survey.income.cpb>as.numeric(quart[2,2])&i.Survey.income.cpb<=as.numeric(quart[3,2]),3),
                                   quart.cpb=replace(quart.cpb,i.Survey.income.cpb>as.numeric(quart[1,2])&i.Survey.income.cpb<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.cpb.lwr=1) %>% mutate(quart.cpb.lwr=replace(quart.cpb.lwr,i.Survey.income.cpb.lwr>as.numeric(quart[3,2]),4),
                                 quart.cpb.lwr=replace(quart.cpb.lwr,i.Survey.income.cpb.lwr>as.numeric(quart[2,2])&i.Survey.income.cpb.lwr<=as.numeric(quart[3,2]),3),
                                 quart.cpb.lwr=replace(quart.cpb.lwr,i.Survey.income.cpb.lwr>as.numeric(quart[1,2])&i.Survey.income.cpb.lwr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.cpb.upr=1) %>% mutate(quart.cpb.upr=replace(quart.cpb.upr,i.Survey.income.cpb.upr>as.numeric(quart[3,2]),4),
                                     quart.cpb.upr=replace(quart.cpb.upr,i.Survey.income.cpb.upr>as.numeric(quart[2,2])&i.Survey.income.cpb.upr<=as.numeric(quart[3,2]),3),
                                     quart.cpb.upr=replace(quart.cpb.upr,i.Survey.income.cpb.upr>as.numeric(quart[1,2])&i.Survey.income.cpb.upr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.all=1) %>% mutate(quart.all=replace(quart.all,i.Survey.income.all>as.numeric(quart[3,2]),4),
                                 quart.all=replace(quart.all,i.Survey.income.all>as.numeric(quart[2,2])&i.Survey.income.all<=as.numeric(quart[3,2]),3),
                                 quart.all=replace(quart.all,i.Survey.income.all>as.numeric(quart[1,2])&i.Survey.income.all<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.all.lwr=1) %>% mutate(quart.all.lwr=replace(quart.all.lwr,i.Survey.income.all.lwr>as.numeric(quart[3,2]),4),
                                 quart.all.lwr=replace(quart.all.lwr,i.Survey.income.all.lwr>as.numeric(quart[2,2])&i.Survey.income.all.lwr<=as.numeric(quart[3,2]),3),
                                 quart.all.lwr=replace(quart.all.lwr,i.Survey.income.all.lwr>as.numeric(quart[1,2])&i.Survey.income.all.lwr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.all.upr=1) %>% mutate(quart.all.upr=replace(quart.all.upr,i.Survey.income.all.upr>as.numeric(quart[3,2]),4),
                                     quart.all.upr=replace(quart.all.upr,i.Survey.income.all.upr>as.numeric(quart[2,2])&i.Survey.income.all.upr<=as.numeric(quart[3,2]),3),
                                     quart.all.upr=replace(quart.all.upr,i.Survey.income.all.upr>as.numeric(quart[1,2])&i.Survey.income.all.upr<=as.numeric(quart[2,2]),2)) %>%
  mutate(quart.noloss=1) %>% mutate(quart.noloss=replace(quart.noloss,i.Survey.income.noloss>as.numeric(quart[3,2]),4),
                                 quart.noloss=replace(quart.noloss,i.Survey.income.noloss>as.numeric(quart[2,2])&i.Survey.income.noloss<=as.numeric(quart[3,2]),3),
                                 quart.noloss=replace(quart.noloss,i.Survey.income.noloss>as.numeric(quart[1,2])&i.Survey.income.noloss<=as.numeric(quart[2,2]),2))

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
satis$quart.fert.lwr<-factor(dF.income$quart.fert.lwr)
satis$quart.fert.upr<-factor(dF.income$quart.fert.upr)

satis$quart.bmass<-factor(dF.income$quart.bmass)
satis$quart.bmass.lwr<-factor(dF.income$quart.bmass.lwr)
satis$quart.bmass.upr<-factor(dF.income$quart.bmass.upr)

satis$quart.cpb<-factor(dF.income$quart.cpb)
satis$quart.cpb.lwr<-factor(dF.income$quart.cpb.lwr)
satis$quart.cpb.upr<-factor(dF.income$quart.cpb.upr)

satis$quart.all<-factor(dF.income$quart.all)
satis$quart.all.lwr<-factor(dF.income$quart.all.lwr)
satis$quart.all.upr<-factor(dF.income$quart.all.upr)

satis$quart.noloss<-factor(dF.income$quart.noloss)

#mx <- dF.pov %>% group_by(Cocoa.income.quart) %>% summarise(satisfaction=mean(Satisfaction.life.overall,na.rm=T),satisfaction.se=sd(Satisfaction.life.overall,na.rm=T)/length(Cocoa.income.quart))

satis <- satis %>% mutate(quartile="top") %>% mutate(quartile=replace(quartile,as.numeric(income.quartile)<3,"bottom")) %>% 
  mutate(Fertiliser=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert),type="response", se.fit=TRUE)$fit,Fertiliser.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert.lwr),type="response", se.fit=TRUE)$fit,
         Fertiliser.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert.upr),type="response", se.fit=TRUE)$fit, Biomass=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass),type="response", se.fit=TRUE)$fit,
         Biomass.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass.lwr),type="response", se.fit=TRUE)$fit, Biomass.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass.upr),type="response", se.fit=TRUE)$fit,
         Capsid=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb),type="response", se.fit=TRUE)$fit,Capsid.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb.lwr),type="response", se.fit=TRUE)$fit,
         Capsid.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb.upr),type="response", se.fit=TRUE)$fit, All=predict.glm(x, data.frame(Cocoa.income.quart=quart.all),type="response", se.fit=TRUE)$fit,
         All.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.all.lwr),type="response", se.fit=TRUE)$fit,All.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.all.upr),type="response", se.fit=TRUE)$fit,
         NoLoss=predict.glm(x, data.frame(Cocoa.income.quart=quart.noloss),type="response", se.fit=TRUE)$fit)
  

#sat <- satis %>% summarise(Measure="Satisfaction",Original=mean(likert,na.rm=T))
satis.lwr<-satis %>% select(plot,quartile,Fertiliser.lwr,Biomass.lwr,Capsid.lwr,All.lwr) %>% 
  gather(key="parameter",value="new.likert",c(-plot,-quartile)) %>%
  group_by(parameter,quartile) %>% summarise(Measure="Satisfaction",Lower=mean(new.likert,na.rm=T))
satis.lwr$parameter<-gsub(".lwr","",satis.lwr$parameter)
satis.upr<-satis %>% select(plot,quartile,Fertiliser.upr,Biomass.upr,Capsid.upr,All.upr) %>% 
  gather(key="parameter",value="new.likert",c(-plot,-quartile)) %>%
  group_by(parameter,quartile) %>% summarise(Measure="Satisfaction",Upper=mean(new.likert,na.rm=T))
satis.upr$parameter<-gsub(".upr","",satis.upr$parameter)

satis <- satis %>% select(plot,quartile,likert,Fertiliser,Biomass,Capsid,All,NoLoss) %>% 
  gather(key="parameter",value="new.likert",c(-plot,-likert,-quartile)) %>%
  group_by(parameter,quartile) %>% summarise(Measure="Satisfaction",Original=mean(likert,na.rm=T),Potential=mean(new.likert,na.rm=T))

satis <- left_join(satis,satis.lwr,by=c("parameter","quartile","Measure"))
satis <- left_join(satis,satis.upr,by=c("parameter","quartile","Measure"))

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
food$quart.fert.lwr<-factor(dF.income$quart.fert.lwr)
food$quart.fert.upr<-factor(dF.income$quart.fert.upr)

food$quart.bmass<-factor(dF.income$quart.bmass)
food$quart.bmass.lwr<-factor(dF.income$quart.bmass.lwr)
food$quart.bmass.upr<-factor(dF.income$quart.bmass.upr)

food$quart.cpb<-factor(dF.income$quart.cpb)
food$quart.cpb.lwr<-factor(dF.income$quart.cpb.lwr)
food$quart.cpb.upr<-factor(dF.income$quart.cpb.upr)

food$quart.all<-factor(dF.income$quart.all)
food$quart.all.lwr<-factor(dF.income$quart.all.lwr)
food$quart.all.upr<-factor(dF.income$quart.all.upr)

food$quart.noloss<-factor(dF.income$quart.noloss)
food$amount<-dF.pov[match(dF.income$plot,dF.pov$plot),"Food.amount"]


#food.2<-summarySE(dF.pov, measurevar="Food.security", groupvars=c("Cocoa.income.quart"))
food <- food %>% mutate(quartile="top") %>% mutate(quartile=replace(quartile,as.numeric(income.quartile)<3,"bottom")) %>% 
  mutate(Fertiliser=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert),type="response", se.fit=TRUE)$fit,Fertiliser.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert.lwr),type="response", se.fit=TRUE)$fit,
         Fertiliser.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.fert.upr),type="response", se.fit=TRUE)$fit, Biomass=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass),type="response", se.fit=TRUE)$fit,
         Biomass.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass.lwr),type="response", se.fit=TRUE)$fit, Biomass.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.bmass.upr),type="response", se.fit=TRUE)$fit,
         Capsid=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb),type="response", se.fit=TRUE)$fit,Capsid.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb.lwr),type="response", se.fit=TRUE)$fit,
         Capsid.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.cpb.upr),type="response", se.fit=TRUE)$fit, All=predict.glm(x, data.frame(Cocoa.income.quart=quart.all),type="response", se.fit=TRUE)$fit,
         All.lwr=predict.glm(x, data.frame(Cocoa.income.quart=quart.all.lwr),type="response", se.fit=TRUE)$fit,All.upr=predict.glm(x, data.frame(Cocoa.income.quart=quart.all.upr),type="response", se.fit=TRUE)$fit,
         NoLoss=predict.glm(x, data.frame(Cocoa.income.quart=quart.noloss),type="response", se.fit=TRUE)$fit)

food.lwr <- food %>% select(plot,quartile,Fertiliser.lwr,Biomass.lwr,Capsid.lwr,All.lwr) %>% gather(key="parameter",value="prob",c(-plot,-quartile)) %>%
  group_by(parameter,quartile) %>% summarise(Measure="Food Security",Lower=mean(prob,na.rm=T))
food.lwr$parameter<-gsub(".lwr","",food.lwr$parameter)
food.upr <- food %>% select(plot,quartile,Fertiliser.upr,Biomass.upr,Capsid.upr,All.upr) %>% gather(key="parameter",value="prob",c(-plot,-quartile)) %>%
  group_by(parameter,quartile) %>% summarise(Measure="Food Security",Upper=mean(prob,na.rm=T))
food.upr$parameter<-gsub(".upr","",food.upr$parameter)

food <- food %>% select(plot,quartile,amount,Fertiliser,Biomass,Capsid,All,NoLoss) %>% gather(key="parameter",value="prob",c(-plot,-amount,-quartile)) %>%
  group_by(parameter,quartile) %>% summarise(Measure="Food Security",Original=mean(amount,na.rm=T),Potential=mean(prob,na.rm=T))
food<-left_join(food,food.lwr,by=c("parameter","quartile","Measure"))
food<-left_join(food,food.upr,by=c("parameter","quartile","Measure"))

output=bind_rows(output,food)

#write.csv(output,paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.NewMeans.csv"))
write.csv(output,paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.alt.NewMeans.csv"))