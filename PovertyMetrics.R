library(plyr)
library(ggplot2)
library(reshape)
library(gridExtra)


setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

#source("/Volumes/ELDS/ECOLIMITS/R_codes/HelperFunctions/summarySE.R")

dF.pov<-data.frame(read.csv(paste0(getwd(),"/HouseholdData/PovertyMeasures1.csv")),stringsAsFactors = F)
#dF.pov$z.Food.security<-(dF.pov$Food.security-mean(dF.pov$Food.security))/sd(dF.pov$Food.security)
quart<-ddply(dF.pov,.(Cocoa.income.quart),summarise,income=max(Cocoa.Income,na.rm=T))

dF.income<-data.frame(read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv")),stringsAsFactors = F)

#assign new quartile values
dF.income$new.quartile<-1
dF.income[dF.income$i.Survey.income>quart[1,2],"new.quartile"]<-2
dF.income[dF.income$i.Survey.income>quart[2,2],"new.quartile"]<-3
dF.income[dF.income$i.Survey.income>quart[3,2],"new.quartile"]<-4

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

#to calculate probability of household having a child miss school at new income:
dF.income$New.Income<-rowSums(cbind(dF.income$Survey.income,dF.income$i.Survey.income))

results<-list()
for(i in 1:length(dF.income$New.Income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$New.Income[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$New.Income[i]), se.fit=TRUE)
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

educ<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
educ$plot<-as.character(dF.income$plot)
educ$Survey.income<-dF.income$Survey.income
educ$New.income<-dF.income$New.Income
write.csv(educ,paste0(getwd(),"/Analysis/ES/Education.probabilities.wincome.csv"))

output<-data.frame(cbind("Education",mean(educ$orig.prob),mean(educ$prob)),stringsAsFactors = F)
colnames(output)<-c("Measure","Original","Potential")

#find mean probability before and after income shift
g1<-ggplot(educ,aes(log(Survey.income),orig.prob))+geom_point()+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95))+
  geom_hline(yintercept=mean(educ$orig.prob),linetype="dashed",color="grey")+geom_hline(yintercept=mean(educ$prob),linetype="dashed",color="black")+
  geom_vline(xintercept=log(mean(educ$Survey.income)),color="grey",linetype="dashed") +
  geom_vline(xintercept = log(mean(educ$New.income)),color="black",linetype="dashed") +
  xlab("Log of Income [cedis]")+ylab("Probability of Child Missing School")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Probability.education.income.increase.survey.pdf"))


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

results<-list()
for(i in 1:length(dF.income$New.Income)){
  pi.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$New.Income[i]),
                       type="response", se.fit=TRUE)
  y<-data.frame(as.numeric(pi.hat$fit),stringsAsFactors = F)
  l.hat = predict.glm(x, data.frame(Cocoa.Income=dF.income$New.Income[i]), se.fit=TRUE)
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

TV<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
TV$plot<-as.character(dF.income$plot)
TV$Survey.income<-dF.income$Survey.income
TV$New.income<-dF.income$New.Income
write.csv(TV,paste0(getwd(),"/Analysis/ES/Asset.tv.probabilities.wincome.csv"))

output[2,1:3]<-cbind("Asset (TV)",mean(TV$orig.prob),mean(TV$prob))

g2<-ggplot(TV,aes(log(Survey.income),orig.prob))+geom_point()+geom_errorbar(width=.1, aes(ymin=orig.ci5, ymax=orig.ci95))+
  geom_vline(xintercept = log(mean(TV$New.income)),linetype="dashed")+geom_hline(yintercept = mean(TV$prob),linetype="dashed") +
  geom_vline(xintercept = log(mean(TV$Survey.income)),linetype="dashed",color="grey")+geom_hline(yintercept = mean(TV$orig.prob),linetype="dashed",color="grey")+
  xlab("Log of Income [cedis]")+ylab("Probability of Household Owning a TV")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Probability.assetTV.income.increase.survey.pdf"))

#do for satisfaction and food security, using quartiles

dF.pov$Cocoa.income.quart<-factor(dF.pov$Cocoa.income.quart)
x<-glm(Satisfaction.life.overall~Cocoa.income.quart,family=poisson,data=dF.pov)
summary(x)

#check the residual deviance/degrees of freedom, if greater than 1 data is overdispersed
1-pchisq(x$deviance,x$df.residual)

#land<-data.frame(do.call(rbind.data.frame,results),stringsAsFactors = F)
satis<-data.frame(as.character(dF.income$plot),stringsAsFactors = F)
colnames(satis)<-"plot"
satis$likert<-dF.pov[match(dF.income$plot,dF.pov$PLOTCODE),"Satisfaction.life.overall"]
satis$income.quartile<-dF.income$Survey.quart
satis$new.quartile<-factor(dF.income$new.quartile)

mx<-ddply(dF.pov,.(Cocoa.income.quart),summarise,satisfaction=mean(Satisfaction.life.overall,na.rm=T),satisfaction.se=sd(Satisfaction.life.overall,na.rm=T)/length(Cocoa.income.quart))

satis$new.likert<-predict.glm(x, data.frame(Cocoa.income.quart=satis$new.quartile),
            type="response", se.fit=TRUE)$fit

output[3,1:3]<-cbind("Satisfaction",mean(satis$likert),mean(satis$new.likert))

g4<-ggplot(mx,aes(Cocoa.income.quart,satisfaction))+geom_point(color="black")+geom_errorbar(aes(ymin=satisfaction-satisfaction.se,ymax=satisfaction+satisfaction.se),width=0.1)+
  ylim(1,3)+geom_hline(yintercept = mean(satis$likert),linetype="dashed",color="grey")+
  geom_hline(yintercept = mean(satis$new.likert),linetype="dashed")+
  xlab("Cocoa Income Quartiles")+ylab("Reported Satisfaction [0-4]")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Satisfaction.vs.incomequartile.pdf"))

#write.csv(land,paste0(getwd(),"/Analysis/ES/Landarea.probabilities.wincome.csv"))

#do again for food security, and income quartile for plot farmers
x<-glm(Food.amount~Cocoa.income.quart,family=binomial,data=dF.pov)
summary(x)

#dF.income$total.land<-dF.pov[match(dF.income$plot,dF.pov$PLOTCODE),"Land.area.ha"]
#dF.income$Food.security<-dF.pov[match(dF.income$plot,dF.pov$PLOTCODE),"Food.security"]
#dF.income$Gender<-dF.pov[match(dF.income$plot,dF.pov$PLOTCODE),"Gender"]

#x<-glm(Food.security~Survey.income+total.land,family=poisson,data=dF.income)
#summary(x)

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
food$income.quartile<-dF.income$Survey.quart
food$new.quartile<-factor(dF.income$new.quartile)
food$amount<-dF.pov[match(dF.income$plot,dF.pov$PLOTCODE),"Food.amount"]

mx<-ddply(dF.pov,.(Cocoa.income.quart),summarise,food=mean(Food.amount,na.rm=T),se=sd(Food.amount,na.rm=T)/length(Cocoa.income.quart))

#food.2<-summarySE(dF.pov, measurevar="Food.security", groupvars=c("Cocoa.income.quart"))
food$new.amount<-predict.glm(x, data.frame(Cocoa.income.quart=food$new.quartile),
            type="response", se.fit=TRUE)$fit

output[4,1:3]<-cbind("Food Amount",mean(food$amount),mean(food$new.amount))

g3<-ggplot(mx,aes(Cocoa.income.quart,food))+geom_point()+geom_errorbar(aes(ymin=food-se,ymax=food+se),width=0.1)+
  geom_hline(yintercept = mean(food$amount),linetype="dashed",color="grey")+
  geom_hline(yintercept = mean(food$new.amount),linetype="dashed")+
  xlab("Cocoa Income Quartiles")+ylab("Probability of Having Adequate Amount of Food")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Income.vs.food.security.survey.pdf"))

g5<-grid.arrange(g1,g2,g3,g4,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ES/Povertymeasures.vs.income.figures.pdf"),g5,width=10,height=10)

output$Original<-as.numeric(output$Original)
output$Potential<-as.numeric(output$Potential)

write.csv(output,paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.NewMeans.csv"))
