#Code producing final figures for ES Cocoa paper

library(ggplot2)
library(reshape)
library(plyr)
library(gridExtra)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

#calcluation of cocoa income per farm (season 2014-15)

#comparison of poverty measures and cocoa income
dF<-data.frame(read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv")),stringsAsFactors = F)
dF.pov<-data.frame(read.csv(paste0(getwd(),"/HouseholdData/PovertyMeasures.csv")),stringsAsFactors = F)

dF[,44:53]<-dF.pov[match(dF$plot,dF.pov$PLOTCODE),3:12]
dF[is.na(dF)]<-0

dF$S.income.ha<-dF$Cocoa.Income/dF$Land.area.ha

lm_eqn <- function(df){
  m <- lm(y ~ x, df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))                 
}

#plot measured income per ha with those reported in survey (per ha)
df<-data.frame(cbind(dF$S.income.ha,dF$o.final.income))
colnames(df)<-c("x","y")
g1<-ggplot(dF,aes(S.income.ha,o.final.income))+geom_point()+geom_smooth(method="lm")+
  xlab("Survey Cocoa Income [cedis/ha]")+ylab("Estimated Cocoa Income [cedis/ha]")+
  geom_text(aes(label=plot),hjust=0.5, vjust=0)+
  geom_text(x=2000,y=900,label=lm_eqn(df), parse = TRUE)+
  geom_abline(slope=1,linetype="dashed")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))

#comparing per ha survey income with estimated income less inputs
df2<-data.frame(cbind(dF$S.income.ha,dF$o.finalp.income))
colnames(df2)<-c("x","y")
g2<-ggplot(dF,aes(S.income.ha,o.finalp.income))+geom_point()+geom_smooth(method="lm")+
  xlab("Survey Cocoa Income [cedis/ha]")+ylab("Estimated Cocoa Income Less Inputs [cedis/ha]")+
  #geom_text(aes(label=plot),hjust=0.5, vjust=0)+
  geom_text(x=2000,y=900,label=lm_eqn(df2), parse = TRUE)+
  geom_abline(slope=1,linetype="dashed")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))

#total income (not per ha) comparison
df3<-data.frame(cbind(dF$Cocoa.Income,dF$o.estp.income))
colnames(df3)<-c("x","y")

g3<-ggplot(dF,aes(Cocoa.Income,o.estp.income))+geom_point()+geom_smooth(method="lm")+
  xlab("Survey Cocoa Income [cedis]")+ylab("Estimated Cocoa Income Less Inputs [cedis]")+
  geom_text(aes(label=plot),hjust=0.5, vjust=0)+
  geom_text(x=15000,y=5000,label=lm_eqn(df3), parse = TRUE)+
  geom_abline(slope=1,linetype="dashed")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))

df4<-df3[df3$x<10000,]
g4<-ggplot(dF[dF$Cocoa.Income<10000,],aes(Cocoa.Income,o.estp.income))+geom_point()+geom_smooth(method="lm")+
  xlab("Survey Cocoa Income [cedis]")+ylab("Estimated Cocoa Income Less Inputs [cedis]")+
  #geom_text(aes(label=plot),hjust=0.5, vjust=0)+
  geom_text(x=4000,y=20000,label=lm_eqn(df4), parse = TRUE)+
  geom_abline(slope=1,linetype="dashed")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))

g5<-grid.arrange(g1,g2,g3,g4,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp_IncomeComparison.pdf",g5,height=12,width=12)
rm(g1,g2,g3,g4,g5,df2,df3,df4)

#calculate quartiles from plot values
#dF$Cocoa.Income.plot<-dF$i.pot.income-dF$i.lbc.loss
quant<-quantile(dF$o.estp.income)
dF$Cocoa.income.quart.plot<-1
dF[dF$o.estp.income>quant[2],"Cocoa.income.quart.plot"]<-2
dF[dF$o.estp.income>quant[3],"Cocoa.income.quart.plot"]<-3
dF[dF$o.estp.income>quant[4],"Cocoa.income.quart.plot"]<-4

dF$Cocoa.income.quart.plot<-factor(dF$Cocoa.income.quart.plot)
dF$Cocoa.income.quart<-factor(dF$Cocoa.income.quart)

#graph income quartile with field cocoa income estimate
g1<-ggplot(dF,aes(Cocoa.income.quart.plot,o.estp.income))+geom_boxplot()+
  xlab("Income Quartile")+ylab("Estimated Cocoa Income")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))

df1<-data.frame(cbind(as.character(dF$plot),dF$Cocoa.income.quart,dF$Cocoa.Income,dF$o.estp.income))
colnames(df1)<-c("plot","Cocoa.income.quart","Survey","Estimated")
df2<-melt(df1,id.vars=c("plot","Cocoa.income.quart"))
df2$value<-as.numeric(as.character(df2$value))

g2<-ggplot(df2,aes(Cocoa.income.quart,value,fill=variable))+geom_boxplot()+
  xlab("Survey Income Quartile")+ylab("Estimated Cocoa Income")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,legend.position = "top")
g3<-grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp_IncomeMeasures.pdf",g3,height=6,width=12)

#calculate values of quartiles from sub-sample
dF.inc<-ddply(dF,.(Cocoa.income.quart),summarise,cocoaIncome=mean(o.estp.income,na.rm=T),cI.sd=sd(o.estp.income,na.rm=T),noFarms=length(o.estp.income))
#add income values from household survey
x<-ddply(dF.pov,.(Cocoa.income.quart),summarise,cocoaIncome.1=mean(Cocoa.Income,na.rm=T),cI.sd.1=sd(Cocoa.Income,na.rm=T),noFarms.1=length(Cocoa.Income))
dF.inc[,5:7]<-x[match(dF.inc$Cocoa.income.quart,x$Cocoa.income.quart),2:4]
x<-ddply(dF,.(Cocoa.income.quart.plot),summarise,cocoaIncome.2=mean(o.estp.income,na.rm=T),cI.sd.2=sd(o.estp.income,na.rm=T),noFarms.2=length(o.estp.income))
dF.inc[,8:10]<-x[match(dF.inc$Cocoa.income.quart,x$Cocoa.income.quart),2:4]

write.csv(dF.inc,paste0(getwd(),"/Analysis/ES/Income.quartiles.table.",season,".csv"))

#calculate food security z-score (number-mean)/SD
dF$z.Food.security<-(dF$Food.security-mean(dF.pov$Food.security))/sd(dF.pov$Food.security)
dF.pov$z.Food.security<-(dF.pov$Food.security-mean(dF.pov$Food.security))/sd(dF.pov$Food.security)

#do ANOVA analysis using survey quartiles
#dx<-list()
x<-aov(Gender~factor(Cocoa.income.quart),data=dF)
results<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results$quarts<-row.names(results)
results$category<-"Gender"
x<-aov(Basic.needs~factor(Cocoa.income.quart),data=dF)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Basic Needs"
x<-aov(Health~factor(Cocoa.income.quart),data=dF)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Health"
x<-aov(Education~factor(Cocoa.income.quart),data=dF)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Education"
x<-aov(Assets~factor(Cocoa.income.quart),data=dF)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Assets"
x<-aov(z.Food.security~factor(Cocoa.income.quart),data=dF)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Food Security"

#pick out significant relationships
results.1<-results[results$p.adj<0.1,]

#do again for household survey
x<-aov(Gender~factor(Cocoa.income.quart),data=dF.pov)
results<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results$quarts<-row.names(results)
results$category<-"Gender"
x<-aov(Basic.needs~factor(Cocoa.income.quart),data=dF.pov)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Basic Needs"
x<-aov(Health~factor(Cocoa.income.quart),data=dF.pov)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Health"
x<-aov(Education~factor(Cocoa.income.quart),data=dF.pov)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Education"
x<-aov(Assets~factor(Cocoa.income.quart),data=dF.pov)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Assets"
x<-aov(z.Food.security~factor(Cocoa.income.quart),data=dF.pov)
results[(nrow(results)+1):(nrow(results)+6),1:4]<-data.frame(TukeyHSD(x, conf.level = 0.95)[[1]])
results[(nrow(results)-5):nrow(results),"quarts"]<-results[1:6,"quarts"]
results[(nrow(results)-5):nrow(results),"category"]<-"Food Security"

#pick out significant relationships
results.2<-results[results$p.adj<0.05,]

results.1$sample<-"Plots"
results.2$sample<-"Survey"
results.3<-data.frame(rbind(results.1,results.2),stringsAsFactors = F)
results.3$quarts<-paste(" ",results.3$quarts)
#save table of values
write.csv(results.3,paste0(getwd(),"/Analysis/ES/Poverty.quartiles.table.",season,".csv"))

#combine for graphing income vs poverty measure
dF.1<-data.frame(cbind(dF$plot,dF$o.final.income,dF[,44:52],dF$z.Food.security),stringsAsFactors = F)
colnames(dF.1)<-c("plot","est.income",colnames(dF.1[3:11]),"Food.security")
dF.2<-melt(dF.1,id.vars=c("plot","est.income","Cocoa.Income","Land.area.ha","Cocoa.area.ha","Cocoa.income.quart"))
dF.2$sig<-dX[match(dF.2$variable,dX[1:3,"Category"]),"sig"]
dF.2$variable<-as.character(dF.2$variable)
#dF.2[!is.na(dF.2$sig),"variable"]<-paste0(dF.2[!is.na(dF.2$sig),"variable"],"*")

ggplot(dF.2,aes(factor(Cocoa.income.quart),value))+geom_boxplot()+facet_wrap(~variable)+
  xlab("Income Quartile")+ylab("Index Value")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp_PovertyMeasures.pdf",height=6,width=11)

#combine for graphing income vs poverty measure
dF.3<-data.frame(cbind(dF.pov[,3:11],dF.pov$z.Food.security),stringsAsFactors = F)
colnames(dF.3)<-c(colnames(dF.pov[3:11]),"Food.security")
dF.4<-melt(dF.3,id.vars=c("Land.area.ha","Cocoa.area.ha","Cocoa.Income","Cocoa.income.quart"))

dF.4$sig<-dX[match(dF.4$variable,gsub(".all","",dX[4:7,"Category"])),"sig"]
dF.4$variable<-as.character(dF.4$variable)
dF.4[!is.na(dF.4$sig),"variable"]<-paste0(dF.4[!is.na(dF.4$sig),"variable"],"*")

ggplot(dF.4,aes(factor(Cocoa.income.quart),value))+geom_boxplot()+facet_wrap(~variable)+
  xlab("Income Quartile")+ylab("Index Value")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp_PovertyMeasures.all.pdf",height=6,width=11)

#identify which groups are actually different
#x<-pairwise.t.test(dF$Gender, dF$Cocoa.income.quart, p.adjust = "bonferroni")[[3]]
#dX[dX$Category=="Gender","quart"]<-paste(which(x<0.05,arr.ind = T))

#cocoa yield model
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta6.median.csv"))
tmp<-tmp[!is.na(tmp$full),]

tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Soil\nMoisture","Distance\nFrom Forest","Age of Cocoa","Soil pH","(Intercept)"))

#add ES Component
tmp[tmp$Comparison=="Phosphorous"|tmp$Comparison=="Soil\nMoisture"|tmp$Comparison=="Soil pH"|tmp$Comparison=="Nitrogen"|tmp$Comparison=="Soil pH:\nSoil Moisture","Component"]<-"Soil"
tmp[tmp$Comparison=="Distance to\nBiomass"|tmp$Comparison=="Shade Density"|tmp$Comparison=="Harvesting\nLabour"|tmp$Comparison=="Cocoa\nDensity"|tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]"|tmp$Comparison=="Canopy Gap"|tmp$Comparison=="Age of Cocoa"|tmp$Comparison=="Yield\nVariation","Component"]<-"Management"
tmp[tmp$Comparison=="Black Pod\nIncidence"|tmp$Comparison=="Capsid Incidence"|tmp$Comparison=="Mistletoe","Component"]<-"Disease"
tmp[tmp$Comparison=="Cherelle Set"|tmp$Comparison=="Pollination\nTreatment"|tmp$Comparison=="Flower\nBuds"|tmp$Comparison=="Biomass:Cherelle Set","Component"]<-"Pollination"
tmp[tmp$Comparison=="Mean\nTemperature"|tmp$Comparison=="Flower Buds:\nMean Temperature"|tmp$Comparison=="Water\nStress","Component"]<-"Micro-Climate"
tmp[tmp$Comparison=="Distance\nFrom Forest"|tmp$Comparison=="Forest Distance:Mistletoe","Component"]<-"Landscape"

tmp$Component<-factor(tmp$Component)

ggplot(tmp[!is.na(tmp$Importance),], aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL,color=Component)) + geom_errorbar(width = 0.1,size=1.0) + geom_point(size=2.0)+
  theme(text = element_text(size=16),legend.key = element_rect(colour = "white", fill = NA))+ggtitle(paste0("Influence of ES factors on Plot Yield"))+geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Variable [ranked by importance]")+ylab("Coefficient")+theme(panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),axis.text.x=element_text(angle = 45,hjust=1))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure3_YieldModel.pdf",height=6,width=11)

#calculate maximum possible yield
dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.",year,".csv"))

#open calculated yield contribution per factor
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

#plot to compare measured vs modelled yield
ggplot(dF.1,aes(yield.tree,yield.mod))+geom_point()+geom_abline(slope=1,intercept=0)+xlab("Measured Yield [kg tree-1]")+ylab("Modelled Yield [kg tree-1]")+
  ylim(0,2.2)+xlim(0,2.2)+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig1_Comparisonofmodelledyield.pdf",height=6,width=6)

#load calculated contributions by factor
dF.4<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.per.tree.per.farm.contribution",season,".med.csv"))

#replace negative values with 0
#dF.4[dF.4$value<0,"value"]<-0

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg tree-1]")+
  xlab("Farm")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    ,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_pertree_",season,".med.pdf"))

#load inputs for income calculations
dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.",season,".csv"))

