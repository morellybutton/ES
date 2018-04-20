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

dF[,(ncol(dF)+1):(ncol(dF)+ncol(dF.pov)-2)]<-dF.pov[match(dF$plot,dF.pov$PLOTCODE),3:ncol(dF.pov)]
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

#cocoa yield model
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta6.median.csv"))
tmp<-tmp[!is.na(tmp$full),]

tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Soil\nMoisture","Distance\nFrom Forest","Age of Cocoa","Soil pH","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

g1<-ggplot(tmp[!is.na(tmp$Importance),], aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width = 0.1,size=1.0) + geom_point(size=2.0)+
  theme(text = element_text(size=16),legend.key = element_rect(colour = "white", fill = NA))+ggtitle(paste0("Influence of ES factors on Plot Yield"))+geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Variable [ranked by importance]")+ylab("Coefficient")+theme(panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),axis.text.x=element_text(angle = 45,hjust=1))
g1 + coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure3_YieldModel.pdf",height=6,width=11)

#calculate maximum possible yield
dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.",year,".csv"))

#open calculated yield contribution per factor
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

#plot to compare measured vs modelled yield
ggplot(dF.1,aes(yield.tree,yield.mod))+geom_point()+geom_abline(slope=1,intercept=0)+xlab("Measured Yield [kg tree-1]")+ylab("Modelled Yield [kg tree-1]")+
  ylim(0,2.2)+xlim(0,2.2)+theme_classic()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig1_Comparisonofmodelledyield.pdf",height=6,width=6)

#load calculated contributions by factor
dF.4<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.per.tree.per.farm.contribution",season,".med.csv"))

#replace negative values with 0
#dF.4[dF.4$value<0,"value"]<-0

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg tree-1]")+
  xlab("Farm")+theme_classic()+theme(axis.text.x=element_text(angle = 45,hjust=1),legend.title=element_blank(),legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/Yield_potential_increase_pertree_",season,".med.pdf"))

#load inputs for income calculations
dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.",season,".csv"))

#look at yield soil pH relationship
df<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.",year,".csv"))

g1<-ggplot(df,aes(pH,HeavyCrop)) + geom_point() + stat_smooth(method="lm")+
  xlab("Soil pH")+ylab("Median Yield [kg/tree]")+theme(
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

g2<-ggplot(df,aes(soil.moist,HeavyCrop)) + geom_point() + stat_smooth(method="lm")+
  xlab("Soil Moisture")+ylab("Median Yield [kg/tree]")+theme(
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

g3<-ggplot(df,aes(Age.of.cocoa,HeavyCrop)) + geom_point() + stat_smooth(method="lm")+
  xlab("Age of Cocoa")+ylab("Median Yield [kg/tree]")+theme(
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

g4<-ggplot(df,aes(Biomass,HeavyCrop)) + geom_point() + stat_smooth(method="lm")+
  xlab("Distance to Biomass")+ylab("Median Yield [kg/tree]")+theme(
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

g5<-ggplot(df,aes(Cocoa.density,HeavyCrop)) + geom_point() + stat_smooth(method="lm")+
  xlab("Cocoa Density")+ylab("Median Yield [kg/tree]")+theme(
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

g6<-ggplot(df,aes(distance.cont,HeavyCrop)) + geom_point() + stat_smooth(method="lm")+
  xlab("Distance from Forest")+ylab("Median Yield [kg/tree]")+theme(
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

g7<-ggplot(df,aes(No.applications.yr,HeavyCrop)) + geom_point() + stat_smooth(method="lm")+
  xlab("Average Number of Fertiliser Applications [/yr]")+ylab("Median Yield [kg/tree]")+theme(
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

g8<-grid.arrange(g7,g4,g2,g6,g1,g5,g3,ncol=3)
ggsave(paste0(getwd(),"/Analysis/ES/YieldModel.LinearComparisons.pdf"),g8,height=10,width=10)