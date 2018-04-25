#Code producing final figures for ES Cocoa paper

library(tidverse)
library(gridExtra)
library(ggpubr)

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

tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Distance\nFrom Forest","Capsid Incidence","Canopy Gap","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

g1<-ggplot(tmp[!is.na(tmp$Importance),], aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width = 0.1,size=1.0) + geom_point(size=2.0)+
  theme(text = element_text(size=16),legend.key = element_rect(colour = "white", fill = NA))+ggtitle(paste0("Influence of ES factors on Per Tree Yield"))+geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size")+theme(panel.background=element_blank(),axis.line.x = element_line(color = 'black'),axis.line.y = element_line(color = 'black'),axis.text.x=element_text(angle = 45,hjust=1))
g1 + coord_flip()
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure1_YieldModel.pdf")

#dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.",year,".csv"))

#open calculated yield contribution per factor
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

#plot to compare measured vs modelled yield
r.adj<-summary(lm(yield.mod~yield.tree,data=dF.1))$adj.r.squared
ggplot(dF.1,aes(yield.tree,yield.mod))+geom_point()+geom_abline(slope=1,intercept=0)+xlab("Measured Yield [kg tree-1]")+ylab("Modelled Yield [kg tree-1]")+
  ylim(0,2.2)+xlim(0,2.2)+theme_classic()+geom_text(aes(label = paste("R^2: ",signif(r.adj,2),sep="")),parse=T,x=0.25,y=1.5)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig1_Comparisonofmodelledyield.pdf",height=6,width=6)

g1<-ggplot(dF.1,aes(Biomass,y.biomass))+geom_point()+geom_errorbar(aes(x=Biomass,ymin=y.biomass.lwr,ymax=y.biomass.upr),width=0.1)+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Distance to Biomass [m]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()
g2<-ggplot(dF.1,aes(Cocoa.density,y.cdensity))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Cocoa Density [ha-1]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=Cocoa.density,ymin=y.cdensity.lwr,ymax=y.cdensity.upr),width=0.1)
g3<-ggplot(dF.1,aes(distance.cont,y.fdist))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Distance from Forest [m]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=distance.cont,ymin=y.fdist.lwr,ymax=y.fdist.upr),width=0.1)
g4<-ggplot(dF.1,aes(No.applications.yr,y.fert))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Fertiliser Application [No/yr]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=No.applications.yr,ymin=y.fert.lwr,ymax=y.fert.upr),width=0.1)
g5<-ggplot(dF.1,aes(Canopy.gap.dry,y.cgap))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Canopy Gap [%]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=Canopy.gap.dry,ymin=y.cgap.lwr,ymax=y.cgap.upr),width=0.1)
g6<-ggplot(dF.1,aes(PropCPB,y.cpb))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Capsid Incidence [prop pods]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=PropCPB,ymin=y.cpb.lwr,ymax=y.cpb.upr))

g7<-grid.arrange(g1,g2,g3,g4,g5,g6,ncol=3)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig2_modelparameters.pdf",g7,width=8,height=5)

#load calculated contributions by factor
dF.4<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.per.tree.per.farm.contribution",season,".med.csv"))
dF.2 <- read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))

dF.4 <- left_join(dF.4,dF.2 %>% select(plot,yield.pot),by="plot")

ggplot(dF.4,aes(fct_reorder(plot,yield.pot,.desc=T),value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg tree-1]")+
  xlab("Farm")+theme_classic()+theme(axis.text.x=element_text(angle = 45,hjust=1),legend.title=element_blank(),legend.position="top")
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/SuppFig3_YieldIncreasePotential.pdf")

#load inputs for income calculations
dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))
#convert to usd
usd<-0.23

g1<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.bmass*usd)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dotted") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("Distance From Biomass") + xlim(-100,1500) + ylim(-100,1500) + geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g2<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.fert*usd)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("Fertiliser Application") + xlim(-100,2600) + ylim(-100,2600)+ geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g3<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.cgap*usd)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("Canopy Gap") + xlim(-100,1400) + ylim(-100,1400)+ geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g4<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.cpb*usd)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("Capsid Management") + xlim(-100,2600) + ylim(-100,2600)+ geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g5<-grid.arrange(g1,g3,g2,g4,ncol=2)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure2_CocoaIncome_potential_increase.byparameter.pdf"),g5,height=6,width=7)

#need to do final arrow figures of poverty changes
dF.2<-read.csv(paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.NewMeans.csv"))
dF.3 <- dF.2 %>% select(-X) %>% gather(key="variable",value="value",c(-Measure,-parameter,-quartile))
g1<-ggplot(dF.3 %>% filter(Measure=="Education")) + geom_point(aes(variable,value)) + theme_classic() +
  ylab("Quartile Mean Probability") + xlab("") + ggtitle("Missing School") + ylim(0,1.0)+
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                       x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                      x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Capsids"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Capsids"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                   x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14))
  
  
g2<-ggplot(dF.3 %>% filter(Measure=="Assets")) + geom_point(aes(variable,value)) + theme_classic() +
  ylab("Quartile Mean Probability") + xlab("") + ggtitle("Owning a TV") + ylim(0,1.0)+
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                       x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                      x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Capsid"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Capsid"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                   x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14))

g3<-ggplot(dF.3 %>% filter(Measure=="Food Security")) + geom_point(aes(variable,value)) + theme_classic() +
  ylab("Quartile Mean Probability") + xlab("") + ggtitle("Adequate Amount of Food") + ylim(0,1.0)+
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                 x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                 x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                   x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Capsid"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Capsid"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                               x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14))

g4<-ggplot(dF.3 %>% filter(Measure=="Satisfaction")) + geom_point(aes(variable,value)) + theme_classic() +
  ylab("Quartile Mean Likert Value") + xlab("") + ggtitle("Satisfaction with Life") + ylim(0,4)+
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                        x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                        x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                           x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                          x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Capsid"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                       x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Capsid"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                      x="Original",xend="Potential",color=parameter,linetype=quartile),size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14))

ggarrange(g1,g3,g2,g4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure3_changeinpovertyindices.byquartile.pdf"),height=7,width=8)

