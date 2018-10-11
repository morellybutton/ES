#Code producing final figures for ES Cocoa paper

library(tidyverse)
library(gridExtra)
library(ggpubr)
#library(arm)
#library(car)
library(lattice)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

#cocoa yield model
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta6.median.csv"))
tmp<-tmp[!is.na(tmp$full),]

tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Distance\nFrom Forest","Capsid Incidence","Canopy Gap","(Intercept)"))
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=F),"Comparison"])

g1<-ggplot(tmp[!is.na(tmp$Importance),], aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width = 0.1,size=1.0) + geom_point(size=2.0)+ 
  theme_classic() + ggtitle(paste0("Influence of ES factors on\nPer Tree Yield"))+geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") +theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5))
g1 + coord_flip() + geom_text(aes(label="n = 36"),x=0.75,y=-0.55) 
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure1_YieldModel.pdf")

#flip some of the factors for "presentations"
tmp2<- tmp %>% filter(Comparison=="Distance to\nBiomass"|Comparison=="Distance\nFrom Forest") %>%
  mutate(Estimate=-Estimate,Lower.CL=-Lower.CL,Upper.CL=-Upper.CL)
tmp2$Comparison<-c("Proximity to\nPollinator Habitat","Proximity to\nIntact Forest")
tmp2<-bind_rows(tmp2,tmp %>% filter(Comparison!="Distance to\nBiomass"&Comparison!="Distance\nFrom Forest"))
tmp2$Comparison<-factor(tmp2$Comparison,levels=tmp2[order(tmp2$Importance,decreasing=F),"Comparison"])

g1<-ggplot(tmp2[!is.na(tmp2$Importance),], aes(x = Comparison, y = Estimate, ymin = Lower.CL, ymax = Upper.CL)) + geom_errorbar(width = 0.1,size=1.0) + geom_point(size=2.0)+ 
  theme_classic() + ggtitle(paste0("Influence of ES factors on\nPer Tree Yield"))+geom_hline(yintercept = 0,linetype="dashed")+
  xlab("Variable [ranked by importance]")+ylab("Effect Size") +theme(text = element_text(size=24),plot.title = element_text(hjust = 0.5))
g1 + coord_flip() + geom_text(aes(label="n = 36"),x=0.75,y=-0.55) 
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Impact/ResilienceEvent/Figure1_YieldModel.pdf")

#diagnostic plots for cocoa yield model
year="2014"
season="1415"

dF<-read_csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.",year,".csv"))

(fm01<-lm(HeavyCrop~Canopy.gap.dry+Cocoa.density+soil.moist+PropCPB+No.applications.yr+Biomass+distance.cont,data=dF))
summary(fm01)
fm01s<-standardize(fm01)
summary(fm01s)

#compare observed responses vs within-group fitted values
## create data frame of residuals, fitted values, and variable
diagnos <- data.frame(Resid = resid(fm01s, type = "pearson"), Fitted = fitted(fm01s))

g1<-xyplot(Resid ~ Fitted, data = diagnos,xlab="Fitted Values",ylab="Residuals",main="Yield")

#Assumption 2: Random effects are normally distributed with mean zero and covariance matrix (not depending on the group) and are independent for different groups
#test for normality of residuals
## overal QQ normal plot
g2<-qqmath(~Resid, data = diagnos, distribution = qnorm,xlab="Normal Quantiles",ylab="Residuals",main="Yield", panel = function(x, ...) {
  panel.qqmathline(x, ...)
  panel.qqmath(x, ...)
})

g3<-grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig1_DiagnosticPlots.pdf",g3,height=4,width=8)

#open calculated yield contribution per factor
dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

#plot to compare measured vs modelled yield
r.adj<-summary(lm(yield.mod~yield.tree,data=dF.1))$adj.r.squared
ggplot(dF.1,aes(yield.tree,yield.mod))+geom_point()+geom_abline(slope=1,intercept=0,linetype="dashed")+xlab("Measured Yield [kg tree-1]")+ylab("Modelled Yield [kg tree-1]")+
  ylim(0,2.2)+xlim(0,2.2)+theme_classic()+geom_text(aes(label = paste("R^2: ",signif(r.adj,2),sep="")),parse=T,x=0.25,y=1.5)+geom_errorbar(aes(ymin=yield.mod.lwr,ymax=yield.mod.upr))+
  geom_text(aes(label="n = 36"),x=0.25,y=1.35) 
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig2_Comparisonofmodelledyield.pdf",height=6,width=6)

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
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig3_modelparameters.pdf",g7,width=8,height=5)

#load calculated contributions by factor
dF.4<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.per.tree.per.farm.contribution",season,".med.csv"))
dF.2 <- read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))

dF.4 <- left_join(dF.4 %>% filter(variable!="Canopy Gap"),dF.2 %>% select(plot,yield.pot),by="plot")

ggplot(dF.4,aes(fct_reorder(plot,yield.pot,.desc=T),value,fill=variable))+geom_bar(stat="identity")+ylab("Yield Increase Potential [kg tree-1]")+
  xlab("Farm")+theme_classic()+theme(axis.text.x=element_blank(),legend.title=element_blank(),legend.position="top",text = element_text(size=16))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/SuppFig4_YieldIncreasePotential.pdf")

#calculate changes in yield under different parameter options
dF <- read_csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))

dF.yield <- dF %>% select(plot,o.yield.ha,land.area,y.pot.ha,y.pot.fert.ha,y.pot.fert.ha.lwr,y.pot.fert.ha.upr,y.pot.bmass.ha,y.pot.bmass.ha.lwr,y.pot.bmass.ha.upr,
                          y.pot.cpb.ha,y.pot.cpb.ha.lwr,y.pot.cpb.ha.upr) %>%
  mutate(orig.yield=o.yield.ha*land.area,pot.yield.total=y.pot.ha*land.area,pot.yield.bmass=y.pot.bmass.ha*land.area,pot.yield.bmass.max=y.pot.bmass.ha.lwr*land.area,
         pot.yield.bmass.min=y.pot.bmass.ha.upr*land.area, pot.yield.fert=y.pot.fert.ha*land.area,pot.yield.fert.min=y.pot.fert.ha.lwr*land.area,pot.yield.fert.max=y.pot.fert.ha.upr*land.area,
         pot.yield.cpb=y.pot.cpb.ha*land.area,pot.yield.cpb.min=y.pot.cpb.ha.upr*land.area,pot.yield.cpb.max=y.pot.cpb.ha.lwr*land.area) %>% group_by(plot) %>%
  mutate(potential.yield.max=sum(pot.yield.fert.max,pot.yield.bmass.max,pot.yield.cpb.min),potential.yield.min=sum(pot.yield.fert.min,pot.yield.bmass.min,pot.yield.cpb.max)) %>%
  ungroup() %>%
  summarise(orig.total=sum(orig.yield)/1000,potential.yield=sum(orig.yield,pot.yield.total)/1000,potential.yield.max=sum(orig.yield,potential.yield.max)/1000,potential.yield.min=sum(orig.yield,potential.yield.min)/1000,
            potential.yield.biomass=sum(orig.yield,pot.yield.bmass)/1000,potential.yield.biomass.max=sum(orig.yield,pot.yield.bmass.max)/1000,potential.yield.biomass.min=sum(orig.yield,pot.yield.bmass.min)/1000,
            potential.yield.fert=sum(orig.yield,pot.yield.fert)/1000,potential.yield.fert.max=sum(orig.yield,pot.yield.fert.max)/1000,potential.yield.fert.min=sum(orig.yield,pot.yield.fert.min)/1000,
            potential.yield.cpb=sum(orig.yield,pot.yield.cpb)/1000,potential.yield.cpb.max=sum(orig.yield,pot.yield.cpb.max)/1000,potential.yield.cpb.min=sum(orig.yield,pot.yield.cpb.min)/1000) 
upr.yield<-dF.yield[,grep("max",colnames(dF.yield))]
lwr.yield<-dF.yield[,grep("min",colnames(dF.yield))]
upr.yield<-upr.yield %>% gather(key="parameter",value="upr") %>% mutate(parameter=gsub(".max","",parameter))
lwr.yield<-lwr.yield %>% gather(key="parameter",value="lwr") %>% mutate(parameter=gsub(".min","",parameter))
dF.yield<-dF.yield %>% gather(key="parameter",value="yield")

dF.yield <- left_join(dF.yield,upr.yield,by="parameter")
dF.yield <- left_join(dF.yield,lwr.yield,by="parameter")

dF.yield <- dF.yield %>% filter(parameter=="orig.total"|!is.na(upr))

dF.yield <- dF.yield %>% group_by(parameter) %>% mutate(yield.prop = yield/dF.yield$yield[dF.yield$parameter=="orig.total"],prop.upr=upr/dF.yield$yield[dF.yield$parameter=="orig.total"],prop.lwr=lwr/dF.yield$yield[dF.yield$parameter=="orig.total"])
  
dF.yield$parameter<-factor(dF.yield$parameter,levels=c("orig.total","potential.yield","potential.yield.biomass","potential.yield.fert",
                                                       "potential.yield.cgap","potential.yield.cpb"), labels=c("Original","Total Potential","Distance from Biomass\nPotential",
                                                                                                               "Fertiliser Application\nPotential", "Canopy Gap\nPotential", "Capsid Management\nPotential"))
ggplot(dF.yield %>% filter(parameter!="Original"), aes(fct_reorder(parameter,yield),yield.prop)) + geom_point(size=2.0) + theme_classic() + theme(axis.text.x=element_text(angle = 45,hjust=1),text = element_text(size=16))+
  xlab("Model Parameter") + ylab("Relative Increase in Yield") + ylim(0,4) + geom_errorbar(aes(ymin=prop.lwr,ymax=prop.upr),width=0.03,size=1.0)+ coord_fixed(ratio = 1)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure2_relativeincreaseinyields.pdf"))

#load inputs for income calculations
dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))
#convert to usd
usd<-0.23

g1<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.bmass*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=i.pot.net.margin.bmass.upr*usd,ymin=i.pot.net.margin.bmass.lwr*usd)) +
  geom_abline(intercept = 0,slope=1,linetype="dotted") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("Distance From Biomass") + xlim(-350,1100) + ylim(-350,1100) + geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g2<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.fert*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=i.pot.net.margin.fert.upr*usd,ymin=i.pot.net.margin.fert.lwr*usd))+
  geom_abline(intercept = 0,slope=1,linetype="dotted") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("Fertiliser Application") + xlim(-350,1100) + ylim(-350,1100) + geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g3<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.cpb*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=i.pot.net.margin.cpb.upr*usd,ymin=i.pot.net.margin.cpb.lwr*usd))+
  geom_abline(intercept = 0,slope=1,linetype="dotted") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("Capsid Management")+ xlim(-350,1100) + ylim(-350,1100) + geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g4<-ggplot(dF.3,aes(o.net.margin*usd,i.pot.net.margin.all*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=i.pot.net.margin.all.upr*usd,ymin=i.pot.net.margin.all.lwr*usd))+
  geom_abline(intercept = 0,slope=1,linetype="dotted") + xlab("Original Net Margin [US$/ha]") + ylab("Potential Net Margin [US$/ha]")+
  ggtitle("All Management Options") + xlim(-350,1600) + ylim(-350,1600)+ geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

g5<-grid.arrange(g1,g3,g2,g4,ncol=2)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/FigureS4_CocoaIncome_potential_increase.byparameter.pdf"),g5,height=6,width=7)

#Relationship between inputs and yields from survey
i.mod<-lm(Input.ha~o.yield.ha,data=dF.3)
input<-coefficients(i.mod)
r.adj <- summary(i.mod)$adj.r.squared

g1<-ggplot(dF.3,aes(o.yield.ha,Input.ha)) + geom_point() +stat_smooth(method="lm") + theme_classic() + theme(text = element_text(size=14)) +
  xlab("Yield [kg/ha]") + ylab("Input costs [cedis/ha]") + geom_text(aes(100,2000,label=paste("R^2==",signif(r.adj,2),sep="")),parse=T)
g2<-ggplot(dF.3,aes(o.yield.ha,Labour.ha)) + geom_point() +stat_smooth(method="lm") + theme_classic() + theme(text = element_text(size=14)) +
  xlab("Yield [kg/ha]") + ylab("Labour costs [cedis/ha]")
g3<-grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig5_labour.input.costs.pdf",g3,height=4,width=9)

#Final Changes to Poverty Indices
dF.2<-read.csv(paste0(getwd(),"/Analysis/ES/PovertyMeasureChanges.NewMeans.csv"))
dF.3 <- dF.2 %>% select(-X) %>% gather(key="variable",value="value",c(-Measure,-parameter,-quartile,-Lower,-Upper)) %>%
  mutate(parameter=replace(parameter,parameter=="NoLoss","No LBC Loss"),parameter=replace(parameter,parameter=="All","All Options"),parameter=replace(parameter,parameter=="Capsids","Capsid")) %>%
  mutate(Lower=replace(Lower,variable=="Original",NA),Upper=replace(Upper,variable=="Original",NA))


g1<-ggplot() + geom_point(data=dF.3 %>% filter(Measure=="Education"&parameter!="No LBC Loss"&variable=="Original"), aes(variable,value))+ geom_point(data=dF.3 %>% filter(Measure=="Education"&parameter!="No LBC Loss"&variable!="Original"),aes(variable,value,color=parameter)) + 
  theme_classic() +
  geom_errorbar(data=dF.3 %>% filter(Measure=="Education"&parameter!="No LBC Loss"), aes(x=variable,ymax=Upper,ymin=Lower,color=parameter),width=0.1) +
  ylab("Quartile Mean Probability") + xlab("") + ggtitle("Missing School") + ylim(0,1.0)+
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size=1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                       x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                      x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Capsid"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="Capsid"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                   x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="All Options"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="All Options"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                   x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  #geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="No LBC Loss"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
  #                                                                                      x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  #geom_segment(data=dF.3 %>% filter(Measure=="Education"&parameter=="No LBC Loss"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
  #                                                                                     x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="solid",size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14),legend.title = element_blank()) + labs(color="Model Parameter",alpha="Cocoa Income Quartile")
  
  
g2<-ggplot() + geom_point(data=dF.3 %>% filter(Measure=="Assets"&parameter!="No LBC Loss"&variable=="Original"),aes(variable,value)) + geom_point(data=dF.3 %>% filter(Measure=="Assets"&parameter!="No LBC Loss"&variable!="Original"),aes(variable,value,color=parameter)) +
  theme_classic() +
  geom_errorbar(data=dF.3 %>% filter(Measure=="Assets"&parameter!="No LBC Loss"), aes(x=variable,ymax=Upper,ymin=Lower,color=parameter),width=0.1) +
  ylab("Quartile Mean Probability") + xlab("") + ggtitle("Owning a TV") + ylim(0,1.0)+
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                       x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                      x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Capsid"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="Capsid"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                   x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="All Options"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                        x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="All Options"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                       x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  #geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="No LBC Loss"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
  #                                                                                     x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  #geom_segment(data=dF.3 %>% filter(Measure=="Assets"&parameter=="No LBC Loss"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
  #                                                                                    x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="solid",size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14),legend.title = element_blank()) + labs(color="Model Parameter",alpha="Cocoa Income Quartile")


g3<-ggplot() + geom_point(data=dF.3 %>% filter(Measure=="Food Security"&parameter!="No LBC Loss"&variable=="Original"),aes(variable,value)) + geom_point(data=dF.3 %>% filter(Measure=="Food Security"&parameter!="No LBC Loss"&variable!="Original"),aes(variable,value,color=parameter)) + 
  theme_classic() +
  geom_errorbar(data=dF.3 %>% filter(Measure=="Food Security"&parameter!="No LBC Loss"), aes(x=variable,ymax=Upper,ymin=Lower,color=parameter),width=0.1) +
  ylab("Quartile Mean Probability") + xlab("") + ggtitle("Adequate Amount of Food") + ylim(0,1.0)+
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                 x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                 x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                   x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Capsid"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="Capsid"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                               x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="All Options"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                     x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="All Options"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                    x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  #geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="No LBC Loss"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
  #                                                                                   x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  #geom_segment(data=dF.3 %>% filter(Measure=="Food Security"&parameter=="No LBC Loss"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
  #                                                                                  x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="solid",size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14),legend.title = element_blank()) + labs(color="Model Parameter",alpha="Cocoa Income Quartile")

g4<-ggplot() + geom_point(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter!="No LBC Loss"&variable=="Original"),aes(variable,value)) + geom_point(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter!="No LBC Loss"&variable!="Original"),aes(variable,value,color=parameter)) + 
  theme_classic() +
  geom_errorbar(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter!="No LBC Loss"), aes(x=variable,ymax=Upper,ymin=Lower,color=parameter),width=0.1) +
  ylab("Quartile Mean Likert Value") + xlab("") + ggtitle("Satisfaction with Life") + ylim(0,4)+
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                        x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Biomass"), aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                        x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Fertiliser"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                           x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Fertiliser"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                          x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Capsid"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                       x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="Capsid"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                      x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="All Options"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
                                                                                            x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1/8,size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="All Options"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
                                                                                           x="Original",xend="Potential",color=parameter,alpha=quartile),alpha=1,size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  #geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="No LBC Loss"), aes(y=value[variable=="Original"&quartile=="bottom"],yend=value[variable=="Potential"&quartile=="bottom"],
  #                                                                                          x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="dotdash",size = 1,arrow = arrow(length = unit(0.3, "cm"))) +
  #geom_segment(data=dF.3 %>% filter(Measure=="Satisfaction"&parameter=="No LBC Loss"),aes(y=value[variable=="Original"&quartile=="top"],yend=value[variable=="Potential"&quartile=="top"],
  #                                                                                         x="Original",xend="Potential",color=parameter,linetype=quartile),linetype="solid",size = 1,arrow = arrow(length = unit(0.3, "cm")))+
  theme(text = element_text(size=14),legend.title = element_blank())

ggarrange(g1,g3,g2,g4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Figure3_changeinpovertyindices.byquartile.pdf"),height=7,width=8)

#compare modelled yield increases vs just doubling existing yields, recalculate input costs using equation
dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))
#convert to usd
usd<-0.23
kg<-65
bag_cedi<-350

i.mod<-lm(Input.ha~o.yield.ha,data=dF.3)
input<-coefficients(i.mod)
r.adj <- summary(i.mod)$adj.r.squared

dF.3 <- dF.3 %>% group_by(plot) %>% mutate(y.pot.ha.lwr=sum(y.pot.bmass.ha.upr,y.pot.fert.ha.lwr,y.pot.cpb.ha.upr),
                        y.pot.ha.upr=sum(y.pot.bmass.ha.lwr,y.pot.fert.ha.upr,y.pot.cpb.ha.lwr),
                        y.pot.int.ha=sum(y.pot.fert.ha,y.pot.cpb.ha), y.pot.int.ha.lwr=sum(y.pot.fert.ha.lwr,y.pot.cpb.ha.upr),y.pot.int.ha.upr=sum(y.pot.fert.ha.upr,y.pot.cpb.ha.lwr))
dF.comp <- dF.3 %>% select(plot,o.yield.tree,o.yield.ha,yield.pot,y.pot.ha,y.pot.ha.lwr,y.pot.ha.upr,o.final.income,land.area,shold,LBCs.loss,rent.ha,i.pot.net.margin.all,i.pot.net.margin.all.lwr,i.pot.net.margin.all.upr,Input.ha,y.pot.int.ha,y.pot.int.ha.lwr,y.pot.int.ha.upr) %>%
  group_by(plot) %>% mutate(i.yield.2x.ha=2*o.yield.ha, y.tot.ha=sum(o.yield.ha,y.pot.ha),y.tot.ha.lwr=sum(o.yield.ha,y.pot.ha.lwr),y.tot.ha.upr=sum(o.yield.ha,y.pot.ha.upr)) %>%
  mutate(i.2x.input=i.yield.2x.ha*input[2]+input[1],i.tot.input=y.pot.int.ha*input[2]+input[1],i.tot.input.lwr=y.pot.int.ha.lwr*input[2]+input[1], i.tot.input.upr=y.pot.int.ha.upr*input[2]+input[1]) %>%
  mutate(i.2x.income=i.yield.2x.ha/kg*bag_cedi*land.area,i.tot.income=y.tot.ha/kg*bag_cedi*land.area,i.tot.income.lwr=y.tot.ha.lwr/kg*bag_cedi*land.area,i.tot.income.upr=y.tot.ha.upr/kg*bag_cedi*land.area) %>%
  mutate(i.2x.lbcloss=i.2x.income*LBCs.loss/100,i.tot.lbcloss=i.tot.income*LBCs.loss/100,i.tot.lbcloss.lwr=i.tot.income.lwr*LBCs.loss/100,i.tot.lbcloss.upr=i.tot.income.upr*LBCs.loss/100) %>%
  mutate(i.2x.income.final=(i.2x.income-i.2x.lbcloss-(i.2x.income-i.2x.lbcloss)*shold-i.2x.input*land.area)*usd,
         i.tot.income.final=(i.tot.income-i.tot.lbcloss-(i.tot.income-i.tot.lbcloss)*shold-i.tot.input*land.area)*usd,
         i.tot.income.final.lwr=(i.tot.income.lwr-i.tot.lbcloss.lwr-(i.tot.income.lwr-i.tot.lbcloss.lwr)*shold-i.tot.input.lwr*land.area)*usd,
         i.tot.income.final.upr=(i.tot.income.upr-i.tot.lbcloss.upr-(i.tot.income.upr-i.tot.lbcloss.upr)*shold-i.tot.input.upr*land.area)*usd)

ggplot(dF.comp, aes(i.yield.2x.ha,y.tot.ha)) + geom_point() + geom_abline(intercept=0,slope=1,linetype="dashed") +
  theme_classic() + geom_errorbar(aes(ymax=y.tot.ha.upr,ymin=y.tot.ha.lwr)) + xlab("Yields If Doubled [kg/ha]") +
  ylab("Modelled Yields with Ecological Limits [kg/ha]") + xlim(0,2000) + ylim(0,2000)

ggplot(dF.comp, aes(i.2x.income.final,i.tot.income.final)) + geom_point() + geom_abline(intercept=0,slope=1,linetype="dashed") +
  theme_classic() + geom_errorbar(aes(ymax=i.tot.income.final.upr,ymin=i.tot.income.final.lwr)) + xlab("Final Income If Yields Doubled [US$]") +
  ylab("Final Income with Ecological Limits [US$]")

