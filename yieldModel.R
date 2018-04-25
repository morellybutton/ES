#Code for analyzing output of cocoa yield model

library(MuMIn)
library(arm)
library(car)
library(AICcmodavg)
library(gridExtra)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

dF<-read_csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.",year,".csv"))

qqp(dF$HeavyCrop,"norm")

options(na.action = "na.omit")

(fm10<-lm(HeavyCrop~Tmax+Chset+CN.ratio+K.meq+Tot.P+Shade.density+Age.of.cocoa+Canopy.gap.dry+Cocoa.density+pH+Mist+soil.moist+PropCPB+PropBP+No.applications.yr+Biomass+distance.cont,data=dF))
summary(fm10)
fm10s<-standardize(fm10)
summary(fm10s)

(fm11<-lm(HeavyCrop~Canopy.gap.dry+Cocoa.density+soil.moist+PropCPB+No.applications.yr+Biomass+distance.cont,data=dF))
summary(fm11)
fm11s<-standardize(fm11)
summary(fm11s)

#2014/15
(fm01<-lm(HeavyCrop~Age.of.cocoa+Cocoa.density+pH+Tot.P+soil.moist+PropCPB+No.applications.yr+Biomass+distance.cont,data=dF))
summary(fm01)
fm01s<-standardize(fm01)
summary(fm01s)

options(na.action = "na.fail")
fm01d<-dredge(fm11s)

#delta AIC = 6, 12 models
dredg.m01<-subset(fm01d,delta<6)
topmodels1.avg<-model.avg(dredg.m01)
sink(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,".median_delta6.txt"))
summary(topmodels1.avg)
sink()

#delta AIC = 2, 4 models
dredg.m02<-subset(fm01d,delta<2)
topmodels1.avg<-model.avg(dredg.m02) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,".median_delta2.txt"))
summary(topmodels1.avg)
sink()

#assign candidate set of models manually, removing redundant models using nesting rule
cand.set<-list()
#delta 2 only has two models
cand.set[[1]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))
cand.set[[2]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))

cand.set1<-list()

#orig. delta 6 has eight  models
cand.set1[[1]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))
cand.set1[[2]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))
cand.set1[[3]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+No.applications.yr+soil.moist,data=dF))
cand.set1[[4]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+No.applications.yr+soil.moist,data=dF))
cand.set1[[5]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+pH,data=dF))
cand.set1[[6]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+distance.cont+No.applications.yr+pH,data=dF))
cand.set1[[7]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+distance.cont+No.applications.yr,data=dF))
cand.set1[[8]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+No.applications.yr,data=dF))
cand.set1[[9]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+No.applications.yr,data=dF))

cand.set1<-list()
#redone. delta 6 has four  models
cand.set1[[1]]<-standardize(lm(HeavyCrop~Biomass+Canopy.gap.dry+Cocoa.density+distance.cont+No.applications.yr+PropCPB,data=dF))
cand.set1[[2]]<-standardize(lm(HeavyCrop~Biomass+Cocoa.density+distance.cont+No.applications.yr+PropCPB,data=dF))
cand.set1[[3]]<-standardize(lm(HeavyCrop~Biomass+Cocoa.density+distance.cont+No.applications.yr,data=dF))
cand.set1[[4]]<-standardize(lm(HeavyCrop~Biomass+Cocoa.density+No.applications.yr,data=dF))

cand.set<-cand.set1

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(cand.set), sep = " ")

##generate AICc table
res.table <-aictab(cand.set = cand.set, modnames = Modnames, sort = TRUE)
write.csv(res.table,paste0(getwd(),"/Analysis/ES/AICtab_HC",season,"_delta6.median.csv"))

topmodels2.avg<-model.avg(cand.set) 
sink(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta6.median.txt"))
summary(topmodels2.avg)
sink() 

x1<-as.data.frame(summary(topmodels2.avg)$importance)
x1$Comparison<-rownames(x1)
colnames(x1)<-c("Importance","Comparison")

#calculate model average and confidence intervals
vars<-list()
for(i in 1:nrow(x1)){
  vars[[i]]<-data.frame(cbind(x1$Comparison[i],modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,x1$Comparison[i],uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)
}
vars.1<-do.call(rbind.data.frame,vars)
colnames(vars.1)<-c("Parameter","Estimate","Uncond.SE","Lower.CL","Upper.CL")
vars.1[nrow(vars.1)+1,]<-data.frame(cbind("(Intercept)",modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Mod.avg.beta,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Uncond.SE,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Lower.CL,modavg(cand.set,"(Intercept)",uncond.se = "revised",modnames = Modnames)$Upper.CL),stringsAsFactors = F)

vars.1[,2:5]<-sapply(vars.1[,2:5],as.numeric)

#create figure of coefficients with confidence intervals
tmp<-as.data.frame(t(topmodels2.avg[[2]]))
tmp$Comparison <- rownames(tmp)
tmp[,4:7]<-vars.1[match(tmp$Comparison,vars.1$Parameter),2:5]

#add importance
tmp$Importance<-x1[match(tmp$Comparison,x1$Comparison),"Importance"]
#write confidence intervals and order of importance
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
#write.csv(tmp,paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta6.median.csv"))

#order by importance
#tmp<-tmp[!is.na(tmp$Importance),]
#tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta2.wooutlier.csv"))
tmp<-read.csv(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,"_delta6.median.csv"))
tmp<-tmp[!is.na(tmp$full),]

#for delta 6
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Distance\nFrom Forest","Capsid Incidence","Canopy Gap","(Intercept)"))

#without outlier
#tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Distance\nFrom Forest","Soil\nMoisture","Cocoa\nDensity","Mistletoe","Avg Fertiliser\nApplications [yr-1]","(Intercept)"))
vars<-gsub("z.","",tmp$X)
vars<-vars[vars!="(Intercept)"]
dF.1<-dF[,match(vars,colnames(dF))]

#for delta 6
dF.2<-data.frame(cbind(mean(dF.1$Biomass,na.rm=T),sd(dF.1$Biomass,na.rm=T),mean(dF.1$Canopy.gap.dry,na.rm=T),sd(dF.1$Canopy.gap.dry,na.rm=T),mean(dF.1$Cocoa.density,na.rm=T),sd(dF.1$Cocoa.density,na.rm=T),mean(dF.1$distance.cont,na.rm=T),sd(dF.1$distance.cont,na.rm=T),mean(dF.1$No.applications.yr,na.rm=T),sd(dF.1$No.applications.yr,na.rm=T),mean(dF.1$PropCPB,na.rm=T),sd(dF.1$PropCPB,na.rm=T)),stringsAsFactors = F)
colnames(dF.2)<-c(paste0(vars[1],".m"),paste0(vars[1],".sd"),paste0(vars[2],".m"),paste0(vars[2],".sd"),paste0(vars[3],".m"),paste0(vars[3],".sd"),paste0(vars[4],".m"),paste0(vars[4],".sd"),paste0(vars[5],".m"),paste0(vars[5],".sd"),paste0(vars[6],".m"),paste0(vars[6],".sd"))

#standardize variables
dF.1$plot<-as.character(dF$plot)
dF.1$z.Biomass<-(dF.1$Biomass-dF.2$Biomass.m)/dF.2$Biomass.sd/2
dF.1$z.Cocoa.density<-(dF.1$Cocoa.density-dF.2$Cocoa.density.m)/dF.2$Cocoa.density.sd/2
dF.1$z.distance.cont<-(dF.1$distance.cont-dF.2$distance.cont.m)/dF.2$distance.cont.sd/2
dF.1$z.No.applications.yr<-(dF.1$No.applications.yr-dF.2$No.applications.yr.m)/dF.2$No.applications.yr.sd/2
dF.1$z.Canopy.gap.dry<-(dF.1$Canopy.gap.dry-dF.2$Canopy.gap.dry.m)/dF.2$Canopy.gap.dry.sd/2
dF.1$z.PropCPB<-(dF.1$PropCPB-dF.2$PropCPB.m)/dF.2$PropCPB.sd/2

dF.1$yield.tree<-dF$HeavyCrop
dF.1$yield.ha<-dF$HeavyCrop*dF$Cocoa.density

#calculate max per tree yield
max.yield<-max(dF.1$yield.tree)
#calculate yield gap per plot
dF.1$yield.gap<-max.yield-dF.1$yield.tree
#calculate yield gap per ha (from cocoa density)
dF.1$yield.gap.ha<-dF.1$yield.gap*dF.1$Cocoa.density

#calculate yield potential per factor based on farm's 2014/15 measures, using AICcmodavg
#for delta 6
dF.1$yield.mod<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry

r.adj<-summary(lm(yield.mod~yield.tree,data=dF.1))$adj.r.squared
ggplot(dF.1,aes(yield.tree,yield.mod)) + geom_point() + xlim(0,1.6) + ylim(0,1.6) +theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + geom_text(aes(1.0,0.25,label=paste0("R2 = ",signif(r.adj,2))))

#calculate yield potential per factor based on farm's 2014/15 measures, without Intercept?
#calculate biomass yield contribution
dF.1$y.biomass<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.biomass.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Lower.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Lower.CL"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.biomass.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Upper.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Upper.CL"]*mean(dF.1$z.Canopy.gap.dry)

g1<-ggplot(dF.1,aes(Biomass,y.biomass))+geom_point()+geom_errorbar(aes(x=Biomass,ymin=y.biomass.lwr,ymax=y.biomass.upr),width=0.1)+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Distance to Biomass [m]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()

#calculate cocoa density yield contribution
dF.1$y.cdensity<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.cdensity.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Lower.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Lower.CL"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.cdensity.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Upper.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Upper.CL"]*mean(dF.1$z.Canopy.gap.dry)

g2<-ggplot(dF.1,aes(Cocoa.density,y.cdensity))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Cocoa Density [ha-1]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=Cocoa.density,ymin=y.cdensity.lwr,ymax=y.cdensity.upr),width=0.1)

#calculate distance from forest yield contribution
dF.1$y.fdist<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.fdist.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Lower.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Lower.CL"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.fdist.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Upper.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Upper.CL"]*mean(dF.1$z.Canopy.gap.dry)

g3<-ggplot(dF.1,aes(distance.cont,y.fdist))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Distance from Forest [m]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=distance.cont,ymin=y.fdist.lwr,ymax=y.fdist.upr),width=0.1)

#calculate fertilizer yield contribution
dF.1$y.fert<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.fert.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Lower.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Lower.CL"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.fert.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Upper.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Upper.CL"]*mean(dF.1$z.Canopy.gap.dry)

g4<-ggplot(dF.1,aes(No.applications.yr,y.fert))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Fertiliser Application [No/yr]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=No.applications.yr,ymin=y.fert.lwr,ymax=y.fert.upr),width=0.1)

#calculate canopy gap yield contribution
dF.1$y.cgap<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry
dF.1$y.cgap.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Lower.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Lower.CL"]*dF.1$z.Canopy.gap.dry
dF.1$y.cgap.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Upper.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Upper.CL"]*dF.1$z.Canopy.gap.dry

g5<-ggplot(dF.1,aes(Canopy.gap.dry,y.cgap))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Canopy Gap [%]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=Canopy.gap.dry,ymin=y.cgap.lwr,ymax=y.cgap.upr),width=0.1)

#calculate capsid yield contribution
dF.1$y.cpb<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.cpb.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Lower.CL"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Lower.CL"]*mean(dF.1$z.Canopy.gap.dry)
dF.1$y.cpb.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Upper.CL"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Upper.CL"]*mean(dF.1$z.Canopy.gap.dry)

g6<-ggplot(dF.1,aes(PropCPB,y.cpb))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Capsid Incidence [prop pods]")+ylab("Contribution to Yield [kg/tree]")+theme_classic()+geom_errorbar(aes(x=PropCPB,ymin=y.cpb.lwr,ymax=y.cpb.upr))

g7<-grid.arrange(g1,g2,g3,g4,g5,g6,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig_modelparameters.pdf",g7,height=9,width=7)

write.csv(dF.1,paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

#check components to modeled yield
dF.1$check<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+dF.1$y.biomass+dF.1$y.cdensity+dF.1$y.fdist+dF.1$y.fert+dF.1$y.cgap+dF.1$y.cpb

#calculate potential yield increase by factor, create new dataframe
dF.3<-data.frame(cbind(as.character(dF.1$plot),dF.1$yield.tree,dF.1$yield.ha),stringsAsFactors = F)
colnames(dF.3)<-c("plot","o.yield.tree","o.yield.ha")

dF.3$y.biomass.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*max(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry-dF.1$yield.mod 

dF.3$y.biomass.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*min(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry-dF.1$yield.mod 

dF.3$y.cdensity.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*min(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry-dF.1$yield.mod 

dF.3$y.cdensity.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*max(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry-dF.1$yield.mod 

dF.3$y.fert.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*max(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry-dF.1$yield.mod 

dF.3$y.fert.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*min(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry-dF.1$yield.mod 

dF.3$y.cgap.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*max(dF.1$z.Canopy.gap.dry)-dF.1$yield.mod 

dF.3$y.cgap.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*dF.1$z.PropCPB+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*min(dF.1$z.Canopy.gap.dry)-dF.1$yield.mod 

#calculate yield increasing potential by summing
#minimizing capsid
dF.3$yield.pot.cpb<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Capsid Incidence","Estimate"]*min(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Estimate"]*dF.1$z.Canopy.gap.dry-dF.1$yield.mod 

#calculate percent increase in per tree yield
#biomass
dF.3$y.pot.pct.bmass<-dF.3$y.biomass.2/as.numeric(dF.3$o.yield.tree)*100
#fertiliser
dF.3$y.pot.pct.fert<-dF.3$y.fert.1/as.numeric(dF.3$o.yield.tree)*100
#canopy gap
dF.3$y.pot.pct.cgap<-dF.3$y.cgap.1/as.numeric(dF.3$o.yield.tree)*100
#capsid
dF.3$y.pot.pct.cpb<-dF.3$yield.pot.cpb/as.numeric(dF.3$o.yield.tree)*100

dF.3 <- dF.3 %>% group_by(plot) %>% mutate(yield.pot=sum(y.biomass.2,y.fert.1,y.cgap.1,yield.pot.cpb))

#calculate absolute yield increase per factor
dF.4 <- dF.3 %>% select(plot,y.biomass.2,y.fert.1,y.cgap.1,yield.pot.cpb)
colnames(dF.4)<-c("plot","Biomass","Fert","Canopy Gap","Capsid")

dF.4<- dF.4 %>% gather(key="variable",value="value",-plot)

dF.4$variable<-factor(dF.4$variable,levels=c("Biomass","Fert","Canopy Gap","Capsid"),labels=c("Distance from Biomass","Fertiliser","Canopy Gap","Capsid Incidence"))
#dF.4$plot<-ordered(dF.4$plot,levels=dF.3[order(dF.3$yield.pot,decreasing=T),"plot"])

write.csv(dF.4,paste0(getwd(),"/Analysis/ES/Modelled.yield.per.tree.per.farm.contribution",season,".med.csv"))

#calculate the absolute change in factor for each farm
dF.3$d.biomass<-min(dF.1$Biomass)-dF.1$Biomass
dF.3$d.fert<-max(dF.1$No.applications.yr)-dF.1$No.applications.yr
dF.3$d.cgap<-max(dF.1$Canopy.gap.dry)-dF.1$Canopy.gap.dry
dF.3$d.cpb<-min(dF.1$PropCPB)-dF.1$PropCPB

#calculate increase of yield per ha, per paramater
dF.3<-left_join(dF.3,dF.1 %>% select(plot,Cocoa.density),by="plot")
dF.3 <- dF.3 %>% group_by(plot) %>% mutate(y.pot.bmass.ha=y.biomass.2*Cocoa.density,y.pot.fert.ha=y.fert.1*Cocoa.density,
                                           y.pot.cgap.ha=y.cgap.1*Cocoa.density,y.pot.cpb.ha=yield.pot.cpb*Cocoa.density)
dF.3$y.pot.ha<-dF.3$yield.pot*dF.1$Cocoa.density

#load key household variables
hhold.vars<-read.csv(paste0(getwd(),"/HouseholdData/Management/ES.Yield.vars.csv"))

dF.3$land.area<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"Land.area"]
dF.3$LBCs.loss<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"LBCs.loss"]
dF.3$shold<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"Sharehold"]
#calculate rent per ha
dF.3$rent.ha<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"Rent.cedis"]/dF.3$land.area

#load input and labour costs
hhold.costs<-read.csv(paste0(getwd(),"/HouseholdData/Labour.Input.Costs.csv"))
hhold.costs <- hhold.costs %>% rename(plot=PLOTCODE)
dF.3 <- left_join(dF.3,hhold.costs,by="plot")

write.csv(dF.3,paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.",season,".csv"))
