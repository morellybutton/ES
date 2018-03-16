#Code for analyzing output of cocoa yield model

library(MuMIn)
library(arm)
library(AICcmodavg)

library(plyr)
library(ggplot2)
library(reshape)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_dataset.",year,".csv"))

options(na.action = "na.omit")

#2014/15
(fm01<-lm(HeavyCrop.med~Age.of.cocoa+Cocoa.density+pH+soil.moist+PropCPB+No.applications.yr+Biomass+distance.cont,data=dF))
summary(fm01)
fm01s<-standardize(fm01)
summary(fm01s)

options(na.action = "na.fail")
fm01d<-dredge(fm01s)

#delta AIC = 6, 30 models
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

#check for non-linearities
options(na.action = "na.omit")
#2014/15
(fm02<-lm(HeavyCrop.med~Age.of.cocoa+Cocoa.density+pH+I(pH^2)+soil.moist+PropCPB+No.applications.yr+Biomass+distance.cont,data=dF))
summary(fm02)
fm02s<-standardize(fm02)
summary(fm02s)

options(na.action = "na.fail")
fm02d<-dredge(fm02s)

#delta AIC = 6, 30 models
dredg.m02<-subset(fm02d,delta<6)
topmodels2.avg<-model.avg(dredg.m02)
#sink(paste0(getwd(),"/Analysis/ES/Model.Average_HC",season,".median_delta6.txt"))
summary(topmodels2.avg)
#sink()

#assign candidate set of models manually, removing redundant models using nesting rule
cand.set<-list()
#delta 2 only has two models
cand.set[[1]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))
cand.set[[2]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))

cand.set1<-list()

#delta 6 has eight  models
cand.set1[[1]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))
cand.set1[[2]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+distance.cont+No.applications.yr+soil.moist,data=dF))
cand.set1[[3]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+No.applications.yr+soil.moist,data=dF))
cand.set1[[4]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+No.applications.yr+soil.moist,data=dF))
cand.set1[[5]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+pH,data=dF))
cand.set1[[6]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+distance.cont+No.applications.yr+pH,data=dF))
cand.set1[[7]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+distance.cont+No.applications.yr,data=dF))
cand.set1[[8]]<-standardize(lm(HeavyCrop.med~Age.of.cocoa+Biomass+Cocoa.density+No.applications.yr,data=dF))
cand.set1[[9]]<-standardize(lm(HeavyCrop.med~Biomass+Cocoa.density+No.applications.yr,data=dF))

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
tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Cocoa\nDensity","Avg Fertiliser\nApplications [yr-1]","Soil\nMoisture","Distance\nFrom Forest","Age of Cocoa","Soil pH","(Intercept)"))

#without outlier
#tmp$Comparison<-factor(tmp$Comparison,levels=tmp[order(tmp$Importance,decreasing=T),"Comparison"],labels=c("Distance to\nBiomass","Distance\nFrom Forest","Soil\nMoisture","Cocoa\nDensity","Mistletoe","Avg Fertiliser\nApplications [yr-1]","(Intercept)"))

#add ES Component
tmp[tmp$Comparison=="Phosphorous"|tmp$Comparison=="Soil\nMoisture"|tmp$Comparison=="Soil pH"|tmp$Comparison=="Nitrogen"|tmp$Comparison=="Soil pH:\nSoil Moisture","Component"]<-"Soil"
tmp[tmp$Comparison=="Distance to\nBiomass"|tmp$Comparison=="Shade Density"|tmp$Comparison=="Harvesting\nLabour"|tmp$Comparison=="Cocoa\nDensity"|tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]"|tmp$Comparison=="Canopy Gap"|tmp$Comparison=="Age of Cocoa"|tmp$Comparison=="Yield\nVariation","Component"]<-"Management"
tmp[tmp$Comparison=="Black Pod\nIncidence"|tmp$Comparison=="Capsid Incidence"|tmp$Comparison=="Mistletoe","Component"]<-"Disease"
tmp[tmp$Comparison=="Cherelle Set"|tmp$Comparison=="Pollination\nTreatment"|tmp$Comparison=="Flower\nBuds"|tmp$Comparison=="Biomass:Cherelle Set","Component"]<-"Pollination"
tmp[tmp$Comparison=="Mean\nTemperature"|tmp$Comparison=="Flower Buds:\nMean Temperature"|tmp$Comparison=="Water\nStress","Component"]<-"Micro-Climate"
tmp[tmp$Comparison=="Distance\nFrom Forest"|tmp$Comparison=="Forest Distance:Mistletoe","Component"]<-"Landscape"

tmp$Component<-factor(tmp$Component)

vars<-gsub("z.","",tmp$X)
vars<-vars[vars!="(Intercept)"]
dF.1<-dF[,match(vars,colnames(dF))]

#for delta 6
dF.2<-data.frame(cbind(mean(dF.1$Biomass,na.rm=T),sd(dF.1$Biomass,na.rm=T),mean(dF.1$Cocoa.density,na.rm=T),sd(dF.1$Cocoa.density,na.rm=T),mean(dF.1$No.applications.yr,na.rm=T),sd(dF.1$No.applications.yr,na.rm=T),mean(dF.1$soil.moist,na.rm=T),sd(dF.1$soil.moist,na.rm=T),mean(dF.1$distance.cont,na.rm=T),sd(dF.1$distance.cont,na.rm=T),mean(dF.1$Age.of.cocoa,na.rm=T),sd(dF.1$Age.of.cocoa,na.rm=T),mean(dF.1$pH,na.rm=T),sd(dF.1$pH,na.rm=T)),stringsAsFactors = F)
colnames(dF.2)<-c(paste0(vars[1],".m"),paste0(vars[1],".sd"),paste0(vars[2],".m"),paste0(vars[2],".sd"),paste0(vars[3],".m"),paste0(vars[3],".sd"),paste0(vars[4],".m"),paste0(vars[4],".sd"),paste0(vars[5],".m"),paste0(vars[5],".sd"),paste0(vars[6],".m"),paste0(vars[6],".sd"),paste0(vars[7],".m"),paste0(vars[7],".sd"))

#standardize variables
dF.1$plot<-as.character(dF$Plot.Number)
dF.1$z.Biomass<-(dF.1$Biomass-dF.2$Biomass.m)/dF.2$Biomass.sd/2
dF.1$z.Cocoa.density<-(dF.1$Cocoa.density-dF.2$Cocoa.density.m)/dF.2$Cocoa.density.sd/2
dF.1$z.distance.cont<-(dF.1$distance.cont-dF.2$distance.cont.m)/dF.2$distance.cont.sd/2
dF.1$z.No.applications.yr<-(dF.1$No.applications.yr-dF.2$No.applications.yr.m)/dF.2$No.applications.yr.sd/2
dF.1$z.soil.moist<-(dF.1$soil.moist-dF.2$soil.moist.m)/dF.2$soil.moist.sd/2
dF.1$z.Age.of.cocoa<-(dF.1$Age.of.cocoa-dF.2$Age.of.cocoa.m)/dF.2$Age.of.cocoa.sd/2

dF.1$z.pH<-(dF.1$pH-dF.2$pH.m)/dF.2$pH.sd/2

dF.1$yield.tree<-dF$HeavyCrop.med
dF.1$yield.ha<-dF$HeavyCrop.med.ha

#calculate max per tree yield
max.yield<-max(dF.1$yield.tree)
#calculate yield gap per plot
dF.1$yield.gap<-max.yield-dF.1$yield.tree
#calculate yield gap per ha (from cocoa density)
dF.1$yield.gap.ha<-dF.1$yield.gap*dF.1$Cocoa.density

#calculate yield potential per factor based on farm's 2014/15 measures, using AICcmodavg
#for delta 6
dF.1$yield.mod<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH

#calculate yield potential per factor based on farm's 2014/15 measures, without Intercept?
#calculate biomass yield contribution
dF.1$y.biomass<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa + tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH
dF.1$y.biomass.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Lower.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH
dF.1$y.biomass.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Upper.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH

#calculate cocoa density yield contribution
dF.1$y.cdensity<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH
dF.1$y.cdensity.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Lower.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa
dF.1$y.cdensity.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Upper.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa

#calculate distance from forest yield contribution
dF.1$y.fdist<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa
dF.1$y.fdist.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Lower.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa
dF.1$y.fdist.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Upper.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa

#calculate fertilizer yield contribution
dF.1$y.fert<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa
dF.1$y.fert.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Lower.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa
dF.1$y.fert.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Upper.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa

#calculate soil moisture yield contribution
dF.1$y.smoist<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa
dF.1$y.smoist.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Lower.CL"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa
dF.1$y.smoist.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Upper.CL"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa

#calculate age of cocoa yield contribution
dF.1$y.age<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*mean(dF.1$z.pH)
dF.1$y.age.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Lower.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Lower.CL"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Lower.CL"]*mean(dF.1$z.pH)
dF.1$y.age.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Upper.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Upper.CL"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Upper.CL"]*mean(dF.1$z.pH)+tmp[tmp$Comparison=="Capsid Incidence","Upper.CL"]*mean(dF.1$z.PropCPB)+tmp[tmp$Comparison=="Canopy Gap","Upper.CL"]*mean(dF.1$z.GapDry)

#calculate pH cocoa yield contribution
dF.1$y.pH<-tmp[tmp$Comparison=="(Intercept)","Estimate"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*mean(dF.1$z.Age.of.cocoa)+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH
dF.1$y.pH.lwr<-tmp[tmp$Comparison=="(Intercept)","Lower.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Lower.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Lower.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Lower.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Lower.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Lower.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Lower.CL"]*mean(dF.1$z.Age.of.cocoa)+tmp[tmp$Comparison=="Soil pH","Lower.CL"]*dF.1$z.pH
dF.1$y.pH.upr<-tmp[tmp$Comparison=="(Intercept)","Upper.CL"]*0+tmp[tmp$Comparison=="Distance to\nBiomass","Upper.CL"]*mean(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Upper.CL"]*mean(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Upper.CL"]*mean(dF.1$z.distance.cont)+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Upper.CL"]*mean(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Upper.CL"]*mean(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Upper.CL"]*mean(dF.1$z.Age.of.cocoa)+tmp[tmp$Comparison=="Soil pH","Upper.CL"]*dF.1$z.pH

write.csv(dF.1,paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

dF.1<-read.csv(paste0(getwd(),"/Analysis/ES/Modelled.yield.contribution",season,".delta6.csv"))

#plot contribution of distance from forest to yield
ggplot(dF.1,aes(distance.cont,y.fdist))+geom_point()+stat_smooth(method="lm")+geom_hline(yintercept=0,linetype="dashed")+
  xlab("Distance from Forest [m]")+ylab("Contribution to Yield [kg/tree]")+theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black'))
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig2_distancecontribtoyield.pdf",height=6,width=6)

#check components to modeled yield
dF.1$check<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+dF.1$y.biomass+dF.1$y.cdensity+dF.1$y.fdist+dF.1$y.fert+dF.1$y.smoist+dF.1$y.age+dF.1$y.pH

#calculate potential yield increase by factor, create new dataframe
dF.3<-data.frame(cbind(as.character(dF.1$plot),dF.1$yield.tree,dF.1$yield.ha),stringsAsFactors = F)
colnames(dF.3)<-c("plot","o.yield.tree","o.yield.ha")

dF.3$y.biomass.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*max(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod 

dF.3$y.biomass.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*min(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod 

dF.3$y.cdensity.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*max(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod

dF.3$y.cdensity.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*min(dF.1$z.Cocoa.density)+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod

dF.3$y.fert.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*max(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod

dF.3$y.fert.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*min(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*dF.1$z.soil.moist+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod

dF.3$y.smoist.1<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*max(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod

dF.3$y.smoist.2<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*dF.1$z.Biomass+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*dF.1$z.No.applications.yr+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*min(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod

#calculate yield increasing potential by maxing out influence of biomass, soil moisture, fertiliser
dF.3$yield.pot<-tmp[tmp$Comparison=="(Intercept)","Estimate"]+tmp[tmp$Comparison=="Distance to\nBiomass","Estimate"]*min(dF.1$z.Biomass)+tmp[tmp$Comparison=="Cocoa\nDensity","Estimate"]*dF.1$z.Cocoa.density+tmp[tmp$Comparison=="Distance\nFrom Forest","Estimate"]*dF.1$z.distance.cont+tmp[tmp$Comparison=="Avg Fertiliser\nApplications [yr-1]","Estimate"]*max(dF.1$z.No.applications.yr)+
  tmp[tmp$Comparison=="Soil\nMoisture","Estimate"]*max(dF.1$z.soil.moist)+tmp[tmp$Comparison=="Age of Cocoa","Estimate"]*dF.1$z.Age.of.cocoa+tmp[tmp$Comparison=="Soil pH","Estimate"]*dF.1$z.pH-dF.1$yield.mod

#calculate percent increase in per tree yield
dF.3$y.pot.pct<-dF.3$yield.pot/as.numeric(dF.3$o.yield.tree)*100

#calculate absolute yield increase per factor
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:4]<-cbind(dF.3$y.biomass.2,dF.3$y.fert.1,dF.3$y.smoist.1)
colnames(dF.4)<-c("plot","Biomass","Fert","SoilMoisture")

dF.4<-melt(dF.4,id.vars="plot")
dF.4$variable<-factor(dF.4$variable,labels=c("Distance from Biomass","Fertiliser","Soil Moisture"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

write.csv(dF.4,paste0(getwd(),"/Analysis/ES/Modelled.yield.per.tree.per.farm.contribution",season,".med.csv"))

#calculate the absolute change in factor for each farm
dF.3$d.biomass<-dF.1$Biomass-min(dF.1$Biomass)
dF.3$d.fert<-max(dF.1$No.applications.yr)-dF.1$No.applications.yr
dF.3$d.smoist<-max(dF.1$soil.moist)-dF.1$soil.moist

#calculate increase of yield per ha
dF.3$check<-rowSums(cbind(dF.3$y.biomass.2,dF.3$y.fert.1,dF.3$y.smoist.1))
dF.3$y.pot.ha<-dF.3$yield.pot*dF.1$Cocoa.density/0.36

#load key household variables
hhold.vars<-read.csv(paste0(getwd(),"/HouseholdData/Management/ES.Yield.vars.csv"))

dF.3$land.area<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"Land.area"]
dF.3$LBCs.loss<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"LBCs.loss"]
dF.3$shold<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"Sharehold"]
#calculate rent per ha
dF.3$rent.ha<-hhold.vars[match(dF.3$plot,hhold.vars$Plot),"Rent.cedis"]/dF.3$land.area

#load input and labour costs
hhold.costs<-read.csv(paste0(getwd(),"/HouseholdData/Labour.Input.Costs.csv"))
dF.3[,(ncol(dF.3)+1):(ncol(dF.3)+7)]<-hhold.costs[match(dF.3$plot,hhold.costs$PLOTCODE),2:8]
#dF.3$inputs<-hhold.costs[match(dF.3$plot,hhold.costs$PLOTCODE),"Input.ha"]

write.csv(dF.3,paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.",season,".csv"))
