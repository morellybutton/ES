#Exploring GAMS and HGAMS for cocoa yield modelling

library(mgcv)
library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")

#load per tree estimates
dF<-read.csv(paste0(getwd(),"/Analysis/ES/Yield_anomalies.csv"))

dF.14<-dF %>% filter(season=="2014/15"&tree_size=="all")

#check normality
car::qqp(dF.14$HeavyCrop,"norm")

#explore per yield drivers using basic GAM
yld14_mod1 <- gam(HeavyCrop ~ s(Cocoa.density, k = 5, bs = "tp") + s(Canopy.gap.dry, k = 5, bs = "tp") + s(Age.of.cocoa, k = 5, bs = "tp") + s(soil.moist, k = 5, bs = "tp") + 
                    s(PropCPB, k = 5, bs = "tp") + s(Biomass, k = 5, bs = "tp") + s(No.applications.yr, k = 5, bs = "tp") + s(distance.cont, k = 5, bs = "tp"),
                data=dF.14, method="REML", family="gaussian",select=TRUE)
summary(yld14_mod1)

yld14_mod2 <- gam(HeavyCrop ~ s(Cocoa.density, k = 5, bs = "tp") + s(Canopy.gap.dry, k = 5, bs = "tp") +
                    s(PropCPB, k = 5, bs = "tp") +  s(soil.moist, k = 5, bs = "tp") + s(Biomass, k = 5, bs = "tp") + s(No.applications.yr, k = 5, bs = "tp") + s(distance.cont, k = 5, bs = "tp"),
                  data=dF.14, method="REML", family="gaussian",select=TRUE)
summary(yld14_mod2)

yld14_mod3 <- gam(HeavyCrop ~ s(Cocoa.density, k = 5, bs = "tp") + s(Canopy.gap.dry, k = 5, bs = "tp") +
                    s(PropCPB, k = 5, bs = "tp") + s(Biomass, k = 5, bs = "tp") + s(No.applications.yr, k = 5, bs = "tp") + s(distance.cont, k = 5, bs = "tp"),
                  data=dF.14, method="REML", family="gaussian",select=TRUE)
summary(yld14_mod3)

b<-yld14_mod2
b2<-summary(b)

b2.summ<-data.frame(b2$s.table)
b2.summ$pvalue<-NA
b2.summ<- b2.summ %>% mutate(pvalue=replace(pvalue,p.value<0.001,"<.001")) %>% 
  mutate(pvalue=replace(pvalue,p.value>=0.001&p.value<0.01,"<.01"))  %>%
  mutate(pvalue=replace(pvalue,p.value>0.01&p.value<0.05,"<.05")) %>%
  mutate(pvalue=replace(pvalue,p.value>0.05,"NS"))

pdf(paste0(getwd(),"/Analysis/ES/GAM.yield_factors.pdf"),height=6,width=11)

par(mfrow=c(2,4), pty="m",mar=c(4,4,1,1),cex=1)  
# xlab=""
plot.gam(b, select=1, all.terms=T, shade=T, xlab="Cocoa Density\n[trees/ha]", ylab="") 
text(400, 0.5, paste0("F = ",signif(b2.summ$F[1],3),"; p = ",b2.summ$pvalue[1]))
# xlabs=""
plot.gam(b, select=2, all.terms=T, shade=T, xlab="Canopy Gap\n[%]", ylab="")
text(50, 0.5, paste0("F = ",signif(b2.summ$F[2],3),"; p = ",b2.summ$pvalue[2]))

plot.gam(b, select=3, all.terms=T, shade=T, xlab="Capsid Incidence", ylab="")
text(0.08, 0.5, paste0("F = ",signif(b2.summ$F[3],3),"; p = ",b2.summ$pvalue[3]))

plot.gam(b, select=4, all.terms=T, shade=T, xlab="Soil Moisture\n[%]", ylab="")
text(18, 0.5, paste0("F = ",signif(b2.summ$F[4],3),"; p = ",b2.summ$pvalue[4]))

plot.gam(b, select=5, all.terms=T, shade=T, xlab="Distance from Biomass\n[m]", ylab="")
text(50, 0.5, paste0("F = ",signif(b2.summ$F[5],3),"; p = ",b2.summ$pvalue[5]))

plot.gam(b, select=6, all.terms=T, shade=T, xlab="Fertiliser Applications\n[No/yr]", ylab="")
text(3, -0.5, paste0("F = ",signif(b2.summ$F[6],3),"; p = ",b2.summ$pvalue[6]))

plot.gam(b, select=7, all.terms=T, shade=T, xlab="Distance from Forest\n[m]", ylab="")
text(2500, 0.5, paste0("F = ",signif(b2.summ$F[7],3),"; p = ",b2.summ$pvalue[7]))

plot(1:100, type="n", xaxt="n", yaxt="n", bty="n", xlab="", ylab="")

R2Val<-signif(b2$r.sq,3)
text(x=40, y=30, labels = bquote(R^2 == .(R2Val)),cex=1.25)
text(x=40, y=10, labels = "n = 36",cex=1.25)

dev.off()

#run model checks
yld14.fit <- mgcViz::getViz(b)
mgcViz::qq(yld14.fit, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))

gam.check(yld14.fit,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

#save diagnostic plots
#https://stackoverflow.com/questions/22275610/how-to-get-only-the-plots-from-gam-check

type <- "deviance"
resid <- residuals(b, type = type)
linpred <- napredict(b$na.action, b$linear.predictors)
observed.y <- napredict(b$na.action, b$y)

pdf(paste0(getwd(),"/Analysis/ES/GAM.yield_diagnostic.pdf"))
par(mfrow=c(2,2))

qq.gam(b, rep = 0, level = 0.9, type = type, rl.col = 2, 
       rep.col = "gray80")
hist(resid, xlab = "Residuals", main = "Histogram of residuals")
plot(linpred, resid, main = "Resids vs. linear pred.", 
         xlab = "linear predictor", ylab = "residuals")
plot(fitted(b), observed.y, xlab = "Fitted Values", 
     ylab = "Response", main = "Response vs. Fitted Values")
abline(a=0,b=1,lty=3)
dev.off()

#create datasets for max of fertiliser and min of capsid and biomass distance
dF1<-dF.14 %>% select(plot,HeavyCrop,PropCPB,Biomass,distance.cont,Cocoa.density,Canopy.gap.dry,soil.moist,No.applications.yr)
dF1$Bmass.min<-min(dF1$Biomass)
dF1$CPB.min<-min(dF1$PropCPB)
dF1$Fert.max<-max(dF1$No.applications.yr)

dF1.bmass<-dF1 %>% select(-Biomass) %>% rename(Biomass=Bmass.min)
dF1.cpb<-dF1 %>% select(-PropCPB) %>% rename(PropCPB=CPB.min)
dF1.fert<-dF1 %>% select(-No.applications.yr) %>% rename(No.applications.yr=Fert.max)
dF1.all<-dF1 %>% select(-No.applications.yr,-Biomass,-PropCPB) %>% 
  rename(No.applications.yr=Fert.max,PropCPB=CPB.min,Biomass=Bmass.min)
dF1.int<-dF1 %>% select(-No.applications.yr,-PropCPB) %>% 
  rename(No.applications.yr=Fert.max,PropCPB=CPB.min)

#b1<-plot.gam(b)

#for biomass
p.bmass <- predict(b, dF1.bmass, type = "link", se.fit = TRUE,unconditional = TRUE)
upr.bmass <- p.bmass$fit + (2 * p.bmass$se.fit)
lwr.bmass <- p.bmass$fit - (2 * p.bmass$se.fit)
upr.bmass <- b$family$linkinv(upr.bmass)
lwr.bmass <- b$family$linkinv(lwr.bmass)

mod.bmass<-data.frame(cbind(p.bmass$fit,p.bmass$se.fit))
colnames(mod.bmass)<-c("fit","se.fit")
mod.bmass$upr<-upr.bmass
mod.bmass$lwr<-lwr.bmass
mod.bmass$plot<-dF1.bmass$plot
mod.bmass$original<-dF1.bmass$HeavyCrop
mod.bmass$c.density<-dF1.bmass$Cocoa.density

#for fertilisers
p.fert <- predict(b, dF1.fert, type = "link", se.fit = TRUE,unconditional = TRUE)
upr.fert <- p.fert$fit + (2 * p.fert$se.fit)
lwr.fert <- p.fert$fit - (2 * p.fert$se.fit)
upr.fert <- b$family$linkinv(upr.fert)
lwr.fert <- b$family$linkinv(lwr.fert)

mod.fert<-data.frame(cbind(p.fert$fit,p.fert$se.fit))
colnames(mod.fert)<-c("fit","se.fit")
mod.fert$upr<-upr.fert
mod.fert$lwr<-lwr.fert
mod.fert$plot<-dF1.fert$plot
mod.fert$original<-dF1.fert$HeavyCrop
mod.fert$c.density<-dF1.fert$Cocoa.density

#for all
p.cpb <- predict(b, dF1.cpb, type = "link", se.fit = TRUE,unconditional = TRUE)
upr.cpb <- p.cpb$fit + (2 * p.cpb$se.fit)
lwr.cpb <- p.cpb$fit - (2 * p.cpb$se.fit)
upr.cpb <- b$family$linkinv(upr.cpb)
lwr.cpb <- b$family$linkinv(lwr.cpb)

mod.cpb<-data.frame(cbind(p.cpb$fit,p.cpb$se.fit))
colnames(mod.cpb)<-c("fit","se.fit")
mod.cpb$upr<-upr.cpb
mod.cpb$lwr<-lwr.cpb
mod.cpb$plot<-dF1.cpb$plot
mod.cpb$original<-dF1.cpb$HeavyCrop
mod.cpb$c.density<-dF1.cpb$Cocoa.density

#for all
p.all <- predict(b, dF1.all, type = "link", se.fit = TRUE,unconditional = TRUE)
upr.all <- p.all$fit + (2 * p.all$se.fit)
lwr.all <- p.all$fit - (2 * p.all$se.fit)
upr.all <- b$family$linkinv(upr.all)
lwr.all <- b$family$linkinv(lwr.all)

mod.all<-data.frame(cbind(p.all$fit,p.all$se.fit))
colnames(mod.all)<-c("fit","se.fit")
mod.all$upr<-upr.all
mod.all$lwr<-lwr.all
mod.all$plot<-dF1.all$plot
mod.all$original<-dF1.all$HeavyCrop
mod.all$c.density<-dF1.all$Cocoa.density

#for intensive options
p.int <- predict(b, dF1.int, type = "link", se.fit = TRUE,unconditional = TRUE)
upr.int <- p.int$fit + (2 * p.int$se.fit)
lwr.int <- p.int$fit - (2 * p.int$se.fit)
upr.int <- b$family$linkinv(upr.int)
lwr.int <- b$family$linkinv(lwr.int)

mod.int<-data.frame(cbind(p.int$fit,p.int$se.fit))
colnames(mod.int)<-c("fit","se.fit")
mod.int$upr<-upr.int
mod.int$lwr<-lwr.int
mod.int$plot<-dF1.int$plot
mod.int$original<-dF1.int$HeavyCrop
mod.int$c.density<-dF1.int$Cocoa.density

#calculate relative increase in yields per factor
#multiply by cocoa density
mod.bmass<-mod.bmass %>% mutate(orig.yld=original*c.density,yld.diff=fit/original,yld.diff.upr=upr/original,yld.diff.lwr=lwr/original) 
mod.cpb<-mod.cpb %>% mutate(orig.yld=original*c.density,yld.diff=fit/original,yld.diff.upr=upr/original,yld.diff.lwr=lwr/original) 
mod.fert<-mod.fert %>% mutate(orig.yld=original*c.density,yld.diff=fit/original,yld.diff.upr=upr/original,yld.diff.lwr=lwr/original) 
mod.all<-mod.all %>% mutate(orig.yld=original*c.density,yld.diff=fit/original,yld.diff.upr=upr/original,yld.diff.lwr=lwr/original)
mod.int<-mod.int %>% mutate(orig.yld=original*c.density,yld.diff=fit/original,yld.diff.upr=upr/original,yld.diff.lwr=lwr/original)

#combine into one dataframe
dF.yield<-dF1 %>% select(plot,PropCPB,Biomass,distance.cont,No.applications.yr,Bmass.min,CPB.min,Fert.max) %>% 
  mutate(bmass.diff=Bmass.min-Biomass,cpb.diff=CPB.min-PropCPB,fert.diff=Fert.max-No.applications.yr)
dF.yield<-bind_cols(dF.yield,mod.bmass %>% select(original,c.density,orig.yld,yld.diff,yld.diff.upr,yld.diff.lwr) %>% 
  rename(bmass=yld.diff,bmass.upr=yld.diff.upr,bmass.lwr=yld.diff.lwr))
dF.yield<-bind_cols(dF.yield,mod.cpb %>% select(yld.diff,yld.diff.upr,yld.diff.lwr) %>% 
                      rename(cpb=yld.diff,cpb.upr=yld.diff.upr,cpb.lwr=yld.diff.lwr))
dF.yield<-bind_cols(dF.yield,mod.fert %>% select(yld.diff,yld.diff.upr,yld.diff.lwr) %>% 
                      rename(fert=yld.diff,fert.upr=yld.diff.upr,fert.lwr=yld.diff.lwr))
dF.yield<-bind_cols(dF.yield,mod.all %>% select(yld.diff,yld.diff.upr,yld.diff.lwr) %>% 
                      rename(all=yld.diff,all.upr=yld.diff.upr,all.lwr=yld.diff.lwr))
dF.yield<-bind_cols(dF.yield,mod.int %>% select(yld.diff,yld.diff.upr,yld.diff.lwr) %>% 
                      rename(int=yld.diff,int.upr=yld.diff.upr,int.lwr=yld.diff.lwr))

write.csv(dF.yield,paste0(getwd(),"/Analysis/ES/Modelled.yield.increase.per.farm.contribution1415.GAM.csv"))


inc.bmass<-mod.bmass %>% summarise(original=sum(orig.yld),mod=sum(orig.yld*yld.diff),mod.upr=sum(orig.yld*yld.diff.upr),mod.lwr=sum(orig.yld*yld.diff.lwr)) %>% 
  mutate(value=mod/original,upr=mod.upr/original,lwr=mod.lwr/original) %>% select(value,upr,lwr)
inc.bmass$variable<-"biomass"

inc.cpb<-mod.cpb %>% summarise(original=sum(orig.yld),mod=sum(orig.yld*yld.diff),mod.upr=sum(orig.yld*yld.diff.upr),mod.lwr=sum(orig.yld*yld.diff.lwr)) %>% 
  mutate(value=mod/original,upr=mod.upr/original,lwr=mod.lwr/original) %>% select(value,upr,lwr)
inc.cpb$variable<-"cpb"

inc.fert<-mod.fert %>% summarise(original=sum(orig.yld),mod=sum(orig.yld*yld.diff),mod.upr=sum(orig.yld*yld.diff.upr),mod.lwr=sum(orig.yld*yld.diff.lwr)) %>% 
  mutate(value=mod/original,upr=mod.upr/original,lwr=mod.lwr/original) %>% select(value,upr,lwr)
inc.fert$variable<-"fert"

inc.all<-mod.all %>% summarise(original=sum(orig.yld),mod=sum(orig.yld*yld.diff),mod.upr=sum(orig.yld*yld.diff.upr),mod.lwr=sum(orig.yld*yld.diff.lwr)) %>% 
  mutate(value=mod/original,upr=mod.upr/original,lwr=mod.lwr/original) %>% select(value,upr,lwr)
inc.all$variable<-"all"

yld.inc<-bind_rows(inc.all,inc.bmass)
yld.inc<-bind_rows(yld.inc,inc.cpb)
yld.inc<-bind_rows(yld.inc,inc.fert)

yld.inc <- yld.inc %>% arrange(value)

yld.inc$variable<-factor(yld.inc$variable,levels=c("cpb","biomass","fert","all"), labels=c("Capsid Management\nPotential","Distance from Biomass\nPotential",
                                                                                                               "Fertiliser Application\nPotential","Total Potential"))

ggplot(yld.inc, aes(fct_reorder(variable,value),value)) + geom_point(size=2.0) + theme_classic() + theme(axis.text.x=element_text(angle = 45,hjust=1),text = element_text(size=16))+
  xlab("Model Parameter") + ylab("Relative Increase in Yield") + ylim(0,3) + geom_errorbar(aes(ymin=lwr,ymax=upr),width=0.03,size=1.0) + coord_fixed(ratio = 1) +
  geom_hline(yintercept=1,linetype="dashed")
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Submission/Frontiers/reDraft/Figure2_relativeincreaseinyields.GAM.pdf"))


#load key household variables
hhold.vars<-read.csv(paste0(getwd(),"/HouseholdData/Management/ES.Yield.vars.csv"))

dF.yield$land.area<-hhold.vars[match(dF.yield$plot,hhold.vars$Plot),"Land.area"]
dF.yield$LBCs.loss<-hhold.vars[match(dF.yield$plot,hhold.vars$Plot),"LBCs.loss"]
dF.yield$shold<-hhold.vars[match(dF.yield$plot,hhold.vars$Plot),"Sharehold"]
#calculate rent per ha
dF.yield$rent.ha<-hhold.vars[match(dF.yield$plot,hhold.vars$Plot),"Rent.cedis"]/dF.yield$land.area

#load input and labour costs
hhold.costs<-read.csv(paste0(getwd(),"/HouseholdData/Labour.Input.Costs.csv"))
hhold.costs <- hhold.costs %>% rename(plot=PLOTCODE)
dF.3 <- left_join(dF.yield,hhold.costs,by="plot")

write.csv(dF.3,paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.1415.csv"))

#calculate old and new net margins
dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.1415.csv"))

#cocoa price per 65 kg bag
kg<-65
bag_cedi<-350

#calculate estimated income from total harvest * cocoa price, lbc loss, income to sharehold/rent and 
#final income (per ha) before removing labour and inputs
dF.3 <- dF.3 %>% mutate(orig.income.ha=orig.yld/kg*bag_cedi) %>% mutate(orig.lbc.loss=orig.income.ha*(LBCs.loss/100)) %>% 
  mutate(orig.sharerent.loss=(orig.income.ha-orig.lbc.loss)*shold+rent.ha) %>% 
  mutate(orig.final.income=orig.income.ha-orig.lbc.loss-orig.sharerent.loss,
         orig.final.income.all=orig.income.ha-(orig.income.ha*shold+rent.ha))

#calculate total income farmer gets (without considering outlays) [multiply by land area] and with outlays, then divide by land area
dF.3 <- dF.3 %>% mutate(orig.revenue=orig.final.income*land.area,orig.revenue.all=orig.final.income.all*land.area) %>% 
  mutate(orig.total.margin=orig.revenue-(Labour.ha+Input.ha)*land.area, orig.total.margin.all=orig.revenue.all-(Labour.ha+Input.ha)*land.area) %>% 
  mutate(orig.net.margin=orig.total.margin/land.area,orig.net.margin.all=orig.total.margin.all/land.area)

#plot lbc lost vs not lost
ggplot(dF.3,aes(orig.total.margin,orig.total.margin.all)) + geom_point() + geom_abline(intercept=0,slope=1,linetype="dotted") +
  theme_classic() + xlab("Total Margin with LBC Loss [cedis]") + ylab("Total Margin without LBC loss [cedis]") + geom_hline(yintercept=0,linetype="dashed") +
  geom_vline(xintercept=0,linetype="dashed")
ggsave(paste0(getwd(),"/Analysis/ES/TotalmarginvsLBCloss.GAM_1415.pdf"))

#calculate increase in input costs with greater yields
i.mod<-lm(Input.ha~orig.yld,data=dF.3)
input<-coefficients(i.mod)
r.adj <- summary(i.mod)$adj.r.squared

g1<-ggplot(dF.3,aes(orig.yld,Input.ha)) + geom_point() +stat_smooth(method="lm") + theme_classic() + theme(text = element_text(size=14)) +
  xlab("Yield [kg/ha]") + ylab("Input costs [cedis/ha]") + annotate("text",100,2000,label=paste("R^2==",signif(r.adj,2),sep=""),parse=T)
g2<-ggplot(dF.3,aes(orig.yld,Labour.ha)) + geom_point() +stat_smooth(method="lm") + theme_classic() + theme(text = element_text(size=14)) +
  xlab("Yield [kg/ha]") + ylab("Labour costs [cedis/ha]")
g3<-gridExtra::grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Submission/Frontiers/reDraft/Supp.Fig2_labour.input.costs.pdf",g3,height=4,width=9)

#calculate new input costs with increased yields from fertiliser and capsids
dF.3<-dF.3 %>% group_by(plot) %>% mutate(y.tot.fert.ha=orig.yld*fert,y.tot.fert.ha.lwr=orig.yld*fert.lwr,y.tot.fert.ha.upr=orig.yld*fert.upr,
                                         y.tot.cpb.ha=orig.yld*cpb,y.tot.cpb.ha.lwr=orig.yld*cpb.lwr,y.tot.cpb.ha.upr=orig.yld*cpb.upr,
                                         y.tot.int.ha=orig.yld*int,y.tot.int.ha.lwr=orig.yld*int.lwr,y.tot.int.ha.upr=orig.yld*int.upr,
                                         y.tot.bmass.ha=orig.yld*bmass,y.tot.bmass.ha.lwr=orig.yld*bmass.lwr,y.tot.bmass.ha.upr=orig.yld*bmass.upr,
                                         y.tot.all.ha=orig.yld*all,y.tot.all.ha.lwr=orig.yld*all.lwr,y.tot.all.ha.upr=orig.yld*all.upr) %>% 
  mutate(i.pot.input.fert=y.tot.fert.ha*input[2]+input[1],i.pot.input.fert.lwr=y.tot.fert.ha.lwr*input[2]+input[1],i.pot.input.fert.upr=y.tot.fert.ha.upr*input[2]+input[1],
         i.pot.input.cpb=y.tot.cpb.ha*input[2]+input[1],i.pot.input.cpb.lwr=y.tot.cpb.ha.lwr*input[2]+input[1],i.pot.input.cpb.upr=y.tot.cpb.ha.upr*input[2]+input[1],
         i.pot.input.int=y.tot.int.ha*input[2]+input[1],i.pot.input.int.lwr=y.tot.int.ha.lwr*input[2]+input[1],i.pot.input.int.upr=y.tot.int.ha.upr*input[2]+input[1]) 



#calculate potential total income from per ha increase * cocoa price, fertiliser, need to subtract new input costs and previous Labour
dF.3<-dF.3 %>% mutate(pot.income.fert=y.tot.fert.ha/kg*bag_cedi*land.area,pot.income.fert.lwr=y.tot.fert.ha.lwr/kg*bag_cedi*land.area,
                      pot.income.fert.upr=y.tot.fert.ha.upr/kg*bag_cedi*land.area) %>% 
  mutate(pot.lbc.fert=pot.income.fert*LBCs.loss/100, pot.lbc.fert.lwr=pot.income.fert.lwr*LBCs.loss/100,pot.lbc.fert.upr=pot.income.fert.upr*LBCs.loss/100) %>% 
  mutate(pot.net.margin.fert=(pot.income.fert-pot.lbc.fert-(pot.income.fert-pot.lbc.fert)*shold-(i.pot.input.fert+Labour.ha)*land.area)/land.area,
         pot.net.margin.fert.lwr=(pot.income.fert.lwr-pot.lbc.fert.lwr-(pot.income.fert.lwr-pot.lbc.fert.lwr)*shold-(i.pot.input.fert.lwr+Labour.ha)*land.area)/land.area,
         pot.net.margin.fert.upr=(pot.income.fert.upr-pot.lbc.fert.upr-(pot.income.fert.upr-pot.lbc.fert.upr)*shold-(i.pot.input.fert.upr+Labour.ha)*land.area)/land.area)


#calculate potential total income from per ha increase * cocoa price, fertiliser, need to subtract new input costs and previous Labour, with and without lbc costs
dF.3<-dF.3 %>% mutate(pot.income.fert=y.tot.fert.ha/kg*bag_cedi*land.area,pot.income.fert.lwr=y.tot.fert.ha.lwr/kg*bag_cedi*land.area,
                      pot.income.fert.upr=y.tot.fert.ha.upr/kg*bag_cedi*land.area) %>% 
  mutate(pot.lbc.fert=pot.income.fert*LBCs.loss/100, pot.lbc.fert.lwr=pot.income.fert.lwr*LBCs.loss/100,pot.lbc.fert.upr=pot.income.fert.upr*LBCs.loss/100) %>% 
  mutate(pot.net.margin.fert=(pot.income.fert-pot.lbc.fert-(pot.income.fert-pot.lbc.fert)*shold-(i.pot.input.fert+Labour.ha)*land.area)/land.area,
         pot.net.margin.fert.lwr=(pot.income.fert.lwr-pot.lbc.fert.lwr-(pot.income.fert.lwr-pot.lbc.fert.lwr)*shold-(i.pot.input.fert.lwr+Labour.ha)*land.area)/land.area,
         pot.net.margin.fert.upr=(pot.income.fert.upr-pot.lbc.fert.upr-(pot.income.fert.upr-pot.lbc.fert.upr)*shold-(i.pot.input.fert.upr+Labour.ha)*land.area)/land.area) %>% 
  mutate(pot.net.margin.fert.all=(pot.income.fert-(pot.income.fert)*shold-(i.pot.input.fert+Labour.ha)*land.area)/land.area,
         pot.net.margin.fert.all.lwr=(pot.income.fert.lwr-(pot.income.fert.lwr)*shold-(i.pot.input.fert.lwr+Labour.ha)*land.area)/land.area,
         pot.net.margin.fert.all.upr=(pot.income.fert.upr-(pot.income.fert.upr)*shold-(i.pot.input.fert.upr+Labour.ha)*land.area)/land.area)

#calculate potential total income from per ha increase * cocoa price, biomass, need to subtract new input costs and previous Labour, with and without lbc costs
dF.3<-dF.3 %>% mutate(pot.income.bmass=y.tot.bmass.ha/kg*bag_cedi*land.area,pot.income.bmass.lwr=y.tot.bmass.ha.lwr/kg*bag_cedi*land.area,
                      pot.income.bmass.upr=y.tot.bmass.ha.upr/kg*bag_cedi*land.area) %>% 
  mutate(pot.lbc.bmass=pot.income.bmass*LBCs.loss/100, pot.lbc.bmass.lwr=pot.income.bmass.lwr*LBCs.loss/100,pot.lbc.bmass.upr=pot.income.bmass.upr*LBCs.loss/100) %>% 
  mutate(pot.net.margin.bmass=(pot.income.bmass-pot.lbc.bmass-(pot.income.bmass-pot.lbc.bmass)*shold-(Input.ha+Labour.ha)*land.area)/land.area,
         pot.net.margin.bmass.lwr=(pot.income.bmass.lwr-pot.lbc.bmass.lwr-(pot.income.bmass.lwr-pot.lbc.bmass.lwr)*shold-(Input.ha+Labour.ha)*land.area)/land.area,
         pot.net.margin.bmass.upr=(pot.income.bmass.upr-pot.lbc.bmass.upr-(pot.income.bmass.upr-pot.lbc.bmass.upr)*shold-(Input.ha+Labour.ha)*land.area)/land.area) %>% 
  mutate(pot.net.margin.bmass.all=(pot.income.bmass-(pot.income.bmass)*shold-(Input.ha+Labour.ha)*land.area)/land.area,
         pot.net.margin.bmass.all.lwr=(pot.income.bmass.lwr-(pot.income.bmass.lwr)*shold-(Input.ha+Labour.ha)*land.area)/land.area,
         pot.net.margin.bmass.all.upr=(pot.income.bmass.upr-(pot.income.bmass.upr)*shold-(Input.ha+Labour.ha)*land.area)/land.area)

#calculate potential total income from per ha increase * cocoa price, biomass, need to subtract new input costs and previous Labour, with and without lbc costs
dF.3<-dF.3 %>% mutate(pot.income.cpb=y.tot.cpb.ha/kg*bag_cedi*land.area,pot.income.cpb.lwr=y.tot.cpb.ha.lwr/kg*bag_cedi*land.area,
                      pot.income.cpb.upr=y.tot.cpb.ha.upr/kg*bag_cedi*land.area) %>% 
  mutate(pot.lbc.cpb=pot.income.cpb*LBCs.loss/100, pot.lbc.cpb.lwr=pot.income.cpb.lwr*LBCs.loss/100,pot.lbc.cpb.upr=pot.income.cpb.upr*LBCs.loss/100) %>% 
  mutate(pot.net.margin.cpb=(pot.income.cpb-pot.lbc.cpb-(pot.income.cpb-pot.lbc.cpb)*shold-(i.pot.input.cpb+Labour.ha)*land.area)/land.area,
         pot.net.margin.cpb.lwr=(pot.income.cpb.lwr-pot.lbc.cpb.lwr-(pot.income.cpb.lwr-pot.lbc.cpb.lwr)*shold-(i.pot.input.cpb.lwr+Labour.ha)*land.area)/land.area,
         pot.net.margin.cpb.upr=(pot.income.cpb.upr-pot.lbc.cpb.upr-(pot.income.cpb.upr-pot.lbc.cpb.upr)*shold-(i.pot.input.cpb.upr+Labour.ha)*land.area)/land.area) %>% 
  mutate(pot.net.margin.cpb.all=(pot.income.cpb-(pot.income.cpb)*shold-(i.pot.input.cpb+Labour.ha)*land.area)/land.area,
         pot.net.margin.cpb.all.lwr=(pot.income.cpb.lwr-(pot.income.cpb.lwr)*shold-(i.pot.input.cpb.lwr+Labour.ha)*land.area)/land.area,
         pot.net.margin.cpb.all.upr=(pot.income.cpb.upr-(pot.income.cpb.upr)*shold-(i.pot.input.cpb.upr+Labour.ha)*land.area)/land.area)

#calculate potential total income from per ha increase * cocoa price, biomass, need to subtract new input costs and previous Labour, with and without lbc costs
dF.3<-dF.3 %>% mutate(pot.income.all=y.tot.all.ha/kg*bag_cedi*land.area,pot.income.all.lwr=y.tot.all.ha.lwr/kg*bag_cedi*land.area,
                      pot.income.all.upr=y.tot.all.ha.upr/kg*bag_cedi*land.area) %>% 
  mutate(pot.lbc.all=pot.income.all*LBCs.loss/100, pot.lbc.all.lwr=pot.income.all.lwr*LBCs.loss/100,pot.lbc.all.upr=pot.income.all.upr*LBCs.loss/100) %>% 
  mutate(pot.net.margin.all=(pot.income.all-pot.lbc.all-(pot.income.all-pot.lbc.all)*shold-(i.pot.input.int+Labour.ha)*land.area)/land.area,
         pot.net.margin.all.lwr=(pot.income.all.lwr-pot.lbc.all.lwr-(pot.income.all.lwr-pot.lbc.all.lwr)*shold-(i.pot.input.int.lwr+Labour.ha)*land.area)/land.area,
         pot.net.margin.all.upr=(pot.income.all.upr-pot.lbc.all.upr-(pot.income.all.upr-pot.lbc.all.upr)*shold-(i.pot.input.int.upr+Labour.ha)*land.area)/land.area) %>% 
  mutate(pot.net.margin.all.all=(pot.income.all-(pot.income.all)*shold-(i.pot.input.int+Labour.ha)*land.area)/land.area,
         pot.net.margin.all.all.lwr=(pot.income.all.lwr-(pot.income.all.lwr)*shold-(i.pot.input.int.lwr+Labour.ha)*land.area)/land.area,
         pot.net.margin.all.all.upr=(pot.income.all.upr-(pot.income.all.upr)*shold-(i.pot.input.int.upr+Labour.ha)*land.area)/land.area)

usd<-0.23
#create figure of 4 options (biomass, fertiliser, canopy gap and capsids)
g1<-ggplot(dF.3 %>% filter(pot.net.margin.bmass>0&orig.net.margin>0),aes(orig.net.margin*usd,pot.net.margin.bmass*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=pot.net.margin.bmass.upr*usd,ymin=pot.net.margin.bmass.lwr*usd)) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.bmass<0),aes(orig.net.margin*usd,pot.net.margin.bmass*usd),color="grey") +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.bmass<0),aes(ymax=pot.net.margin.bmass.upr*usd,ymin=pot.net.margin.bmass.lwr*usd),color="grey",width=0) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.bmass>0&orig.net.margin<0),aes(orig.net.margin*usd,pot.net.margin.bmass*usd),shape=1) +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.bmass>0&orig.net.margin<0),aes(ymax=pot.net.margin.bmass.upr*usd,ymin=pot.net.margin.bmass.lwr*usd),width=0) +
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [$US/ha]") + ylab("Potential Net Margin [$US/ha]")+
  ggtitle("Distance From Biomass") + xlim(-500,1000) + ylim(-500,1600)

g2<-ggplot(dF.3 %>% filter(pot.net.margin.fert>0&orig.net.margin>0),aes(orig.net.margin*usd,pot.net.margin.fert*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=pot.net.margin.fert.upr*usd,ymin=pot.net.margin.fert.lwr*usd)) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.fert<0),aes(orig.net.margin*usd,pot.net.margin.fert*usd),color="grey") +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.fert<0),aes(ymax=pot.net.margin.fert.upr*usd,ymin=pot.net.margin.fert.lwr*usd),color="grey",width=0) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.fert>0&orig.net.margin<0),aes(orig.net.margin*usd,pot.net.margin.fert*usd),shape=1) +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.fert>0&orig.net.margin<0),aes(ymax=pot.net.margin.fert.upr*usd,ymin=pot.net.margin.fert.lwr*usd),width=0) +
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [$US/ha]") + ylab("Potential Net Margin [$US/ha]")+
  ggtitle("Fertiliser Application") + xlim(-500,1000) + ylim(-500,1100)


#g3<-ggplot(dF.3,aes(o.net.margin,i.pot.net.margin.cgap)) + geom_point() + theme_classic()+
#  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [cedis/ha]") + ylab("Potential Net Margin [cedis/ha]")+
#  ggtitle("Canopy Gap") + xlim(-500,6500) + ylim(-500,6500)

g3<-ggplot(dF.3 %>% filter(pot.net.margin.cpb>0&orig.net.margin>0),aes(orig.net.margin*usd,pot.net.margin.cpb*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=pot.net.margin.cpb.upr*usd,ymin=pot.net.margin.cpb.lwr*usd)) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.cpb<0),aes(orig.net.margin*usd,pot.net.margin.cpb*usd),color="grey") +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.cpb<0),aes(ymax=pot.net.margin.cpb.upr*usd,ymin=pot.net.margin.cpb.lwr*usd),color="grey",width=0) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.cpb>0&orig.net.margin<0),aes(orig.net.margin*usd,pot.net.margin.cpb*usd),shape=1) +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.cpb>0&orig.net.margin<0),aes(ymax=pot.net.margin.cpb.upr*usd,ymin=pot.net.margin.cpb.lwr*usd),width=0) +
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [$US/ha]") + ylab("Potential Net Margin [$US/ha]")+
  ggtitle("Capsid Management") + xlim(-500,1000) + ylim(-500,1000)

g4<-ggplot(dF.3 %>% filter(pot.net.margin.all>0&orig.net.margin>0),aes(orig.net.margin*usd,pot.net.margin.all*usd)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax=pot.net.margin.all.upr*usd,ymin=pot.net.margin.all.lwr*usd)) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.all<0),aes(orig.net.margin*usd,pot.net.margin.all*usd),color="grey") +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.all<0),aes(ymax=pot.net.margin.all.upr*usd,ymin=pot.net.margin.all.lwr*usd),color="grey",width=0) +
  geom_point(data=dF.3 %>% filter(pot.net.margin.all>0&orig.net.margin<0),aes(orig.net.margin*usd,pot.net.margin.all*usd),shape=1) +
  geom_errorbar(data=dF.3 %>% filter(pot.net.margin.all>0&orig.net.margin<0),aes(ymax=pot.net.margin.all.upr*usd,ymin=pot.net.margin.all.lwr*usd),width=0) +
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [$US/ha]") + ylab("Potential Net Margin [$US/ha]")+
  ggtitle("All Management Options") + xlim(-500,1000) + ylim(-500,2000)

g5<-gridExtra::grid.arrange(g3,g1,g2,g4,ncol=2)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Submission/Frontiers/reDraft/Figure3_CocoaIncome_potential_increase.byparameter.pdf"),g5,height=8,width=9)

#calculate percent net margin increase based on original income,
#calculate percent net margin increase based on original income by summing increase and dividing by absolute value of original income,
#replace negative values with 1s
dF.3 <- dF.3 %>% group_by(plot) %>% mutate(prop.bmass=pot.net.margin.bmass/abs(orig.net.margin),prop.bmass.lwr=pot.net.margin.bmass.lwr/orig.net.margin,prop.bmass.upr=pot.net.margin.bmass.upr/orig.net.margin,
                                           prop.fert=pot.net.margin.fert/orig.net.margin,prop.fert.lwr=pot.net.margin.fert.lwr/orig.net.margin,prop.fert.upr=pot.net.margin.fert.upr/orig.net.margin,
                                           prop.cpb=pot.net.margin.cpb/orig.net.margin,prop.cpb.lwr=pot.net.margin.cpb.lwr/orig.net.margin,prop.cpb.upr=pot.net.margin.cpb.upr/orig.net.margin,
                                           prop.all=pot.net.margin.all/orig.net.margin,prop.all.lwr=pot.net.margin.all.lwr/orig.net.margin,prop.all.upr=pot.net.margin.all.upr/orig.net.margin,
                                           prop.noloss=orig.net.margin.all/orig.net.margin) 


#compare age of cocoa to distance
tmp1<-summary(lm(Age.of.cocoa~distance.cont,data=dF.14))
g1<-ggplot(dF.14,aes(distance.cont,Age.of.cocoa)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=40,label = paste0("italic(R) ^ 2 == ",signif(tmp1$adj.r.squared,3)), parse = TRUE)+
  xlab("") + ylab("Age of Cocoa [years]")

tmp2<-summary(lm(Tot.P~distance.cont,data=dF.14))
g2<-ggplot(dF.14,aes(distance.cont,Tot.P)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=12,label = paste0("italic(R) ^ 2 == ",signif(tmp2$adj.r.squared,3)), parse = TRUE)+
  xlab("") + ylab("Available Phosphorus [ppm]")

tmp3<-summary(lm(CN.ratio~distance.cont,data=dF.14))
g3<-ggplot(dF.14,aes(distance.cont,CN.ratio)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=16,label = paste0("italic(R) ^ 2 == ",signif(tmp3$adj.r.squared,3)), parse = TRUE)+
  xlab("Distance from Forest [m]") + ylab("Soil C:N")

tmp4<-summary(lm(K.meq~distance.cont,data=dF.14))
g4<-ggplot(dF.14,aes(distance.cont,K.meq)) + geom_point() + stat_smooth(method="lm") +
  theme_classic()+annotate("text",x=500,y=3,label = paste0("italic(R) ^ 2 == ",signif(tmp4$adj.r.squared,3)), parse = TRUE)+
  xlab("Distance from Forest [m]") + ylab("Potassium [meq]")

ggpubr::ggarrange(g1,g2,g3,g4,ncol=2,nrow=2,labels="auto")
ggsave(paste0(getwd(),"/Analysis/ES/DistanceFromForestPredictors.pdf"),width=9,height = 8)
