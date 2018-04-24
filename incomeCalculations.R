library(tidyverse)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.",season,".csv"))
#remove duplicates
dF.3 <- dF.3 %>% distinct(plot,.keep_all=T)


#cocoa price per 65 kg bag
kg<-65
bag_cedi<-350

#calculate income estimate per ha from measured yield

#calculate total harvest (yield/ha*land area)
#dF.3$o.totharvest<-dF.3$o.yield.ha*dF.3$land.area

#calculate estimated income from total harvest * cocoa price
dF.3$o.pot.totincome.ha<-dF.3$o.yield.ha/kg*bag_cedi
#calculate income after LBC theft
dF.3$o.lbc.loss<-dF.3$o.pot.totincome.ha*(dF.3$LBCs.loss/100)

#calculate income to sharehold and/or rent
dF.3$o.sharerent.loss<-(dF.3$o.pot.totincome.ha-dF.3$o.lbc.loss)*(dF.3$shold)+dF.3$rent.ha

#calculate final income (per ha) before removing labour and inputs
dF.3$o.final.income<-dF.3$o.pot.totincome-dF.3$o.lbc.loss-dF.3$o.sharerent.loss

#bar plot of current income ordered by potential yield increase
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:4]<-cbind(dF.3$o.final.income,-1*dF.3$o.lbc.loss,-1*dF.3$o.sharerent.loss)
colnames(dF.4)<-c("plot","Cocoa.income","LBC.loss","Share.Rent")

dF.4<-dF.4 %>% gather(key="variable",value="value",-plot)
dF.4$variable<-factor(dF.4$variable,labels=c("Cocoa Income\nLess Rent","LBC Loss","Shareholder/Rent"))
dF.4$plot<-ordered(as.character(dF.4$plot),levels=paste(as.character(dF.3[order(dF.3$o.final.income,decreasing=T),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Gross Cocoa Income [cedis/ha]")+
  xlab("Farm")+theme_classic()+theme(axis.text.x=element_text(angle = 45,hjust=1)
                                      ,legend.title=element_blank()
                                      ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome_estimated_",season,".median.pdf"))

#calculate total income farmer gets (without considering outlays) [multiply by land area]
dF.3$o.revenue<-dF.3$o.final.income*dF.3$land.area
#calculate total income farmer gets (considering outlays) [multiply by land area]
dF.3$o.total.margin<-dF.3$o.revenue-(dF.3$Labour.ha-dF.3$Input.ha)*dF.3$land.area

#then divide by land area to get per ha
dF.3$o.net.margin<-dF.3$o.total.margin/dF.3$land.area

#bar plot of current income minus cultivation costs
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:7]<-cbind(dF.3$o.net.margin,-1*dF.3$Fertiliser.Compost.ha,-1*dF.3$Herb.Fung.ha,-1*dF.3$Pest.ha,-1*dF.3$Weeding.ha,-1*dF.3$Harvest.ha)
colnames(dF.4)<-c("plot","Cocoa.income","Fert.cost","Herb.Fung.cost","Pest.cost","Weeding.cost","Harvesting.cost")

dF.4<-dF.4 %>% gather(key="variable",value="value",-plot)
dF.4$variable<-factor(dF.4$variable,labels=c("Estimated Cocoa Income\n(less inputs)","Fertiliser/Compost Cost","Herbicide/Fungicide Cost","Pesticide Cost","Weeding Labour Cost","Harvesting Labour Cost"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$o.final.income,decreasing=T),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Estimated Cocoa Income [cedis/ha]")+
  xlab("Farm")+theme_classic()+theme(axis.text.x=element_text(angle = 45,hjust=1)
                                     ,legend.title=element_blank()
                                     ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome.ha_estimated_",season,".median.pdf"))

#bar plot of current income minus cultivation costs
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:7]<-cbind(dF.3$o.total.margin,-1*dF.3$Fertiliser.Compost.ha*dF.3$land.area,-1*dF.3$Herb.Fung.ha*dF.3$land.area,-1*dF.3$Pest.ha*dF.3$land.area,-1*dF.3$Weeding.ha*dF.3$land.area,-1*dF.3$Harvest.ha*dF.3$land.area)
colnames(dF.4)<-c("plot","Cocoa.income","Fert.cost","Herb.Fung.cost","Pest.cost","Weeding.cost","Harvesting.cost")

dF.4<-dF.4 %>% gather(key="variable",value="value",-plot)
dF.4$variable<-factor(dF.4$variable,labels=c("Estimated Cocoa Income","Fertiliser/Compost Cost","Herbicide/Fungicide Cost","Pesticide Cost","Weeding Labour Cost","Harvesting Labour Cost"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$o.final.income,decreasing=T),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Total Estimated Cocoa Income")+
  xlab("Farm")+theme_classic()+theme(axis.text.x=element_text(angle = 45,hjust=1)
                                     ,legend.title=element_blank()
                                     ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome.tot_estimated_",season,".median.pdf"))

#calculate increase in input costs with greater yields
i.mod<-lm(Input.ha~o.yield.ha,data=dF.3)
input<-coefficients(i.mod)
r.adj <- summary(i.mod)$adj.r.squared

g1<-ggplot(dF.3,aes(o.yield.ha,Input.ha)) + geom_point() +stat_smooth(method="lm") + theme_classic()+
  xlab("Yield [kg/ha]") + ylab("Input costs [cedis/ha]") + geom_text(aes(100,2000,label=paste0("R2 = ",signif(r.adj,2))))
g2<-ggplot(dF.3,aes(o.yield.ha,Labour.ha)) + geom_point() +stat_smooth(method="lm") + theme_classic()+
  xlab("Yield [kg/ha]") + ylab("Labour costs [cedis/ha]")
g3<-grid.arrange(g1,g2,ncol=2)
ggsave("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/Supp.Fig_labour.input.costs.pdf",g3,height=5,width=7)

#calculate new input costs with increased yields from fertiliser and capsids
dF.3<-dF.3 %>% group_by(plot) %>% mutate(y.tot.fert.ha=sum(y.pot.fert.ha,o.yield.ha),y.tot.cpb.ha=sum(y.pot.cpb.ha,o.yield.ha)) %>% 
  mutate(i.pot.input.fert=y.tot.fert.ha*input[2]+input[1]-Input.ha,i.pot.input.cpb=y.tot.cpb.ha*input[2]+input[1]-Input.ha) %>%
  mutate(y.pot.bmass.ha=replace(y.pot.bmass.ha,y.pot.bmass.ha<0,0),i.pot.input.fert=replace(i.pot.input.fert,i.pot.input.fert<0,0),
         i.pot.input.cpb=replace(i.pot.input.cpb,i.pot.input.cpb<0,0))

#calculate potential total income from per ha increase * cocoa price, fertiliser
dF.3$i.pot.income.fert<-dF.3$y.tot.fert.ha/kg*bag_cedi*dF.3$land.area
#calculate income after LBC theft
dF.3$i.lbc.fert<-dF.3$i.pot.income.fert*(dF.3$LBCs.loss/100)
#subtract labour and orig input costs again (just difference with enhanced yields)
dF.3$i.pot.net.margin.fert<-(dF.3$o.total.margin+dF.3$i.pot.income.fert-dF.3$i.lbc.fert-(dF.3$i.pot.income.fert-dF.3$i.lbc.fert)*(dF.3$shold)-dF.3$i.pot.input.fert*dF.3$land.area)/dF.3$land.area

#calculate potential income from per ha increase * cocoa price, biomass
dF.3$i.pot.income.bmass<-dF.3$y.pot.bmass.ha/kg*bag_cedi*dF.3$land.area
#calculate income after LBC theft
dF.3$i.lbc.bmass<-dF.3$i.pot.income.bmass*(dF.3$LBCs.loss/100)
#subtract labour and original input costs and new lbc and sharehold
dF.3$i.pot.net.margin.bmass<-(dF.3$o.total.margin+dF.3$i.pot.income.bmass-dF.3$i.lbc.bmass-(dF.3$i.pot.income.bmass-dF.3$i.lbc.bmass)*(dF.3$shold))/dF.3$land.area

#calculate potential income from per ha increase * cocoa price, canopy gap
dF.3$i.pot.income.cgap<-dF.3$y.pot.cgap.ha/kg*bag_cedi*dF.3$land.area
#calculate income after LBC theft
dF.3$i.lbc.cgap<-dF.3$i.pot.income.cgap*(dF.3$LBCs.loss/100)
#do not need to subtract labour and original input costs again, just lbc and sharehold
dF.3$i.pot.net.margin.cgap<-(dF.3$o.total.margin+dF.3$i.pot.income.cgap-dF.3$i.lbc.cgap-(dF.3$i.pot.income.cgap-dF.3$i.lbc.bmass)*(dF.3$shold))/dF.3$land.area

#calculate potential income from per ha increase * cocoa price, capsids
dF.3$i.pot.income.cpb<-dF.3$y.tot.cpb.ha/kg*bag_cedi*dF.3$land.area
#calculate income after LBC theft
dF.3$i.lbc.cpb<-dF.3$i.pot.income.cpb*(dF.3$LBCs.loss/100)
#do not need to subtract labour and orig input costs again (just difference with enhanced yields)
dF.3$i.pot.net.margin.cpb<-(dF.3$o.total.margin+dF.3$i.pot.income.cpb-dF.3$i.lbc.cpb-(dF.3$i.pot.income.cpb-dF.3$i.lbc.cpb)*(dF.3$shold)-dF.3$i.pot.input.cpb)/dF.3$land.area

#calculate percent net margin increase based on original income, replace negative values with 0s
dF.3 <- dF.3 %>% group_by(plot) %>% mutate(i.prop.bmass=i.pot.net.margin.bmass/o.net.margin,i.prop.fert=i.pot.net.margin.fert/o.net.margin,
                                           i.prop.cgap=i.pot.net.margin.cgap/o.net.margin,i.prop.cpb=i.pot.net.margin.cpb/o.net.margin) %>%
  mutate(i.prop.bmass=replace(i.prop.bmass,i.prop.bmass<0,0),i.prop.fert=replace(i.prop.fert,i.prop.fert<0,0),i.prop.cgap=replace(i.prop.cgap,i.prop.cgap<0,0),
         i.prop.cpb=replace(i.prop.cpb,i.prop.cpb<0,0))

#create figure of 4 options (biomass, fertiliser, canopy gap and capsids)
g1<-ggplot(dF.3,aes(o.net.margin,i.pot.net.margin.bmass)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [cedis/ha]") + ylab("Potential Net Margin [cedis/ha]")+
  ggtitle("Distance From Biomass") + xlim(-500,6500) + ylim(-500,6500)

g2<-ggplot(dF.3,aes(o.net.margin,i.pot.net.margin.fert)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [cedis/ha]") + ylab("Potential Net Margin [cedis/ha]")+
  ggtitle("Fertiliser Application") + xlim(-500,11000) + ylim(-500,11000)

g3<-ggplot(dF.3,aes(o.net.margin,i.pot.net.margin.cgap)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [cedis/ha]") + ylab("Potential Net Margin [cedis/ha]")+
  ggtitle("Canopy Gap") + xlim(-500,6500) + ylim(-500,6500)

g4<-ggplot(dF.3,aes(o.net.margin,i.pot.net.margin.cpb)) + geom_point() + theme_classic()+
  geom_abline(intercept = 0,slope=1,linetype="dashed") + xlab("Original Net Margin [cedis/ha]") + ylab("Potential Net Margin [cedis/ha]")+
  ggtitle("Capsid Management") + xlim(-500,10000) + ylim(-500,10000)

g5<-grid.arrange(g1,g3,g2,g4,ncol=2)
ggsave(paste0("/users/alex/Documents/Research/Africa/ECOLIMITS/Pubs/EcosystemServices/CocoaIncome_potential_increase",season,".byparameter.pdf"),g5,height=8,width=9)

write.csv(dF.3,paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))