library(plyr)
library(ggplot2)
library(reshape)

dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.",season,".csv"))

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
dF.4[,2:4]<-cbind(dF.3$o.final.income,dF.3$o.lbc.loss,dF.3$o.sharerent.loss)
colnames(dF.4)<-c("plot","Cocoa.income","LBC.loss","Share.Rent")

dF.4<-melt(dF.4,id.vars="plot")
dF.4$variable<-factor(dF.4$variable,labels=c("Cocoa Income\nLess Rent","LBC Loss","Shareholder/Rent"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Gross Cocoa Income [cedis/ha]")+
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
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome_estimated_",season,".median.pdf"))

#calculate total income farmer gets (without considering outlays) [multiply by land area]
dF.3$o.est.income<-dF.3$o.final.income*dF.3$land.area
#calculate total income farmer gets (considering outlays) [multiply by land area]
dF.3$o.estp.income<-dF.3$o.est.income-(dF.3$Labour.ha-dF.3$Input.ha)*dF.3$land.area

#then divide by land area to get per ha
dF.3$o.finalp.income<-dF.3$o.estp.income/dF.3$land.area

#bar plot of current income minus cultivation costs
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:7]<-cbind(dF.3$o.finalp.income,dF.3$Fertiliser.Compost.ha,dF.3$Herb.Fung.ha,dF.3$Pest.ha,dF.3$Weeding.ha,dF.3$Harvest.ha)
colnames(dF.4)<-c("plot","Cocoa.income","Fert.cost","Herb.Fung.cost","Pest.cost","Weeding.cost","Harvesting.cost")

dF.4<-melt(dF.4,id.vars="plot")
dF.4$variable<-factor(dF.4$variable,labels=c("Estimated Cocoa Income\n(less inputs)","Fertiliser/Compost Cost","Herbicide/Fungicide Cost","Pesticide Cost","Weeding Labour Cost","Harvesting Labour Cost"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Estimated Cocoa Income [cedis/ha]")+
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
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome.ha_estimated_",season,".median.pdf"))

#bar plot of current income minus cultivation costs
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:7]<-cbind(dF.3$o.estp.income,dF.3$Fertiliser.Compost.ha*dF.3$land.area,dF.3$Herb.Fung.ha*dF.3$land.area,dF.3$Pest.ha*dF.3$land.area,dF.3$Weeding.ha*dF.3$land.area,dF.3$Harvest.ha*dF.3$land.area)
colnames(dF.4)<-c("plot","Cocoa.income","Fert.cost","Herb.Fung.cost","Pest.cost","Weeding.cost","Harvesting.cost")

dF.4<-melt(dF.4,id.vars="plot")
dF.4$variable<-factor(dF.4$variable,labels=c("Estimated Cocoa Income","Fertiliser/Compost Cost","Herbicide/Fungicide Cost","Pesticide Cost","Weeding Labour Cost","Harvesting Labour Cost"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Total Estimated Cocoa Income")+
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
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome.tot_estimated_",season,".median.pdf"))

#calculate income estimate from measured yield
#calculate total harvest (yield/ha*land area)
#dF.3$i.totharvest<-dF.3$y.pot.ha*dF.3$land.area

#calculate potential income from per ha increase * cocoa price
dF.3$i.pot.income<-dF.3$y.pot.ha/kg*bag_cedi
#calculate income after LBC theft
dF.3$i.lbc.loss<-dF.3$i.pot.income*(dF.3$LBCs.loss/100)
#calculate income to sharehold and/or rent
dF.3$i.sharerent.loss<-(dF.3$i.pot.income-dF.3$i.lbc.loss)*(dF.3$shold)+dF.3$rent.ha
#calculate final income per ha
dF.3$i.final.income<-dF.3$i.pot.income-dF.3$i.lbc.loss-dF.3$i.sharerent.loss

#calculate percent income increase based on original income
#dF.3$i.prop.nincome<-dF.3$i.final.income/dF.3$o.final.income

#bar plot of current income ordered by potential yield increase
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:4]<-cbind(dF.3$i.final.income,dF.3$i.lbc.loss,dF.3$i.sharerent.loss)
colnames(dF.4)<-c("plot","Cocoa.income","LBC.loss","Share.Rent")

dF.4<-melt(dF.4,id.vars="plot")
dF.4$variable<-factor(dF.4$variable,labels=c("Actual Cocoa Income","LBC Loss","Shareholder/Rent"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Potential Increased Cocoa Income [cedis/ha]")+
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
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome_potential_increase",season,".median.pdf"))

write.csv(dF.3,paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))
