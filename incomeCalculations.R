library(plyr)
library(ggplot2)
library(reshape)
library(gridExtra)

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
year="2014"
season="1415"

dF.3<-read.csv(paste0(getwd(),"/Analysis/ES/Income.calculation.inputs.",season,".csv"))

#cocoa price per 65 kg bag
kg<-65
bag_cedi<-350

#calculate income estimate per ha from measured yield
#calculate total harvest (yield/ha*land area)
#dF.3$o.totharvest<-dF.3$o.yield.ha*dF.3$land.area

#calculate estimated income from total harvest * cocoa price
dF.3$o.totincome.ha<-dF.3$o.yield.ha/kg*bag_cedi

#calculate income after LBC theft
dF.3$o.lbc.loss<-dF.3$o.totincome.ha*(dF.3$LBCs.loss/100)

#calculate income to sharehold and/or rent
dF.3$o.sharerent.loss<-(dF.3$o.totincome.ha-dF.3$o.lbc.loss)*(dF.3$shold)+dF.3$rent.ha

#calculate gross farm margin (per ha)
dF.3$o.gross.margin<-dF.3$o.totincome.ha-dF.3$Input.ha
#dF.3$o.final.income<-dF.3$o.pot.totincome-dF.3$o.lbc.loss-dF.3$o.sharerent.loss
#dF.3[dF.3$o.gross.margin<0,"o.gross.margin"]<-dF.3[dF.3$o.gross.margin<0,"o.pot.totincome.ha"]

#calculate net margin
dF.3$o.net.margin<-dF.3$o.gross.margin-dF.3$o.lbc.loss-dF.3$o.sharerent.loss-dF.3$Labour.ha

#bar plot of current income ordered by potential yield increase
#dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
#dF.4[,2:6]<-cbind(dF.3$o.gross.margin,dF.3$o.pot.totincome,dF.3$o.lbc.loss,dF.3$Labour.ha,dF.3$Input.ha)
#colnames(dF.4)<-c("plot","Gross.Farm.Margin","Cocoa.income","LBC.loss","Labour","Inputs")

#dF.4<-melt(dF.4,id.vars=c("plot","Gross.Farm.Margin"))
#dF.4$variable<-factor(dF.4$variable,labels=c("Cocoa Income","LBC Loss","Labour","Inputs"))
#dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

#g1<-ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(data=dF.4[dF.4$variable=="Cocoa Income",],stat="identity")+geom_bar(data=dF.4[dF.4$variable!="Cocoa Income",],stat="identity",aes(plot,-value))+ylab("Income and Costs [cedis/ha]")+
#xlab("")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
#,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.title=element_blank()
#,legend.position="top")
#g2<-ggplot(dF.4[dF.4$variable=="Cocoa Income",],aes(plot,Gross.Farm.Margin))+geom_bar(stat="identity")+ylab("Gross Farm Margin [cedis/ha]")+
#xlab("Farm")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
  #,axis.text.x=element_blank()
#,axis.text.x=element_text(angle = 45,hjust=1))
#g3<-grid.arrange(g1,g2,ncol=1)
#ggsave(paste0(getwd(),"/Analysis/ES/CocoaGrossMargin_estimated_",season,".median.pdf"),g3,height=8, width=12)

#bar plot of current income minus cultivation costs
#dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
#dF.4[,2:7]<-cbind(dF.3$o.finalp.income,dF.3$Fertiliser.Compost.ha,dF.3$Herb.Fung.ha,dF.3$Pest.ha,dF.3$Weeding.ha,dF.3$Harvest.ha)
#colnames(dF.4)<-c("plot","Cocoa.income","Fert.cost","Herb.Fung.cost","Pest.cost","Weeding.cost","Harvesting.cost")

#dF.4<-melt(dF.4,id.vars="plot")
#dF.4$variable<-factor(dF.4$variable,labels=c("Estimated Cocoa Income\n(less inputs)","Fertiliser/Compost Cost","Herbicide/Fungicide Cost","Pesticide Cost","Weeding Labour Cost","Harvesting Labour Cost"))
#dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

#bar plot of gross farm margin minus rent costs
#dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
#dF.4[,2:3]<-cbind(dF.3$o.gross.margin,dF.3$o.sharerent.loss*(-1))
#colnames(dF.4)<-c("plot","Gross.Farm","Rent.Sharehold")

#dF.4<-melt(dF.4,id.vars="plot")
#dF.4$variable<-factor(dF.4$variable,labels=c("Gross Farm Margin","Rent/Sharehold"))
#dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

#ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Cocoa Income [cedis]")+
#xlab("Farm")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.title=element_blank()
#,legend.position="top")
#ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome.less.rent_",season,".median.pdf"))

#bar plot of current income minus cultivation costs
#dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
#dF.4[,2:7]<-cbind(dF.3$o.estp.income,dF.3$Fertiliser.Compost.ha*dF.3$land.area,dF.3$Herb.Fung.ha*dF.3$land.area,dF.3$Pest.ha*dF.3$land.area,dF.3$Weeding.ha*dF.3$land.area,dF.3$Harvest.ha*dF.3$land.area)
#colnames(dF.4)<-c("plot","Cocoa.income","Fert.cost","Herb.Fung.cost","Pest.cost","Weeding.cost","Harvesting.cost")

#dF.4<-melt(dF.4,id.vars="plot")
#dF.4$variable<-factor(dF.4$variable,labels=c("Estimated Cocoa Income","Fertiliser/Compost Cost","Herbicide/Fungicide Cost","Pesticide Cost","Weeding Labour Cost","Harvesting Labour Cost"))
#dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$yield.pot,decreasing=T),"plot"],sep=",")))

#ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Total Estimated Cocoa Income")+
#xlab("Farm")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
#,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.title=element_blank()
#,legend.position="top")
#ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome.tot_estimated_",season,".median.pdf"))

#calculate income estimate from measured yield
#calculate total harvest (yield/ha*land area)
#dF.3$i.totharvest<-dF.3$y.pot.ha*dF.3$land.area

lm_eqn <- function(df){
  m <- lm(y ~ x, df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))                 
}

#calculate relationship between input costs and yield
df<-data.frame(cbind(dF.3$o.yield.ha,dF.3$Input.ha))
colnames(df)<-c("x","y")
g1<-ggplot(dF.3,aes(o.yield.ha,Input.ha))+geom_point()+stat_smooth(method="lm")+
  geom_text(x=250,y=1750,label=lm_eqn(df), parse = TRUE)+xlab("Cocoa Yield [kg/ha]")+
  ylab("Input Costs [cedis/ha]")+theme(
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

df<-data.frame(cbind(dF.3$o.yield.ha,dF.3$Labour.ha))
colnames(df)<-c("x","y")
g2<-ggplot(dF.3,aes(o.yield.ha,Labour.ha))+geom_point()+stat_smooth(method="lm")+
  geom_text(x=250,y=900,label=lm_eqn(df), parse = TRUE)+xlab("Cocoa Yield [kg/ha]")+
  ylab("Labour Costs [cedis/ha]")+theme(
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
g3<-grid.arrange(g1,g2,ncol=2)
ggsave(paste0(getwd(),"/Analysis/ES/Inputcosts.v.yield.",season,".median.pdf"),g3,height=5,width=10)

m<-lm(data=dF.3,Input.ha~o.yield.ha)

#calculate potential income from new yield * cocoa price
dF.3$i.pot.income<-dF.3$y.pot.ha/kg*bag_cedi+dF.3$o.totincome.ha

#calculate income after LBC theft
dF.3$i.lbc.loss<-dF.3$i.pot.income*(dF.3$LBCs.loss/100)
#calculate income lost to sharehold and/or rent
dF.3$i.sharerent.loss<-(dF.3$i.pot.income-dF.3$i.lbc.loss)*(dF.3$shold)+dF.3$rent.ha
#calculate new input costs
dF.3$i.Input.ha<-dF.3$y.pot.ha*coef(m)[2]+coef(m)[1]

#calculate new farm gross margin
dF.3$i.gross.margin<-dF.3$i.pot.income-dF.3$i.Input.ha
#calculate new farm net margin
dF.3$i.net.margin<-dF.3$i.gross.margin-dF.3$i.lbc.loss-dF.3$i.sharerent.loss-dF.3$Labour.ha

#calculate previous total cocoa income
dF.3$o.total.income<-dF.3$o.totincome.ha*dF.3$land.area

#calculate potential cocoa income
dF.3$i.total.income<-dF.3$i.pot.income*dF.3$land.area

#calculate percent income increase based on original income
dF.3$i.prop.income<-dF.3$i.total.income/dF.3$o.total.income

#plot net margin vs cocoa income for current and potential incomes
dF.4<-data.frame(dF.3$o.total.income,stringsAsFactors = F)
dF.4[(nrow(dF.4)+1):(nrow(dF.4)+nrow(dF.3)),1]<-dF.3$i.total.income
dF.4[1:nrow(dF.3),2]<-dF.3$o.net.margin
dF.4[(nrow(dF.3)+1):(nrow(dF.3)*2),2]<-dF.3$i.net.margin

colnames(dF.4)<-c("cocoa.income","net.margin")
dF.4$calc<-"current"
dF.4[37:72,"calc"]<-"potential"

ggplot(dF.4,aes(cocoa.income,net.margin))+geom_point(aes(color=calc))+stat_smooth(method="lm",formula=y~log(x))+
  xlab("Cocoa Income [cedis]")+ylab("Net Margin [cedis/ha]")+geom_hline(yintercept=0,linetype="dashed")+theme(
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
  ,legend.position="top"
  ,legend.key = element_rect(colour = "white", fill = NA))
ggsave(paste0(getwd(),"/Analysis/ES/Netmargin.v.cocoa.income.",season,".pdf"),height=6,width=8)

#calculate income increase from survey values
dF.pov<-data.frame(read.csv(paste0(getwd(),"/HouseholdData/PovertyMeasures.csv")),stringsAsFactors = F)
dF.3$Survey.income<-dF.pov[match(dF.3$plot,dF.pov$PLOTCODE),"Cocoa.Income"]
dF.3$Survey.quart<-dF.pov[match(dF.3$plot,dF.pov$PLOTCODE),"Cocoa.income.quart"]

#calculate values of income quartiles from survey
quart<-ddply(dF.pov,.(Cocoa.income.quart),summarise,income=max(Cocoa.Income,na.rm=T))

#calculate increase in income from proportions
dF.3$i.Survey.income<-dF.3$Survey.income*dF.3$i.prop.income

#bar plot of increase in survey income
dF.4<-data.frame(dF.3$plot,stringsAsFactors = F)
dF.4[,2:3]<-cbind(dF.3$Survey.income/dF.3$land.area,(dF.3$i.Survey.income-dF.3$Survey.income)/dF.3$land.area)
colnames(dF.4)<-c("plot","Cocoa.income","add.Cocoa.income")

dF.4<-melt(dF.4,id.vars="plot")
dF.4$variable<-factor(dF.4$variable,labels=c("Reported Cocoa Income","Potential Income Increase"))
dF.4$plot<-ordered(dF.4$plot,levels=paste(as.character(dF.3[order(dF.3$Survey.income/dF.3$land.area,decreasing=F),"plot"],sep=",")))

ggplot(dF.4,aes(plot,value,fill=variable))+geom_bar(stat="identity")+ylab("Potential Cocoa Income [cedis/ha]")+
  xlab("Farm")+geom_vline(xintercept=6.5,linetype="dashed")+geom_vline(xintercept=14.5,linetype="dashed")+
  geom_vline(xintercept=24.5,linetype="dashed")+theme(
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
ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome_potential_increase",season,".survey.pdf"))

#compare current and potential net margin
ggplot(dF.3,aes(o.net.margin,i.net.margin))+geom_point()+ylab("Potential Net Margin [cedis/ha]")+
  xlab("Current Net Margin [cedis/ha]")+geom_vline(xintercept=0,linetype="dashed")+geom_hline(yintercept=0,linetype="dashed")+
  theme(
    plot.background = element_blank()
    ,panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black')
    ,axis.line.y = element_line(color = 'black')
    ,text = element_text(size=20)
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
    ,legend.title=element_blank()
    ,legend.position="top")
ggsave(paste0(getwd(),"/Analysis/ES/NetMargin_potential_increase",season,".pdf"))

#plot land area, LBC loss and shareholder/rent agreement relative to income increase
#g1<-ggplot(dF.3,aes(land.area,log(i.Survey.income)))+geom_point()+stat_smooth(method="lm")+
#ylab("Log of Potential Cocoa Income [cedis]")+xlab("Land Area (ha)")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.title=element_blank()
#,legend.position="top")
#g2<-ggplot(dF.3,aes(LBCs.loss,log(i.Survey.income)))+geom_point()+stat_smooth(method="lm")+
#ylab("Log of Potential Cocoa Income [cedis]")+xlab("LBC loss [%]")+theme(
#plot.background = element_blank()
#,panel.background = element_blank()
#,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#,axis.line.x = element_line(color = 'black')
#,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
#,legend.title=element_blank()
#,legend.position="top")
#g3<-ggplot(dF.3,aes(rent.ha,log(i.Survey.income)))+geom_point()+stat_smooth(method="lm")+
# ylab("Log of Potential Cocoa Income [cedis]")+xlab("Rent [cedis/ha]")+theme(
#plot.background = element_blank()
# ,panel.background = element_blank()
# ,panel.grid.major = element_blank()
#,panel.grid.minor = element_blank()
#,panel.border = element_blank()
#   ,axis.line.x = element_line(color = 'black')
#    ,axis.line.y = element_line(color = 'black')
    #,axis.text.x=element_blank()
    #,axis.text.x=element_text(angle = 45,hjust=1)
#    ,legend.title=element_blank()
#    ,legend.position="top")
#g4<-grid.arrange(g1,g2,g3,ncol=3)
#ggsave(paste0(getwd(),"/Analysis/ES/CocoaIncome_potential_increase.vs.outlays.",season,".survey.pdf"),g4,width=15,height=5)

write.csv(dF.3,paste0(getwd(),"/Analysis/ES/Income.calculations.",season,".csv"))
