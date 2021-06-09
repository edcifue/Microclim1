files<-list.files("./Lolly/data",pattern = "data.*_1",full.names = T)
basename(files)
basen<-substr(basename(files),6,13)
listdata<-lapply(files,read.csv2,h=F)
str(data.frame(listdata[[1]],sensor=basen[1]))
for(i in 1:length(listdata)) listdata[[i]]<-data.frame(listdata[[i]],sensor=basen[i])
datum<-do.call(rbind,listdata)
colnames(datum)<-c("ID","Date","TimeZone","Temp1","Temp2","Temp3","SoilMoisture","Shake","ErrorFlag","Unknown","Sensor")
str(datum)
datum$ID<-as.integer(factor(datum$Date))
length(unique(datum$ID))

### Comparar temp1,2 y 3 del mismo datalogger----
condi<-datum$ID%in%(46025:47500)
unique(datum$Date[condi])
rango<-range(datum[condi,c("Temp1","Temp2","Temp3")])

table(substr(datum$Date[datum$ID%in%(62+0:1000*96)],12,16))
table(substr(datum$Date[datum$ID%in%(14+0:1000*96)],12,16))

for(i in unique(datum$Sensor)) {
  with(datum[condi&datum$Sensor==i,],plot(1,col=NA,ylim=rango,xlim=range(ID),main=i,xlab="ID",ylab="Temperature"))
  with(datum[condi&datum$Sensor==i,],lines(ID,Temp1,col="blue"))
  with(datum[condi&datum$Sensor==i,],lines(ID,Temp2,col="green"))
  with(datum[condi&datum$Sensor==i,],lines(ID,Temp3,col="red"))
  legend("topright",legend=c("Soil","Mid","Top"),col=c("blue","green","red"),lty=1,bty="n")
  abline(v=62+0:1000*96,col="grey",lty=1)
  abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
  
}


##Comparar temp1 entre data loggers----
plot(datum[condi,c("ID","Temp1")],col=NA,xlim=c(46025,47700),ylab="Soil temperature")
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,Temp1,type="l",col=rainbow(n=9)[i]))
abline(v=62+0:1000*96,col="grey",lty=1)
abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c(unique(datum$Sensor)),col=c(rainbow(n=9)),lty=1,bty="n",cex=0.7)

plot(datum[condi,c("ID","Temp1")],col=NA,xlim=c(46025,47700),ylab="Soil temperature")
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,Temp1,type="l",col=c("red","pink","blue","green","green","yellow","yellow","pink","blue")[i]))
abline(v=62+0:1000*96,col="grey",lty=1)
abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c("Pasto","Palma","Colgado","Arbustos","Muro"),col=c("blue","green","red","pink","yellow"),lty=1,bty="n",cex=0.7)

dt<-datum[condi,c("ID","Temp1","Sensor")] 
##Pasto
plot(dt[dt$Sensor=="94202093",]$ID,dt[dt$Sensor=="94202093",]$Temp1,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202100",]$ID,dt[dt$Sensor=="94202100",]$Temp1,col="blue")
legend("topright",legend=c("94202093","94202100"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Muro
plot(dt[dt$Sensor=="94202096",]$ID,dt[dt$Sensor=="94202096",]$Temp1,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202098",]$ID,dt[dt$Sensor=="94202098",]$Temp1,col="blue")
legend("topright",legend=c("94202096","94202098"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Palma
plot(dt[dt$Sensor=="94202094",]$ID,dt[dt$Sensor=="94202094",]$Temp1,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202095",]$ID,dt[dt$Sensor=="94202095",]$Temp1,col="blue")
legend("topright",legend=c("94202094","94202095"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Arbustos
plot(dt[dt$Sensor=="94202092",]$ID,dt[dt$Sensor=="94202092",]$Temp1,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202099",]$ID,dt[dt$Sensor=="94202099",]$Temp1,col="blue")
legend("topright",legend=c("94202092","94202099"),col=c("red","blue"),lty=1,bty="n",cex=0.7)


##Comparar temp2 entre data loggers----
plot(datum[condi,c("ID","Temp2")],col=NA,xlim=c(46025,47700),ylab="Mid temperature")
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,Temp2,type="l",col=rainbow(n=9)[i]))
abline(v=62+0:1000*96,col="grey",lty=1)
abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c(unique(datum$Sensor)),col=c(rainbow(n=9)),lty=1,bty="n",cex=0.7)

plot(datum[condi,c("ID","Temp2")],col=NA,xlim=c(46025,47700),ylab="Mid temperature")
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,Temp2,type="l",col=c("red","pink","blue","green","green","yellow","yellow","pink","blue")[i]))
abline(v=62+0:1000*96,col="grey",lty=1)
abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c("Pasto","Palma","Colgado","Arbustos","Muro"),col=c("blue","green","red","pink","yellow"),lty=1,bty="n",cex=0.7)

dt<-datum[condi,c("ID","Temp2","Sensor")] 
##Pasto
plot(dt[dt$Sensor=="94202093",]$ID,dt[dt$Sensor=="94202093",]$Temp2,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202100",]$ID,dt[dt$Sensor=="94202100",]$Temp2,col="blue")
legend("topright",legend=c("94202093","94202100"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Muro
plot(dt[dt$Sensor=="94202096",]$ID,dt[dt$Sensor=="94202096",]$Temp2,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202098",]$ID,dt[dt$Sensor=="94202098",]$Temp2,col="blue")
legend("topright",legend=c("94202096","94202098"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Palma
plot(dt[dt$Sensor=="94202094",]$ID,dt[dt$Sensor=="94202094",]$Temp2,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202095",]$ID,dt[dt$Sensor=="94202095",]$Temp2,col="blue")
legend("topright",legend=c("94202094","94202095"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Arbustos
plot(dt[dt$Sensor=="94202092",]$ID,dt[dt$Sensor=="94202092",]$Temp2,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202099",]$ID,dt[dt$Sensor=="94202099",]$Temp2,col="blue")
legend("topright",legend=c("94202092","94202099"),col=c("red","blue"),lty=1,bty="n",cex=0.7)


##Comparar temp3 entre data loggers----
plot(datum[condi,c("ID","Temp3")],col=NA,xlim=c(46025,47700),ylab="Top temperature")
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,Temp3,type="l",col=rainbow(n=9)[i]))
abline(v=42+0:1000*96,col="grey",lty=1)
abline(v=-6+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c(unique(datum$Sensor)),col=c(rainbow(n=9)),lty=1,bty="n",cex=0.7)

plot(datum[condi,c("ID","Temp3")],col=NA,xlim=c(46025,47700),ylab="Top temperature")
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,Temp3,type="l",col=c("red","pink","blue","green","green","yellow","yellow","pink","blue")[i]))
abline(v=62+0:1000*96,col="grey",lty=1)
abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c("Pasto","Palma","Colgado","Arbustos","Muro"),col=c("blue","green","red","pink","yellow"),lty=1,bty="n",cex=0.7)

dt<-datum[condi,c("ID","Temp3","Sensor")] 
##Pasto
plot(dt[dt$Sensor=="94202093",]$ID,dt[dt$Sensor=="94202093",]$Temp3,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202100",]$ID,dt[dt$Sensor=="94202100",]$Temp3,col="blue")
legend("topright",legend=c("94202093","94202100"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Muro
plot(dt[dt$Sensor=="94202096",]$ID,dt[dt$Sensor=="94202096",]$Temp3,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202098",]$ID,dt[dt$Sensor=="94202098",]$Temp3,col="blue")
legend("topright",legend=c("94202096","94202098"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Palma
plot(dt[dt$Sensor=="94202094",]$ID,dt[dt$Sensor=="94202094",]$Temp3,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202095",]$ID,dt[dt$Sensor=="94202095",]$Temp3,col="blue")
legend("topright",legend=c("94202094","94202095"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Arbustos
plot(dt[dt$Sensor=="94202092",]$ID,dt[dt$Sensor=="94202092",]$Temp3,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202099",]$ID,dt[dt$Sensor=="94202099",]$Temp3,col="blue")
legend("topright",legend=c("94202092","94202099"),col=c("red","blue"),lty=1,bty="n",cex=0.7)



##Comparar soil moisture entre data loggers----
plot(datum[condi,c("ID","SoilMoisture")],col=NA)
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,SoilMoisture,type="l",col=rainbow(n=9)[i]))
abline(v=62+0:1000*96,col="grey",lty=1)
abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c(unique(datum$Sensor)),col=c(rainbow(n=9)),lty=1,bty="n",cex=0.7)

plot(datum[condi,c("ID","SoilMoisture")],col=NA,xlim=c(46025,47700),ylab="Soil moisture")
for(i in 1:length(unique(datum$Sensor))) with(datum[condi & datum$Sensor==unique(datum$Sensor)[i],],lines(ID,SoilMoisture,type="l",col=c("red","pink","blue","green","green","yellow","yellow","pink","blue")[i]))
abline(v=62+0:1000*96,col="grey",lty=1)
abline(v=14+0:1000*96,col="darkgoldenrod",lty=2)
legend("topright",legend=c("Pasto","Palma","Colgado","Arbustos","Muro"),col=c("blue","green","red","pink","yellow"),lty=1,bty="n",cex=0.7)

dt<-datum[condi,c("ID","SoilMoisture","Sensor")] 
##Pasto
plot(dt[dt$Sensor=="94202093",]$ID,dt[dt$Sensor=="94202093",]$SoilMoisture,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202100",]$ID,dt[dt$Sensor=="94202100",]$SoilMoisture,col="blue")
legend("topright",legend=c("94202093","94202100"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Muro
plot(dt[dt$Sensor=="94202096",]$ID,dt[dt$Sensor=="94202096",]$SoilMoisture,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202098",]$ID,dt[dt$Sensor=="94202098",]$SoilMoisture,col="blue")
legend("topright",legend=c("94202096","94202098"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Palma
plot(dt[dt$Sensor=="94202094",]$ID,dt[dt$Sensor=="94202094",]$SoilMoisture,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202095",]$ID,dt[dt$Sensor=="94202095",]$SoilMoisture,col="blue")
legend("topright",legend=c("94202094","94202095"),col=c("red","blue"),lty=1,bty="n",cex=0.7)
##Arbustos
plot(dt[dt$Sensor=="94202092",]$ID,dt[dt$Sensor=="94202092",]$SoilMoisture,type="l",col="red",ylab="Soil temperature",xlab="ID")
lines(dt[dt$Sensor=="94202099",]$ID,dt[dt$Sensor=="94202099",]$SoilMoisture,col="blue")
legend("topright",legend=c("94202092","94202099"),col=c("red","blue"),lty=1,bty="n",cex=0.7)



##Separar por fechas
library(stringr)
t1<-str_split_fixed(datum$Date,"[.]",3)
t2<-str_split_fixed(t1[,3]," ",2)
dat<-data.frame(datum,t1[,1:2],t2)
colnames(dat)<-c("ID","Date","TimeZone","Temp1","Temp2","Temp3","SoilMoisture","Shake","ErrorFlag","Unknown","Sensor","Year","Month","Day","Hour")
dat$Month<-as.numeric(dat$Month)
dat$Day<-as.numeric(dat$Day)
str(dat)

## dat<-dat[dat$Year=="2020",]
## dat<-dat[dat$Year=="2021",]

###TEMP 1
##Todos los datos ----
plot(dat$ID,dat$Temp1)
## variacion mensual----
boxplot(dat$Temp1~dat$Month)
## variacion diaria por mes----
boxplot(dat[dat$Month=="01",]$Temp1~dat[dat$Month=="01",]$Day)
boxplot(dat[dat$Month=="02",]$Temp1~dat[dat$Month=="02",]$Day)
boxplot(dat[dat$Month=="03",]$Temp1~dat[dat$Month=="03",]$Day)
boxplot(dat[dat$Month=="04",]$Temp1~dat[dat$Month=="04",]$Day)
boxplot(dat[dat$Month=="05",]$Temp1~dat[dat$Month=="05",]$Day)
boxplot(dat[dat$Month=="06",]$Temp1~dat[dat$Month=="06",]$Day)
boxplot(dat[dat$Month=="07",]$Temp1~dat[dat$Month=="07",]$Day)
boxplot(dat[dat$Month=="08",]$Temp1~dat[dat$Month=="08",]$Day)
boxplot(dat[dat$Month=="09",]$Temp1~dat[dat$Month=="09",]$Day)
boxplot(dat[dat$Month=="10",]$Temp1~dat[dat$Month=="10",]$Day)
boxplot(dat[dat$Month=="11",]$Temp1~dat[dat$Month=="11",]$Day)
boxplot(dat[dat$Month=="12",]$Temp1~dat[dat$Month=="12",]$Day)

##Temperatura promedio mensual----

tmm<-data.frame(sdMonth=tapply(dat$Temp1,dat$Month,sd),MeanMonth=tapply(dat$Temp1,dat$Month,mean),Month=row.names(tapply(dat$Temp1,dat$Month,mean)))
tmm$Month<-as.numeric(tmm$Month)
plot(tmm$Month,tmm$MeanMonth,type="l",ylim=range(c(tmm$MeanMonth-tmm$sdMonth,tmm$MeanMonth+tmm$sdMonth)),xlab="Month",ylab="Temperature 1")
arrows(tmm$Month,tmm$MeanMonth-tmm$sdMonth,tmm$Month,tmm$MeanMonth+tmm$sdMonth, code=3, angle=90, length=0.05)

##Temperatura promedio diaria---

tdm01<-data.frame(sdDay=tapply(dat[dat$Month=="1",]$Temp1,dat[dat$Month=="1",]$Day,sd),MeanDay=tapply(dat[dat$Month=="1",]$Temp1,dat[dat$Month=="1",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="1",]$Temp1,dat[dat$Month=="1",]$Day,mean)))
tdm01$Day<-as.numeric(tdm01$Day)
plot(tdm01$Day,tdm01$MeanDay,type="l",ylim=range(c(tdm01$MeanDay-tdm01$sdDay,tdm01$MeanDay+tdm01$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm01$Day,tdm01$MeanDay-tdm01$sdDay,tdm01$Day,tdm01$MeanDay+tdm01$sdDay, code=3, angle=90, length=0.05)

tdm02<-data.frame(sdDay=tapply(dat[dat$Month=="2",]$Temp1,dat[dat$Month=="2",]$Day,sd),MeanDay=tapply(dat[dat$Month=="2",]$Temp1,dat[dat$Month=="2",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="2",]$Temp1,dat[dat$Month=="2",]$Day,mean)))
tdm02$Day<-as.numeric(tdm02$Day)
plot(tdm02$Day,tdm02$MeanDay,type="l",ylim=range(c(tdm02$MeanDay-tdm02$sdDay,tdm02$MeanDay+tdm02$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm02$Day,tdm02$MeanDay-tdm02$sdDay,tdm02$Day,tdm02$MeanDay+tdm02$sdDay, code=3, angle=90, length=0.05)

tdm03<-data.frame(sdDay=tapply(dat[dat$Month=="3",]$Temp1,dat[dat$Month=="3",]$Day,sd),MeanDay=tapply(dat[dat$Month=="3",]$Temp1,dat[dat$Month=="3",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="3",]$Temp1,dat[dat$Month=="3",]$Day,mean)))
tdm03$Day<-as.numeric(tdm03$Day)
plot(tdm03$Day,tdm03$MeanDay,type="l",ylim=range(c(tdm03$MeanDay-tdm03$sdDay,tdm03$MeanDay+tdm03$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm03$Day,tdm03$MeanDay-tdm03$sdDay,tdm03$Day,tdm03$MeanDay+tdm03$sdDay, code=3, angle=90, length=0.05)

tdm04<-data.frame(sdDay=tapply(dat[dat$Month=="4",]$Temp1,dat[dat$Month=="4",]$Day,sd),MeanDay=tapply(dat[dat$Month=="4",]$Temp1,dat[dat$Month=="4",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="4",]$Temp1,dat[dat$Month=="4",]$Day,mean)))
tdm04$Day<-as.numeric(tdm04$Day)
plot(tdm04$Day,tdm04$MeanDay,type="l",ylim=range(c(tdm04$MeanDay-tdm04$sdDay,tdm04$MeanDay+tdm04$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm04$Day,tdm04$MeanDay-tdm04$sdDay,tdm04$Day,tdm04$MeanDay+tdm04$sdDay, code=3, angle=90, length=0.05)

tdm05<-data.frame(sdDay=tapply(dat[dat$Month=="5",]$Temp1,dat[dat$Month=="5",]$Day,sd),MeanDay=tapply(dat[dat$Month=="5",]$Temp1,dat[dat$Month=="5",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="5",]$Temp1,dat[dat$Month=="5",]$Day,mean)))
tdm05$Day<-as.numeric(tdm05$Day)
plot(tdm05$Day,tdm05$MeanDay,type="l",ylim=range(c(tdm05$MeanDay-tdm05$sdDay,tdm05$MeanDay+tdm05$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm05$Day,tdm05$MeanDay-tdm05$sdDay,tdm05$Day,tdm05$MeanDay+tdm05$sdDay, code=3, angle=90, length=0.05)

tdm06<-data.frame(sdDay=tapply(dat[dat$Month=="6",]$Temp1,dat[dat$Month=="6",]$Day,sd),MeanDay=tapply(dat[dat$Month=="6",]$Temp1,dat[dat$Month=="6",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="6",]$Temp1,dat[dat$Month=="6",]$Day,mean)))
tdm06$Day<-as.numeric(tdm06$Day)
plot(tdm06$Day,tdm06$MeanDay,type="l",ylim=range(c(tdm06$MeanDay-tdm06$sdDay,tdm06$MeanDay+tdm06$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm06$Day,tdm06$MeanDay-tdm06$sdDay,tdm06$Day,tdm06$MeanDay+tdm06$sdDay, code=3, angle=90, length=0.05)

tdm07<-data.frame(sdDay=tapply(dat[dat$Month=="7",]$Temp1,dat[dat$Month=="7",]$Day,sd),MeanDay=tapply(dat[dat$Month=="7",]$Temp1,dat[dat$Month=="7",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="7",]$Temp1,dat[dat$Month=="7",]$Day,mean)))
tdm07$Day<-as.numeric(tdm07$Day)
plot(tdm07$Day,tdm07$MeanDay,type="l",ylim=range(c(tdm07$MeanDay-tdm07$sdDay,tdm07$MeanDay+tdm07$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm07$Day,tdm07$MeanDay-tdm07$sdDay,tdm07$Day,tdm07$MeanDay+tdm07$sdDay, code=3, angle=90, length=0.05)

tdm08<-data.frame(sdDay=tapply(dat[dat$Month=="8",]$Temp1,dat[dat$Month=="8",]$Day,sd),MeanDay=tapply(dat[dat$Month=="8",]$Temp1,dat[dat$Month=="8",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="8",]$Temp1,dat[dat$Month=="8",]$Day,mean)))
tdm08$Day<-as.numeric(tdm08$Day)
plot(tdm08$Day,tdm08$MeanDay,type="l",ylim=range(c(tdm08$MeanDay-tdm08$sdDay,tdm08$MeanDay+tdm08$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm08$Day,tdm08$MeanDay-tdm08$sdDay,tdm08$Day,tdm08$MeanDay+tdm08$sdDay, code=3, angle=90, length=0.05)

tdm09<-data.frame(sdDay=tapply(dat[dat$Month=="9",]$Temp1,dat[dat$Month=="9",]$Day,sd),MeanDay=tapply(dat[dat$Month=="9",]$Temp1,dat[dat$Month=="9",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="9",]$Temp1,dat[dat$Month=="9",]$Day,mean)))
tdm09$Day<-as.numeric(tdm09$Day)
plot(tdm09$Day,tdm09$MeanDay,type="l",ylim=range(c(tdm09$MeanDay-tdm09$sdDay,tdm09$MeanDay+tdm09$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm09$Day,tdm09$MeanDay-tdm09$sdDay,tdm09$Day,tdm09$MeanDay+tdm09$sdDay, code=3, angle=90, length=0.05)

tdm10<-data.frame(sdDay=tapply(dat[dat$Month=="10",]$Temp1,dat[dat$Month=="10",]$Day,sd),MeanDay=tapply(dat[dat$Month=="10",]$Temp1,dat[dat$Month=="10",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="10",]$Temp1,dat[dat$Month=="10",]$Day,mean)))
tdm10$Day<-as.numeric(tdm10$Day)
plot(tdm10$Day,tdm10$MeanDay,type="l",ylim=range(c(tdm10$MeanDay-tdm10$sdDay,tdm10$MeanDay+tdm10$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm10$Day,tdm10$MeanDay-tdm10$sdDay,tdm10$Day,tdm10$MeanDay+tdm10$sdDay, code=3, angle=90, length=0.05)

tdm11<-data.frame(sdDay=tapply(dat[dat$Month=="11",]$Temp1,dat[dat$Month=="11",]$Day,sd),MeanDay=tapply(dat[dat$Month=="11",]$Temp1,dat[dat$Month=="11",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="11",]$Temp1,dat[dat$Month=="11",]$Day,mean)))
tdm11$Day<-as.numeric(tdm11$Day)
plot(tdm11$Day,tdm11$MeanDay,type="l",ylim=range(c(tdm11$MeanDay-tdm11$sdDay,tdm11$MeanDay+tdm11$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm11$Day,tdm11$MeanDay-tdm11$sdDay,tdm11$Day,tdm11$MeanDay+tdm11$sdDay, code=3, angle=90, length=0.05)

tdm12<-data.frame(sdDay=tapply(dat[dat$Month=="12",]$Temp1,dat[dat$Month=="12",]$Day,sd),MeanDay=tapply(dat[dat$Month=="12",]$Temp1,dat[dat$Month=="12",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="12",]$Temp1,dat[dat$Month=="12",]$Day,mean)))
tdm12$Day<-as.numeric(tdm12$Day)
plot(tdm12$Day,tdm12$MeanDay,type="l",ylim=range(c(tdm12$MeanDay-tdm12$sdDay,tdm12$MeanDay+tdm12$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm12$Day,tdm12$MeanDay-tdm12$sdDay,tdm12$Day,tdm12$MeanDay+tdm12$sdDay, code=3, angle=90, length=0.05)

###TEMP 2
##Todos los datos ----
with(dat[45000:nrow(dat),],plot(ID,Temp2,type="l"))
lines(dat$ID,dat$Temp1,type="l",col="red")
lines(dat$ID,dat$Temp3,type="l",col="blue")
## variacion mensual----
boxplot(dat$Temp2~dat$Month)
## variacion diaria por mes----
boxplot(dat[dat$Month=="06",]$Temp2~dat[dat$Month=="06",]$Day)
boxplot(dat[dat$Month=="02",]$Temp2~dat[dat$Month=="02",]$Day)
boxplot(dat[dat$Month=="03",]$Temp2~dat[dat$Month=="03",]$Day)
boxplot(dat[dat$Month=="04",]$Temp2~dat[dat$Month=="04",]$Day)
boxplot(dat[dat$Month=="05",]$Temp2~dat[dat$Month=="05",]$Day)
boxplot(dat[dat$Month=="06",]$Temp2~dat[dat$Month=="06",]$Day)
boxplot(dat[dat$Month=="07",]$Temp2~dat[dat$Month=="07",]$Day)
boxplot(dat[dat$Month=="08",]$Temp2~dat[dat$Month=="08",]$Day)
boxplot(dat[dat$Month=="09",]$Temp2~dat[dat$Month=="09",]$Day)
boxplot(dat[dat$Month=="10",]$Temp2~dat[dat$Month=="10",]$Day)
boxplot(dat[dat$Month=="11",]$Temp2~dat[dat$Month=="11",]$Day)
boxplot(dat[dat$Month=="12",]$Temp2~dat[dat$Month=="12",]$Day)

##Temperatura promedio mensual----

tmm<-data.frame(sdMonth=tapply(dat$Temp2,dat$Month,sd),MeanMonth=tapply(dat$Temp2,dat$Month,mean),Month=row.names(tapply(dat$Temp2,dat$Month,mean)))
tmm$Month<-as.numeric(tmm$Month)
plot(tmm$Month,tmm$MeanMonth,type="l",ylim=range(c(tmm$MeanMonth-tmm$sdMonth,tmm$MeanMonth+tmm$sdMonth)),xlab="Month",ylab="Temperature 1")
arrows(tmm$Month,tmm$MeanMonth-tmm$sdMonth,tmm$Month,tmm$MeanMonth+tmm$sdMonth, code=3, angle=90, length=0.05)

##Temperatura promedio diaria---

tdm01<-data.frame(sdDay=tapply(dat[dat$Month=="1",]$Temp2,dat[dat$Month=="1",]$Day,sd),MeanDay=tapply(dat[dat$Month=="1",]$Temp2,dat[dat$Month=="1",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="1",]$Temp2,dat[dat$Month=="1",]$Day,mean)))
tdm01$Day<-as.numeric(tdm01$Day)
plot(tdm01$Day,tdm01$MeanDay,type="l",ylim=range(c(tdm01$MeanDay-tdm01$sdDay,tdm01$MeanDay+tdm01$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm01$Day,tdm01$MeanDay-tdm01$sdDay,tdm01$Day,tdm01$MeanDay+tdm01$sdDay, code=3, angle=90, length=0.05)

tdm02<-data.frame(sdDay=tapply(dat[dat$Month=="2",]$Temp2,dat[dat$Month=="2",]$Day,sd),MeanDay=tapply(dat[dat$Month=="2",]$Temp2,dat[dat$Month=="2",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="2",]$Temp2,dat[dat$Month=="2",]$Day,mean)))
tdm02$Day<-as.numeric(tdm02$Day)
plot(tdm02$Day,tdm02$MeanDay,type="l",ylim=range(c(tdm02$MeanDay-tdm02$sdDay,tdm02$MeanDay+tdm02$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm02$Day,tdm02$MeanDay-tdm02$sdDay,tdm02$Day,tdm02$MeanDay+tdm02$sdDay, code=3, angle=90, length=0.05)

tdm03<-data.frame(sdDay=tapply(dat[dat$Month=="3",]$Temp2,dat[dat$Month=="3",]$Day,sd),MeanDay=tapply(dat[dat$Month=="3",]$Temp2,dat[dat$Month=="3",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="3",]$Temp2,dat[dat$Month=="3",]$Day,mean)))
tdm03$Day<-as.numeric(tdm03$Day)
plot(tdm03$Day,tdm03$MeanDay,type="l",ylim=range(c(tdm03$MeanDay-tdm03$sdDay,tdm03$MeanDay+tdm03$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm03$Day,tdm03$MeanDay-tdm03$sdDay,tdm03$Day,tdm03$MeanDay+tdm03$sdDay, code=3, angle=90, length=0.05)

tdm04<-data.frame(sdDay=tapply(dat[dat$Month=="4",]$Temp2,dat[dat$Month=="4",]$Day,sd),MeanDay=tapply(dat[dat$Month=="4",]$Temp2,dat[dat$Month=="4",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="4",]$Temp2,dat[dat$Month=="4",]$Day,mean)))
tdm04$Day<-as.numeric(tdm04$Day)
plot(tdm04$Day,tdm04$MeanDay,type="l",ylim=range(c(tdm04$MeanDay-tdm04$sdDay,tdm04$MeanDay+tdm04$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm04$Day,tdm04$MeanDay-tdm04$sdDay,tdm04$Day,tdm04$MeanDay+tdm04$sdDay, code=3, angle=90, length=0.05)

tdm05<-data.frame(sdDay=tapply(dat[dat$Month=="5",]$Temp2,dat[dat$Month=="5",]$Day,sd),MeanDay=tapply(dat[dat$Month=="5",]$Temp2,dat[dat$Month=="5",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="5",]$Temp2,dat[dat$Month=="5",]$Day,mean)))
tdm05$Day<-as.numeric(tdm05$Day)
plot(tdm05$Day,tdm05$MeanDay,type="l",ylim=range(c(tdm05$MeanDay-tdm05$sdDay,tdm05$MeanDay+tdm05$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm05$Day,tdm05$MeanDay-tdm05$sdDay,tdm05$Day,tdm05$MeanDay+tdm05$sdDay, code=3, angle=90, length=0.05)

tdm06<-data.frame(sdDay=tapply(dat[dat$Month=="6",]$Temp2,dat[dat$Month=="6",]$Day,sd),MeanDay=tapply(dat[dat$Month=="6",]$Temp2,dat[dat$Month=="6",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="6",]$Temp2,dat[dat$Month=="6",]$Day,mean)))
tdm06$Day<-as.numeric(tdm06$Day)
plot(tdm06$Day,tdm06$MeanDay,type="l",ylim=range(c(tdm06$MeanDay-tdm06$sdDay,tdm06$MeanDay+tdm06$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm06$Day,tdm06$MeanDay-tdm06$sdDay,tdm06$Day,tdm06$MeanDay+tdm06$sdDay, code=3, angle=90, length=0.05)

tdm07<-data.frame(sdDay=tapply(dat[dat$Month=="7",]$Temp2,dat[dat$Month=="7",]$Day,sd),MeanDay=tapply(dat[dat$Month=="7",]$Temp2,dat[dat$Month=="7",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="7",]$Temp2,dat[dat$Month=="7",]$Day,mean)))
tdm07$Day<-as.numeric(tdm07$Day)
plot(tdm07$Day,tdm07$MeanDay,type="l",ylim=range(c(tdm07$MeanDay-tdm07$sdDay,tdm07$MeanDay+tdm07$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm07$Day,tdm07$MeanDay-tdm07$sdDay,tdm07$Day,tdm07$MeanDay+tdm07$sdDay, code=3, angle=90, length=0.05)

tdm08<-data.frame(sdDay=tapply(dat[dat$Month=="8",]$Temp2,dat[dat$Month=="8",]$Day,sd),MeanDay=tapply(dat[dat$Month=="8",]$Temp2,dat[dat$Month=="8",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="8",]$Temp2,dat[dat$Month=="8",]$Day,mean)))
tdm08$Day<-as.numeric(tdm08$Day)
plot(tdm08$Day,tdm08$MeanDay,type="l",ylim=range(c(tdm08$MeanDay-tdm08$sdDay,tdm08$MeanDay+tdm08$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm08$Day,tdm08$MeanDay-tdm08$sdDay,tdm08$Day,tdm08$MeanDay+tdm08$sdDay, code=3, angle=90, length=0.05)

tdm09<-data.frame(sdDay=tapply(dat[dat$Month=="9",]$Temp2,dat[dat$Month=="9",]$Day,sd),MeanDay=tapply(dat[dat$Month=="9",]$Temp2,dat[dat$Month=="9",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="9",]$Temp2,dat[dat$Month=="9",]$Day,mean)))
tdm09$Day<-as.numeric(tdm09$Day)
plot(tdm09$Day,tdm09$MeanDay,type="l",ylim=range(c(tdm09$MeanDay-tdm09$sdDay,tdm09$MeanDay+tdm09$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm09$Day,tdm09$MeanDay-tdm09$sdDay,tdm09$Day,tdm09$MeanDay+tdm09$sdDay, code=3, angle=90, length=0.05)

tdm10<-data.frame(sdDay=tapply(dat[dat$Month=="10",]$Temp2,dat[dat$Month=="10",]$Day,sd),MeanDay=tapply(dat[dat$Month=="10",]$Temp2,dat[dat$Month=="10",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="10",]$Temp2,dat[dat$Month=="10",]$Day,mean)))
tdm10$Day<-as.numeric(tdm10$Day)
plot(tdm10$Day,tdm10$MeanDay,type="l",ylim=range(c(tdm10$MeanDay-tdm10$sdDay,tdm10$MeanDay+tdm10$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm10$Day,tdm10$MeanDay-tdm10$sdDay,tdm10$Day,tdm10$MeanDay+tdm10$sdDay, code=3, angle=90, length=0.05)

tdm11<-data.frame(sdDay=tapply(dat[dat$Month=="11",]$Temp2,dat[dat$Month=="11",]$Day,sd),MeanDay=tapply(dat[dat$Month=="11",]$Temp2,dat[dat$Month=="11",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="11",]$Temp2,dat[dat$Month=="11",]$Day,mean)))
tdm11$Day<-as.numeric(tdm11$Day)
plot(tdm11$Day,tdm11$MeanDay,type="l",ylim=range(c(tdm11$MeanDay-tdm11$sdDay,tdm11$MeanDay+tdm11$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm11$Day,tdm11$MeanDay-tdm11$sdDay,tdm11$Day,tdm11$MeanDay+tdm11$sdDay, code=3, angle=90, length=0.05)

tdm12<-data.frame(sdDay=tapply(dat[dat$Month=="12",]$Temp2,dat[dat$Month=="12",]$Day,sd),MeanDay=tapply(dat[dat$Month=="12",]$Temp2,dat[dat$Month=="12",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="12",]$Temp2,dat[dat$Month=="12",]$Day,mean)))
tdm12$Day<-as.numeric(tdm12$Day)
plot(tdm12$Day,tdm12$MeanDay,type="l",ylim=range(c(tdm12$MeanDay-tdm12$sdDay,tdm12$MeanDay+tdm12$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm12$Day,tdm12$MeanDay-tdm12$sdDay,tdm12$Day,tdm12$MeanDay+tdm12$sdDay, code=3, angle=90, length=0.05)


###TEMP 3
##Todos los datos ----
plot(dat$ID,dat$Temp3)
## variacion mensual----
boxplot(dat$Temp3~dat$Month)
## variacion diaria por mes----
boxplot(dat[dat$Month=="06",]$Temp3~dat[dat$Month=="06",]$Day)
boxplot(dat[dat$Month=="02",]$Temp3~dat[dat$Month=="02",]$Day)
boxplot(dat[dat$Month=="03",]$Temp3~dat[dat$Month=="03",]$Day)
boxplot(dat[dat$Month=="04",]$Temp3~dat[dat$Month=="04",]$Day)
boxplot(dat[dat$Month=="05",]$Temp3~dat[dat$Month=="05",]$Day)
boxplot(dat[dat$Month=="06",]$Temp3~dat[dat$Month=="06",]$Day)
boxplot(dat[dat$Month=="07",]$Temp3~dat[dat$Month=="07",]$Day)
boxplot(dat[dat$Month=="08",]$Temp3~dat[dat$Month=="08",]$Day)
boxplot(dat[dat$Month=="09",]$Temp3~dat[dat$Month=="09",]$Day)
boxplot(dat[dat$Month=="10",]$Temp3~dat[dat$Month=="10",]$Day)
boxplot(dat[dat$Month=="11",]$Temp3~dat[dat$Month=="11",]$Day)
boxplot(dat[dat$Month=="12",]$Temp3~dat[dat$Month=="12",]$Day)

##Temperatura promedio mensual----

tmm<-data.frame(sdMonth=tapply(dat$Temp2,dat$Month,sd),MeanMonth=tapply(dat$Temp2,dat$Month,mean),Month=row.names(tapply(dat$Temp2,dat$Month,mean)))
tmm$Month<-as.numeric(tmm$Month)
plot(tmm$Month,tmm$MeanMonth,type="l",ylim=range(c(tmm$MeanMonth-tmm$sdMonth,tmm$MeanMonth+tmm$sdMonth)),xlab="Month",ylab="Temperature 1")
arrows(tmm$Month,tmm$MeanMonth-tmm$sdMonth,tmm$Month,tmm$MeanMonth+tmm$sdMonth, code=3, angle=90, length=0.05)

##Temperatura promedio diaria---

tdm01<-data.frame(sdDay=tapply(dat[dat$Month=="1",]$Temp3,dat[dat$Month=="1",]$Day,sd),MeanDay=tapply(dat[dat$Month=="1",]$Temp3,dat[dat$Month=="1",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="1",]$Temp3,dat[dat$Month=="1",]$Day,mean)))
tdm01$Day<-as.numeric(tdm01$Day)
plot(tdm01$Day,tdm01$MeanDay,type="l",ylim=range(c(tdm01$MeanDay-tdm01$sdDay,tdm01$MeanDay+tdm01$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm01$Day,tdm01$MeanDay-tdm01$sdDay,tdm01$Day,tdm01$MeanDay+tdm01$sdDay, code=3, angle=90, length=0.05)

tdm02<-data.frame(sdDay=tapply(dat[dat$Month=="2",]$Temp3,dat[dat$Month=="2",]$Day,sd),MeanDay=tapply(dat[dat$Month=="2",]$Temp3,dat[dat$Month=="2",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="2",]$Temp3,dat[dat$Month=="2",]$Day,mean)))
tdm02$Day<-as.numeric(tdm02$Day)
plot(tdm02$Day,tdm02$MeanDay,type="l",ylim=range(c(tdm02$MeanDay-tdm02$sdDay,tdm02$MeanDay+tdm02$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm02$Day,tdm02$MeanDay-tdm02$sdDay,tdm02$Day,tdm02$MeanDay+tdm02$sdDay, code=3, angle=90, length=0.05)

tdm03<-data.frame(sdDay=tapply(dat[dat$Month=="3",]$Temp3,dat[dat$Month=="3",]$Day,sd),MeanDay=tapply(dat[dat$Month=="3",]$Temp3,dat[dat$Month=="3",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="3",]$Temp3,dat[dat$Month=="3",]$Day,mean)))
tdm03$Day<-as.numeric(tdm03$Day)
plot(tdm03$Day,tdm03$MeanDay,type="l",ylim=range(c(tdm03$MeanDay-tdm03$sdDay,tdm03$MeanDay+tdm03$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm03$Day,tdm03$MeanDay-tdm03$sdDay,tdm03$Day,tdm03$MeanDay+tdm03$sdDay, code=3, angle=90, length=0.05)

tdm04<-data.frame(sdDay=tapply(dat[dat$Month=="4",]$Temp3,dat[dat$Month=="4",]$Day,sd),MeanDay=tapply(dat[dat$Month=="4",]$Temp3,dat[dat$Month=="4",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="4",]$Temp3,dat[dat$Month=="4",]$Day,mean)))
tdm04$Day<-as.numeric(tdm04$Day)
plot(tdm04$Day,tdm04$MeanDay,type="l",ylim=range(c(tdm04$MeanDay-tdm04$sdDay,tdm04$MeanDay+tdm04$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm04$Day,tdm04$MeanDay-tdm04$sdDay,tdm04$Day,tdm04$MeanDay+tdm04$sdDay, code=3, angle=90, length=0.05)

tdm05<-data.frame(sdDay=tapply(dat[dat$Month=="5",]$Temp3,dat[dat$Month=="5",]$Day,sd),MeanDay=tapply(dat[dat$Month=="5",]$Temp3,dat[dat$Month=="5",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="5",]$Temp3,dat[dat$Month=="5",]$Day,mean)))
tdm05$Day<-as.numeric(tdm05$Day)
plot(tdm05$Day,tdm05$MeanDay,type="l",ylim=range(c(tdm05$MeanDay-tdm05$sdDay,tdm05$MeanDay+tdm05$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm05$Day,tdm05$MeanDay-tdm05$sdDay,tdm05$Day,tdm05$MeanDay+tdm05$sdDay, code=3, angle=90, length=0.05)

tdm06<-data.frame(sdDay=tapply(dat[dat$Month=="6",]$Temp3,dat[dat$Month=="6",]$Day,sd),MeanDay=tapply(dat[dat$Month=="6",]$Temp3,dat[dat$Month=="6",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="6",]$Temp3,dat[dat$Month=="6",]$Day,mean)))
tdm06$Day<-as.numeric(tdm06$Day)
plot(tdm06$Day,tdm06$MeanDay,type="l",ylim=range(c(tdm06$MeanDay-tdm06$sdDay,tdm06$MeanDay+tdm06$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm06$Day,tdm06$MeanDay-tdm06$sdDay,tdm06$Day,tdm06$MeanDay+tdm06$sdDay, code=3, angle=90, length=0.05)

tdm07<-data.frame(sdDay=tapply(dat[dat$Month=="7",]$Temp3,dat[dat$Month=="7",]$Day,sd),MeanDay=tapply(dat[dat$Month=="7",]$Temp3,dat[dat$Month=="7",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="7",]$Temp3,dat[dat$Month=="7",]$Day,mean)))
tdm07$Day<-as.numeric(tdm07$Day)
plot(tdm07$Day,tdm07$MeanDay,type="l",ylim=range(c(tdm07$MeanDay-tdm07$sdDay,tdm07$MeanDay+tdm07$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm07$Day,tdm07$MeanDay-tdm07$sdDay,tdm07$Day,tdm07$MeanDay+tdm07$sdDay, code=3, angle=90, length=0.05)

tdm08<-data.frame(sdDay=tapply(dat[dat$Month=="8",]$Temp3,dat[dat$Month=="8",]$Day,sd),MeanDay=tapply(dat[dat$Month=="8",]$Temp3,dat[dat$Month=="8",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="8",]$Temp3,dat[dat$Month=="8",]$Day,mean)))
tdm08$Day<-as.numeric(tdm08$Day)
plot(tdm08$Day,tdm08$MeanDay,type="l",ylim=range(c(tdm08$MeanDay-tdm08$sdDay,tdm08$MeanDay+tdm08$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm08$Day,tdm08$MeanDay-tdm08$sdDay,tdm08$Day,tdm08$MeanDay+tdm08$sdDay, code=3, angle=90, length=0.05)

tdm09<-data.frame(sdDay=tapply(dat[dat$Month=="9",]$Temp3,dat[dat$Month=="9",]$Day,sd),MeanDay=tapply(dat[dat$Month=="9",]$Temp3,dat[dat$Month=="9",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="9",]$Temp3,dat[dat$Month=="9",]$Day,mean)))
tdm09$Day<-as.numeric(tdm09$Day)
plot(tdm09$Day,tdm09$MeanDay,type="l",ylim=range(c(tdm09$MeanDay-tdm09$sdDay,tdm09$MeanDay+tdm09$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm09$Day,tdm09$MeanDay-tdm09$sdDay,tdm09$Day,tdm09$MeanDay+tdm09$sdDay, code=3, angle=90, length=0.05)

tdm10<-data.frame(sdDay=tapply(dat[dat$Month=="10",]$Temp3,dat[dat$Month=="10",]$Day,sd),MeanDay=tapply(dat[dat$Month=="10",]$Temp3,dat[dat$Month=="10",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="10",]$Temp3,dat[dat$Month=="10",]$Day,mean)))
tdm10$Day<-as.numeric(tdm10$Day)
plot(tdm10$Day,tdm10$MeanDay,type="l",ylim=range(c(tdm10$MeanDay-tdm10$sdDay,tdm10$MeanDay+tdm10$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm10$Day,tdm10$MeanDay-tdm10$sdDay,tdm10$Day,tdm10$MeanDay+tdm10$sdDay, code=3, angle=90, length=0.05)

tdm11<-data.frame(sdDay=tapply(dat[dat$Month=="11",]$Temp3,dat[dat$Month=="11",]$Day,sd),MeanDay=tapply(dat[dat$Month=="11",]$Temp3,dat[dat$Month=="11",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="11",]$Temp3,dat[dat$Month=="11",]$Day,mean)))
tdm11$Day<-as.numeric(tdm11$Day)
plot(tdm11$Day,tdm11$MeanDay,type="l",ylim=range(c(tdm11$MeanDay-tdm11$sdDay,tdm11$MeanDay+tdm11$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm11$Day,tdm11$MeanDay-tdm11$sdDay,tdm11$Day,tdm11$MeanDay+tdm11$sdDay, code=3, angle=90, length=0.05)

tdm12<-data.frame(sdDay=tapply(dat[dat$Month=="12",]$Temp3,dat[dat$Month=="12",]$Day,sd),MeanDay=tapply(dat[dat$Month=="12",]$Temp3,dat[dat$Month=="12",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="12",]$Temp3,dat[dat$Month=="12",]$Day,mean)))
tdm12$Day<-as.numeric(tdm12$Day)
plot(tdm12$Day,tdm12$MeanDay,type="l",ylim=range(c(tdm12$MeanDay-tdm12$sdDay,tdm12$MeanDay+tdm12$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm12$Day,tdm12$MeanDay-tdm12$sdDay,tdm12$Day,tdm12$MeanDay+tdm12$sdDay, code=3, angle=90, length=0.05)

###Soil Moisture
##Todos los datos ----
plot(dat$ID,dat$SoilMoisture)
## variacion mensual----
boxplot(dat$SoilMoisture~dat$Month)
## variacion diaria por mes----
boxplot(dat[dat$Month=="06",]$SoilMoisture~dat[dat$Month=="06",]$Day)
boxplot(dat[dat$Month=="02",]$SoilMoisture~dat[dat$Month=="02",]$Day)
boxplot(dat[dat$Month=="03",]$SoilMoisture~dat[dat$Month=="03",]$Day)
boxplot(dat[dat$Month=="04",]$SoilMoisture~dat[dat$Month=="04",]$Day)
boxplot(dat[dat$Month=="05",]$SoilMoisture~dat[dat$Month=="05",]$Day)
boxplot(dat[dat$Month=="06",]$SoilMoisture~dat[dat$Month=="06",]$Day)
boxplot(dat[dat$Month=="07",]$SoilMoisture~dat[dat$Month=="07",]$Day)
boxplot(dat[dat$Month=="08",]$SoilMoisture~dat[dat$Month=="08",]$Day)
boxplot(dat[dat$Month=="09",]$SoilMoisture~dat[dat$Month=="09",]$Day)
boxplot(dat[dat$Month=="10",]$SoilMoisture~dat[dat$Month=="10",]$Day)
boxplot(dat[dat$Month=="11",]$SoilMoisture~dat[dat$Month=="11",]$Day)
boxplot(dat[dat$Month=="12",]$SoilMoisture~dat[dat$Month=="12",]$Day)

##Temperatura promedio mensual----

tmm<-data.frame(sdMonth=tapply(dat$SoilMoisture,dat$Month,sd),MeanMonth=tapply(dat$SoilMoisture,dat$Month,mean),Month=row.names(tapply(dat$SoilMoisture,dat$Month,mean)))
tmm$Month<-as.numeric(tmm$Month)
plot(tmm$Month,tmm$MeanMonth,type="l",ylim=range(c(tmm$MeanMonth-tmm$sdMonth,tmm$MeanMonth+tmm$sdMonth)),xlab="Month",ylab="Temperature 1")
arrows(tmm$Month,tmm$MeanMonth-tmm$sdMonth,tmm$Month,tmm$MeanMonth+tmm$sdMonth, code=3, angle=90, length=0.05)

##Temperatura promedio diaria---

tdm01<-data.frame(sdDay=tapply(dat[dat$Month=="1",]$SoilMoisture,dat[dat$Month=="1",]$Day,sd),MeanDay=tapply(dat[dat$Month=="1",]$SoilMoisture,dat[dat$Month=="1",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="1",]$SoilMoisture,dat[dat$Month=="1",]$Day,mean)))
tdm01$Day<-as.numeric(tdm01$Day)
plot(tdm01$Day,tdm01$MeanDay,type="l",ylim=range(c(tdm01$MeanDay-tdm01$sdDay,tdm01$MeanDay+tdm01$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm01$Day,tdm01$MeanDay-tdm01$sdDay,tdm01$Day,tdm01$MeanDay+tdm01$sdDay, code=3, angle=90, length=0.05)

tdm02<-data.frame(sdDay=tapply(dat[dat$Month=="2",]$SoilMoisture,dat[dat$Month=="2",]$Day,sd),MeanDay=tapply(dat[dat$Month=="2",]$SoilMoisture,dat[dat$Month=="2",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="2",]$SoilMoisture,dat[dat$Month=="2",]$Day,mean)))
tdm02$Day<-as.numeric(tdm02$Day)
plot(tdm02$Day,tdm02$MeanDay,type="l",ylim=range(c(tdm02$MeanDay-tdm02$sdDay,tdm02$MeanDay+tdm02$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm02$Day,tdm02$MeanDay-tdm02$sdDay,tdm02$Day,tdm02$MeanDay+tdm02$sdDay, code=3, angle=90, length=0.05)

tdm03<-data.frame(sdDay=tapply(dat[dat$Month=="3",]$SoilMoisture,dat[dat$Month=="3",]$Day,sd),MeanDay=tapply(dat[dat$Month=="3",]$SoilMoisture,dat[dat$Month=="3",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="3",]$SoilMoisture,dat[dat$Month=="3",]$Day,mean)))
tdm03$Day<-as.numeric(tdm03$Day)
plot(tdm03$Day,tdm03$MeanDay,type="l",ylim=range(c(tdm03$MeanDay-tdm03$sdDay,tdm03$MeanDay+tdm03$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm03$Day,tdm03$MeanDay-tdm03$sdDay,tdm03$Day,tdm03$MeanDay+tdm03$sdDay, code=3, angle=90, length=0.05)

tdm04<-data.frame(sdDay=tapply(dat[dat$Month=="4",]$SoilMoisture,dat[dat$Month=="4",]$Day,sd),MeanDay=tapply(dat[dat$Month=="4",]$SoilMoisture,dat[dat$Month=="4",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="4",]$SoilMoisture,dat[dat$Month=="4",]$Day,mean)))
tdm04$Day<-as.numeric(tdm04$Day)
plot(tdm04$Day,tdm04$MeanDay,type="l",ylim=range(c(tdm04$MeanDay-tdm04$sdDay,tdm04$MeanDay+tdm04$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm04$Day,tdm04$MeanDay-tdm04$sdDay,tdm04$Day,tdm04$MeanDay+tdm04$sdDay, code=3, angle=90, length=0.05)

tdm05<-data.frame(sdDay=tapply(dat[dat$Month=="5",]$SoilMoisture,dat[dat$Month=="5",]$Day,sd),MeanDay=tapply(dat[dat$Month=="5",]$SoilMoisture,dat[dat$Month=="5",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="5",]$SoilMoisture,dat[dat$Month=="5",]$Day,mean)))
tdm05$Day<-as.numeric(tdm05$Day)
plot(tdm05$Day,tdm05$MeanDay,type="l",ylim=range(c(tdm05$MeanDay-tdm05$sdDay,tdm05$MeanDay+tdm05$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm05$Day,tdm05$MeanDay-tdm05$sdDay,tdm05$Day,tdm05$MeanDay+tdm05$sdDay, code=3, angle=90, length=0.05)

tdm06<-data.frame(sdDay=tapply(dat[dat$Month=="6",]$SoilMoisture,dat[dat$Month=="6",]$Day,sd),MeanDay=tapply(dat[dat$Month=="6",]$SoilMoisture,dat[dat$Month=="6",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="6",]$SoilMoisture,dat[dat$Month=="6",]$Day,mean)))
tdm06$Day<-as.numeric(tdm06$Day)
plot(tdm06$Day,tdm06$MeanDay,type="l",ylim=range(c(tdm06$MeanDay-tdm06$sdDay,tdm06$MeanDay+tdm06$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm06$Day,tdm06$MeanDay-tdm06$sdDay,tdm06$Day,tdm06$MeanDay+tdm06$sdDay, code=3, angle=90, length=0.05)

tdm07<-data.frame(sdDay=tapply(dat[dat$Month=="7",]$SoilMoisture,dat[dat$Month=="7",]$Day,sd),MeanDay=tapply(dat[dat$Month=="7",]$SoilMoisture,dat[dat$Month=="7",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="7",]$SoilMoisture,dat[dat$Month=="7",]$Day,mean)))
tdm07$Day<-as.numeric(tdm07$Day)
plot(tdm07$Day,tdm07$MeanDay,type="l",ylim=range(c(tdm07$MeanDay-tdm07$sdDay,tdm07$MeanDay+tdm07$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm07$Day,tdm07$MeanDay-tdm07$sdDay,tdm07$Day,tdm07$MeanDay+tdm07$sdDay, code=3, angle=90, length=0.05)

tdm08<-data.frame(sdDay=tapply(dat[dat$Month=="8",]$SoilMoisture,dat[dat$Month=="8",]$Day,sd),MeanDay=tapply(dat[dat$Month=="8",]$SoilMoisture,dat[dat$Month=="8",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="8",]$SoilMoisture,dat[dat$Month=="8",]$Day,mean)))
tdm08$Day<-as.numeric(tdm08$Day)
plot(tdm08$Day,tdm08$MeanDay,type="l",ylim=range(c(tdm08$MeanDay-tdm08$sdDay,tdm08$MeanDay+tdm08$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm08$Day,tdm08$MeanDay-tdm08$sdDay,tdm08$Day,tdm08$MeanDay+tdm08$sdDay, code=3, angle=90, length=0.05)

tdm09<-data.frame(sdDay=tapply(dat[dat$Month=="9",]$SoilMoisture,dat[dat$Month=="9",]$Day,sd),MeanDay=tapply(dat[dat$Month=="9",]$SoilMoisture,dat[dat$Month=="9",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="9",]$SoilMoisture,dat[dat$Month=="9",]$Day,mean)))
tdm09$Day<-as.numeric(tdm09$Day)
plot(tdm09$Day,tdm09$MeanDay,type="l",ylim=range(c(tdm09$MeanDay-tdm09$sdDay,tdm09$MeanDay+tdm09$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm09$Day,tdm09$MeanDay-tdm09$sdDay,tdm09$Day,tdm09$MeanDay+tdm09$sdDay, code=3, angle=90, length=0.05)

tdm10<-data.frame(sdDay=tapply(dat[dat$Month=="10",]$SoilMoisture,dat[dat$Month=="10",]$Day,sd),MeanDay=tapply(dat[dat$Month=="10",]$SoilMoisture,dat[dat$Month=="10",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="10",]$SoilMoisture,dat[dat$Month=="10",]$Day,mean)))
tdm10$Day<-as.numeric(tdm10$Day)
plot(tdm10$Day,tdm10$MeanDay,type="l",ylim=range(c(tdm10$MeanDay-tdm10$sdDay,tdm10$MeanDay+tdm10$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm10$Day,tdm10$MeanDay-tdm10$sdDay,tdm10$Day,tdm10$MeanDay+tdm10$sdDay, code=3, angle=90, length=0.05)

tdm11<-data.frame(sdDay=tapply(dat[dat$Month=="11",]$SoilMoisture,dat[dat$Month=="11",]$Day,sd),MeanDay=tapply(dat[dat$Month=="11",]$SoilMoisture,dat[dat$Month=="11",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="11",]$SoilMoisture,dat[dat$Month=="11",]$Day,mean)))
tdm11$Day<-as.numeric(tdm11$Day)
plot(tdm11$Day,tdm11$MeanDay,type="l",ylim=range(c(tdm11$MeanDay-tdm11$sdDay,tdm11$MeanDay+tdm11$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm11$Day,tdm11$MeanDay-tdm11$sdDay,tdm11$Day,tdm11$MeanDay+tdm11$sdDay, code=3, angle=90, length=0.05)

tdm12<-data.frame(sdDay=tapply(dat[dat$Month=="12",]$SoilMoisture,dat[dat$Month=="12",]$Day,sd),MeanDay=tapply(dat[dat$Month=="12",]$SoilMoisture,dat[dat$Month=="12",]$Day,mean),Day=row.names(tapply(dat[dat$Month=="12",]$SoilMoisture,dat[dat$Month=="12",]$Day,mean)))
tdm12$Day<-as.numeric(tdm12$Day)
plot(tdm12$Day,tdm12$MeanDay,type="l",ylim=range(c(tdm12$MeanDay-tdm12$sdDay,tdm12$MeanDay+tdm12$sdDay)),xlab="Day",ylab="Temperature 1")
arrows(tdm12$Day,tdm12$MeanDay-tdm12$sdDay,tdm12$Day,tdm12$MeanDay+tdm12$sdDay, code=3, angle=90, length=0.05)
