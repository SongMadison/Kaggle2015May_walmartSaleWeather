#explainatary analysis:
  
  ##mypath<-file.path("/afs/cs.wisc.edu/u/s/o/songwang/RScript","products.pdf")
  library(lattice)
foreach(i = 1:4) %dopar% {
  
  mypath<-file.path("/afs/cs.wisc.edu/u/s/o/songwang/RScript/itemplots",paste("item_",i,".pdf",sep=""))
  pdf(file=mypath)
  a<-xyplot(units~date|store_nbr,data=subset(train,item_nbr==i),ylab="units sold (log scale)",
            main=paste("units sold across 45 stores (item_",i,")",sep=""),
            panel=function(x,y){
              panel.xyplot(x,y)
              panel.lmline(x,y,lty=2,col="red")
            }
  )
  print(a)  
  dev.off()
}

freq<-NULL
for(i in 1:111){
  freq<-c(freq, sum(subset(train,item_nbr==i&date>"2013-04-01")$units>0))
}

library(lattice) #why have to library multiple times
system.time( foreach(i = 1:45) %dopar% {
  library(lattice)
  mypath<-file.path("/afs/cs.wisc.edu/u/s/o/songwang/RScript/storeplots",paste("store_",i,".pdf",sep=""))
  pdf(file=mypath)
  a<-xyplot(units~date|item_nbr,data=subset(train,store_nbr==i),ylab="units sold (log scale)",
            main=paste("units sold across 111 items (store_",i,")",sep=""),
            panel=function(x,y){
              panel.xyplot(x,y)
              panel.lmline(x,y,lty=2,col="red")
            }
  )
  print(a)
  j<-key[which(key$store_nbr==i),]$station_nbr
  weather_j <-subset(weather,station_nbr==j)
  plot(weather_j$date,weather_j$preciptotal,ylab="precipatation",main=paste("precipatation at station",j))
  lines(weather_j$date,as.numeric(as.character(weather_j$preciptotal)))
  points(weather_j[which(weather_j$storm==1),]$date,
         as.numeric(as.character(weather_j[which(weather_j$storm==1),]$preciptotal))
         ,col="red")
  dev.off()
})

  
  
  sample<-store2_5[1:90,]
  weekend<-subset(sample,weekday%in%c("Sat","Sun"),xlab="date",ylab="units (in log)")
  plot(sample$date,sample$units,ylim=c(-1,7))
  lines(sample$date,sample$preciptotal,col="blue")
  lines(sample$date,sample$snowfall,col="red")
  points(weekend$date,weekend$units, col="red")
  lines(sample$date,sample$tavg/30,col="green")
  lines(sample$date,sample$avgspeed/20,col="purple")
  title("units sold and weather (item 5 at store 2)")
  
  sample<-store2_5[1:455,]
  weekend<-subset(sample,weekday%in%c("Sat","Sun"),xlab="date",ylab="units (in log)")
  plot(sample$date,sample$units,ylim=c(-1,7))
  lines(sample$date,sample$preciptotal,col="blue")
  lines(sample$date,sample$snowfall,col="red")
  points(weekend$date,weekend$units, col="red")
  lines(sample$date,sample$tavg/30,col="green")
  lines(sample$date,sample$avgspeed/20,col="purple")
  title("units sold and weather (item 5 at store 2)")
  
  
  
  store8_5<-subset(train,item_nbr==5&store_nbr==8)
  plot(store8_5$date,store8_5$units,ylim=c(-1,7))
  weather4<-subset(weather,station_nbr==4)
  lines(weather4$date,weather4$preciptotal,col="blue")
  lines(weather4$date,weather4$snowfall,col="red")
  #points(weekend$date,weekend$units, col="red")
  #lines(weather14$date,weather14$tavg/15,col="green")
  abline(1,0)
  lines(c(as.Date("2013-04-01"),as.Date("2013-04-01")),c(-1,7),col="green")
  
  title("units sold item 5 at store8 from 2012-01-01 -  2014-10-31") 
  
  
  
  -----------------------------------------------------------
  plot(store2_5$date,store2_5$units,ylab="units sold",main="item 5 at store 2")
  lines(store2_5$date,store2_5$units)
  preciptotal<-store2_5$preciptotal[]
  points(store2_5[which(store2_5$storm==1),]$date,store2_5[which(store2_5$storm==1),]$units,col="red")
  points(store2_5$date,model1$fitted,col="red")
  lines(store2_5$date,model1$fitted,col="red")
  head(store2_5)
  
  #points(store2_5_test$date,rep(log(1+35),nrow(store2_5_test)),col="red")
  points(store2_5_test$date,rep(5,nrow(store2_5_test)),col="red")
  lines(store2_5_test$date,store2_5_test$units)
  plot(weather14$date,weather14$preciptotal,ylab="precipation",main="precipation at station 14")
  lines(weather14$date,as.numeric(as.character(weather14$preciptotal)))
  points(weather14[which(weather14$storm==1),]$date,
         as.numeric(as.character(weather14[which(weather14$storm==1),]$preciptotal))
         ,col="red")
 
  
  sample<-subset(train,item_nbr==50&store_nbr==14)
  sampleweather<-subset(weather,station_nbr==16)
  weekend<-subset(sample,weekday%in%c("Sat","Sun"))
  
  plot(sample$date,sample$units,ylim=c(-1,6))
  lines(sample$date,sample$units)
  lines(sampleweather$date,sampleweather$preciptotal,col="blue")
  lines(sampleweather$date,sampleweather$snowfall,col="red")
  points(weekend$date,weekend$units, col="red")
  lines(sampleweather$date,sampleweather$tavg/15,col="green")
  abline(1,0)
  lines(c(as.Date("2013-04-01"),as.Date("2013-04-01")),c(-1,7),col="red")
  title("units sold item 50 at store 14 from 2012-01-01 -  2014-10-31")  
  
  
  
  
 # --------------------------------
  
    sample<-store14_50
  weekend<-subset(sample,weekday%in%c("Sat","Sun"))
  plot(sample$date,sample$units,ylim=c(-0.2,3),xlab="date",ylab="units (in log)")
  points(weekend$date,weekend$units, col="red")
  lines(sample$date,sample$preciptotal/2,col="blue")
  lines(sample$date,sample$snowfall,col="red")
  points(weekend$date,weekend$units, col="red")
  lines(sample$date,sample$tavg/30,col="green")
  lines(sample$date,sample$avgspeed/20,col="purple")
  title("units sold and weather( (item 50 at store 14))")

