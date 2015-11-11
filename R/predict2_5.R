# run one time 
test$units<-0
#load ("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/stores.RData")


# ##some extra codes.
# the result is save in the stores.RData
# stores<-NULL
# for(item in 1:111){
#   store=NULL
#   for(i in 1:45){
#     dat<-subset(train,store_nbr==i&item_nbr==item)
#     if (sum(dat$units>0)>10) {
#       store <- c(store , i)
#     }
#   }
#   stores[item]<-list(store)
# }
# 
# #parallel
# stores<-NULL
# systime<-system.time( 
#   result<-foreach(item = 1:111)%dopar% {
#     store=NULL
#     for(i in 1:45){
#       dat<-subset(train,store_nbr==i&item_nbr==item)
#       if (sum(dat$units>0)>10) {
#         store <- c(store , i)
#       }
#     }
#     stores[item]<-list(store)
#   }
# )

for(item in 1: 111){
  stores<-result[[item]][[1]] #36
  
  for( store in stores){
    if (store == 35)  break
    station<-key[store,2]
    
    weather_station<-subset(weather,station_nbr==station)
    weather_station$avgspeed<-as.numeric(as.character(weather_station$avgspeed))
    weather_station$avgspeed[which(is.na(weather_station$avgspeed))]=mean(weather_station$avgspeed)
    store_train=subset(train,store_nbr==store)
    store_dates<-unique(store_train$date)
    weather_station_train<-subset(weather_station,date%in% store_dates)
    weather_station_train=weather_station_train[,c("tavg","depart","snowfall","snow",
                                                   "preciptotal","rain","storm","avgspeed")]
    
    
    store_train<-subset(train,store_nbr==store)
    store_test<-subset(test_full,store_nbr==store)
    #weather 111 a loop 
    store_train$weekday<-as.factor(as.integer(store_train$date)%%7)
    levels(store_train$weekday)=c("Thus","Fri","Sat","Sun","Mon","Tues","Wed")
    store_train$tavg <- rep(weather_station_train$tavg,each=111)
    store_train$depart<-rep(weather_station_train$depart,each=111)
    store_train$dewpoint<-rep(weather_station_train$dewpoint,each=111)
    store_train$snowfall<-rep(weather_station_train$snowfall,each=111)
    store_train$snow<-rep(weather_station_train$snow,each=111)
    store_train$preciptotal<-rep(weather_station_train$preciptotal,each=111)
    store_train$rain<-rep(weather_station_train$rain,each=111)
    store_train$storm<-rep(weather_station_train$storm,each=111)
    store_train$avgspeed<-rep(weather_station_train$avgspeed,each=111)
    #store_train$holiday<-as.factor(as.integer(store_train$date)%%7)
    
    
    
    
    store_item<-subset(store_train,item_nbr==item)
    store_item_test=subset(store_test,item_nbr==item)
    MIN_date<-store_item[min(which(store_item$units>0)),]$date-20
    MAX_date<-store_item[max(which(store_item$units>0)),]$date+20
    if(MIN_date>="2013-04-01"|MAX_date<="2014-04-01"){
      store_item<-subset(store_item,date>=MIN_date&date<=MAX_date)
      store_item_test<-subset(store_item_test,date>=MIN_date&date<=MAX_date)
    }
    
    
    #     par(mfcol=c(2,1))
    #     plot(store_item$date,store_item$units,ylab="units sold",main="item ? at store ?")
    #     lines(store_item$date,store_item$units)
    #     points(store_item[which(store_item$storm==1),]$date,store_item[which(store_item$storm==1),]$units,col="red")
    #     boxplot(store_item[1:455,]$units~store_item[1:455,]$weekday)
    #     
    if (any(is.na(store_item$avgspeed))){
      store_item$avgspeed[which(is.na(store_item$avgspeed))]<-mean(store_item$avgspeed[which(!is.na(store_item$avgspeed))])
    }
    
    
    
    model1<-glm(units~date1+sin(2*pi*date1/7)+cos(2*pi*date1/7)
                +sin(2*pi*date1/30)+cos(2*pi*date1/30)
                +sin(2*pi*date1/365)+cos(2*pi*date1/365)
                +tavg+depart
                +avgspeed + preciptotal
                +snowfall + storm          
                ,family="poisson",data=store_item)
    #     summary(model1)
    Finalmodel<-step(model1,trace=0)
    
    
    
    #     par(mfcol=c(1,1))
    #     plot(store_item$date,store_item$units,xlab="date",ylab="units sold",main="item 5 at store 2")
    #     lines(store_item$date,store_item$units)
    #     points(store_item$date,predict(Finalmodel,newdata=store_item,type="response"),col="red")
    #     
    # #     plot(Finalmodel$fitted,store_item$units)  
    if(MIN_date>="2013-04-01"|MAX_date<="2014-04-01"){
      test[which(test$store_nbr==store & test$item_nbr==item &test$date>=MIN_date&test$date<=MAX_date),]$units<-
        predict(Finalmodel,newdata=store_item_test,type="response")
    }else{
      test[which(test$store_nbr==store & test$item_nbr==item),]$units<-
        predict(Finalmodel,newdata=store_item_test,type="response")
    }
    
  }
}
predict<-test$units


# test$units<-predict
# test$units<-floor(test$units*(test$units>0.6))
 submit$units<-test$units
write.csv(submit,file="~/Desktop/sampleSubmission.csv")
subset(test,item_nbr==109&store_nbr==3)$units

