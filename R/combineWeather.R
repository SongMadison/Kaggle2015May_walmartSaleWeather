#weather_full<-weather
#weather=weather_full[,c("tavg","depart","snowfall","snow","preciptotal","rain","storm")]


#combining the weather data to train and testing data set.
# there are a lot of missing data at station==5

#weather_tmp<-NULL
#key<-subset(key,station_nbr!=5)
#train<-subset(train,store_nbr!=35)
## total 44 stores 111 item ids and 19 weather stations
# library(doParallel)
# cl <- makeCluster(10)
# registerDoParallel(cl)
# 
# foreach(i = 1:nrow(train)) %dopar% {
#   store_i<-train[i,]$store_nbr
#   station_i<-key[i,2]
#   date_i<-train[i,]$date
#   weather_tmp=rbind(weather_tmp,subset(weather_full,station_nbr==station_i
#                                        &date==date_i)[,c("tavg","depart","dewpoint","snowfall"
#                                                          ,"snow","preciptotal","rain","storm")])  
# }
# 
# foreach(i = 1:nrow(test)) %dopar% {
#   store_i<-test[i,]$store_nbr
#   station_i<-key[i,2]
#   date_i<-test[i,]$date
#   weather_tmp=rbind(weather_tmp,subset(weather_full,station_nbr==station_i
#                                        &date==date_i)[,c("tavg","depart","dewpoint","snowfall"
#                                                          ,"snow","preciptotal","rain","storm")])  
# }
# 
# stopCluster(cl)





# test$tavg<-sample(weather$tavg,nrow(test),replace=TRUE)
# test$depart<-sample(weather$depart,nrow(test),replace=TRUE)
# test$snowfall<-sample(weather$snowfall,nrow(test),replace=TRUE)
# test$snow<-sample(weather$snow,nrow(test),replace=TRUE)
# test$preciptotal<-sample(weather$preciptotal,nrow(test),replace=TRUE)
# test$rain<-sample(weather$rain,nrow(test),replace=TRUE)
# test$avgspeed<-sample(weather$avgspeed,nrow(test),replace=TRUE)
# test$storm<-1
# 
# for(i in 1:(nrow(test)/111)) {
#   j<-(i-1)*111+1
#   store_j<-test[j,]$store_nbr
#   station_j<-key[store_j,2]
#   date_j<-test[j,]$date
#   weather_j=subset(weather,station_nbr==station_j&date==as.character(date_j))[,c("tavg","depart","snowfall" 
#      ,"snow","preciptotal","rain","avgspeed","storm")] 
#   for(k in 1:110){
#     test[j+k,c("tavg","depart","snowfall" ,"snow","preciptotal","rain","avgspeed","storm")] =weather_j 
#   }
# }


train$tavg<-sample(weather$tavg,nrow(train),replace=TRUE)
train$depart<-sample(weather$depart,nrow(train),replace=TRUE)
train$snowfall<-sample(weather$snowfall,nrow(train),replace=TRUE)
train$snow<-sample(weather$snow,nrow(train),replace=TRUE)
train$preciptotal<-sample(weather$preciptotal,nrow(train),replace=TRUE)
train$rain<-sample(weather$rain,nrow(train),replace=TRUE)
train$storm<-sample(weather$storm,nrow(train),replace=TRUE)
train$rain<-sample(weather$rain,nrow(train),replace=TRUE)
train$avgspeed<-sample(weather$avgspeed,nrow(train),replace=TRUE)

kk=2
for(i in (kk-1)*4160+(1:4160)) {  
  j<-(i-1)*111+1
  store_j<-train[j,]$store_nbr
  station_j<-key[store_j,2]
  date_j<-train[j,]$date
  weather_j=subset(weather,station_nbr==station_j&date==as.character(date_j))[,c("tavg",
       "depart","snowfall","snow","preciptotal","rain","avgspeed","storm")] 
  for(k in 1:110){
    train[j+k,c("tavg","depart","snowfall" ,"snow","preciptotal",
                "rain","avgspeed","storm")] =weather_j 
  }
}
#write.csv(test,file="~/Desktop/2015 S/998proj3/test_full_1.csv")