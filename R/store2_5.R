#read in the data with some prelimilary cleaning

#source("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/code/R/readin.R")
#source("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/code/R/combineWeather.R")


weather14<-subset(weather,station_nbr==14)
weather14$avgspeed<-as.numeric(as.character(weather14$avgspeed))
weather14$avgspeed[which(is.na(weather14$avgspeed))]=mean(weather14$avgspeed)
store2_train=subset(train,store_nbr==2)
store2_dates<-unique(store2_train$date)
weather14_train<-subset(weather14,date%in% store2_dates)
weather14_train=weather14_train[,c("tavg","depart","dewpoint","snowfall","snow",
                                   "preciptotal","rain","storm","avgspeed")]


#part a) create the store2_train and test set 
#first just deal with store2  # 111 product + about 4 1300days, +weather info
store2_train<-subset(train,store_nbr==2)
store2_test<-subset(test,store_nbr==2)
#weather 111 a loop 
store2_train$weekday<-as.factor(as.integer(store2_train$date)%%7)
levels(store2_train$weekday)=c("Thus","Fri","Sat","Sun","Mon","Tues","Wed")
store2_train$tavg<-rep(weather14_train$tavg,each=111)
store2_train$depart<-rep(weather14_train$depart,each=111)
store2_train$dewpoint<-rep(weather14_train$dewpoint,each=111)
store2_train$snowfall<-rep(weather14_train$snowfall,each=111)
store2_train$snow<-rep(weather14_train$snow,each=111)
store2_train$preciptotal<-rep(weather14_train$preciptotal,each=111)
store2_train$rain<-rep(weather14_train$rain,each=111)
store2_train$storm<-rep(weather14_train$storm,each=111)
store2_train$avgspeed<-rep(weather14_train$avgspeed,each=111)
#store2_train$holiday<-as.factor(as.integer(store2_train$date)%%7)
holiday<-c("2012-01-02","2012-01-16","2012-02-20","2012-05-28","2012-07-04","2012-09-03","2012-10-08"
           ,"2012-11-12","2012-11-22","2012-12-25","2013-01-01","2013-01-19","2013-02-16"
           )

data<-subset(store2_train,date%in%holiday)

# store2_5<-subset(store2_train,item_nbr==5)
# raindays<-numeric(nrow(store2_5))
# for(i in 1:nrow(store2_5)){
#   for(j in (i-2):(i+10)){
#     if(store2_5$preciptotal[j]>0.005|store2_5$snowfall[j]>0.05){
#       raindays[i]=raindays[i]+1
#     }else {
#       break
#     }
#   }
# }
# store2_train$raindays<-rep(raindays,each=111)

store2_5<-subset(store2_train,item_nbr==5)
#store2_train
#[1] "date"        "store_nbr"   "item_nbr"    "units"       "date1"       "tavg"        "depart"      "dewpoint"    "snowfall"   
#[10] "snow"        "preciptotal" "rain"        "storm"       "raindays"  

#similary store2_test

# weather14<-subset(weather,station_nbr==14)
# store2_dates<-unique(store2_test$date)
# weather14_test<-subset(weather14,date%in% store2_dates)
# weather14_test=weather14_test[,c("tavg","depart","dewpoint","snowfall","snow","preciptotal","rain","storm")]
# 
# store2_test$tavg<-rep(weather14_test$tavg,each=111)
# store2_test$depart<-rep(weather14_test$depart,each=111)
# store2_test$dewpoint<-rep(weather14_test$dewpoint,each=111)
# store2_test$snowfall<-rep(weather14_test$snowfall,each=111)
# store2_test$snow<-rep(weather14_test$snow,each=111)
# store2_test$preciptotal<-rep(weather14_test$preciptotal,each=111)
# store2_test$rain<-rep(weather14_test$rain,each=111)
# store2_test$storm<-rep(weather14_test$storm,each=111)
# unique(store2_train$item_nbr)
# store2_93<-subset(store2_test,item_nbr==93)
# raindays<-numeric(nrow(store2_93))
# for(i in 1:nrow(store2_93)){
#   for(j in 0:(i-1)){\newpage\newpage\newpage\newpage
#     if(store2_93$preciptotal[i-j]>0.02|store2_93$snowfall[i-j]>0.05){
#       raindays[i]=raindays[i]+1
#     }else {
#       break
#     }
#   }
# }
# store2_test$raindays<-rep(raindays,each=111)



###############
###part b    ##
###############



#######################################################
### extract weahter data for station_nbr=14 ###########
#######################################################

#combine the weather14 and store=2,6,38,42
#which(key$station_nbr==14)

unique(store2_train[which(store2_train$storm==1),]$date )
#[1] "2012-01-24" "2012-03-19" "2012-04-13" "2012-04-30" "2012-05-29" "2012-08-18"
#"2012-09-26" "2012-10-13" "2013-02-12"












store2_5<-subset(store2_train,item_nbr==5)
#store2_5<-subset(store2_5,date>"2012-06-01"&date<"2013-03-31")
store2_5_test=subset(store2_test,item_nbr==5)
par(mfcol=c(2,1))
plot(store2_5$date,store2_5$units,ylab="units sold",main="item 5 at store 2")
lines(store2_5$date,store2_5$units)
points(store2_5[which(store2_5$storm==1),]$date,store2_5[which(store2_5$storm==1),]$units,col="red")
boxplot(store2_5[1:455,]$units~store2_5[1:455,]$weekday)


store2_5$dewpoint[which(is.na(store2_5$dewpoint))]=c(20,57,57)
store2_5$avgspeed[which(is.na(store2_5$avgspeed))]<-mean(store2_5$avgspeed[which(!is.na(store2_5$avgspeed))])


store2_5_455<-subset(store2_5,date<"2013-04-01")


model1<-glm(units~date1+sin(2*pi*date1/7)+cos(2*pi*date1/7)
             +sin(2*pi*date1/30)+cos(2*pi*date1/30)
            +sin(2*pi*date1/365)+cos(2*pi*date1/365)
           +tavg+depart
           +avgspeed + preciptotal
           +snowfall + storm          
           ,family="poisson",data=store2_5_455)
summary(model1)
Finalmodel<-step(model1)



summary(Finalmodel)
anova(Finalmodel)

par(mfcol=c(1,1))
plot(store2_5_455$date,store2_5_455$units,xlab="date",ylab="units sold",main="item 5 at store 2")
lines(store2_5_455$date,store2_5_455$units)
points(store2_5_455$date,predict(Finalmodel,newdata=store2_5_455,type="response"),col="red")
plot(Finalmodel)
plot(Finalmodel$fitted,store2_5[1:455,]$units)
qqnorm(Finalmodel$resid)
qqline(Finalmodel$resid)

anova(Finalmodel)
test2_test=subset(test_full,store_nbr==2&item_nbr==5)
pred<-predict(Finalmodel,newdata=test2_test,type="response")

test$



#moving average
f7=rep(1/7,7)
units_lag<-filter(store2_5$units[1:455],f7,sides=2)
plot(units_lag)

model1<-lm(units_lag~date1+sin(2*pi*as.integer(date1)/7)+cos(2*pi*as.integer(date1)/7)
           +sin(2*pi*as.integer(date1)/30)+cos(2*pi*as.integer(date1)/30)
           +store2_5$tavg[1:455]+dewpoint+store2_5$depart[1:455]
           +store2_5$avgspeed[1:455]
           #+store2_5$storm[1:455]+store2_5$storm[2:456]+store2_5$storm[3:457]            
           #+store2_5$weekday[1:455]
           +store2_5$preciptotal[2:456]
           +store2_5$storm[1:455]
           +store2_5$storm[2:456]
           +store2_5$storm[3:457]
           ,data=store2_5[3:457,])
summary(model1)
step(model1)






store2_5$units_lag<-NULL
store2_5$units_lag[1:455]<-units_lag
lm2<-lm(units_lag~weekday*storm+tavg+depart+dewpoint+preciptotal+snowfall,data=store2_5[1:455,])
summary(lm2)


par(mfcol=c(1,1))
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






acf(store2_5$units[1:455],main="item=5")
pacf(store2_5$units[1:455],main="item 5 at store 2")

#there seems not much auto-correlation among the observations:

# regress on the future weather (people may tend to prepare for the bad weather), some people may tend to buy after the weather
# in case next time. the the next two weather
# people may tend to buy stuff one weekends, there will be cycles 
# quarter effect this may confound with the weather over the year. We will put this in the model, a little bit overfitted 
# code the days around the storm as the 1, else as 0.
# 

# averaged units over 7 days

lm1<-lm(units~storm+tavg+depart+dewpoint+preciptotal+snowfall,data=store2_5[1:455,])
summary(lm1)

f7=rep(1/7,7)
units_lag<-filter(store2_5$units[1:455],f7,sides=2)
plot(y_lag)
store2_5$units_lag<-NULL
store2_5$units_lag[1:455]<-units_lag
lm2<-lm(units_lag~weekday*storm+tavg+depart+dewpoint+preciptotal+snowfall,data=store2_5[1:455,])
summary(lm2)


lm3<-lm(units~date1+weekday*storm+sin(2*pi*as.integer(date)/30)+
          sin(2*pi*as.integer(date)/90)+
          tavg+depart+dewpoint+preciptotal+snowfall,data=store2_5[1:455,])
summary(lm3)


data<-store2_5[1:455,]
storm_index<-which(store2_5[1:455,]$storm==1)
storm_indexL_R<-cbind((storm_index-1),storm_index+1)
storm<-NULL
for(i in 1:nrow(storm_indexL_R)){
  storm<-c(storm,storm_indexL_R[i,1],storm_indexL_R[i,2])
}
storm<-unique(storm)
store2_5[storm,]$storm=1
time<-1:455
lm4<-lm(units~date1+weekday*storm+sin(2*pi*as.integer(date)/30)
        +tavg+depart+dewpoint+preciptotal+snowfall,data=store2_5[1:455,])
summary(lm4)
step(lm4,k=450)

drop1(lm4,k=log(450))

lm5<-lm(units~date1+sin(2*pi*as.integer(date)/30)
        +depart+preciptotal+snowfall,data=store2_5[1:455,])
summary(lm5)
step(lm5)
?step
drop1(lm5)
#remove the missing value before when using step


storm_1<-store2_5[1:455,]$storm[3:455]
precip_1<-store2_5[1:455,]$preciptotal[3:455]
Model<-lm(units~date1+weekday+precip_1+sin(2*pi*as.integer(date)/30),data=store2_5[1:453,])
summary(Model)
plot(store2_5[1:455,]$date,store2_5[1:455,]$units)
points(store2_5[1:455,]$date,Model$fitted,col="red")



unitdiff<-store2_5$units[3:455]-store2_5$units[2:454]
Model<-lm(unitdiff~date1+weekday+storm_1+sin(2*pi*as.integer(date)/30),data=store2_5[1:453,])
summary(Model)





