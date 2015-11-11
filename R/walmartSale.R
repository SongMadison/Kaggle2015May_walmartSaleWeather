setwd("/afs/cs.wisc.edu/u/s/o/songwang/RScript")
#read in the data with some prelimilary cleaning
source("readin.R")

#focus on the four stores around weather station 14, they 2,16,38,42
#how to generate mutiple graphs
# library(lattice)
# for(store_id in c(2,16,38,42)){
#   xyplot(units~date|item_nbr,data=subset(train,store_nbr==store_id),ylab="units sold",
#          main=paste("units sold at store ",store_id),
#          panel=function(x,y){
#            panel.xyplot(x,y)
#            panel.lmline(x,y,lty=2,col="red")  
#          }
#   )  
# }


##########
###  Duty 1:  Determine whether Walmart discontinued that product  (on data from store 2)
##########

weather14<-subset(weather,station_nbr==14)
store2_train=subset(train,store_nbr==2)
store2_dates<-unique(store2_train$date)
weather14_train<-subset(weather14,date%in% store2_dates)
weather14_train=weather14_train[,c("tavg","depart","dewpoint","snowfall","snow","preciptotal","rain","storm")]


# weather14_train_expanded<-NULL   
# #really slow!!! 
# for(i in 1:nrow(weather14_train)){
#   for(j in 1:111){
#     weather14_train_expanded<-rbind(weather14_train_expanded,weather14_train[i,])
#   }  
# }


#part a) create the store2_train and test set 
#first just deal with store2  # 111 product + about 4 1300days, +weather info
store2_train<-subset(train,store_nbr==2)
store2_test<-subset(test,store_nbr==2)
#weather 111 a loop 
store2_train$tavg<-rep(weather14_train$tavg,each=111)
store2_train$depart<-rep(weather14_train$depart,each=111)
store2_train$dewpoint<-rep(weather14_train$dewpoint,each=111)
store2_train$snowfall<-rep(weather14_train$snowfall,each=111)
store2_train$snow<-rep(weather14_train$snow,each=111)
store2_train$preciptotal<-rep(weather14_train$preciptotal,each=111)
store2_train$rain<-rep(weather14_train$rain,each=111)
store2_train$storm<-rep(weather14_train$storm,each=111)
store2_train$weekday<-as.factor(as.integer(store2_train$date)%%7)
levels(store2_train$weekday)=c("Thus","Fri","Sat","Sun","Mon","Tues","Wed")
#store2_train$holiday<-as.factor(as.integer(store2_train$date)%%7)


store2_93<-subset(store2_train,item_nbr==93)
raindays<-numeric(nrow(store2_93))
for(i in 1:nrow(store2_93)){
  for(j in 0:(i-1)){
    if(store2_93$preciptotal[i-j]>0.02|store2_93$snowfall[i-j]>0.05){
      raindays[i]=raindays[i]+1
    }else {
      break
    }
  }
}
store2_train$raindays<-rep(raindays,each=111)
#store2_train
#[1] "date"        "store_nbr"   "item_nbr"    "units"       "date1"       "tavg"        "depart"      "dewpoint"    "snowfall"   
#[10] "snow"        "preciptotal" "rain"        "storm"       "raindays"  

#similary store2_test
weather14<-subset(weather,station_nbr==14)
store2_dates<-unique(store2_test$date)
weather14_test<-subset(weather14,date%in% store2_dates)
weather14_test=weather14_test[,c("tavg","depart","dewpoint","snowfall","snow","preciptotal","rain","storm")]

store2_test$tavg<-rep(weather14_test$tavg,each=111)
store2_test$depart<-rep(weather14_test$depart,each=111)
store2_test$dewpoint<-rep(weather14_test$dewpoint,each=111)
store2_test$snowfall<-rep(weather14_test$snowfall,each=111)
store2_test$snow<-rep(weather14_test$snow,each=111)
store2_test$preciptotal<-rep(weather14_test$preciptotal,each=111)
store2_test$rain<-rep(weather14_test$rain,each=111)
store2_test$storm<-rep(weather14_test$storm,each=111)
unique(store2_train$item_nbr)
store2_93<-subset(store2_test,item_nbr==93)
raindays<-numeric(nrow(store2_93))
for(i in 1:nrow(store2_93)){
  for(j in 0:(i-1)){
    if(store2_93$preciptotal[i-j]>0.02|store2_93$snowfall[i-j]>0.05){
      raindays[i]=raindays[i]+1
    }else {
      break
    }
  }
}
store2_test$raindays<-rep(raindays,each=111)



###############
###part b    ##
###############



#######################################################
### extract weahter data for station_nbr=14 ###########
#######################################################

#combine the weather14 and store=2,6,38,42
#which(key$station_nbr==14)

unique(store2_train[which(store2_train$storm==1),]$date )
#[1] "2012-01-24" "2012-03-19" "2012-04-13" "2012-04-30" "2012-05-29" "2012-08-18" "2012-09-26" "2012-10-13" "2013-02-12"


####draw a plot for item 93
store2_93<-subset(store2_train,item_nbr==93)
store2_93_test<-subset(store2_test,item_nbr==93)
par(mfcol=c(1,1))
plot(store2_93$date,store2_93$units,ylab="units sold",main="item 93 at store 2")
lines(store2_93$date,store2_93$units)
points(store2_93[which(store2_93$storm==1),]$date,store2_93[which(store2_93$storm==1),]$units,col="red")
head(store2_93)

points(store2_93_test$date,rep(0,nrow(store2_93_test)),col="red")
lines(store2_93_test$date,store2_93_test$units)
plot(weather14$date,weather14$preciptotal,ylab="precipation",main="precipation at station 14")
lines(weather14$date,as.numeric(as.character(weather14$preciptotal)))
points(weather14[which(weather14$storm==1),]$date,
       as.numeric(as.character(weather14[which(weather14$storm==1),]$preciptotal))
       ,col="red")









#clearly, from the units~date plot, we can tell that walmart discontinued selling this product around Feb,2013. 
#erefore we predict the sale as 0.

store2_5<-subset(store2_train,item_nbr==5)
#store2_5<-subset(store2_5,date>"2012-06-01"&date<"2013-03-31")
store2_5_test=subset(store2_test,item_nbr==5)
par(mfcol=c(2,1))
plot(store2_5$date,store2_5$units,ylab="units sold",main="item 5 at store 2")
lines(store2_5$date,store2_5$units)
points(store2_5[which(store2_5$storm==1),]$date,store2_5[which(store2_5$storm==1),]$units,col="red")
boxplot(store2_5$units~store2_5$weekday)
model1<-lm(units~date+sin(2*pi*as.integer(date)/7)+cos(2*pi*as.integer(date)/7),data=store2_5)
# preciptotal
#month effect is difficult to specify
summary(model1)
points(store2_5[which(store2_5$storm==1),]$date,store2_5[which(store2_5$storm==1),]$units,col="red")
points(store2_5$date,model1$fitted,col="red")

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
storm_indexL_R<-cbind((storm_index-3),storm_index+3)
storm<-NULL
for(i in 1:nrow(storm_indexL_R)){
  storm<-c(storm,storm_indexL_R[i,1]:storm_indexL_R[i,2])
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





units_lag<-filter(store2_93$units[1:450],f7,sides=2)
plot(units_lag)
plot(y_lag)
abline(lm(units_lag~y_lag))
summary(lm1<-lm(units_lag~y_lag))
plot(lm1)

item5<-subset(train,item_nbr==5&store_nbr%in%c(2,16,38,42))
head(item5)
boxplot(item5$units~item5$store_nbr)
store42_5<-subset(item5,store_nbr==42)
store42_5<-cbind(store42_5,store2_5[,-c(1:5)])
lm4<-lm(units_lag[-(454:455)]~date1  #+weekday #+sin(2*pi*as.integer(date)/30)
        +storm+tavg+depart+dewpoint+preciptotal+snowfall,data=store2_5[3:455,])
summary(lm4)

units_lag<-filter(store2_5$units[1:455],f7,sides=2)



par(mfcol=c(1,1))
plot(store42_5$date,store42_5$units)
plot(store2_5$date,store2_5$units)
store2_85<-subset(store2_train,item_nbr==85)
par(mfcol(c(1,1))
plot(store2_85$date,store2_85$units,ylab="units sold",main="item 85 at store 2")
lines(store2_85$date,store2_85$units)
points(store2_85[which(store2_85$storm==1),]$date,store2_85[which(store2_85$storm==1),]$units,col="red")
head(store2_85)
store2_85_test<-subset(store2_test,item_nbr==85)
points(store2_85_test$date,rep(1,nrow(store2_85_test)),col="red")


plot(weather14$date,weather14$preciptotal,ylab="precipation",main="precipation at station 14")
lines(weather14$date,as.numeric(as.character(weather14$preciptotal)))
points(weather14[which(weather14$storm==1),]$date,
       as.numeric(as.character(weather14[which(weather14$storm==1),]$preciptotal))
       ,col="red")
store2_85$snowfall<-weather14[ind,]$snowfall
store2_85$preciptotal<-weather14[ind,]$preciptotal
store2_85$storm<-weather14[ind,]$storm
store2_85$tavg<-weather14[ind,]$tavg
store2_85$dewpoint<-weather14[ind,]$dewpoint
store2_85$resultspeed<-weather14[ind,]$resultspeed
store2_85$depart<-weather14[ind,]$depart

store2_85_test=subset(store2_test,item_nbr==85)
store2_85_test$storm<-weather14[ind1,]$storm
store2_85_test$snowfall<-weather14[ind1,]$snowfall
store2_85_test$preciptotal<-weather14[ind1,]$preciptotal
store2_85_test$tavg<-weather14[ind1,]$tavg
store2_85_test$dewpoint<-weather14[ind1,]$dewpoint
store2_85_test$resultspeed<-weather14[ind1,]$resultspeed
store2_85_test$depart<-weather14[ind1,]$depart

lm1<-lm(units~storm+snowfall+preciptotal+tavg+dewpoint+resultspeed+depart,data=store2_85[1:450,])
summary(lm1)
store2_85_test$units<-predict(lm1,store2_85_test[,5:11])
#create some new variables
# raindays, rainfall in 7days, new reponse with shifting
# store2_93

ind<-NULL
for(i in 1:nrow(weather14)){
  if (weather14$date[i]%in% store2_93$date){
    ind<-c(ind,i)
  }
}

store2_93_test=subset(store2_test,item_nbr==93)
ind1<-NULL
for(i in 1:nrow(weather14)){
  if (weather14$date[i]%in% store2_93_test$date){
    ind1<-c(ind1,i)
  }
}

store2_93$snowfall<-weather14[ind,]$snowfall
store2_93$preciptotal<-weather14[ind,]$preciptotal
store2_93$storm<-weather14[ind,]$storm
store2_93$tavg<-weather14[ind,]$tavg
store2_93$dewpoint<-weather14[ind,]$dewpoint
store2_93$resultspeed<-weather14[ind,]$resultspeed
store2_93$depart<-weather14[ind,]$depart

store2_93_test=subset(store2_test,item_nbr==93)
store2_93_test$storm<-weather14[ind1,]$storm
store2_93_test$snowfall<-weather14[ind1,]$snowfall
store2_93_test$preciptotal<-weather14[ind1,]$preciptotal
store2_93_test$tavg<-weather14[ind1,]$tavg
store2_93_test$dewpoint<-weather14[ind1,]$dewpoint
store2_93_test$resultspeed<-weather14[ind1,]$resultspeed
store2_93_test$depart<-weather14[ind1,]$depart




lm1<-lm(units~storm+snowfall+preciptotal+tavg+dewpoint+resultspeed+depart,data=store2_93[1:450,])
summary(lm1)
store2_93_test$units<-predict(lm1,store2_93_test[,5:11])


lm2<-lm(units_lag~y_lag+raindays[1:450]+storm+snowfall+preciptotal+tavg+dewpoint+resultspeed+depart,data=store2_93[1:450,])
summary(lm2)
storm_index<-which(weather14[ind,]$storm==1)
storm_indexL_R<-cbind((storm_index-3),storm_index+3)
nearstorm=NULL
for(i in 1:nrow(storm_indexL_R)){
  a=storm_indexL_R[i,1]
  b=storm_indexL_R[i,2]
  nearstorm<-c(nearstorm,(a:b))
}
near_store2_93<-store2_93[nearstorm,]
plot(near_store2_93[,-(1:3)])

model<-lm(units~storm+snowfall+preciptotal,data=near_store2_93)
summary(model)
raindays<-numeric(nrow(store2_93))
for(i in 1:nrow(store2_93)){
  for(j in 0:(i-1)){
    if(store2_93$preciptotal[i-j]>0.02|store2_93$snowfall[i-j]>0.05){
      raindays[i]=raindays[i]+1
    }else {
      break
    }
  }
}
boxplot(store2_93$units~raindays)

raintotal3<-store2_93$preciptotal[1:450]
f5=rep(1/5,5)
y_lag<-filter(raintotal3,f5,sides=2)
plot(y_lag)

units_lag<-filter(store2_93$units[1:450],f5,sides=2)
plot(units_lag)
plot(y_lag)
abline(lm(units_lag~y_lag))
summary(lm1<-lm(units_lag~y_lag))
plot(lm1)


store2_93_test=subset(store2_test,item_nbr==93)
store2_93_test$storm<-weather14[ind1,]$storm
store2_93_test$snowfall<-weather14[ind1,]$snowfall
store2_93_test$preciptotal<-weather14[ind1,]$preciptotal
store2_93_test$tavg<-weather14[ind1,]$tavg
store2_93_test$dewpoint<-weather14[ind1,]$dewpoint
store2_93_test$resultspeed<-weather14[ind1,]$resultspeed
store2_93_test$depart<-weather14[ind1,]$depart

snow_lag<-filter(weather14[ind,]$snowfall[1:400],f5,sides=2)
plot(snow_lag,units_lag)
summary(lm1<-lm(store2_93$units[1:400]~y_lag+raindays[1:400]+weather14[ind,]$tmin
                [1:400]))
summary(lm1<-lm(units_lag~y_lag+raindays[1:400]+weather14[ind,]$tmin
                [1:400]))


store2_5<-subset(store2,item_nbr==5)
plot(store2_5$date,store2_5$units)

par(mfcol=c(2,1))
plot(store2_5$date,store2_5$units,ylab="units sold",main="item 5 at store 2 (log scale)")
lines(store2_5$date,store2_5$units)
points(store2_5[which(store2_5$storm==1),]$date,store2_5[which(store2_5$storm==1),]$units,col="red")
head(store2_5)
store2_5_test<-subset(store2_test,item_nbr==5)
points(store2_5_test$date,rep(5,nrow(store2_5_test)),col="red")

plot(weather14$date,weather14$preciptotal,ylab="precipatation",main="precipatation at station 14")
lines(weather14$date,as.numeric(as.character(weather14$preciptotal)))
points(weather14[which(weather14$storm==1),]$date,
       as.numeric(as.character(weather14[which(weather14$storm==1),]$preciptotal))
       ,col="red")

raintotal3<-store2_5$preciptotal[1:400]
f5=rep(1/5,5)
y_lag<-filter(raintotal3,f5,sides=2)
plot(y_lag)

units_lag<-filter(store2_5$units[1:400],f5,sides=2)
plot(units_lag)
plot(y_lag)
abline(lm(units_lag~y_lag))
summary(lm1<-lm(units_lag~y_lag))
plot(lm1)

snow_lag<-filter(weather14[ind,]$snowfall[1:400],f5,sides=2)
plot(snow_lag,units_lag)
summary(lm1<-lm(units_lag~y_lag+raindays[1:400]+weather14[ind,]$tmin
                [1:400]))

par(mfcol=c(2,1))
store2_11<-subset(store2,item_nbr==11)
plot(store2_11$date,store2_11$units,ylab="units sold",main="item 11 at store 2")
lines(store2_11$date,store2_11$units)
points(store2_11[which(store2_11$storm==1),]$date,store2_11[which(store2_11$storm==1),]$units,col="red")
head(store2_11)
store2_11_test<-subset(store2_test,item_nbr==11)
points(store2_11_test$date,rep(11,nrow(store2_11_test)),col="red")

plot(weather14$date,weather14$preciptotal,ylab="precipation",main="precipation at station 14")
lines(weather14$date,as.numeric(as.character(weather14$preciptotal)))
points(weather14[which(weather14$storm==1),]$date,
       as.numeric(as.character(weather14[which(weather14$storm==1),]$preciptotal))
       ,col="red")
plot(as.factor(weather14[ind,]$preciptotal),store2_11$units)
summary(lm(store2_11$units~weather14[ind,]$preciptotal))


