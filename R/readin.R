##read data and chnage date as Date type, units into log(units)
# source("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/code/R/readin.R")
rm(list=ls())
train<-read.csv("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/train.csv",header=T)
weather<-read.csv("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/weather.csv",header=T)
test<-read.csv("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/test.csv",header=T)
test_full<-read.csv("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/test_full.csv",header=T)
key<-read.csv("/afs/cs.wisc.edu/u/s/o/songwang/Desktop/2015 S/998proj3/key.csv",header=T)


train$date<-as.Date(as.character(train$date))
test$date<-as.Date(as.character(test$date))

test$date1<-as.integer(test$date-test$date[1])
train$date1<-as.integer(train$date-train$date[1])
max(train$date1)
max(test$date1)
#train$units<-log(1+train$units)

weather$date<-as.Date(as.character(weather$date))
weather$tmax<-as.numeric(weather$tmax)
weather$tmin<-as.numeric(weather$tmin)
weather$depart<-as.numeric(weather$depart)

weather14<-weather[which(weather$station_nbr==14),] #weather station 14
#weather14<-weather14[,c(1,2,3,4,6,13,14,15)]

a<-as.character(weather$preciptotal)
a<-gsub(pattern="\\s",replacement="",x=a)
a<-gsub(pattern="[M]",replacement="0.00",x=a)
a<-gsub(pattern="[T]",replacement="0.005",x=a)
weather$preciptotal<-as.numeric(a)

b<-as.character(weather$snowfall)
b<-gsub(pattern="\\s",replacement="",x=b)
b<-gsub(pattern="[M]",replacement="0.0",x=b)
b<-gsub(pattern="[T]",replacement="0.05",x=b)
weather$snowfall<-as.numeric(b)


#precise logic to define the snow/rain events provided the adiministor of the competition.
library(stringr)
inches_snow=2.0
## logic operator cannot be applied elementwise. I will convert as numeric
weather$snow = ((str_detect(weather$codesum,"SN") + str_detect(weather$codesum,"SG" )>0)*
    (weather$snowfall >= inches_snow))


inches_rain=1.0
weather$rain =   (str_detect(weather$codesum,"RA"))* (!str_detect(weather$codesum,"SN")) *
    (weather$preciptotal>= inches_rain )

weather$storm<-as.integer(weather$snow +weather$rain>0)

#code weather around a storm as 1
storm_index<-which(weather$storm==1)
storm=NULL
for(i in 1:length(storm_index)){
  storm<-c(storm,(storm_index[i]-3):(storm_index[i]+3))
}
storm<-unique(storm)
weather[storm,]$storm=1

weather$tmin<-as.numeric(as.character(weather$tmin))
weather$tmax<-as.numeric(as.character(weather$tmax))
#weather$tavg<-NULL
#station 7 have no avg temperature
a<-weather$tavg
a<-gsub(pattern="\\s",replacement="",x=a)
a<-as.numeric(as.character(a))[]
for( i in 1:length(a)){
  if (is.na(a[i])){
    a[i]<-(weather$tmin[i]+weather$tmax[i])/2
  }
}
weather$tavg<-a


# weather$wetbulb<-as.numeric(as.character(weather$wetbulb))
# weather$heat<-as.numeric(as.character(weather$heat))
# weather$cool<-as.numeric(as.character(weather$cool))
weather$avgspeed[which(is.na(weather$avgspeed))]=mean(weather$avgspeed[which(!is.na(weather$avgspeed))])
weather$avgspeed<-as.numeric(as.character(weather$avgspeed))



