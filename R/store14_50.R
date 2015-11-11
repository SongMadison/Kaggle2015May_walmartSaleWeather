#read in the data with some prelimilary cleaning
source("readin.R")


#item=50 at store14

weather16<-subset(weather,station_nbr==16)
weather16$avgspeed<-as.numeric(as.character(weather16$avgspeed))
weather16$avgspeed[which(is.na(weather16$avgspeed))]=mean(weather16$avgspeed)
store14_train=subset(train,store_nbr==14)
store14_dates<-unique(store14_train$date)
weather16_train<-subset(weather16,date%in% store14_dates)
weather16_train=weather16_train[,c("tavg","depart","dewpoint","snowfall","snow",
                                   "preciptotal","rain","storm","avgspeed")]


#part a) create the store14_train and test set 
#first just deal with store14  # 111 product + about 4 1300days, +weather info
store14_train<-subset(train,store_nbr==14)
store14_test<-subset(test,store_nbr==14)
#weather 111 a loop 
store14_train$weekday<-as.factor(as.integer(store14_train$date)%%7)
levels(store14_train$weekday)=c("Thus","Fri","Sat","Sun","Mon","Tues","Wed")
store14_train$tavg<-rep(weather16_train$tavg,each=111)
store14_train$depart<-rep(weather16_train$depart,each=111)
store14_train$dewpoint<-rep(weather16_train$dewpoint,each=111)
store14_train$snowfall<-rep(weather16_train$snowfall,each=111)
store14_train$snow<-rep(weather16_train$snow,each=111)
store14_train$preciptotal<-rep(weather16_train$preciptotal,each=111)
store14_train$rain<-rep(weather16_train$rain,each=111)
store14_train$storm<-rep(weather16_train$storm,each=111)
store14_train$avgspeed<-rep(weather16_train$avgspeed,each=111)
#store14_train$h-oliday<-as.factor(as.integer(store14_train$date)%%7)
holiday<-c("2012-01-02","2012-01-16","2012-02-20","2012-05-28","2012-07-04","2012-09-03","2012-10-08"
           ,"2012-11-12","2012-11-22","2012-12-25","2013-01-01","2013-01-19","2013-02-16"
)

data<-subset(store14_train,date%in%holiday)

# store14_50<-subset(store14_train,item_nbr==50)
# raindays<-numeric(nrow(store14_50))
# for(i in 1:nrow(store14_50)){
#   for(j in (i-2):(i+10)){
#     if(store14_50$preciptotal[j]>0.005|store14_50$snowfall[j]>0.05){
#       raindays[i]=raindays[i]+1
#     }else {
#       break
#     }
#   }
# }
# store14_train$raindays<-rep(raindays,each=111)

store14_50<-subset(store14_train,item_nbr==50)

###############
###part b    ##
###############



#######################################################
### extract weahter data for station_nbr=14 ###########
#######################################################

#combine the weather16 and store=2,6,38,42
#which(key$station_nbr==14)

unique(store14_train[which(store14_train$storm==1),]$date )
#[1] "2012-01-24" "2012-03-19" "2012-04-13" "2012-04-30" "2012-05-29" "2012-08-18"
#"2012-09-26" "2012-10-13" "2013-02-12"












store14_50<-subset(store14_train,item_nbr==50)
#store14_50<-subset(store14_50,date>"2012-06-01"&date<"2013-03-31")
store14_50_test=subset(store14_test,item_nbr==50)
par(mfcol=c(2,1))
plot(store14_50$date,store14_50$units,ylab="units sold",main="item 5 at store 2")
lines(store14_50$date,store14_50$units)
points(store14_50[which(store14_50$storm==1),]$date,store14_50[which(store14_50$storm==1),]$units,col="red")
boxplot(store14_50[1:455,]$units~store14_50[1:455,]$weekday)


store14_50$dewpoint[which(is.na(store14_50$dewpoint))]=c(20,57,57)
store14_50$avgspeed[which(is.na(store14_50$avgspeed))]<-mean(store14_50$avgspeed[which(!is.na(store14_50$avgspeed))])


store14_50_455<-subset(store14_50,date<"2013-04-01")


storm_index<-which(store14_50[1:455,]$storm==1)
storm=NULL
for(i in 1:length(storm_index)){
  storm<-c(storm,(storm_index[i]-2),(storm_index[i]-1),(storm_index[i]+1),(storm_index[i]+2))
}
storm<-unique(storm)
store14_50[storm,]$storm=1


model1<-lm(store14_50$units[1:455]~date1+sin(2*pi*as.integer(date1)/7)+cos(2*pi*as.integer(date1)/7)
           +sin(2*pi*as.integer(date1)/30)+cos(2*pi*as.integer(date1)/30)
           +store14_50$tavg[1:455]+dewpoint
           #+store14_50$depart[1:455]
           +store14_50$avgspeed[1:455]
           #+store14_50$storm[1:455]+store14_50$storm[2:456]+store14_50$storm[3:457]            
           #+store14_50$weekday[1:455]
           +store14_50$preciptotal[2:456]
           +store14_50$snowfall[2:456]
           +store14_50$storm[1:455]
           ,data=store14_50[1:455,])
summary(model1)
step(model1)


Finalmodel<-lm(store14_50$units[1:455]~date1+cos(2*pi*as.integer(date1)/7)
               +store14_50$tavg[1:455]
               +store14_50$dewpoint[1:455]
               #+store14_50$storm[1:455]
               ,data=store14_50[1:455,])

summary(Finalmodel)
anova(Finalmodel)

par(mfcol=c(1,1))
plot(store14_50_455$date,store14_50_455$units,xlab="date",ylab="units sold",main="item 5 at store 2")
lines(store14_50_455$date,store14_50_455$units)
points(store14_50_455$date,Finalmodel$fitted,col="red")
plot(Finalmodel)
plot(Finalmodel$fitted,store14_50[1:455,]$units)
qqnorm(Finalmodel$resid)
qqline(Finalmodel$resid)


units_indicator<-as.factor((exp(store14_50$units)-1)>0)

units_integer<-store14_50$units

glmmodel<-glm(units_integer~date1+sin(2*pi*as.integer(date1)/7)+cos(2*pi*as.integer(date1)/7)
    +sin(2*pi*as.integer(date1)/30)+cos(2*pi*as.integer(date1)/30)
    +sin(2*pi*as.integer(date1)/365)+cos(2*pi*as.integer(date1)/365)
    #+store14_50$dewpoint linear in tavg
    +store14_50$depart
    +store14_50$avgspeed
    +store14_50$tavg
    +store14_50$storm
    ,family="poisson",data=store14_50)

summary(glmmodel)

Finalmodel <- step(glmmodel)


par(mfcol = c(1,1))

plot(store14_50$date,predict(Finalmodel, type = "response"),
     xlab="date",ylab="units sols",main="Poisson regression",col="red")
points(store14_50$date,units_integer)

plot(store14_50$date,units_integer-predict(Finalmodel,type="response"))

summary(Finalmodel)
anova(Finalmodel)

store14_test$date1<-as.integer(store14_test$date-as.Date("2012-01-01"))
pred<-predict(Finalmodel,store14_50_test)

hist(units3,breaks=20)
a<-table(units3)
# 1  2  3  4  5  6  7  8  9 10 13 14 15 16 17 23 25 29 37 
# 55 22 10  6  6  5  3  3  6  2  1  1  1  1  1  1  1  1  1
a<-cbind(as.numeric(names(a)),as.matrix(a))
x<-a[,2]/sum(a[,2])
mean(c(rep(0,150), units3))
plot(x)

for(lambda in seq(1,4,0.1)){
  for( i in a[,1]){
    
  }
  
}

f7=rep(1/7,7)
units_lag<-filter(store14_50$units[1:455],f7,sides=2)
plot(units_lag)

model1<-lm(units_lag~date1+sin(2*pi*as.integer(date1)/7)+cos(2*pi*as.integer(date1)/7)
           +sin(2*pi*as.integer(date1)/30)+cos(2*pi*as.integer(date1)/30)
           +store14_50$tavg[1:455]+dewpoint+store14_50$depart[1:455]
           +store14_50$avgspeed[1:455]
           #+store14_50$storm[1:455]+store14_50$storm[2:456]+store14_50$storm[3:457]            
           #+store14_50$weekday[1:455]
           +store14_50$preciptotal[2:456]
           +store14_50$storm[1:455]
           ,data=store14_50[1:455,])
summary(model1)
step(model1)

Model<-lm(units_lag~date1+sin(2*pi*as.integer(date1)/30)
+store14_50$tavg[1:455]+dewpoint
+store14_50$avgspeed[1:455]
+store14_50$storm[3:457]
,data=store14_50[1:455,])
summary(Model)

