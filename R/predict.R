#This file is used to predict the units for 
# Walmart kaggle competition
# using poisson regression


## there are four patterns: some data +no data; no data +some data; some data +some data (seasonal, everyday stuff)

store14_50<-subset(train,store_nbr==14&item_nbr==50)
#store14_50<-subset(store14_50,date>"2012-06-01"&date<"2013-03-31")
store14_50_test=subset(test,store_nbr==14&item_nbr==50)

#store14_50$dewpoint[which(is.na(store14_50$dewpoint))]=c(20,57,57)
store14_50$avgspeed[which(is.na(store14_50$avgspeed))]<-mean(store14_50$avgspeed[which(!is.na(store14_50$avgspeed))])

storm_index<-which(store14_50[1:455,]$storm==1)
storm=NULL
for(i in 1:length(storm_index)){
  storm<-c(storm,(storm_index[i]-2),(storm_index[i]),(storm_index[i]+1),(storm_index[i]+2))
}
storm<-unique(storm)
store14_50[storm,]$storm=1


glmmodel<-glm(units~date1+sin(2*pi*date1/7)+cos(2*pi*date1/7)
              +sin(2*pi*date1/30)+cos(2*pi*date1/30)
              +sin(2*pi*date1/365)+cos(2*pi*date1/365)
              #+store14_50$dewpoint linear in tavg
              +depart
              +avgspeed
              +tavg
              +storm
              ,family="poisson",data=store14_50)

#summary(glmmodel)
#can I have my final model
Finalmodel<-step(glmmodel)

plot(store14_50$date,predict(glmmodel,type="response"),ylim=c(0,1))
points(store14_50$date,units_integer,col="red")

test_sub<-subset(test,store_nbr==14&item_nbr==50)

#do I need to put the exact subset of variables when predicting?
test.predict<-predict(test_sub,Finalmodel,type="response")
subset(test,store_nbr==14&item_nbr==50)$units<-test.predict


par(mfcol=c(1,1))

plot(store14_50$date,predict(Finalmodel,type="response"),
     xlab="date",ylab="units sols",main="Poisson regression",col="red")
points(store14_50$date,units_integer)

plot(store14_50$date,units_integer-predict(Finalmodel,type="response"))

summary(Finalmodel)
anova(Finalmodel)
pred<-predict(Finalmodel,store14_test)

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