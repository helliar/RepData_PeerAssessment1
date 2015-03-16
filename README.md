# RepData_PeerAssessment1
setwd("C:\\Hailiang\\my training\\Reproducible Research\\assignment1");
data=read.csv("activity.csv");
data1<-na.omit(data);
data2<-aggregate(.~date,data=data1,sum);
hist(data2$steps)
mean(data2$steps)
median(data2$steps)
data3<-aggregate(.~interval,data=data1,mean);
plot(data3$interval, data3$steps)
data3[data3$steps==max(data3$steps),1]
data4<-data[is.na(data),];
nrow(data4)
data5=data3[,1:2];
data6=data4[,2:3];
mergedata=merge(data5,data6,by="interval");
mergedata[,4]=mergedata[,1];
Newdata<-mergedata[,c(2,3,4)];
names(Newdata)<-c("steps","date","interval");
Newdata1<-Newdata[order(Newdata$date),];
q1=rbind(data1,Newdata1);
q2=q1[order(q1$date,q1$interval),];
dataq2<-aggregate(.~date,data=q2,sum);
hist(dataq2$steps)
mean(dataq2$steps);
median(dataq2$steps);
q3<-q2;
q3[,4]=q2[,2]
names(q3)<-c("steps","date","interval","day");
q3$day<-weekdays(as.Date(q3$date));
q3$day[q3$day=="Monday"]="weekday";
q3$day[q3$day=="Tuesday"]="weekday";
q3$day[q3$day=="Wednesday"]="weekday";
q3$day[q3$day=="Thursday"]="weekday";
q3$day[q3$day=="Friday"]="weekday";
q3$day[q3$day=="Saturday"]="weekend";
q3$day[q3$day=="Sunday"]="weekend";
q4<-q3[q3$day=="weekday",];
q5<-q3[q3$day=="weekend",];
q41<-aggregate(.~interval,data=q4[,1:3],mean);
q51<-aggregate(.~interval,data=q5[,1:3],mean);
plot(q41$interval, q41$steps)
plot(q51$interval, q51$steps)
