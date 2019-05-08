setwd("F:/thesis data")
mydata2<-read.csv("completemetrodata.csv")
head(mydata2)
mydata2$From.Date<-NULL
sum(is.na(mydata2))
mydata2$PM10[mydata2$PM10==0]<-NA
mydata2$AT[mydata2$AT==0]<-NA
mydata2$SR[mydata2$SR==0]<-NA
mydata2$WS[mydata2$WS==0]<-NA
mydata2$RH[mydata2$RH==0]<-NA
mydata2$WD[mydata2$WD==0]<-NA
sum(is.na(mydata2))
mydata2_clean1<-na.omit(mydata2)
summary(mydata2)
boxplot(mydata2)
bench1<-568.07+1.5*IQR(mydata2_clean1$PM10)
bench2<-216.99+1.5*IQR(mydata2_clean1$SR)
mydata3_clean1<-subset(mydata2_clean1,PM10<=bench1)
mydata4_clean1<-subset(mydata3_clean1,SR<=bench2)
boxplot(mydata4_clean1)
bench3<-174.09-1.5*IQR(mydata2_clean1$SR)
mydata5_clean1<-subset(mydata4_clean1,SR>=bench3)
boxplot(mydata5_clean1)
#additional variable
pdpm10<-NULL
for(i in 2:length(mydata5_clean1$PM10))
{
  pdpm10[i]<-mydata5_clean1$PM10[i-1]
}
pdpm10[1]<-mydata5_clean1$PM10[mean(mydata5_clean1$PM10)]
mydata_clean5<-data.frame(mydata5_clean1,pdpm10)
data=mydata_clean5
samplesize = 0.80 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

model3<-lm(log(datatrain$PM10)~(datatrain$RH)+log(datatrain$pdpm10)+datatrain$WS,datatrain)
summary(model3)
library(VIF)
library(car)
vif(model3)
model2<-lm(log(datatrain$PM10)~(datatrain$RH)+(datatrain$WS)+(datatrain$AT),datatrain)
summary(model2)

pr.lm<-predict(model3,newdata = data.frame(datatrain))
MSE.lm <- sum((pr.lm - datatrain$PM10)^2)/nrow(datatrain)
RMSE.lm=sqrt(MSE.lm)
pr.lm2<-predict(model2,newdata = datatrain)
MSE.lm2 <- sum((pr.lm2 - datatrain$PM10)^2)/nrow(datatrain)
RMSE.lm2=sqrt(MSE.lm2)
#test prediction
model4<-lm(log(datatest$PM10)~(datatest$RH)+(datatest$SR)+(datatest$WD)+(datatest$WS)+(datatest$AT)+log(datatest$pdpm10),datatest)
summary(model4)

plot(datatest$PM10,exp(model4$fitted.values),xlab="Actual PM10",ylab = "Predicted PM10",col="blue",pch=16)
abline(0,1,col="red")




#plot actual vs fitted

plot(datatrain$PM10,exp(pr.lm),xlab="Actual PM10",ylab = "Predicted PM10",col="blue",pch=16)
abline(0,1,col="red")

plot(datatrain$PM10,exp(pr.lm2),xlab="Actual PM10",ylab = "Predicted PM10",col="blue",pch=16)
abline(0,1,col="red")




## Scale data for neural network

max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
## Fit neural network 

# install library
install.packages("neuralnet ")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(PM10~ RH+WS+pdpm10, trainNN, hidden = 1 , linear.output = T,stepmax = 1e7 )   #76% accuracy
NN1 = neuralnet(PM10~ AT+RH+SR+WD+WS+pdpm10, trainNN, hidden = 1 , linear.output = T,stepmax = 1e7 )   #74% accuracy

NNWPd= neuralnet(PM10~ AT+RH+WS, trainNN, hidden = 1 , linear.output = T,stepmax = 1e7 )
# plot neural network
plot(NN)
plot(NNWPd)
## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1,3,4,5,6,7)])
predict_testNN = (predict_testNN$net.result * (max(data$PM10) - min(data$PM10))) + min(data$PM10)

plot(datatest$PM10, predict_testNN, col='blue', pch=16, ylab = "predicted PM10 from NN", xlab = "real PM10")

abline(0,1)

##rmse for training dataset
predict_trainNN1 = compute(NN, trainNN[,c(1,3,4,5,6,7)])
predict_trainNN1 = (predict_trainNN1$net.result * (max(data$PM10) - min(data$PM10))) + min(data$PM10)


# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$PM10 - predict_testNN)^2) / nrow(datatest)) ^ 0.5
RMSE.NN
#rmse for training datset
RMSE.NN1 = (sum((datatrain$PM10 - predict_trainNN1)^2) / nrow(datatrain)) ^ 0.5
RMSE.NN1


## Prediction using neural network fro without pdpm10

predict_testNNwpd = compute(NNWPd, testNN[,c(1,3,4,5,6)])
predict_testNNwpd = (predict_testNNwpd$net.result * (max(data$PM10) - min(data$PM10))) + min(data$PM10)

plot(datatest$PM10, predict_testNNwpd, col='blue', pch=16, ylab = "predicted PM10 from NN", xlab = "real PM10")

abline(0,1)
RMSE.NN2 = (sum((datatest$PM10 - predict_testNNwpd)^2) / nrow(datatest)) ^ 0.5
RMSE.NN2
#rmse for training dataset
predict_trainNNwpd = compute(NNWPd, trainNN[,c(1,3,4,5,6)])
predict_trainNNwpd = (predict_trainNNwpd$net.result * (max(data$PM10) - min(data$PM10))) + min(data$PM10)

RMSE.NN3 = (sum((datatrain$PM10 - predict_trainNNwpd)^2) / nrow(datatrain)) ^ 0.5
RMSE.NN3



neuralnew<-data.frame(datatest$PM10,predict_testNN)
write.csv(neuralnew,"newtempincluded.csv")      #accuracy is 74%
newmatrix1<-data.frame(datatest$PM10,predict_testNNwpd)
write.csv(newmatrix1,"withoutpdpm10NN.csv")
#LOOCV
# load the library
library(caret)

# define training control
train_control <- trainControl(method="LOOCV")
# train the model
model <- train(PM10~., data=datatrain, trControl=train_control, method="rf")
# summarize results
print(model)
x_test=datatest[,c(1,3,4,5,6,7)]

#RAndom Forest
set.seed(101)
library(randomForest)
rf<-randomForest((PM10)~RH+WS+WD+SR+(pdpm10)+AT,data=datatrain)
rf1<-randomForest((PM10)~RH+WS+WD+SR+(pdpm10)+AT,data=datatest)
summary(rf)

new<-data.frame(datatest$PM10,rf1$predicted)
write.csv(new,"rf1.csv")
#important variables from NN
library(caret)

# varimp
control <- trainControl(method="LOOCV")
model <- train(PM10~AT+RH+WS, data=data, method="nnet",trControl=control)
model5<-train(PM10~RH+WS+pdpm10, data=data, method="nnet",trControl=control)
imp<-varImp(model3)
imp1<-varImp(model2)
imp2<-varImp(model)
imp3<-varImp(model5)

#correlation
library(corrplot)
m<-cor(data)
corrplot(m,method = "circle",type = "upper")
corrplot(m, method="number",type = "upper")


library(randomForest)
rf<-randomForest(PM10~.,data)
rf$importance

#correlation
library(PerformanceAnalytics)
chart.Correlation(mydata5_clean1, histogram=TRUE, pch=19,col="blue")
#p value
a=NULL
for(i in 1:ncol(new))
  {
 a[i]<- cor.test(new$PM10,new[,i])$p.value
  }
a
data.frame(names(new),a)

