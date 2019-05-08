setwd("F:/thesis data")
df<-read.csv("imputeddata.csv")
names(df)
str(df)
head(df)
library(lubridate)
df$Date <- dmy(df$Date)
plot(df$PM10,type="l")
acf(df$PM10)  #acf decays slowly
#we need to difference the model to make it stationary
dff<-diff(df$PM10)
acf(diff(df$PM10)) #0,1,2
pacf(diff(df$PM10)) #1,2,3,4,5,6


model1<-arima(df$PM10, order=c(0,1,1))
SSE1<-sum(model1$residuals^2)
model1.test<-Box.test(model1$residuals, lag = log(length(model1$residuals)))

model2<-arima(df$PM10, order=c(0,1,2))
SSE2<-sum(model2$residuals^2)
model2.test<-Box.test(model2$residuals, lag = log(length(model2$residuals)))

model3<-arima(df$PM10, order=c(7,1,1))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals, lag = log(length(model3$residuals)))

model4<-arima(df$PM10, order=c(7,1,2))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals, lag = log(length(model4$residuals)))



error<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, model1.test$p.value), 
               c(model2$aic, SSE2, model2.test$p.value), c(model3$aic, SSE3, model3.test$p.value),
               c(model4$aic, SSE4, model4.test$p.value))
colnames(error)<-c('Arima(0,1,1)','Arima(0,1,2)', 'Arima(7,1,1)', 'Arima(7,1,2)')



format(error, scientific=FALSE)



# Sarima
sarima(df$PM10, 7,1,2,0,0,0)





