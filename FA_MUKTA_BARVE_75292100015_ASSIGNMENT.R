library("tseries")

data1 = read.csv(file.choose(),header=T)#indexData
View(data1)
class(data1)#dataframe

#To convert data into time series
#install.packages("xts")
library(xts)
data1$date=as.Date(data1$date)
datats1=xts(data1$spx,data1$date)
View(datats1)
str(datats1)
plot(data1,main = "Data1")
plot(datats1, main= "Data1 Time Series")
abline(reg=lm(datats1~time(datats1))) #code for trend line
cycle(datats1)#No cyclic trend
library(seastests)
isSeasonal(datats1,tes="combined",freq=12)#Seasonality is not present


#Returns as percent change
datats2=data1[-c(2,4)]
View(datats2)
str(datats2)
datats2=xts(datats2$returns,datats2$date)
plot(datats2,main = "returns")#Looks stationary
adf.test(datats2)
kpss.test(datats2)
#STATIONARY
isSeasonal(datats2,tes="combined",freq=12)
#NO SEASONALITY
cycle(datats2)
#NO CYCLIC VARIATIONS
acf(datats2)
pacf(datats2)


#Train and test splitting
data_split=sort(sample(nrow(datats2),nrow(datats2)*0.7))
train=datats2[data_split,]
test=datats2[-data_split,]
plot(train)
adf.test(train)
kpss.test(train)
#STATIONARY
acf(train) #1 significant-->q=1
pacf(train)#7 significant--> p=7

#ARMA
a=arma(train,order=c(7,0))
a
summary(a)
library(forecast)
forecasted.data=forecast(a,h=30)
accuracy(forecasted.data,test)#RMSE value very small-->model fits the test data
plot(forecasted.data,col="red")
#AIC is smaller--> model is better fit

#ARIMA
a2=auto.arima(train,trace=TRUE,test="adf",ic="aicc",approximation=FALSE)
forecasted.data=forecast(a2,h=2000)
accuracy(forecasted.data,test)#RMSE value very small-->model fits the test data
plot(forecasted.data,col="red")

library(rugarch)
#install.packages("FinTS")
library(FinTS)
#install.packages("e1071")
library(e1071)
#check ARCH effect
ArchTest(datats2)
#p-val<0.05--> Reject H0--> ARCH effect present
#install.packages("fGarch")
library(fGarch)
arch=garchFit(-garch(1,0),data=train,trace=F)

#building GARCH model
garch(datats2,gard="numerical",trace=T)

spec= ugarchspec(variance.model = list(garchOrder = c(1, 1)))
fit=ugarchfit(spec=spec,data=train)                                 
fit

ugarchforecast(fit)
#Series--> mean constant, sigma is volatile


