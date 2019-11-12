install.packages("aTSA")
library(TSA)
data(larain)
summary(larain)
larain
plot(larain)
plot(y=larain,x=zlag(larain))

#random walks
Y=rnorm(100,0,2) #100 samples of 0 mean and sd 2
plot.ts(Y)
Y=rbinom(100,1,.5)
Y=2*Y-1
plot.ts(Y)

data(rwalk)
plot(rwalk)
mean(rwalk)
var(rwalk)

#Moving average 1st order
data("ma1.1.s")
plot(ma1.1.s)


#MOVING AVERABE; SIDY
set.seed(272019)
data1=arima.sim(model= list(c(-.9,2)), mean=0,100)
data1
plot.ts(data1)


#Auto Regressive
library(TSA)
data(ar2.s)
plot.ts(ar2.s)
summary(ar2.s)
length(ar2.s)

#
#install.packages("quantmod")
library(quantmod)
start=as.Date("2018-02-19")
end=as.Date("2019-02-19")

getSymbols("NVDA",src="yahoo",from=start,to=end)
NVDA
plot.ts(NVDA$NVDA.Adjusted)

#install.packages("forecast")
library(forecast)

ndiffs(NVDA$NVDA.Adjusted)
#tells you how many times to difference for it to be stationary
plot(diff(NVDA$NVDA.Adjusted))
plot(diff(NVDA$NVDA.Adjusted,2))
plot(diff(NVDA$NVDA.Adjusted,5))

#over differencing messes with the autocorrelation and autocovariance, dont do it.

data(electricity)
plot(electricity)
plot(diff(log(electricity)))

data(oil.price)
plot(oil.price)
plot(diff(log(oil.price)))

library(forecast)
auto.arima(oil.price)
auto.arima(log(oil.price))

data(ima22.s)
plot(ima22.s)
auto.arima(ima22.s)

########
end=as.Date("2019-03-05")
start=as.Date("2017-03-05")
getSymbols("AAPL",src="yahoo",from=start,to=end)
summary(AAPL)
library(forecast)
plot(AAPL[,4])
auto.arima(AAPL[,4])
#nonstationary, actually random walk
length(AAPL[,4])
fit=auto.arima(AAPL[400:502,4])
forecast(fit,h=10)
plot(forecast(fit,h=100))


## Next class
data(ma1.1.s)
plot(ma1.1.s)
acf(ma1.1.s)  #sample auto correlation function for ma1.1.s
pacf(ma1.1.s)  #partial autocorrelation fxn
eacf(ma1.1.s)  #extended autocorrelation fxn

auto.arima(ma1.1.s)

data(ma1.2.s)
plot(ma1.2.s)
acf(ma1.2.s)
pacf(ma1.2.s)  #partial autocorrelation fxn
e=eacf(ma1.2.s)  #extended autocorrelation fxn

auto.arima(ma1.2.s)


data(ma2.s)
plot(ma2.s)
acf(ma2.s)
acf(ma2.s,ci.type="ma")
pacf(ma2.s)  #partial autocorrelation fxn
eacf(ma2.s)  #extended autocorrelation fxn

auto.arima(ma2.s)

data(ar2.s)
plot(ar2.s)
acf(ar2.s)
acf(ar2.s,ci.type="ma")
pacf(ar2.s)  #partial autocorrelation fxn
eacf(ar2.s)  #extended autocorrelation fxn

auto.arima(ar2.s)


#_________________________________________#
install.packages("aTSA")
library(aTSA)
?stationary.test
?aTSA
#stationary.test(x)  # same as adf.test(x)
#stationary.test(x, method = "pp") # same as pp.test(x)
#stationary.test(x, method = "kpss") # same as kpss.test(x)
#click 'index' at bottom to see all fxns
?kpss.test

set.seed(1234)
#Exercise 20 ch.6
series=arima.sim(n=48,list(ar=0.7))
plot(series)
auto.arima(series)
ro1=0.7
ro5=0.7^5
acf(series,lag.max=7)[1:7]
#standard error of r1
e1=sqrt((1-0.7^2)/48)
#standard error r5
e5=sqrt(1/48*(1+0.7^2)/(1-0.7^2))
conf.1deviation=c(ro5-e5 ,ro5+e5)
conf.1deviation
est_r=acf(series,lag.max=7)[1:7]
est_r[5]


adf.test(series)
adf.test(ts(rnorm(100)))##DEFFFF STATIONARY
kpss.test(ts(rnorm(100)))



##########################
data(ma1.1.s)
plot(ma1.1.s)
acf(ma1.1.s)
eacf(ma1.1.s)
arima(ma1.1.s,order=c(0,0,1),method="ML")
arima(ma1.1.s)
?arima

arima(ma1.1.s,order=c(0,0,1),method="ML",include.mean=F)
arima(ma1.1.s,order=c(0,0,1),method="CSS",include.mean=F)
arima(ma1.1.s,order=c(0,0,1),method="CSS-ML",include.mean=F)

data(hare)
plot(hare)
acf(hare)
acf(log(hare))
ndiffs(log(hare))
m=auto.arima(log(hare))
arima(hare,order=c(2,0,0),method="ML")
pacf(hare)
arima(hare,order=c(2,0,0),method="CSS")

####_____BOOTSTRAPPING_____####
fit=arima(hare,order=c(2,0,0),include.mean=T)
set.seed(1234)
out=arima.boot(fit,cond.boot=T,is.normal=T,B=1000,init=hare)
?arima.boot
summary(out)
apply(out,2,function(x) quantile(x,c(0.025,0.975)))
apply(out,2,function(x) sd(x))
fit
length(hare)



###HW scratch
ma1=arima.sim(n=60,list(ma=0.6))
acf(ma1)[1:7]

lags1=rep(1,1000)
for (i in 1:1000) {
  ma1= arima.sim(list(order = c(0,0,1), ma = c(0.5)), n = 60)
  lags1[i]=acf(ma1,plot=F)$acf[2]
}
summary(lags1)
lags1=as.numeric(lags1)
hist(lags1)

var(lags1)

mean(lags1)
#box pierce, yjung box
#yjung box test for dependence of residuals
lb=LB.test(fit,type="Ljung-Box")
?LB.test
#LB.test(model, lag = 12, type = c("Ljung-Box", "Box-Pierce"), no.error = FALSE,omit.initial = TRUE)


###############
install.packages("TTR")
library(TTR)
library(forecast)
library(TSA)
births=scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthsts=ts(births,frequency=12,start=c(1946,1))
plot(birthsts)
d=decompose(birthsts)
plot(d)
?TTR
plot(d$x-d$seasonal)
plot(d$x-d$trend)

m=auto.arima(birthsts)
plot(forecast(m))
hist(residuals(m))
LB.test(m,lag=20)
#want high pval, then dont rejec the null. null is model good.
Box.test(m$residuals,lag=20)

rain=scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
raints=ts(rain,start=c(1813))
ndiffs(raints)
plot(raints)
raints
acf(raints)

?HoltWinters
raintsfore=HoltWinters(raints,beta=F,gamma=F)
raintsfore
#beta is True for means that depend on time, i.e. trend
#gamma is true for seasonal components.
#alpha ranges [0,1], the smaller it is, the more weight is given to older observation

plot(raintsfore)
plot(forecast(raintsfore))
plot(forecast(auto.arima(raints)))

res=raintsfore$x-raintsfore$fitted
res=res[,1]
hist(res)
summary(res)
acf(res,lag.max=20)
Box.test(res,lag=20,type="Ljung-Box")


skirt=scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirts=ts(skirt,start=1866)
plot(skirts)
acf(skirts)
ar1d1=auto.arima(skirts)

skirtsfit=HoltWinters(skirts,gamma=F)
skirtsfit
plot(skirtsfit)
plot(ar1d1)

plot(forecast(skirtsfit))
plot(forecast(ar1d1))

res=skirtsfit$x-skirtsfit$fitted
res=res[,1]
plot(res)
plot(ar1d1$residuals)
summary(res)
summary(ar1d1$residuals)
hist(res)
hist(ar1d1$residuals)




fancy=scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
fancyts=ts(fancy,start=c(1987,1),frequency=12)
plot(fancyts)
plot(decompose(fancyts))
fancylog=log(fancyts)
plot(fancylog)
plot(decompose(fancylog))

fancyfit=HoltWinters(fancylog)
fancyfit
plot(fancyfit)
plot(forecast(fancyfit,h=100))

res=fancyfit$x-fancyfit$fitted
res=res[,1]
plot(res)
summary(res)
hist(res)


data(color)
plot(color)
acf(color)
ndiffs(color)
fit=auto.arima(color)
plot(forecast(fit,15))

data(hare)
plot(hare)
fit=auto.arima(hare)
plot(forecast(fit,50))
fit=auto.arima(sqrt(hare))
plot(forecast(fit,50))
#how do we undo the square root to forecast the actual hare population

library(aTSA)
trans=function(x){return(x^2)}
fit=auto.arima(sqrt(hare))
plot(forecast(fit,50,transform=trans))
forecast(fit,10,transform=trans)

data("electricity")
fit1=auto.arima(log(electricity))
fit1
plot(fit1)
forecast(fit1,50)
str(fit1)



##______Seasonal_Models____-_-_-_-_-
data(co2)
plot(co2)
plot(decompose(co2))
plot(diff(co2))
plot(decompose(diff(co2)))
plot(diff(diff(co2),lag=12))
x=diff(diff(co2),lag=12)
acf(as.vector(x))
#an appropriate model may be ARIMA(0,1,1)x(0,1,1)_s=12
m=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
hist(m$residuals)
Box.test(m$residuals,lag=20)
acf(m$residuals)
plot(m$residuals)
#or rstandard(m) = standardized residuals
plot(forecast(m,24))
plot(m,n.ahead=24)
library(aTSA)





getSymbols("FB",src="yahoo",from=start,to=end) 
getSymbols("AAPL",src="yahoo",from=start,to=end)
getSymbols("IBM",src="yahoo",from=start,to=end)
getSymbols("NVDA",src="yahoo",from=start,to=end)
getSymbols("SIRI",src="yahoo",from=start,to=end)
getSymbols("SPOT",src="yahoo",from=start,to=end)
getSymbols("GOOGL",src="yahoo",from=start,to=end)
getSymbols("AMZN",src="yahoo",from=start,to=end)
getSymbols("WFC",src="yahoo",from=start,to=end)
getSymbols("GE",src="yahoo",from=start,to=end)
getSymbols("EBAY",src="yahoo",from=start,to=end)
getSymbols("S",src="yahoo",from=start,to=end)
getSymbols("VERI",src="yahoo",from=start,to=end)
getSymbols("DIS",src="yahoo",from=start,to=end)
getSymbols("NKE",src="yahoo",from=start,to=end)
getSymbols("NFLX",src="yahoo",from=start,to=end)
getSymbols("WMT",src="yahoo",from=start,to=end)
getSymbols("TSLA",src="yahoo",from=start,to=end)
getSymbols("TGT",src="yahoo",from=start,to=end)

ts.plot(fb)
ts.plot(apl)
ts.plot(ibm)
ts.plot(nvda)
ts.plot(siri)
ts.plot(spot)
ts.plot(googl)
ts.plot(amzn)
ts.plot(wfc)
ts.plot(ge)
ts.plot(ebay)
ts.plot(sprt)
ts.plot(veri)
ts.plot(dsny)
ts.plot(nke)
ts.plot(nflx)
ts.plot(wlmt)
ts.plot(tsla)
ts.plot(trgt)


stk=fb[(length(fb)-201):length(fb)]
window=100
upper=window
lower=1
windows=NULL
end=upper

data=data.frame(prev=numeric(),actual=numeric(),holts_pred=numeric(),arima_pred=numeric(),window=character())
for (i in 1:end) {
  actual=stk[(upper+1)]
  prev=stk[upper]
  
  stk_mod_arima=auto.arima(stk[lower:upper])
  arima=forecast(stk_mod_arima,h=1)$mean[[1]]
  
  stk_mod_holt=HoltWinters(stk[lower:upper],gamma=F,beta=F)
  holts=forecast(stk_mod_holt,h=1)$mean[[1]]
  
  windows=paste0(lower,"-",upper)
  tmp=data.frame(prev=prev,actual=actual,holts_pred=holts,arima_pred=arima,window=windows)
  
  data=rbind(data,tmp)

  upper=upper+1
  lower=lower+1
}
data$holts_success=ifelse(((data$actual >= data$prev) & (data$holts_pred >= data$prev))|((data$actual <= data$prev) & (data$holts_pred <= data$prev)),
                          "success", "failure")
data$arima_success=ifelse(((data$actual >= data$prev) & (data$arima_pred >= data$prev))|((data$actual <= data$prev) & (data$arima_pred <= data$prev)),
                          "success", "failure")
data
barplot(table(data$holts_success))
barplot(table(data$arima_success))


plot(m,n.ahead=24)


int_0_7=pbeta(0.7, 0.25,0.43) - pbeta(0, 0.25,0.43)
int_7_1=pbeta(1, 0.25,0.43) - pbeta(0.7, 0.25,0.43)
int_0_7+int_7_1
plot()

hist(rbeta(1000, 15, 3),breaks=seq(0,1,by=.02))
