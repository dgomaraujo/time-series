
######################## CHAPTER 1 ########################

### A flying start: Air passenger booking
data(AirPassengers)
AP <- AirPassengers
AP
class(AP)
start(AP); end(AP); frequency(AP)
plot(AP, ylab = "Passengers (1000's)")
layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

### Unemployment: Maine
Maine.month <- read.csv("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/Maine.dat", sep="", header = TRUE)
attach(Maine.month)
class(Maine.month)
Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)
Maine.month.ts
Maine.annual.ts <- aggregate(Maine.month.ts)/12
Maine.annual.ts
layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")
Maine.Feb <- window(Maine.month.ts, start = c(1996,2), freq = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996,8), freq = TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)
Feb.ratio
Aug.ratio
US.month <- read.csv("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/USunemp.dat", sep="", header=T)
US.month
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq = 12)
plot(US.month.ts, ylab = "unemployed (%)")

### Multiple time series: Electricity, beer and chocolate data
CBE <- read.delim("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/cbe.dat", header=T)
CBE
CBE[1:4, ]
class(CBE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))
AP.elec <- ts.intersect(AP, Elec.ts)
AP.elec
start(AP.elec)
end(AP.elec)
AP.elec[1:3, ]
AP <- AP.elec[,1]; Elec <- AP.elec[,2]
layout(1:2)
plot(AP, main = "", ylab = "Air passengers / 1000's")
plot(Elec, main = "", ylab = "Electricity production / MkWh")
plot(as.vector(AP), as.vector(Elec),
       xlab = "Air passengers / 1000's",
       ylab = "Electricity production / MWh")
abline(reg = lm(Elec ~ AP))
cor(AP, Elec)

### Quarterly exchange rate: GBP to NZ dollar
Z <-pounds_nz <- read.csv("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/pounds_nz.dat", sep="")
Z[1:4, ]
Z.ts <- ts(Z, st = 1991, fr = 4)
plot(Z.ts, xlab = "time / years",
       ylab = "Quarterly exchange rate in $NZ / pound")
Z.92.96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))
Z.96.98 <- window(Z.ts, start = c(1996, 1), end = c(1998, 1))
layout (1:2)
plot(Z.92.96, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )
plot(Z.96.98, ylab = "Exchange rate in $NZ/pound",
       xlab = "Time (years)" )

### Global temperature series
install.packages("readr")
library(readr)
Global <- read_csv("global.dat", col_names = FALSE)
Global
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.ts)
plot(Global.annual)
New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
New.time <- time(New.series)
plot(New.series); abline(reg=lm(New.series ~ New.time))
plot(decompose(Elec.ts))
Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)

### Decomposition
plot(decompose(Elec.ts))
Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)



######################## CHAPTER 2 ########################

### Herald Square in Manhattan

Herald.dat <- read.delim("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/Herald.dat")
attach (Herald.dat)
Herald.dat

x <- CO; y <- Benzoa; n <- length(x)
sum((x - mean(x))*(y - mean(y))) / (n - 1)
mean((x - mean(x)) * (y - mean(y)))
cov(x, y)
cov(x,y) / (sd(x)*sd(y))
cor(x,y)

### Wave heights (mm relative to still water level)

wave.dat <- read.csv("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/wave.dat", sep="")
wave.dat
attach(wave.dat)
plot(ts(waveht)) ; plot(ts(waveht[1:60]))
acf(waveht)$acf[2]
acf(waveht, type = c("covariance"))$acf[2]

### Air passenger series

data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])
sd(AP[7:138])
sd(AP[7:138] - AP.decom$trend[7:138])
sd(AP.decom$random[7:138])

### Font Reservoir series

Fontdsdt.dat <- read.csv("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/Fontdsdt.dat", sep="")
attach(Fontdsdt.dat)
Fontdsdt.dat
plot(ts(adflow), ylab = 'adflow')
acf(adflow, xlab = 'lag (months)', main="")


######################## CHAPTER 3 ########################

### Building approvals and building activity time series

Build.dat <- read.delim("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/ApprovActiv.dat")
attach(Build.dat)
App.ts <- ts(Approvals, start = c(1996,1), freq=4)
Act.ts <- ts(Activity, start = c(1996,1), freq=4)
ts.plot(App.ts, Act.ts, lty = c(1,3))
acf(ts.union(App.ts, Act.ts))
app.ran <- decompose(App.ts)$random
app.ran.ts <- window (app.ran, start = c(1996, 3) )
act.ran <- decompose (Act.ts)$random
act.ran.ts <- window (act.ran, start = c(1996, 3) )
acf (ts.union(app.ran.ts, act.ran.ts))
ccf (app.ran.ts, act.ran.ts)
print(acf(ts.union(app.ran.ts, act.ran.ts)))

### Yearly sales of VCRs in the US home market between 1980 and 1989

T79 <- 1:10
Tdelt <- (1:100) / 10
Sales <- c(840,1470,2110,4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales <- cumsum(Sales)
Bass.nls <- nls(Sales ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /
                          (1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)
Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p+q) * Tdelt)
Bpdf <- m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2
plot(Tdelt, Bpdf, xlab = "Year from 1979",
       ylab = "Sales per year", type='l')
points(T79, Sales)
Bcdf <- m * (1 - ngete)/(1 + (q/p)*ngete)
plot(Tdelt, Bcdf, xlab = "Year from 1979",
       ylab = "Cumulative sales", type='l')
points(T79, Cusales)

### Complaints to a motoring organisation

Motor.dat <- read.csv("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/motororg.dat", sep="")
Comp.ts <- ts(Motor.dat$complaints, start = c(1996, 1), freq = 12)
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")
Comp.hw1 <- HoltWinters(Comp.ts, beta = 0, gamma = 0) ; Comp.hw1
plot(Comp.hw1)
Comp.hw1$SSE
Comp.hw2 <- HoltWinters(Comp.ts, alpha = 0.2, beta=0, gamma=0)
Comp.hw2
Comp.hw2$SSE

### Sales of Australian wine

wine.dat <- read.delim("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/wine.dat")
sweetw.ts <- ts(wine.dat$sweetw, start = c(1980,1), freq = 12)
plot(sweetw.ts, xlab= "Time (months)", ylab = "sales (1000 litres)")
sweetw.hw <- HoltWinters (sweetw.ts, seasonal = "mult")
sweetw.hw ; sweetw.hw$coef ; sweetw.hw$SSE
sqrt(sweetw.hw$SSE/length(sweetw))
sd(sweetw)
plot (sweetw.hw$fitted)
plot (sweetw.hw)

### Four-year-ahead forecasts for the air passenger data

AP.hw <- HoltWinters(AP, seasonal = "mult")
plot(AP.hw)
AP.predict <- predict(AP.hw, n.ahead = 4 * 12)
ts.plot(AP, AP.predict, lty = 1:2)

### p52-53, Bass curve fitted using nls

T79 <- 1:10
Tdelt <- (1:100) / 10
Sales <- c(840,1470,2110,4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales <- cumsum(Sales)
Bass.nls <- nls(Sales ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /
                        (1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)

Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p+q) * Tdelt)
Bpdf <- m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2
plot(Tdelt, Bpdf, xlab = "Year from 1979", 
     ylab = "Sales per year", type='l')
points(T79, Sales)
Bcdf <- m * (1 - ngete)/(1 + (q/p)*ngete)
plot(Tdelt, Bcdf, xlab = "Year from 1979", 
     ylab = "Cumulative sales", type='l')
points(T79, Cusales)





######################## CHAPTER 7 ########################

### Australian electricity production series

dev.off()
CBE <- read.delim("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/cbe.dat")
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
layout(c(1, 1, 2, 3))
plot(Elec.ts)
plot(diff(Elec.ts))
plot(diff(log(Elec.ts)))

###  ARIMA(1, 1, 1) 

x <- w <- rnorm(1000)
for (i in 3:1000) x[i] <- 0.5 * x[i - 1] + x[i - 1] - 0.5 *
        x[i - 2] + w[i] + 0.3 * w[i - 1]
arima(x, order = c(1, 1, 1))
x <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.5,ma = 0.3), n = 1000)
arima(x, order = c(1, 1, 1))

###  IMA(1, 1) model fitted to the beer production series

Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)
Beer.ima <- arima(Beer.ts, order = c(0, 1, 1))
Beer.ima
acf(resid(Beer.ima))
Beer.1991 <- predict(Beer.ima, n.ahead = 12)
sum(Beer.1991$pred)

###  Fitting procedure
# The seasonal ARI model provides the better fit since it has the smallest AIC

AIC (arima(log(Elec.ts), order = c(1,1,0),
             seas = list(order = c(1,0,0), 12)))
AIC (arima(log(Elec.ts), order = c(0,1,1),
             seas = list(order = c(0,0,1), 12)))

# S&P500 series

library(MASS)
data(SP500)
plot(SP500, type = 'l')
acf(SP500)
acf((SP500 - mean(SP500))^2)

### Volatility in climate series    

install.packages("tseries")
library(tseries)
stemp <- read_csv("C:/Users/dgoma/Google Drive/Pessoal/TCC/Datasets/stemp.dat", col_names = FALSE)
stemp.ts <- ts(stemp, start = 1850, freq = 12)
plot(stemp.ts)
stemp.best <- get.best.arima(stemp.ts, maxord = rep(2,6))
stemp.best[[3]]
stemp.arima <- arima(stemp.ts, order = c(1,1,2), seas = list(order = c(2,0,1), 12))
stemp.arima <- arima(stemp.ts, order = c(1,1,2), seas = list(order = c(1,0,1), 12))
t(confint(stemp.arima))
stemp.res <- resid(stemp.arima)
layout(1:2)
acf(stemp.res)
acf(stemp.res^2)
stemp.garch <- garch(stemp.res, trace = F)
t(confint(stemp.garch))
stemp.garch.res <- resid(stemp.garch)[-1]
acf(stemp.garch.res)
acf(stemp.garch.res^2)


# p144
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
        best.aic <- 1e8
        n <- length(x.ts)
        for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3]) 
                for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6]) 
                {
                        fit <- arima(x.ts, order = c(p,d,q),  
                                     seas = list(order = c(P,D,Q), 
                                                 frequency(x.ts)), method = "CSS")
                        fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
                        if (fit.aic < best.aic) 
                        {
                                best.aic <- fit.aic
                                best.fit <- fit
                                best.model <- c(p,d,q,P,D,Q) 
                        }
                }
        list(best.aic, best.fit, best.model)
}

# p149, simulated GARCH model
set.seed(1)
alpha0 <- 0.1
alpha1 <- 0.4
beta1 <- 0.2
w <- rnorm(10000)
a <- rep(0, 10000)
h <- rep(0, 10000)
for (i in 2:10000) {
        h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i - 
                                                                   1]
        a[i] <- w[i] * sqrt(h[i])
}
acf(a)
acf(a^2)
