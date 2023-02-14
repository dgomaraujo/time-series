# Inserir dados
venlafaxina<-c(0.331837835,0.344798754,0.35597098,0.358771135,0.369180328,
               0.368826848,0.396354958,0.401997855,0.412408941,0.405553945,
               0.379392315,0.432096855,0.411768441,0.422596697,0.443614866,
               0.44245281,0.406953874,0.442895843,0.424795885,0.42912548,
               0.445485049,0.451163544,0.421107294,0.495914413,0.445565353,
               0.4917852,0.522995195,0.505349212,0.513631068,0.546000314,
               0.515016541,0.655454079,0.683414062,0.6644144,0.697475222,
               0.705720991,0.549373133,0.555732447,0.577157402,0.534658796,
               0.536990076,0.58411698,0.53058124,0.619346031,0.592122391,
               0.62441653,0.619856185,0.661972722,0.621523448,0.627372134,
               0.664075876,0.65504267,0.661732328,0.660953116,0.682299219,
               0.703328561,0.735431919,0.733947672,0.752520113,0.767292809,
               0.722318419,0.771555621,0.725726493,0.726156756,0.747313258,
               0.708149158,0.793115283,0.763987982,0.788613039,0.819724473,
               0.838044823,0.874253649,0.816702928,0.814573232,0.907468995,
               0.913858012,0.903679945,0.859149191,0.975736934,0.887575625,
               0.969362636,0.937370661,0.882478605,0.932099534)
desvenlafaxina<-c(0.003179897,0.002347067,0.001589948,0.002190595,0.003444888,
                  0.001916771,0.002914905,0.005829811,0.005750313,0.003179897,
                  0.008214733,0.007154768,0.004985595,0.006681822,0.007084793,
                  0.003524903,0.003411197,0.006507513,0.006035194,0.007609592,
                  0.00976127,0.003935996,0.010574709,0.008921591,0.005198425,
                  0.009446793,0.01039685,0.010743411,0.014295668,0.012354923,
                  0.012216299,0.012216299,0.013429264,0.011436535,0.0091319,
                  0.010136929,0.014680304,0.007128594,0.011074615,0.013572843,
                  0.014422755,0.011177635,0.010817066,0.010559517,0.010683386,
                  0.011644903,0.020492331,0.025129443,0.032526865,0.028846466,
                  0.02351429,0.028283908,0.032767943,0.034645871,0.041984506,
                  0.033268641,0.054900676,0.044432366,0.052122108,0.057877051,
                  0.054584368,0.068824425,0.065733046,0.077695491,0.062734309,
                  0.080356947,0.087202529,0.099878171,0.103150412,0.098093647,
                  0.108986604,0.109702254,0.123787053,0.123289133,0.140672914,
                  0.142494762,0.143977067,0.172317931,0.19405049,0.182093475,
                  0.182391032,0.181983945,0.155736928,0.185470648)

# Converter dados para ts
venlafaxina_ts <- ts(cbind(venlafaxina), start = c(2014), frequency = 12)
desvenlafaxina_ts <- ts(cbind(desvenlafaxina), start = c(2014), frequency = 12)

### Carregar pacote
library(fpp2)
library(tidyverse)
library(ggplot2)

############ Analise preliminar ############ 

# Para podermos ter uma visao inicial da serie, a figura 1 retrata o grafico.

# Visualizacao da serie
a <- autoplot(desvenlafaxina_ts) +
  ylab("DDD/1.000 hab./dia") + 
  xlab("Ano")
a + theme_minimal()

# Ha uma tendencia de crescimento ao longo do tempo, indicando um aumento no 
# consumo ao longo dos anos.

# Os dados serao diferenciados para remover a tendencia.

# Diferenciacao
D_desvenlafaxina_ts <- diff(desvenlafaxina_ts)
b <- autoplot(D_desvenlafaxina_ts) +
  ylab("DDD/1.000 hab./dia") + xlab("Ano")
b + theme_minimal()


# Na figura acima, removida a tendancia, vemos que ha uma grande variacao do 
# numero de dados ao redor de uma linha. 

# Dados parecem ser estacionarios. Investigar sazonalidade dos dados.

# Sazonalidade
p <- ggseasonplot(D_desvenlafaxina_ts) + ggtitle("") +
  xlab("Mês") + ylab("DDD/1.000 hab./dia") + labs(colour = "Ano")
p + theme_minimal()


# Vamos observar outro grafico de sazonalidade, o subseriesplot.

d <- ggsubseriesplot(D_desvenlafaxina_ts) + ggtitle("") +
  xlab("Mês") + ylab("DDD/1.000 hab./dia")
d + theme_minimal()


# Observamos que ha uma reducao moderada no consumo de Venlafaxina entre os 
# meses de dezembro e janeiro, com excecao do ano de 2017, onde houve uma
# queda consideravel, nao houve uma mudanca significativa nos demais anos. A
# nossa serie tem tendencia e nao tem sazonalidade,

# acf e pacf

ggtsdisplay(venlafaxina, theme=theme_minimal())
ggtsdisplay(desvenlafaxina, theme=theme_minimal())


#############################################

library(forecast)
library(astsa)
library(MLmetrics)

#Split data into train and test sets
train <- window(venlafaxina_ts, start = c(2014,1), end = c(2019,12))
test <- window(venlafaxina_ts, start = c(2020,1))


  
##################### ETS model ##################### 

# ets
ets <- ets(train) 
ets.forecast <- forecast (ets, h=12)
MAPE(ets.forecast$mean, test) * 100 #30.19

p <- autoplot(train) +
  autolayer(ets.forecast$mean, series="Valor predito") +
  autolayer(test, series="Valor real") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 

# residuals
print(summary(ets))
checkresiduals(ets)

##################### Holt-Winters model ##################### 
# holt winters
HW <- HoltWinters(train)
hw.forecast <- forecast (HW, h=12)
MAPE(hw.forecast$mean, test) * 100 #11.90

p <- autoplot(train) +
  autolayer(hw.forecast$mean, series="Valor predito") +
  autolayer(test, series="Valor real") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 


# residuals
print(summary(HW))
checkresiduals(HW)


##################### ARIMA model ##################### 
# fit with arima model
fit_arima <- auto.arima(train,d=1,D=1,stepwise=FALSE,approximation=FALSE,
                        trace=TRUE) #residual SD = 197.8
arima.forecast <- forecast (fit_arima, h=12)
MAPE(arima.forecast$mean, test) * 100 #20.06

p <- autoplot(train) +
  autolayer(arima.forecast$mean, series="Valor predito") +
  autolayer(test, series="Valor real") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 

# residuals
print(summary(fit_arima))
checkresiduals(fit_arima)

##################### SARIMA model ##################### 

#Train a SARIMA model
sarima.forecast <- sarima.for(train, n.ahead=length(test),
                              p=1,d=1,q=1,P=1,D=1,Q=0,S=12)
#Check the MAPE
MAPE(sarima.forecast$pred, test) * 100 #4.86

p <- autoplot(train) +
  autolayer(sarima.forecast$mean, series="Valor predito") +
  autolayer(test, series="Valor real") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 

# residual
sarima(train, 1,1,1,1,1,0,12)


##################### Neural network model ##################### 

#Train a neural network model
neuralnet <- nnetar(train)
#Generate forecasts with the model
neuralnet.forecast <- forecast(neuralnet, h=length(test))
#Check the MAPE
MAPE(neuralnet.forecast$mean, test) * 100 #6.66

p <- autoplot(train) +
  autolayer(neuralnet.forecast$mean, series="Valor predito") +
  autolayer(test, series="Valor real") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 

# residual
print(summary(neuralnet))
checkresiduals(neuralnet)

##################### Correlação Cruzada ##################### 

venlafaxina<-c(0.331837835,0.344798754,0.35597098,0.358771135,0.369180328,
               0.368826848,0.396354958,0.401997855,0.412408941,0.405553945,
               0.379392315,0.432096855,0.411768441,0.422596697,0.443614866,
               0.44245281,0.406953874,0.442895843,0.424795885,0.42912548,
               0.445485049,0.451163544,0.421107294,0.495914413,0.445565353,
               0.4917852,0.522995195,0.505349212,0.513631068,0.546000314,
               0.515016541,0.655454079,0.683414062,0.6644144,0.697475222,
               0.705720991,0.549373133,0.555732447,0.577157402,0.534658796,
               0.536990076,0.58411698,0.53058124,0.619346031,0.592122391,
               0.62441653,0.619856185,0.661972722,0.621523448,0.627372134,
               0.664075876,0.65504267,0.661732328,0.660953116,0.682299219,
               0.703328561,0.735431919,0.733947672,0.752520113,0.767292809,
               0.722318419,0.771555621,0.725726493,0.726156756,0.747313258,
               0.708149158,0.793115283,0.763987982,0.788613039,0.819724473,
               0.838044823,0.874253649,0.816702928,0.814573232,0.907468995,
               0.913858012,0.903679945,0.859149191,0.975736934,0.887575625,
               0.969362636,0.937370661,0.882478605,0.932099534)
desvenlafaxina<-c(0.003179897,0.002347067,0.001589948,0.002190595,0.003444888,
                  0.001916771,0.002914905,0.005829811,0.005750313,0.003179897,
                  0.008214733,0.007154768,0.004985595,0.006681822,0.007084793,
                  0.003524903,0.003411197,0.006507513,0.006035194,0.007609592,
                  0.00976127,0.003935996,0.010574709,0.008921591,0.005198425,
                  0.009446793,0.01039685,0.010743411,0.014295668,0.012354923,
                  0.012216299,0.012216299,0.013429264,0.011436535,0.0091319,
                  0.010136929,0.014680304,0.007128594,0.011074615,0.013572843,
                  0.014422755,0.011177635,0.010817066,0.010559517,0.010683386,
                  0.011644903,0.020492331,0.025129443,0.032526865,0.028846466,
                  0.02351429,0.028283908,0.032767943,0.034645871,0.041984506,
                  0.033268641,0.054900676,0.044432366,0.052122108,0.057877051,
                  0.054584368,0.068824425,0.065733046,0.077695491,0.062734309,
                  0.080356947,0.087202529,0.099878171,0.103150412,0.098093647,
                  0.108986604,0.109702254,0.123787053,0.123289133,0.140672914,
                  0.142494762,0.143977067,0.172317931,0.19405049,0.182093475,
                  0.182391032,0.181983945,0.155736928,0.185470648)
ccf(venlafaxina, desvenlafaxina)

library(forecast)
p <- ggCcf(Venlafaxina,Desvenlafaxina)
p + theme_minimal()
print(ccf(venlafaxina, desvenlafaxina, plot = FALSE))


library(astsa)
lag2.plot (Venlafaxina, Desvenlafaxina, 11)

##################### Correlação Cruzada ##################### 

venlafaxina<-as.ts(c(0.331837835,0.344798754,0.35597098,0.358771135,0.369180328,
               0.368826848,0.396354958,0.401997855,0.412408941,0.405553945,
               0.379392315,0.432096855,0.411768441,0.422596697,0.443614866,
               0.44245281,0.406953874,0.442895843,0.424795885,0.42912548,
               0.445485049,0.451163544,0.421107294,0.495914413,0.445565353,
               0.4917852,0.522995195,0.505349212,0.513631068,0.546000314,
               0.515016541,0.655454079,0.683414062,0.6644144,0.697475222,
               0.705720991,0.549373133,0.555732447,0.577157402,0.534658796,
               0.536990076,0.58411698,0.53058124,0.619346031,0.592122391,
               0.62441653,0.619856185,0.661972722,0.621523448,0.627372134,
               0.664075876,0.65504267,0.661732328,0.660953116,0.682299219,
               0.703328561,0.735431919,0.733947672,0.752520113,0.767292809,
               0.722318419,0.771555621,0.725726493,0.726156756,0.747313258,
               0.708149158,0.793115283,0.763987982,0.788613039,0.819724473,
               0.838044823,0.874253649,0.816702928,0.814573232,0.907468995,
               0.913858012,0.903679945,0.859149191,0.975736934,0.887575625,
               0.969362636,0.937370661,0.882478605,0.932099534))
desvenlafaxina<-as.ts(c(0.003179897,0.002347067,0.001589948,0.002190595,0.003444888,
                  0.001916771,0.002914905,0.005829811,0.005750313,0.003179897,
                  0.008214733,0.007154768,0.004985595,0.006681822,0.007084793,
                  0.003524903,0.003411197,0.006507513,0.006035194,0.007609592,
                  0.00976127,0.003935996,0.010574709,0.008921591,0.005198425,
                  0.009446793,0.01039685,0.010743411,0.014295668,0.012354923,
                  0.012216299,0.012216299,0.013429264,0.011436535,0.0091319,
                  0.010136929,0.014680304,0.007128594,0.011074615,0.013572843,
                  0.014422755,0.011177635,0.010817066,0.010559517,0.010683386,
                  0.011644903,0.020492331,0.025129443,0.032526865,0.028846466,
                  0.02351429,0.028283908,0.032767943,0.034645871,0.041984506,
                  0.033268641,0.054900676,0.044432366,0.052122108,0.057877051,
                  0.054584368,0.068824425,0.065733046,0.077695491,0.062734309,
                  0.080356947,0.087202529,0.099878171,0.103150412,0.098093647,
                  0.108986604,0.109702254,0.123787053,0.123289133,0.140672914,
                  0.142494762,0.143977067,0.172317931,0.19405049,0.182093475,
                  0.182391032,0.181983945,0.155736928,0.185470648))

ccf(venlafaxina, desvenlafaxina)
diff(desvenlafaxina_ts)
diff(desvenlafaxina_ts)

library(forecast)
p <- ggCcf(Venlafaxina,Desvenlafaxina)
p + theme_minimal()
print(ccf(venlafaxina, desvenlafaxina, plot = FALSE))


library(astsa)
lag2.plot (Venlafaxina, Desvenlafaxina, 11)

##################### Previsões para 2021 ##################### 

# Inserir dados
venlafaxina<-c(0.331837835,0.344798754,0.35597098,0.358771135,0.369180328,
               0.368826848,0.396354958,0.401997855,0.412408941,0.405553945,
               0.379392315,0.432096855,0.411768441,0.422596697,0.443614866,
               0.44245281,0.406953874,0.442895843,0.424795885,0.42912548,
               0.445485049,0.451163544,0.421107294,0.495914413,0.445565353,
               0.4917852,0.522995195,0.505349212,0.513631068,0.546000314,
               0.515016541,0.655454079,0.683414062,0.6644144,0.697475222,
               0.705720991,0.549373133,0.555732447,0.577157402,0.534658796,
               0.536990076,0.58411698,0.53058124,0.619346031,0.592122391,
               0.62441653,0.619856185,0.661972722,0.621523448,0.627372134,
               0.664075876,0.65504267,0.661732328,0.660953116,0.682299219,
               0.703328561,0.735431919,0.733947672,0.752520113,0.767292809,
               0.722318419,0.771555621,0.725726493,0.726156756,0.747313258,
               0.708149158,0.793115283,0.763987982,0.788613039,0.819724473,
               0.838044823,0.874253649,0.816702928,0.814573232,0.907468995,
               0.913858012,0.903679945,0.859149191,0.975736934,0.887575625,
               0.969362636,0.937370661,0.882478605,0.932099534)
desvenlafaxina<-c(0.003179897,0.002347067,0.001589948,0.002190595,0.003444888,
                  0.001916771,0.002914905,0.005829811,0.005750313,0.003179897,
                  0.008214733,0.007154768,0.004985595,0.006681822,0.007084793,
                  0.003524903,0.003411197,0.006507513,0.006035194,0.007609592,
                  0.00976127,0.003935996,0.010574709,0.008921591,0.005198425,
                  0.009446793,0.01039685,0.010743411,0.014295668,0.012354923,
                  0.012216299,0.012216299,0.013429264,0.011436535,0.0091319,
                  0.010136929,0.014680304,0.007128594,0.011074615,0.013572843,
                  0.014422755,0.011177635,0.010817066,0.010559517,0.010683386,
                  0.011644903,0.020492331,0.025129443,0.032526865,0.028846466,
                  0.02351429,0.028283908,0.032767943,0.034645871,0.041984506,
                  0.033268641,0.054900676,0.044432366,0.052122108,0.057877051,
                  0.054584368,0.068824425,0.065733046,0.077695491,0.062734309,
                  0.080356947,0.087202529,0.099878171,0.103150412,0.098093647,
                  0.108986604,0.109702254,0.123787053,0.123289133,0.140672914,
                  0.142494762,0.143977067,0.172317931,0.19405049,0.182093475,
                  0.182391032,0.181983945,0.155736928,0.185470648)

# Converter dados para ts
venlafaxina_ts <- ts(cbind(venlafaxina), start = c(2014), frequency = 12)
desvenlafaxina_ts <- ts(cbind(desvenlafaxina), start = c(2014), frequency = 12)

##################### ETS model ##################### 

# ets
ets <- ets(venlafaxina_ts) 
ets.forecast <- forecast (ets, h=12)

p <- autoplot(venlafaxina_ts) +
  autolayer(ets.forecast$mean, series="Valor predito") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 

forecast (ets, h=12)

##################### Holt-Winters model ##################### 
# holt winters
HW <- HoltWinters(venlafaxina_ts)
hw.forecast <- forecast (HW, h=12)

p <- autoplot(venlafaxina_ts) +
  autolayer(hw.forecast$mean, series="Valor predito") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 

forecast (HW, h=12)

##################### ARIMA model ##################### 
# fit with arima model
fit_arima <- auto.arima(venlafaxina_ts,d=1,D=1,stepwise=FALSE,
                        approximation=FALSE,trace=TRUE) #residual SD = 197.8
arima.forecast <- forecast (fit_arima, h=12)

p <- autoplot(venlafaxina_ts) +
  autolayer(arima.forecast$mean, series="Valor predito") +
  xlab("Ano") + ylab("DDD/1.000 hab./dia") + labs(colour = "Legenda") 
p + theme_minimal() 

forecast (fit_arima, h=12)

# residuals
print(summary(fit_arima))
checkresiduals(fit_arima)