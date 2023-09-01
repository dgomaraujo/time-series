###########################
######## Fluoxetina ####### 
###########################

####### Preparação dos dados

### Carregar pacotes
library(tseries)
library(fpp2)
library(fpp3)
library(lmtest)
library(Metrics)

### Inserir dados
fluo <- c(1.1061687062,1.2027878122,1.1211596478,1.1278339607,1.1910795191,
          1.0473197966,1.2788200615,1.2743625277,1.3604967175,1.3005114996,
          1.3351483982,1.4137953193,1.2643543783,1.3414794840,1.3215856347,
          1.2419182639,1.2967607456,1.3567874963,1.4672643445,1.3700265000,
          1.5516255468,1.6484326184,1.6825214678,1.8706289651,1.5753641000,
          1.5848781138,1.6879099991,1.7081832373,1.6911590146,1.7731616920,
          1.7394672346,1.8695578177,1.9624567650,1.8883464106,2.0092673435,
          2.1662950505,1.9236533486,2.1338529141,2.2107103250,2.0767970068,
          2.4180926177,2.3704570541,2.3684040190,2.6149705824,2.8880119768,
          2.6295405084,2.7396041577,2.7363866324,2.5892423419,2.6916729595,
          2.7385803395,2.9212716644,2.7917842033,2.7385951751,2.7917100258,
          3.2469934207,3.2295011129,3.1971646365,3.3955636805,3.2158388403,
          3.1361267449,3.5362596770,2.9586309637,3.0517199704,3.1693152181,
          3.0407129507,3.3034672949,3.6200639884,3.5442033071,3.6421957696,
          3.6805648825,3.5108584580,3.2462109611,3.2956426278,3.4337626769,
          3.6733995159,3.9219922505,3.8682593107,4.0660240821,3.8501953945,
          3.7559649875,3.7114027436,3.4298342041,3.5343111342)

### Transformação dos dados em objetos de séries temporais 
fluo_ts <- ts(cbind(fluo), start = c(2014), frequency = 12)

### Divisão dos dados entre treinamento e teste
train_fluo <- window(fluo_ts, start = c(2014,1), end = c(2019,12))
test_fluo <- window(fluo_ts, start = c(2020,1))



####### Análise preliminar

### Gráfico de linhas
autoplot(fluo_ts) + xlab("Ano") + ylab("DDD/1.000 hab./dia")

### Sazonalidade
ggseasonplot(fluo_ts, year.labels = TRUE) + xlab("Mês") + 
  ylab("DDD/1.000 hab./dia")  + ggtitle("") 
ggmonthplot(fluo_ts) + xlab("Mês") + ylab("DDD/1.000 hab./dia") 

### Diferenciação
autoplot(diff(fluo_ts)) + ylab("DDD/1.000 hab./dia") + xlab("Ano")



#######  ETS

### Modelo e previsão
ets_fluo_man <- ets(model="MAN", train_fluo)
ets_fluo_man
ets_fluo_aan <- ets(model="AAN", train_fluo) 
ets_fluo_aan
ets_fluo_madn <- ets(model="MAN", damped=TRUE, train_fluo)
ets_fluo_madn
ets_fluo_aadn <- ets(model="AAN", damped=TRUE, train_fluo)
ets_fluo_aadn
ets_fluo_maa <- ets(model="MAA", train_fluo)
ets_fluo_maa 
ets_fluo_mada <- ets(model="MAA", damped=TRUE, train_fluo)
ets_fluo_mada 
ets_fluo_mam <- ets(model="MAM", train_fluo)
ets_fluo_mam
ets_fluo_madm <- ets(model="MAM", damped=TRUE, train_fluo)
ets_fluo_madm 
ets_fluo_forecast <- forecast(ets_fluo_mam, h=12)

### Gráfico de resíduos do modelo
checkresiduals(ets_fluo_mam, test=FALSE)

### Teste Ljung-Box de autocorrelações dos resíduos
Box.test((residuals(ets_fluo_mam)), lag=12, type = "Ljung-Box")

### Teste Shapiro-Wilk de normalidade
shapiro.test((residuals(ets_fluo_mam)))

### Gráfico de previsão do modelo
autoplot(train_fluo) +
  autolayer(ets_fluo_forecast$mean, series="Valor previsto", lwd=1.5) +
  autolayer(test_fluo, series="Valor real", lwd=1.5) +
  xlab("Ano") +
  ylab("DDD/1.000 hab./dia") +
  ggtitle("Forecasts from ETS(M,A,M)") +
  guides(colour=guide_legend(title="Legenda"))

### Medidas de qualidade
rmse(test_fluo, ets_fluo_forecast$mean)
mae(test_fluo, ets_fluo_forecast$mean)
mape(test_fluo, ets_fluo_forecast$mean)
mase(test_fluo, ets_fluo_forecast$mean)



#######  ARIMA

### Transformação de Box-Cox
lambda <- BoxCox.lambda(fluo_ts)
autoplot(BoxCox(fluo_ts,lambda))
autoplot(diff(BoxCox(fluo_ts,lambda))) + ylab("DDD/1.000 hab./dia") + xlab("Ano")

### Teste de estacionariedade
adf.test(BoxCox(diff(fluo_ts),lambda))

### Divisão dos dados entre treinamento e teste com os dados transformados
train_fluo_bc <- window(BoxCox(fluo_ts,lambda), start = c(2014,1), end = c(2019,12))
test_fluo_bc <- window(BoxCox(fluo_ts,lambda), start = c(2020,1))

### ACF e PACF da série diferenciada
gg_tsdisplay(as_tsibble(diff(train_fluo_bc)),plot_type='partial')

### Ajuste de modelos
# Modelo inicial: ARIMA(2,1,4)(0,0,1)[12]
a214001_fluo <- Arima(train_fluo_bc, order=c(2,1,4), seasonal=c(0,0,1))
coeftest(a214001_fluo)
a212001_fluo <- Arima(train_fluo_bc, order=c(2,1,2), seasonal=c(0,0,1))
a212001_fluo
a211001_fluo <- Arima(train_fluo_bc, order=c(2,1,1), seasonal=c(0,0,1))
a211001_fluo
a112001_fluo <- Arima(train_fluo_bc, order=c(1,1,2), seasonal=c(0,0,1))
a112001_fluo
a210001_fluo <- Arima(train_fluo_bc, order=c(2,1,0), seasonal=c(0,0,1))
a210001_fluo
a212_fluo <- Arima(train_fluo_bc, order=c(2,1,2))
a212_fluo
a211_fluo <- Arima(train_fluo_bc, order=c(2,1,1))
a211_fluo
a112_fluo <- Arima(train_fluo_bc, order=c(1,1,2))
a112_fluo

# Dentre os modelos testados, o com menor AIC é o ARIMA(2,1,0)(0,0,1)[12]
a210001_fluo
coeftest(a210001_fluo)

### Gráficos do resíduos do modelo
checkresiduals(a210001_fluo, test=FALSE)

### Teste Ljung-Box de autocorrelações dos resíduos
Box.test((residuals(a210001_fluo)), type = "Ljung-Box")

### Teste Shapiro-Wilk de normalidade
shapiro.test((residuals(a210001_fluo)))

### Previsão do modelo
arima_fluo_forecast <- forecast(a210001_fluo, h=12)

### Gráfico com a previsão do modelo
autoplot(train_fluo_bc) +
  autolayer(arima_fluo_forecast$mean, series="Valor previsto", lwd=1.5) +
  autolayer(test_fluo_bc, series="Valor real", lwd=1.5) +
  xlab("Ano") +
  ylab("DDD/1.000 hab./dia") +
  ggtitle("Forecasts from ARIMA(2,1,0)(0,0,1)[12]") +
  guides(colour=guide_legend(title="Legenda"))

### Medidas de qualidade
rmse(test_fluo_bc, arima_fluo_forecast$mean)
mae(test_fluo_bc, arima_fluo_forecast$mean)
mape(test_fluo_bc, arima_fluo_forecast$mean)
mase(test_fluo_bc, arima_fluo_forecast$mean)

### Condições de estacionariedade e invertibilidade
autoplot(a210001_fluo)

### Previsão para 2021 com ARIMA(2,1,0)(0,0,1)[12]
fluo_arima <- Arima(fluo_ts, order=c(2,1,0), seasonal=c(0,0,1))
arima_fluotot_forecast <- forecast(fluo_arima, h=12)
autoplot(arima_fluotot_forecast) +
  xlab("Ano") +
  ylab("DDD/1.000 hab./dia")



###########################
######## Venlafaxina ####### 
###########################

####### Preparação dos dados

### Inserir dados
venl <- c(0.331837835,0.344798754,0.35597098,0.358771135,0.369180328,
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

### Transformação dos dados em objetos de séries temporais 
venl_ts <- ts(cbind(venl), start = c(2014), frequency = 12)

### Divisão dos dados entre treinamento e teste
train_venl <- window(venl_ts, start = c(2014,1), end = c(2019,12))
test_venl <- window(venl_ts, start = c(2020,1))



####### Análise preliminar

### Gráfico de linhas
autoplot(venl_ts) + xlab("Ano") + ylab("DDD/1.000 hab./dia")

### Sazonalidade
ggseasonplot(venl_ts, year.labels = TRUE) + xlab("Mês") + 
  ylab("DDD/1.000 hab./dia")  + ggtitle("") 
ggmonthplot(venl_ts) + xlab("Mês") + ylab("DDD/1.000 hab./dia") 

### Diferenciação
autoplot(diff(venl_ts)) + ylab("DDD/1.000 hab./dia") + xlab("Ano")

### Teste de estacionariedade (p-valor < 0.05 = rejeita H0)
adf.test(venl_ts)



#######  ETS

### Modelo e previsão
ets_venL_man <- ets(model="MAN", train_venl)
ets_venL_man # AIC: -168.7788
ets_venl_aan <- ets(model="AAN", train_venl) 
ets_venl_aan # AIC: -159.2407
ets_venl_maa <- ets(model="MAA", train_venl)
ets_venl_maa # AIC: -129.83406
ets_venl_aaa <- ets(model="AAA", train_venl)
ets_venl_aaa # AIC: -130.03491
ets_venl_mam <- ets(model="MAM", train_venl)
ets_venl_mam # AIC: -171.7267
ets_venl_forecast <- forecast(ets_venl_mam, h=12)

### Gráfico de resíduos do modelo
checkresiduals(ets_fluo_mam, test=FALSE)

### Teste Ljung-Box de autocorrelações dos resíduos
Box.test((residuals(ets_venl_mam)), type = "Ljung-Box")
# p-valor = 0,08436 

### Teste Shapiro-Wilk de normalidade
shapiro.test((residuals(ets_fluo_mam)))
# p-valor = 0.6682

### Gráfico de previsão do modelo
autoplot(train_venl) +
  autolayer(ets_venl_forecast$mean, series="Valor previsto", lwd=1.5) +
  autolayer(test_venl, series="Valor real", lwd=1.5) +
  xlab("Ano") +
  ylab("DDD/1.000 hab./dia") +
  ggtitle("Forecasts from ETS(M,A,M)") +
  guides(colour=guide_legend(title="Legenda"))

### Medidas de qualidade
rmse(test_venl, ets_venl_forecast$mean)
mae(test_venl, ets_venl_forecast$mean)
mape(test_venl, ets_venl_forecast$mean)
mase(test_venl, ets_venl_forecast$mean)



#######  ARIMA

### ACF e PACF da série diferenciada
gg_tsdisplay(as_tsibble(diff(train_venl)),plot_type='partial')

### Ajuste de modelos
# Modelo inicial: ARIMA(5,1,1)(0,0,1)[12]
a511001_venl <- Arima(train_venl, order=c(5,1,1), seasonal=c(0,0,1))
a511001_venl # AIC = -269.1377
coeftest(a511001_venl)
a212001_venl <- Arima(train_venl, order=c(2,1,2), seasonal=c(0,0,1))
a212001_venl # AIC =  -274.8546
a313001_venl <- Arima(train_venl, order=c(3,1,3), seasonal=c(0,0,1))
a313001_venl # AIC =  -271.2844
a212_venl <- Arima(train_venl, order=c(2,1,2))
a212_venl # AIC =  -270.27
a513001_venl <- Arima(train_venl, order=c(5,1,3), seasonal=c(0,0,1))
a513001_venl # AIC = -268.8816
a311001_venl <- Arima(train_venl, order=c(3,1,1), seasonal=c(0,0,1))
a311001_venl # AIC =  -266.5507
a313_venl <- Arima(train_venl, order=c(3,1,3))
a313_venl # AIC =  -266.40
a513_venl <- Arima(train_venl, order=c(5,1,3))
a513_venl # AIC = -264.89
a511_venl <- Arima(train_venl, order=c(5,1,1))
a511_venl # AIC = -265.42   
a311_venl <- Arima(train_venl, order=c(3,1,1))
a311_venl # AIC = -261.19   

# Dentre os modelos testados, o com menor AIC é o ARIMA(2,1,2)(0,0,1)[12]
a212001_venl
coeftest(a212001_venl)

### Gráficos do resíduos do modelo
checkresiduals(a212001_venl, test=FALSE)

### Teste Ljung-Box de autocorrelações dos resíduos
Box.test((residuals(a212001_venl)), lag=12, type = "Ljung-Box")
# p-valor = 0.8206. 

### Teste Shapiro-Wilk de normalidade
shapiro.test((residuals(a212001_venl)))
# p-valor = 0.005258

### Previsão do modelo
arima_venl_forecast <- forecast (a212001_venl, h=12)

### Gráfico com a previsão do modelo
autoplot(train_venl) +
  autolayer(arima_venl_forecast$mean, series="Valor previsto", lwd=1.5) +
  autolayer(test_venl, series="Valor real", lwd=1.5) +
  xlab("Ano") +
  ylab("DDD/1.000 hab./dia") +
  ggtitle("Forecasts from ARIMA(2,1,2)(0,0,1)[12]") +
  guides(colour=guide_legend(title="Legenda"))

### Medidas de qualidade
rmse(test_venl, arima_venl_forecast$mean)
mae(test_venl, arima_venl_forecast$mean)
mape(test_venl, arima_venl_forecast$mean)
mase(test_venl, arima_venl_forecast$mean)

### Condições de estacionariedade e invertibilidade
autoplot(a212001_fluo)

### Previsão para 2021 com ARIMA(2,1,2)(0,0,1)[12]
venl_arima <- Arima(venl_ts, order=c(2,1,2), seasonal=c(0,0,1))
arima_venltot_forecast <- forecast(venl_arima, h=12)
autoplot(arima_venltot_forecast) +
  xlab("Ano") +
  ylab("DDD/1.000 hab./dia")