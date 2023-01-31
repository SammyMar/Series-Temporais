############################### Questão 1 ######################################
library("tseries")
library("itsmr")
library("tsibble")
library("forecast")
library("TSA")

############################## i #####################################
##################### (a)

dados <- read.csv("dados7.csv", dec = ",")

dados2 <- ts(dados$x)

treino <- head(dados2, (length(dados2) - 10))
teste <- tail(dados2, 10)

plot.ts(treino)
acf(treino)
pacf(treino)
TSA::periodogram(treino)

dados_diff <- diff(treino)

plot.ts(dados_diff)
acf(dados_diff, lag.max = 90) # periodo 12
pacf(dados_diff, lag.max = 40)
TSA::periodogram(dados_diff)

dados_diff2 <- diff(dados_diff, 12)

plot.ts(dados_diff2)
acf(dados_diff2, lag.max = 90)
pacf(dados_diff2, lag.max = 40)
TSA::periodogram(dados_diff2)

eacf(treino)

# Modelos

model1 <- stats::arima(treino,
                       order = c(4,1,0),
                       seasonal = list(order = c(0,1,0),
                                       period = 12))
lmtest::coeftest(model1)

model2 <- stats::arima(treino,
                       order = c(3,1,0),
                       seasonal = list(order = c(0,1,0),
                                       period = 12))
lmtest::coeftest(model2)

model3 <- stats::arima(treino,
                       order = c(2,1,0),
                       seasonal = list(order = c(0,1,0),
                                       period = 12))
lmtest::coeftest(model3)


AIC(model1)
AIC(model2)
AIC(model3)


# Diagnósticos
tsdiag(model1)
tsdiag(model2)
tsdiag(model3)

car::qqPlot(model3$res)
cpgram(model1$res)
cpgram(model2$res)
cpgram(model3$res)

# Previsões h passos
plot(forecast(model1, h = 10, boot = TRUE))

forecast::accuracy(teste, as.numeric(forecast(model1, h = 10)$mean))


# Funções de atualização de previsões (SARIMA)
error_update1 <- function(data,
                          orders,
                          coefsig){
  length_test <- nrow(as.data.frame(teste))
  # training data base size
  length_training <- nrow(as.data.frame(treino))
  # complete serie
  error <- matrix(NA, nrow = length_test, ncol = 1)
  prev <- matrix(NA, nrow = length_test, ncol = 1)
  for (i in 1:length_test){
    model <- stats::arima(data[1:length_training + i - 1],
                          order = orders,
                          seasonal = list(order = c(0,1,0),
                                          period = 12),
                          fixed = coefsig,
                          method = "ML")

    prediction <- as.data.frame(forecast(model, h = 1, boot = TRUE))
    error[i] <-  (data[length_training + i] -
                    prediction$`Point Forecast`)
    prev[i] <- prediction$`Point Forecast`
  }
  RMSE <- sqrt(sum(error^2)/length(error))
  MAE <-  sum(abs(error))/length(error)
  return(list(RMSE, MAE, prev))
}

# Previsões 1 passo atualizando (validação)
model1prev <- error_update1(data = dados2,
                            orders = c(4, 1, 0),
                            coefsig = NULL)

# Plot previsões
plot(as.numeric(teste), type = "l")
lines(unlist(model1prev[3]), col = "red")

# dataop <- as.data.frame(model1prev[3])
# colnames(dataop) <- "prev"
# dataop$teste <- teste


mean(abs((teste - unlist(model1prev[3]))/teste)) * 100

################################# (b)


################################# (c)

plot.ts(dados2)
itsmr::plotc(dados2, itsmr::smooth.ma(dados2, q = 1)) # suavização por médias moveis

dados3 <- dados2 - itsmr::smooth.ma(dados2, q = 1)
plot.ts(dados3)
acf(dados3, lag.max = 90) # periodo 12
pacf(dados3, lag.max = 90)
TSA::periodogram(dados3)

dados3_period <- cbind(
  TSA::periodogram(dados3)$freq,
  TSA::periodogram(dados3)$spec
)

freq_dados3 <- head(
  dados3_period[order(dados3_period[,2], decreasing = T),],
  12)

sazo <- itsmr::hr(dados3, 1/freq_dados3[1:10,1])
itsmr::plotc(dados3, sazo)

dados4 <- dados3 - sazo

treino2 <- head(dados4, (length(dados4) - 10))
teste2 <- tail(dados4, 10)


plot.ts(treino2)
acf(treino2, lag.max = 90) # periodo 12
pacf(treino2, lag.max = 90)
TSA::periodogram(treino2)

############################# modelo AR
eacf(treino2)

model11 <- stats::arima(treino2, order = c(3,0,0))
lmtest::coeftest(model11)

model22 <- stats::arima(treino2, order = c(2,0,0))
lmtest::coeftest(model22)

model33 <- stats::arima(treino2, order = c(1,0,0))
lmtest::coeftest(model33)

AIC(model11)
AIC(model22)
AIC(model33)


# Diagnósticos
tsdiag(model11)
tsdiag(model22) # descarta
tsdiag(model33) # descarta

car::qqPlot(model11$res) # sugere normalidade

# Previsões h passos
plot(forecast(model11, h = 10))

forecast::accuracy(teste, as.numeric(forecast(model11, h = 10)$mean))


# Funções de atualização de previsões (AR)
error_update2 <- function(data,
                         orders,
                         coefsig){
  length_test <- nrow(as.data.frame(teste))
  # training data base size
  length_training <- nrow(as.data.frame(treino))
  # complete serie
  error <- matrix(NA, nrow = length_test, ncol = 1)
  prev <- matrix(NA, nrow = length_test, ncol = 1)
  for (i in 1:length_test){
    model <- stats::arima(data[1:length_training + i - 1],
                          order = orders,
                          fixed = coefsig,
                          method = "ML")

    prediction <- as.data.frame(forecast(model, h = 1, boot = TRUE))
    error[i] <-  (data[length_training + i] -
                    prediction$`Point Forecast`)
    prev[i] <- prediction$`Point Forecast`
  }
  RMSE <- sqrt(sum(error^2)/length(error))
  MAE <-  sum(abs(error))/length(error)
  return(list(RMSE, MAE, prev))
}

# Previsões 1 passo atualizando (validação)
model11prev <- error_update2(data = dados4,
                             orders = c(3, 0, 0),
                             coefsig = NULL)

# Plot previsões
plot(as.numeric(teste2), type = "l")
lines(unlist(model11prev[3]), col = "red")

############################## MAPE ########################################
mean(abs((teste2 - unlist(model11prev[3]))/teste2)) * 100






############################### Questão 2 ######################################

##################################### (a) Gerando um AR(3)

simulaAR3 <- function(n, phi1, phi2, phi3, sd){
  n2 <- round(n/10) # 10%
  n <- n + n2 # soma os 10%
  x <-  rep(0,n) # vetor para os resultados
  e <- rnorm(n, mean = 0, sd = sd) # ruido branco
  for (t in 4:n){
    x[t] = phi1*x[t-1] + phi2*x[t-2] + phi3*x[t-3] + e[t]
  }
  x <- x[-(1:n2)] # resultados menos os 10% primeiros valores
  return(x)
}

y <-  simulaAR3(n = 100, phi1 = 0.3, phi2 = 0.1, phi3 = -0.5, sd = 1)
plot(y, type = "o")

################################# (b) Gerando as funções ACF de um AR(3)

# Autocorrelação amostral
autocorr <- function(x){ # x = serie, nl = quantos lags (nl < n)
  n <- length(y)
  gammah <- rep(NA, n + 1)
  for(h in 0:(n-1)){
    aux <- 0
    for(t in 1:(n-h)){
      aux <- (x[t + h] - mean(x))*(x[t] - mean(x)) + aux
    }
    if(h == 0){gammah[h+1] <- aux/n}else{gammah[h+1] <- aux/n}
  }
  rho <- (gammah/gammah[1])
  return(list(rho = rho,
              acf =  gammah))
}

plot(autocorr(y)$rho,
     type = "h",
     xlab = "lag",
     ylab = "Autocorrelação",
     main = "Autocorrelação Amostral")
abline(h = 0)
abline(h = c(1, -1) * 1.96/sqrt(length(y)),
       lty = 2,
       col = "blue")

# Autocorrelação teórica
autocorr2 <- function(x){ # x = serie, nl = quantos lags
  n <- length(x)
  gammah <- rep(NA, n + 1)
  for(h in 0:(n-1)){
    aux <- 0
    for(t in 1:(n-h)){
      aux <- (x[t + h] - 0)*(x[t] - 0) + aux
    }
    if(h == 0){gammah[h+1] <- aux/n}else{gammah[h+1] <- aux/n}
  }
  rho <- (gammah/gammah[1])
  return(list(rho = rho,
              acf =  gammah))
}

plot(autocorr2(y)$rho,
     type = "h",
     xlab = "lag",
     ylab = "Autocorrelação",
     main = "Autocorrelação Teórica")
abline(h = 0)
abline(h = c(1, -1) * 1.96/sqrt(length(y)),
       lty = 2,
       col = "blue")

# Autocorrelação amostal usando acf
acf(y, lag.max = 50)


######################## (c) Estimando parametros de um AR(3) com Yule-Walker

# phi1 = 0.3, phi2 = 0.1, phi3 = -0.5

yuleWalker <- function(y){ # y = serie
  r <- autocorr(y)$rho[2:4]
  mr <- matrix(c(1 , r[1], r[2],
                 r[1], 1, r[1],
                 r[2], r[1], 1),
                 byrow = TRUE,
                 nrow = 3)
  b <- matrix(c(r[1], r[2], r[3]),
              byrow = FALSE,
              nrow = 3)
  solution <- solve(mr, b)
  sigmahat <- autocorr(y)$acf[1] * (1 - sum(solution*r))
  return(list(phihat = solution, sigmahat = sigmahat))
}

##################################### (d) Avaliando LG e TCL

lgm <- function(k, yn){ # yn = tamanho da serie, n = numero de iterações
  phihat1 <- rep(NA, k)
  phihat2 <- rep(NA, k)
  phihat3 <- rep(NA, k)
  sigmahat <- rep(NA, k)

  for (i in 1:k){
    y <-  simulaAR3(n = yn, phi1 = 0.3, phi2 = 0.1, phi3 = -0.5, sd = 1)
    results <- yuleWalker(y)
    phihat1[i] <- results$phihat[1]
    phihat2[i] <- results$phihat[2]
    phihat3[i] <- results$phihat[3]
    sigmahat[i] <- results$sigmahat[1]
  }
  return(list(phihat1 = phihat1,
              phihat2 = phihat2,
              phihat3 = phihat3,
              sigmahat = sigmahat))
}

lgm_m <- function(m, n, yn){ # yn = tamanho da serie, n = numero de iterações
  phihat1 <- rep(NA, n)
  phihat2 <- rep(NA, n)
  phihat3 <- rep(NA, n)
  phihat1_mean <- rep(NA, m)
  phihat2_mean <- rep(NA, m)
  phihat3_mean <- rep(NA, m)
  sigmahat <- rep(NA, n)
  sigmahat_mean <- rep(NA, m)

  for (k in 1:m){
    for (i in 1:n){
      y <-  simulaAR3(n = yn, phi1 = 0.3, phi2 = 0.1, phi3 = -0.5, sd = 1)
      results <- yuleWalker(y)
      phihat1[i] <- results$phihat[1]
      phihat2[i] <- results$phihat[2]
      phihat3[i] <- results$phihat[3]
      sigmahat[i] <- results$sigmahat[1]
    }
    phihat1_mean[k] <- mean(phihat1)
    phihat2_mean[k] <- mean(phihat2)
    phihat3_mean[k] <- mean(phihat3)
    sigmahat_mean[k] <- mean(sigmahat)
  }

  return(list(phihat1_mean = phihat1_mean,
              phihat2_mean = phihat2_mean,
              phihat3_mean = phihat3_mean,
              sigmahat_mean = sigmahat_mean))
}


