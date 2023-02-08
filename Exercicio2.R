########## PACOTES ##############
library(ggplot2)
#################################
#---------------------- QUESTAO 2 -----------------------------------
set.seed(3666)
########################## A ##############################################

AR <- function(coefs, n){
    n <- n + (length(coefs))
    ruido <- rnorm(n,mean = 0,sd =1)
    x <- ruido
    for(t in (length(coefs) + 1):n){
      x[t] <- x[t-1]*coefs[1] + x[t-2]*coefs[2] + x[t-3]*coefs[3] + ruido[t]
    }
    return(x[(length(coefs) + 1):n])
}
coefs <- c(.3,.1,-.5)
n <- 100
y <- AR(coefs,n)
plot.ts(y)
### B #####################################################################

#AMOSTRAL ------
#funcoes
acf.amostral <- function(y){
  n <- length(y)
  gama <- vector()
  for(h in 0:(n-1)){
      gam <- 0
      for (t in 1:(n-h)) {
        gam <- gam+(y[t+h]-mean(y))*(y[t]-mean(y))
      }
      gama[h + 1] <- gam/n
  }
  pho <- gama/gama[1]
  return(pho)
}

#valores
lags <- c(0:(n-1))
acfs <- data.frame(acf = acf.amostral(y),
                   lag = lags)
#plot
cline <- 1.96/sqrt(length(y))
ggplot(data=acfs[1:50,], aes(x=lag, y=acf)) +
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0) +
  geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = cline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -cline), linetype = 2, color = 'darkblue') +
   ylab('Autocorrelação') + xlab('Lags')
#conferindo
acf(y,lag.max = 50)
# TEORICO ----
acf.teorico <- function(y){
  #media de y = 0
  n <- length(y)
  gama <- vector()
  for(h in 0:(n-1)){
    gam <- 0
    for (t in 1:(n-h)) {
      gam <- gam+(y[t+h])*(y[t])
    }
    gama[h + 1] <- gam/n
  }
  pho <- gama/gama[1]
  return(pho)
}

#valores
lags <- c(0:(n-1))
acfs.t <- data.frame(acf = acf.teorico(y),
                   lag = lags)
ggplot(data=acfs.t[1:50,], aes(x=lag, y=acf)) +
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0) +
  geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = cline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -cline), linetype = 2, color = 'darkblue') +
  ylab('Autocorrelação') + xlab('Lags')
pacf(y,lag.max = 50)

### C ######################################################################
set.seed(234)
logLik <- function(coefs, y) {
  a1 <- coefs[1]
  a2 <- coefs[2]
  a3 <- coefs[3]
  c <- coefs[4]
  n <- length(y)
  aux <- 1/(n - 3)
  sigma2 <- 0
  for(t in 4:n){
    sigma2 <- sigma2 + (y[t] - c - y[t-1]*a1 - y[t-2]*a2 - y[t-3]*a3)^2
  }
  return(sigma2*aux)
}

# Define os parâmetros iniciais
coefs_iniciais <- c(runif(3,-1,-1), 1)
y <- AR(c(.3,.1,-.5),10000)
# Chama a função de otimização
fit <- optim(coefs_iniciais, logLik, method = "BFGS", y = y)

# Exibe os parâmetros otimizados
fit$par ## a1,a2,a3 e c
fit$value # sigma^2
# set.seed(43); y <- arima.sim(n = 100, list(ar = c(0.3, 0.1, -0.5)))
# fit <- optim(coefs_iniciais, logLik, method = "BFGS", y = y)
### D #################################################################
set.seed(27)
B <- 2000
a1 <- vector()
a2 <- vector()
a3 <- vector()
sigma2 <- vector()
c <- vector()
n <- 500
coefs <- c(.3,.1,-.5)
for(b in 1:B){
  y <- AR(coefs,n)
  fit <- optim(c(1,1,1,1), logLik, method = 'BFGS', y =y)
  a1[b] <- fit$par[1]
  a2[b] <- fit$par[2]
  a3[b] <- fit$par[3]
  c[b]<- fit$par[4]
  sigma2 <- fit$value

}
estimativas <- data.frame(a1 = a1,
                          a2 = a2,
                          a3 = a3,
                          c = c,
                          sigma2 = sigma2)
apply(estimativas,2,mean ) # MEDIA DOS PARAMETROS
apply(estimativas[,1:4],2,shapiro.test) #TESTE DE NORMALIDADE DE SHAPIRO WILK
qqnorm(estimativas$a1)
qqnorm(estimativas$a2)
qqnorm(estimativas$a3)
qqnorm(estimativas$c)
