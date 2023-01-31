########## PACOTES ##############
#################################


####### QUESTAO 2 ################
set.seed(3666)
## A ##

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

## B ##

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
library(ggplot2)
cline <- 1.96/sqrt(length(y))
ggplot(data=acfs[1:50,], aes(x=lag, y=acf)) +
  geom_errorbar(aes(x=lag, ymax=acf, ymin=0), width=0) +
  geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = cline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -cline), linetype = 2, color = 'darkblue') +
  ggtitle('Autocorrelação Amostral') + ylab('Autocorrelação') + xlab('Lags')
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
  ggtitle('Autocorrelação Teórica') + ylab('Autocorrelação') + xlab('Lags')
pacf(y,lag.max = 50)
