########## PACOTES ##############
#################################


####### QUESTAO 2 ################
## A ##
AR <- function(coefs){
    n <- 1000
    ruido <- rnorm(n,0,1)
    x <- ruido
    for(t in (length(coefs) + 1):n){
      x[t] <- sum(coefs * x[(t-length(coefs)):(t-1)]) + ruido[t]
    }
    return(x)
}
coefs <- c(.3,.1,-.5)
AR(coefs) |> ts.plot()

