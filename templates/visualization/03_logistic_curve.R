### Curva (o funzione) logistica


### Primo modo
curve( (exp(1+0.1*x))/(1+(exp(1+0.1*x))), 
       from = -100, to = 100, 
       xlab ="[x]", 
       ylab="logistic function", 
       ylim = c(0,1))


### In modo diverso
p_x <- function(x) exp(1+0.1*x)/(1 + exp(1+0.1*x))
x<-seq(-100, 100, 0.05) #range di x da -100 a 100 con delta di 0.05
matplot(x, p_x(x), type="l",col=c("red"), ylab ="Probabilità", xlab="x",
        main = "Funzione logistica")

### In modo diverso aggiungendo una o più curve nel grafico
p_x <- function(x) exp(1+0.1*x)/(1 + exp(1+0.1*x))
r <- function(x) exp(1+0.2*x)/(1 + exp(1+0.2*x))
x<-seq(-100, 100, 0.05) #range di x da -100 a 100 con delta di 0.05
matplot(x, cbind(r(x), p_x(x)), type="l", 
        col=c("red", "blue"), 
        ylab ="Probabilità", xlab="x",
        main = "Funzione logistica")
