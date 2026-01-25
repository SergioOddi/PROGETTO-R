####### DISEGNO DI FUNZIONI (curve)

### CIRCUITO RC

### Curva di Vres in funzione del tempo
R = 2000 # ohm
C = 0.000001 # faraday
E = 10 # volt 
curve( (E*exp(-1*x/(R*C))), 
       from = 0, to = 0.02, 
       xlab ="s", 
       ylab="Vres", 
       ylim = c(0,12))
abline(h = 4, v = 0.002, lty=2)


### Curva di Vcap in funzione del tempo
R = 2000 # ohm
C = 0.000001 # faraday
E = 10 # volt 
curve( (E*(1-exp(-1*x/(R*C)))), 
       from = 0, to = 0.02, 
       xlab ="s", 
       ylab="Vcap", 
       ylim = c(0,12))
abline(h=6, v= 0.002, lty = 3) #aggiunge linee orizzontali e verticali definite, con lo #stile indicato da lty, 3 indica linea tratteggiata



#### In modo più ordinato
R = 2000 # ohm
C = 0.000001 # faraday
E = 10 # volt 
x <- seq(from = 0, to = 0.02, by=.0001) ##stabilisce come far variare la x
ESP <-  x/(R*C)   #### definisce l’esponente della potenza di e (in questo caso: beta0 = 1, beta1 = 0.1)
Vres <- E*exp(-ESP) #### qui uso il termine di sopra per scrivere in modo più chiaro la funzione logistica
Vcap <- E*(1-exp(-ESP))
plot(x, Vres, 
     ylim=c(0,12),
     type="l", 
     lwd=1, 
     lty=1, 
     col="red", 
     xlab="time (s)", ylab="V (volt)", main="Andamento temporale di Vcap (nero) e Vres (rosso) in un circuito RC in serie")
# add a horizontal line at p=.5
abline(h = 4, v = 0.002, lty=2)
par(new=TRUE)
plot(x, Vcap,
     ylim=c(0,12),
     type="l", 
     lwd=1, 
     lty=1,
     xlab="time (s)", ylab="V (volt)")


