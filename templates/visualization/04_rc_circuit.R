#### Andamento temporale di Vcap e Vres in un circuito RC con elementi messi in serie
#### Versione 2.0
#### Dicembre 2021


R = 2000     # in ohm
C = 0.000001 # in faraday
E = 10       # in volt 
x <- seq(from = 0, to = 0.02, by=.0001) ### stabilisce il range di valori con cui far variare la x
Vres <- function(x) E*exp(-x/(R*C)) #### qui uso il termine di sopra per scrivere in modo più chiaro la funzione
Vcap <- function(x) E*(1-exp(-x/(R*C)))

matplot(x, cbind(Vres(x), Vcap(x)), ### quali funzioni di x disegnare
     xlim=c(0,0.008),               ### range dell'asse delle ascisse (può essere diverse dal range scelto su cui calcolare le funzioni)
     ylim=c(0,10),                  ### range dell'asse delle ordinate
     type="l",     
     lwd=c(1,1),                    ### spessore delle linee
     lty=c(1,2),                    ### tipo di linea: continua (1), spezzata (2), ecc.
     col=c("red", "blue"),          ### colore  linee: in base all'ordine stabilito nella prima riga
     xlab="time (s)",               ### nome asse x
     ylab="V (volt)",               ### nome asse y 
     main="Andamento temporale di Vcap (nero) e Vres (rosso) in un circuito RC in serie")  #### Titolo grafico
abline(h = 4, v = 0.002, lty=2)     ### aggiunge una linea orizzontale e una verticale con determinati valori di x (h) e y (v)














