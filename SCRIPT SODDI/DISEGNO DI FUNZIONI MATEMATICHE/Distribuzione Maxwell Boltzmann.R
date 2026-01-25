##### Distribuzione velocità delle molecole di Maxwell Boltzmann
#### Versione 1.1
#### 21 dicembre 2021

#Valore numerico della costante dei gas (unità: kJ K-1 mol-1)
R= 8.31446261815324*1000 # R (in g m2 s-2 K-1 mol-1) è importante l'unità di misura delle costanti

##### Setto tre masse molari (g mol-1)
m1 = 4
m2 = 50
m3 = 25

#### Setto tre T (in K)
T1 = 100  ### Pari a xxx°C
T2 = 300  ### Pari a xxx°C
T3 = 500  ### Pari a xxx°C

### Temperatura costante T = 300 K
x <- seq(0, 1500, 0.05) # range di velocità (x) da 0 a 600 (m s-1) con delta di 0.05
F_m1 <-  function(x) 4*pi*x^2*exp((-m1*x^2)/(2*R*T2))*(((m1/(2*pi*R*T2))^(3/2)))
F_m2 <-  function(x) 4*pi*x^2*exp((-m2*x^2)/(2*R*T2))*(((m2/(2*pi*R*T2))^(3/2)))
F_m3 <-  function(x) 4*pi*x^2*exp((-m3*x^2)/(2*R*T2))*(((m3/(2*pi*R*T2))^(3/2)))

matplot(x, cbind( F_m1(x), F_m2(x), F_m3(x) ),
        # xlim=c(0, 1500)
        # ylim=c(0, 0.1),
        type="l",
        lwd = c(1,1,1),   ### spessore delle linee
        lty = c(1,1,1),  ### tipo di linea
        col= c("red", "blue", "black"),
        ylab ="Frazione", 
        xlab="Velocità (m/s)",  #ylab e xlab servono per il nome degli assi #cartesiani
        main = "Dipendenza della frazione di molecole dalla massa molare")

legend("topright", legend = c(m1, m2, m3), col = c("red", "blue", "black"),
       title = "Massa molare (g/mol)", lwd = 2)
abline(v= 400, lty =2)


### Massa molare costante m = 50 g mol-1
x <- seq(0, 1000, 0.05) # range di velocità (x) da 0 a 600 (m s-1) con delta di 0.05
F_T1 <-  function(x) 4*pi*x^2*exp((-m2*x^2)/(2*R*T1))*(((m2/(2*pi*R*T1))^(3/2)))
F_T2 <-  function(x) 4*pi*x^2*exp((-m2*x^2)/(2*R*T2))*(((m2/(2*pi*R*T2))^(3/2)))
F_T3 <-  function(x) 4*pi*x^2*exp((-m2*x^2)/(2*R*T3))*(((m2/(2*pi*R*T3))^(3/2)))

matplot(x, cbind( F_T1(x), F_T2(x), F_T3(x) ),
        # xlim=c(0, 1000)
        # ylim=c(0,0.1),
        type="l",
        lwd = c(1,1,1),   ### spessore delle linee
        lty = c(1,1,1),  ### tipo di linea
        col= c("red", "blue", "black"),
        ylab ="Frazione", 
        xlab="Velocità (m/s)",  #ylab e xlab servono per il nome degli assi #cartesiani
        main = "Dipendenza della frazione di molecole dalla temperatura")

legend("topright", legend = c(T1, T2, T3), col = c("red", "blue", "black"),
       title = "Temperatura (K)", lwd = 2)
abline(v= 400, lty =2)










