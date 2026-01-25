# Testing Assumptions: Normality and Equal Variances ##################################################################################
# Novembre 2020

#carico i dati; il dataset contiene un vettore con i due fattori (Treatment) e una colonna con i loro valori numerici (Value)
dati <- dati_BACE1_actin


# Testing NORMALITY ####################################################################################################################

#Seleziono il primo fattore
datiURB <- subset(dati, Treatment == "URB")

#Domanda: i dati del campione sono distribuiti in modo normale?
#TEST di SHAPIRO (testing normality)
shapiro.test(datiURB$Value)
#Interpretazione: se il p-value del test è < 0.05, non si può rigettare l'ipotesi che i dati siano distribuiti in modo non-normale

#Results: il test ci restituisce un p-value maggiore di 0.05, per cui possiamo accettare l'ipotesi che i dati sono distribuiti in modo normale

#Seleziono il secondo fattore
datiVehicle <- subset(dati, Treatment == "Vehicle")

#TEST di SHAPIRO
#verifico se i dati sono distribuiti in modo normale
shapiro.test(datiVehicle$Value)

#Interpretazione: se il test è positivo (P < 0.05) la variabile non è distribuita in modo normale
#Results: il test ci restituisce un p-value maggiore di 0.05, per cui possiamo accettare l'ipotesi che i dati sono distribuiti in modo normale

# Boxplot dei dati
library("plotly")
library("ggplot2")
library("ggpubr")


p <- ggboxplot(
  dati, x = "Treatment", y = "Value",
  scales ="free", col ="black", fill ="Treatment") +
  geom_jitter(width=0.0, alpha = 1, size = 3) + #caratteristiche dello scatter dei dati
  stat_summary(fun=mean, geom="point", shape=5, size=4) + #fa vedere la media come rombo
  labs(title="", #se serve il titolo
       x = "Treatment", #nome delle ascisse
       y = "Value") + #nome delle ordinate
  theme(text = element_text(size = 36, face="bold", color ="black", family = "Arial"), #qui size stabilisce la dimensione del testo delle variabili nel box
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text=element_text(size=28, face="bold", color ="black"), #setta dimensioni dei caratteri sugli assi coordinati
        axis.title=element_text(size=32, face="bold", color="black"),
        legend.position = "none"  # se si vuole la legenda: legend.position = "bottom"
  ) 

#per vederlo come ggplot2:
p#il rombo indica la media

#per vederlo in plotly:
options(digits = 3);
plotly_build(p) %>% layout(yaxis = list(hoverformat = '.3f')) #qui stabilisco il numero di cifre che plotly mi fa vedere



# Testing asimmetria dei dati ################################################################################################################

#The skewness coefficient (coefficiente di asimmetria) can be computed using the moments R package:
library(moments)

skewness(datiURB$Value, na.rm = FALSE)
skewness(datiVehicle$Value, na.rm = FALSE)
###  INTERPRETAZIONE
#	▪	A zero means no skewness at all (normal distribution).
#▪	A negative value means the distribution is negatively skewed (coda a sx)
#▪	A positive value means the distribution is positively skewed (coda a dx)



#Testing NORMALITY ####################################################################################################################

#Equal Variances (F-test):
#Qui valuto se la varianza è uguale o meno.
#Per farlo useremoF-test to test for homogeneity in variances.
#F-test

var.test(datiURB$Value, datiVehicle$Value)
###  INTERPRETAZIONE: se il test mi dà un p-value > 0.05 posso rigettare l'ipotesi nulla che le "due varianze non siano uguali"
#Results: il test è negativo, quindi le varianze dei due campioni possono essere considerate uguali. 
#Questo aspetto va inserito nei test di ipotesi con il comando "var.equal =" che può essere "TRUE" o "FALSE"

#####In base ai risultati di questi test, posso decidere se effettuare un test parametrico, come il t test (o, se ho più di due gruppi, l'ANOVA); 
#oppure uno non parametrico, come il Mann-Whitney test (o, se ho più gruppi, il Kruskal-Wallis test).

#In alternativa a usare i test non-parametrici, si possono modificare matematicamente i dati originali allo scopo di ottenere dati distributi in modo normale.
#A tal riguardo, vedasi lo script: "Manipolazione algebrica dei dati".
