# Boxplot dei dati

library("reshape2")
library("dplyr")
library("plotly")
library("ggplot2")
library("ggpubr")
library("lattice")


dati <- Dataset_AKTnormPARZ

#RESHAPING DATAset
dati = melt(dati, id.var ="Group")
head(dati)

# Eventualmente rimuovo uno o più livelli per avere un certo numero di gruppi
dati <- subset(dati, variable != "ID")

dati <- subset(dati, Group != "URB_SR1")

dati

#il dataset contiene un vettore con tre trattamenti (Group) e una colonna con i loro valori numerici (Inhibition)


dati$Group <- as.factor(dati$Group)
dati$value <- as.numeric(dati$value)

str(dati)


p <- ggboxplot(
  dati, x = "Group", y = "value", 
  facet.by = "variable",
  scales ="free", col ="black", fill ="Group", 
) +
  geom_jitter(width=0.0, alpha=0.2) + #caratteristiche dello scatter dei dati
  stat_compare_means(
  comparisons = list(c("CTRL", "URB")), 
  label = "p.signif", size =5, vjust = 0.3) + #label: decide se mostrare il numero o l'asterisco, size: la taglia e vjust: la sua posizione rispetto alla graffetta 
  labs(title="", #se serve il titolo
       x="Treatment", #nome delle ascisse
       y = "Normalized value") # + #nome delle ordinate
  #theme(text = element_text(size = 18, face="bold", color ="black", family = "Arial"), #qui size stabilisce la dimensione del testo delle variabili nel box
  #      axis.text.x = element_text(angle = 45, hjust = 1), 
  #      axis.text=element_text(size=14, face="bold", color ="black"), #setta dimensioni dei caratteri sugli assi coordinati
  #      axis.title=element_text(size=16, face="bold", color="black"),
   #     legend.position = "bottom"
  #) 


#per vederlo come ggplot2:
p#il rombo indica la media

#per vederlo in plotly:
options(digits = 3);
plotly_build(p) %>% layout(yaxis = list(hoverformat = '.3f')) #qui stabilisco il numero di cifre che plotly mi fa vedere



group_by(dati, Group) %>%
  summarise(
    count = n(),
    median = median(variable, na.rm = F), 
    mean = mean(variable, na.rm = F),
    sd = sd(variable, na.rm = F),
    min = min(variable, na.rm = F),
    max = max(variable, na.rm = F),
    IQR = IQR(variable, na.rm = F)
  )


library(summarytools)

stby(dati, dati$Group, dfSummary, round.digits=8)#round.digits=8 approssima i dati a 8 cifre decimali


#dataframe: z
dfSummary(value) # mi dà le statistiche delle colonne di z




