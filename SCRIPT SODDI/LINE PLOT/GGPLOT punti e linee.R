#### GGPLOT Punti 
######### Timepoints con media ed errori 
######### 4 novembre 2023

library(ggplot2)
library(ggsignif)
library(dplyr)
library(rstatix)


options(scipen=999)


#### Carico data set con colonne repliche di una misura
dati <- Dataset_ZnOAT_MTT

head(dati)

## Cambiare in fattori i vettori categorici ID e Age
dati$Group <- as.factor(dati$Group)
dati$Concentration <- as.factor(dati$Concentration)
dati$Time <- as.factor(dati$Time)

# Calcola il one-sample t-test per ogni combinazione di Concentration, Treatment e Time; confronto con valore fisso: 100
results <- dati %>%
  group_by(Concentration, Group, Time) %>%
  summarise(
    t_value = t.test(Vitality, mu = 100)$statistic,
    p_value = t.test(Vitality, mu = 100)$p.value
  ) %>%
  ungroup()

# Aggiustamento dei p-value per confronti multipli
p_values_adjusted <- p.adjust(results$p_value, method = "bonferroni")

# Aggiungi i p-value aggiustati al dataframe results
results$p_value_adjusted <- p_values_adjusted

# Unisci i risultati del test t al dataframe originale, se necessario
dati <- merge(dati, results, by = c("Concentration", "Group", "Time"))


# Calcola la media e lo scarto standard per ogni combinazione di Concentration, Group e Time
stats <- dati %>%
  group_by(Concentration, Group, Time) %>%
  summarise(
    mean_vitality = mean(Vitality),
    sd_vitality = sd(Vitality),
    .groups = 'drop' # Evita i messaggi di raggruppamento
  )

### Confronto tra la Vitalità tra i diversi Gruppi
stat.test <- dati %>%
  group_by(Concentration, Time) %>%
  t_test(Vitality ~ Group) %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj")
stat.test  


# Unisci i risultati statistici al dataframe originale, se necessario
dati <- merge(dati, stats, by = c("Concentration", "Group", "Time"))

do=0.0    #### se si vuole separare i valori che si sovrappongono, do = 0.3 o simili
sizeT=16  #### stabilire la dimensione dei font sul grafico

# Crea il grafico utilizzando le colonne corrette per l'errore
p <- ggplot(data= dati, 
            aes(x = Concentration, y = mean_vitality, color = Group)) +
  scale_shape_manual(values =c(22,24))+
  scale_color_manual(values = c("#E7B800", "steelblue"))+
  geom_line(aes(group = Group), position=position_dodge(do), size =0.5, linetype = 1) +
  ylim(c(0, 120))+ #### Range asse y
  geom_errorbar(aes(ymin = mean_vitality - sd_vitality, ymax = mean_vitality + sd_vitality, color = Group), width=.2, position = position_dodge(width = do)) +
  geom_point(aes(shape = Group, color = Group), size = 3, stroke = 1, fill = "white", position = position_dodge(width = do)) +
  facet_wrap(~Time, ncol=1)+           ## se ci altri fattori con cui suddividere i grafici
  #facet_grid(Treatment ~ .,)+ 
  #facet_grid(TIME ~ .,)+                 # Add vertical facets, aka divide the plot up vertically since they share an x axis
  geom_text(aes(label = ifelse(p_value_adjusted < 0.05, sprintf("p=%.5f", p_value_adjusted), "")), #### p=%.5f -> mostra il P value con 5 cifre decimali 
            position = position_dodge(0.2), vjust = -1, check_overlap = TRUE) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", size = 0.5)+ #### Aggiunge un asse orizzontale a un valore fissato
  labs(title="",                                   #se serve il titolo
       x = "Concentration (μg/mL)",                    #nome delle ascisse
       y = "Normalized cell viability (% over control)") +                              #nome delle ordinate
  
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Helvetica"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=sizeT, angle = 0, hjust = 0, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse; hjust sposta le cifre rispetto all'asse
        axis.text.y = element_text(size=sizeT, angle = 0, hjust = 1, face="bold", color="black"),  #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", colour="black"), # Lettering titolo asse y
        axis.line.x = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse x
        axis.line.y = element_line(colour = "black", size = 0.5),  # Colore Linea dell'asse y
        legend.key  = element_rect(fill = "white"),# caratteristiche della legenda
        legend.title = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.text = element_text(size=12, angle = 0, hjust = 1, face="bold", color="black"),
        legend.position = "right" ##### Se non si vuole la legenda, "none" 
  )

# Stampa il grafico
print(p)

# Assumi che il dataset stat.test contenga le colonne x, y per la posizione e p.adj.signif per il valore del p
p <- p + geom_text(data = stat.test, aes(x = Concentration, y = 120, label = p.adj.signif), 
                   vjust = 0, # Ajusta verticalmente per evitare la sovrapposizione con le barre di errore
                   color = "red", size = 5) # Colore del testo, scegli ciò che si adatta al tuo schema di colori

# Stampa il grafico
print(p)


