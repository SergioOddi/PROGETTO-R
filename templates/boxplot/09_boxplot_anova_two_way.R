####### BOXPLOT2 ANOVA a due Vie
### 11/12/2023

### Librerie
library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)

### Values
options(scipen=999) #  Esprime i numeri in formato fisso
sizeT = 16 ## per definire la dimensione dei caratteri nel grafico


#### Carico dataset e tengo le colonne che mi servono
df <- ELISA_Hippocampus_12_months_citochine_Foglio1
df <- df[, c(1:4)]

### Dataset con una variabile numerica "len" e "due variabili categoriche"
df$Genotype <- as.factor(df$Genotype)
df$Treatment <- as.factor(df$Treatment)
head(df, 3)


### Per ordinare i livelli nelle variabili categoriche
df$Genotype <- ordered(df$Genotype,
                       levels = c("WT", "Tg"))
df$Treatment <- ordered(df$Treatment,
                        levels = c("VEH", "URB"))

# Creazione del boxplot senza analisi statistiche
bxp <- ggboxplot(df, x = "Genotype", y = "GFAP",
                 color = "black",
                 order =c("WT", "Tg"),  # per ordinare i fattori nella variabile x
                 palette = "npg",
                 fill = "Treatment"
) +
  geom_point(aes(group = Treatment),  # Gruppa i punti in base a 'education_level' per il dodge
             position = position_dodge(0.8),  # Allineamento dei punti
             color = "black",  # Colore dei punti
             fill = "white",  # Riempimento dei punti
             shape = 21,  # Forma dei punti
             stroke = 1,  # Spessore del bordo dei punti
             size = 3  # Dimensione dei punti
  ) +
  stat_summary(aes(group = Treatment),  # Sommario statistico per ogni gruppo
               fun = mean,
               geom = "point",
               shape = 3,
               size = 3,
               color = "black",
               fill = "red",
               position = position_dodge(0.8)
  ) +
  labs(title="",                           #se serve il titolo
       x = "Genotype",                #nome delle ascisse
       y = "Fold over control") +                        #nome delle ordinate
  theme(text = element_text(size=sizeT, color = "black", face="bold", family = "Helvetica"),           #qui size stabilisce caratteristiche del nome delle variabili nel riquadro in alto ai grafici
        axis.text.x = element_text(size=sizeT, angle = 0, hjust = 0.5, face="bold", color="black"), #decide dimensione e inclinazione del nome dei gruppi sulle ascisse
        axis.text.y = element_text(size=sizeT, hjust = 1, face="bold", color="black"),             #decide dimensione e inclinazione del nome dei gruppi sulle ordinate
        axis.title.x = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse x
        axis.title.y = element_text(size=sizeT, face="bold", color="black"),                       # Lettering titolo asse y
        legend.position = "right")

# Visualizza il boxplot senza analisi statistiche
bxp


#### ANALISI STATISTICHE
### Primo test statistico
###### Group the data by the "Genotype" variable and then compare gli effetti sulla variabile dipendente "GFAP" con i diversi livelli della variabile "Treatment":

##### Se serve, stabilisco quali coppie confrontare:
#my_comparisons <- list(c("VEH", "URB"))

### Creo il risultato del primo confronto stat.test
stat.test <- df %>%
  group_by(Genotype) %>%
  t_test(GFAP ~ Treatment,
         comparisons =  NULL, #### altrimenti my_comparisons
         #ref.group = "CTRL",   
         paired = FALSE,
         var.equal = TRUE,
         alternative = "two.sided",
         #       mu = 0,
         conf.level = 0.95,
         detailed = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>% ### holm, ecc.
  add_significance("p.adj")
stat.test

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Genotype", dodge = 0.8)

##### TEST STATISTICI ADDIZIONALI
##### Pairwise comparisons; tenendo fermo il tipo di supplemento, si comparano tra loro gli effetti delle sue diverse dosi...

##### QUI si decidono quali coppie confrontare:
#my_comparisons <- list(c("0.5", "2"))

pwc <- df %>%
  group_by(Treatment) %>% ### tengo ferma la variabile supplemento
  t_test(GFAP ~ Genotype,
         comparisons =  NULL, #### alt: NULL o my_comparisons
         #ref.group = "CTRL",   
         paired = FALSE,
         var.equal = TRUE,
         alternative = "two.sided",
         #       mu = 0,
         conf.level = 0.95,
         detailed = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>% ### holm, ecc.
  add_significance("p.adj")
pwc

# 3. Add statistical test 3: pwc
pwc <- pwc %>% add_xy_position(x = "Genotype", group = "Treatment")


#### Aggiungo il primo test statistico, quello entro il genotipo
p <- bxp + stat_pvalue_manual(
  stat.test,  
  label = "{p.adj}{p.adj.signif}", # Show adjusted p-values and significance levels: label = "{p.adj}{p.adj.signif}"; non aggiustati: label = "p"
  tip.length = 0.01,  ### 0: senza stanghetta, 0.01 o 0.02 con stanghetta di varie dimensioni
  hide.ns = T, # T or F, per non far vedere o meno quando p>0.05 ns
  label.size = 3,    ### dimensione asterisco e numeri
  bracket.size = 0.3,    ### spessore linea
  bracket.nudge.y = 0,  ### distanza tra l'ultimo valore del boxlot e la parentesi...
  bracket.shorten = 0,   ####???
  color = "black",      #### colore della linea
  linetype = 1,       #### tipo di linea
  step.increase = 0,  #### ???
  vjust = -0.1   #### posizione relativa tra asterisco e linea
)

#Aggiungo il secondo test statistico quello entro i Trattamenti
p + stat_pvalue_manual(
  pwc, color = "Treatment", step.group.by = "Treatment",
  label = "{p.adj}{p.adj.signif}", # Show adjusted p-values and significance levels: label = "{p.adj}{p.adj.signif}"; non aggiustati: label = "p"
  tip.length = 0.03,  ### 0: senza stanghetta, 0.01 o 0.02 con stanghetta di varie dimensioni
  hide.ns = T, # T or F, per non far vedere o meno quando p>0.05 ns
  label.size = 3,    ### dimensione asterisco e numeri
  bracket.size = 0.3,    ### spessore linea
  bracket.nudge.y = c(0.5, 3.5),  ### distanza tra l'ultimo valore e la parentesi...; varie coordinate in base al numero di coppie
  bracket.shorten = 0,   ####???
  linetype = 1,       #### tipo di linea
  step.increase = 0,  #### ???
  vjust = -0.1   #### posizione relativa tra asterisco e linea
)