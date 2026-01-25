##### ANALISI dati mRNA eCB system nel muscolo PAZIENTI Finalizzata Napoli
##### 20 maggio 2021

library(dplyr)

#### ANALISI dati sui muscoli

dati <- Dati_espressione_genica_finalizzata_Napoli_xlsx_Adiposo
dati

dati <- dati %>% rename("NAPE_PLD" = "NAPE-PLD")
head(dati)


##### TEST NON PARAMETRICO
######NEL CASO IN CUI, per un evidente fallimento dei test di normalità e di omogeneità delle varianze, NON SI POTESSE USARE IL MODELLO PARAMETRICO (ANOVA), 
#si deve usare il test non parametrico di Kruskal-Wallis:

##nomi geni da testare
CB1
CB2
GPR55
TRPV1
NAPE_PLD
FAAH
DAGLa
DAGLb
MAGL

kruskal.test(MAGL ~        # nome del vettore con i valori numerici (variabile dipendente)
               Dieta,       # nome del vettore con i FATTORI (variabile indipendente)
             data = dati)   # nome dataset

#Multiple pairwise-comparison between groups
#Se dall' output del Kruskal-Wallis test, c'è una diff. sign. fra i gruppi
#It’s possible to use the function pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
pairwise.wilcox.test(dati$DAGLb, 
                     dati$Dieta,
                     p.adjust.method = "holm")
