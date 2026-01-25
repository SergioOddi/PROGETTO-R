### Vulcano Plot
# v1. 6 aprile 2024

library(ggplot2)
library(ggrepel)


library(EnhancedVolcano)
library(dplyr)

# Assumo che dati sia già il tuo dataframe corretto
dati <- Dataset_VOLCANO_PLOT_mRNA_HIPP
  
  
  
# Preparazione dei dati
datiTg <- subset(dati, Genotype == "Tg2576")
datiTg <- datiTg[, -c(1:3)] # Rimuovi le prime tre colonne
datiTg <- datiTg[, c(1:37)] # Seleziona le colonne di interesse
datiTg <- datiTg[, -c(31)]
datiTg$Group <- as.factor(datiTg$Group) # Assicurati che Group sia un fattore
datiTg$Group <- recode_factor(datiTg$Group, "Tg" = "Veh"
                                      , "Tg_U" = "URB" )

dati <- datiTg

dati_grouped <- dati %>% 
  group_by(Group) %>%
  summarise_all(mean)

# Calcola il log2 fold change (assumendo che i gruppi siano "Veh" e "URB")
logFC <- log2(dati_grouped[dati_grouped$Group == "URB", -1] / dati_grouped[dati_grouped$Group == "Veh", -1])

# Esegui un test t (sostituire con i tuoi dati reali per i calcoli accurati)
# Nota: Questo è solo un esempio di come potresti procedere
t_test_results <- sapply(2:ncol(dati), function(i) {
  t.test(filter(dati, Group == "Veh")[[i]], filter(dati, Group == "URB")[[i]])$p.value
})

# Prepara i dati per EnhancedVolcano
volcano_data <- data.frame(Gene = names(logFC),
                           Log2FoldChange = as.numeric(logFC),
                           PValue = t_test_results)

# Crea il Volcano Plot
EnhancedVolcano(volcano_data,
                lab = volcano_data$Gene,
                x = 'Log2FoldChange',
                y = 'PValue',
                xlim = c(-4, 4),
                title = "Volcano Plot",
                xlab = "Log2 Fold Change",
                ylab = "-log10(p-value)",
                FCcutoff = 0.6,
                pCutoff = 0.05)




####### ANALISI fino alla colonna 49 (tutti i componenti dell'ECS)

# Assumo che dati sia già il tuo dataframe corretto
dati <- Tg_URB_HIPP_mRNA

# Preparazione dei dati
datiTg <- subset(dati, Genotype == "Tg2576")
datiTg <- datiTg[, -c(1:3)] # Rimuovi le prime tre colonne
datiTg <- datiTg[, c(1:46)] # Seleziona le colonne di interesse
datiTg$Group <- as.factor(datiTg$Group) # Assicurati che Group sia un fattore
datiTg$Group <- recode_factor(datiTg$Group, "Tg" = "Veh"
                              , "Tg_U" = "URB" )

dati <- datiTg

dati_grouped <- dati %>% 
  group_by(Group) %>%
  summarise_all(mean)

# Calcola il log2 fold change (assumendo che i gruppi siano "Veh" e "URB")
logFC <- log2(dati_grouped[dati_grouped$Group == "URB", -1] / dati_grouped[dati_grouped$Group == "Veh", -1])

# Esegui un test t (sostituire con i tuoi dati reali per i calcoli accurati)
# Nota: Questo è solo un esempio di come potresti procedere
t_test_results <- sapply(2:ncol(dati), function(i) {
  t.test(filter(dati, Group == "Veh")[[i]], filter(dati, Group == "URB")[[i]])$p.value
})

# Prepara i dati per EnhancedVolcano
volcano_data <- data.frame(Gene = names(logFC),
                           Log2FoldChange = as.numeric(logFC),
                           PValue = t_test_results)

# Crea il Volcano Plot
EnhancedVolcano(volcano_data,
                lab = volcano_data$Gene,
                x = 'Log2FoldChange',
                y = 'PValue',
                xlim = c(-5, 5),
                title = "Volcano Plot",
                xlab = "Log2 Fold Change",
                ylab = "-log10(p-value)",
                FCcutoff = 0.48,
                pCutoff = 0.05)








#Per modificare l'asse orizzontale di soglia nel Volcano Plot, devi impostare un valore di soglia per il p-value che desideri utilizzare. Questo valore di soglia è spesso scelto in base a standard convenzionali o significatività clinica/biologica. Per esempio, un p-value di 0.05 è comunemente usato come punto di riferimento per la significatività statistica, ma in alcuni campi può essere utilizzato un valore più stringente, come 0.01 o 0.001.

#Nel grafico Volcano, la Y mostra il -log10 del p-value, quindi dovrai convertire il tuo p-value di soglia in questa scala. Per esempio:
  
#- Per un p-value di 0.05, il valore su asse Y sarà `-log10(0.05)`.
#- Per un p-value di 0.01, sarà `-log10(0.01)`.
# - E così via.

# Una volta che hai determinato il valore di soglia desiderato, puoi usare l'argomento `threshold` nella funzione `EnhancedVolcano` per impostare la soglia di significatività per il p-value.


library(EnhancedVolcano)

# Imposta la soglia di p-value
pvalue_threshold <- 0.05
threshold_line <- -log10(pvalue_threshold)

EnhancedVolcano(risultati,
                lab = risultati$Variabile,
                x = 'Log2FoldChange',
                y = 'PValue',
                xlim = c(-15, 15),
                title = "Volcano Plot",
                xlab = "Log2 Fold Change",
                ylab = "-log10(p-value)",
                pCutoff = pvalue_threshold, # Imposta la soglia di p-value qui
                cutoffLineType = 'longdash', # Tipo di linea, es. 'longdash', 'dotted', etc.
                cutoffLineCol = 'red') # Colore della linea di soglia



EnhancedVolcano(risultati,
                lab = risultati$Variabile,
                x = 'Log2FoldChange',
                y = 'PValue',
                xlim = c(min(risultati$Log2FoldChange) - 1, max(risultati$Log2FoldChange) + 1), # Modifica qui per includere tutti i punti
                ylim = c(0, -log10(min(risultati$PValue))), # Modifica qui per includere tutti i punti
                title = "Volcano Plot",
                xlab = "Log2 Fold Change",
                ylab = "-log10(p-value)",
                pCutoff = 0.05)





