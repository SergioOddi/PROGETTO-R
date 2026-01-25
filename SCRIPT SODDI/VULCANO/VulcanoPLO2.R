### Vulcano Plot
# v1.1 8 aprile 2024

if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')

BiocManager::install('EnhancedVolcano')


library(ggplot2)
library(ggrepel)
library(EnhancedVolcano)
library(dplyr)

# Assumo che dati sia già il tuo dataframe corretto
dati <- Dataset_VOLCANO_PLOT_mRNA_HIPP

citation("EnhancedVolcano")

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


# Trasforma i p-value in -log10
logP <- -log10(t_test_results)

# Prepara i dati per EnhancedVolcano
volcano_data2 <- data.frame(Gene = names(logFC),
                           Log2FoldChange = as.numeric(logFC),
                           PValue = logP)


# Crea il Volcano Plot
EnhancedVolcano(volcano_data,
                lab = volcano_data$Gene,
                x = 'Log2FoldChange',
                y = 'PValue',
                xlim = c(-4, 4),
                ylim = c(0, 6),
                xlab = "Log2 Fold Change",
                ylab = "-log10(p-value)",
                FCcutoff = 0.6,
                pCutoff = 0.05,
                pointSize = 5,    # Dimensione dei punti
                labSize = 4,      # Dimensione delle etichette
                titleLabSize = 4, # Dimensione del font del titolo
                axisLabSize = 20,  # Dimensione del font delle etichette degli assi
                col = c("black", "forestgreen", "royalblue", "red2"),  # Colore dei punti
                labCol = "black",   # Colore delle etichette
                colAlpha = 0.5,     # Opacità dei punti
  #              fontFace = "bold",    # Stile del font delle etichette
                legendPosition = "none")  # Posizione della legenda)


      ?EnhancedVolcano
