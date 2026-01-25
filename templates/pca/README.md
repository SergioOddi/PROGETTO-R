# PCA (Principal Component Analysis)

## Quando utilizzare la PCA

La PCA è una tecnica di riduzione dimensionale utilizzata per:
- Ridurre la complessità di dataset multivariati
- Visualizzare pattern in dati ad alta dimensionalità
- Identificare variabili correlate
- Rimuovere ridondanza
- Esplorare separazione tra gruppi

## Script Disponibili

### Template Principali
- **01_pca_new_version.R**: Versione aggiornata completa
- **02_pca_quick.R**: Analisi rapida PCA
- **04_pca_ggplot_visualization.R**: Visualizzazione con ggplot2

### Preparazione Dati
- **03_preliminary_pca_analysis.R**: Analisi preliminare e preparazione dataset

### Tecniche Alternative
- **05_tsne_dimensionality_reduction.R**: t-SNE (per clustering non-lineare)

## Presupposti e Considerazioni

1. **Variabili numeriche**: Solo dati quantitativi
2. **Scaling**: Standardizzare variabili con scale diverse
3. **Correlazioni**: Funziona meglio con variabili correlate
4. **Linearità**: Assume relazioni lineari (usa t-SNE per non-lineari)
5. **Outlier**: Influenzano fortemente i risultati

## Workflow Tipico

1. **Preparazione dati**
   ```r
   # Selezionare solo variabili numeriche
   dati_numeric <- dati %>% select(var1, var2, var3, ...)

   # Rimuovere NA
   dati_clean <- na.omit(dati_numeric)

   # Convertire a matrice
   mat <- as.matrix.data.frame(dati_clean)
   ```

2. **Eseguire PCA**
   ```r
   library(FactoMineR)
   pca_result <- PCA(mat, scale.unit = TRUE, graph = FALSE)
   ```

3. **Analizzare risultati**
   - Scree plot (autovalori)
   - Percentuale varianza spiegata
   - Contributo variabili (loadings)

4. **Visualizzazione**
   - Biplot 2D (PC1-PC2, PC1-PC3, PC2-PC3)
   - Plot 3D interattivo (plotly)
   - Ellissi di confidenza per gruppi

## Interpretazione

### Autovalori
- Componenti con autovalore > 1 sono significative (criterio di Kaiser)
- Scree plot: cerca il "gomito" nella curva

### Varianza Spiegata
- PC1 + PC2 solitamente > 50-60% per buona rappresentazione
- Riporta sempre le percentuali nei plot

### Loadings (Contributi)
- Variabili con loading alto (>0.5) contribuiscono significativamente
- Stesso segno → variabili correlate positivamente
- Segno opposto → correlate negativamente

## Visualizzazioni

### Biplot 2D
```r
library(factoextra)
fviz_pca_biplot(pca_result,
                habillage = gruppi,  # Colorare per gruppo
                addEllipses = TRUE,  # Ellissi confidenza 95%
                ellipse.level = 0.95)
```

### Plot 3D Interattivo
```r
library(plotly)
plot_ly(x = PC1, y = PC2, z = PC3,
        color = gruppi, type = "scatter3d")
```

## Formato Dati Richiesto

```r
# Matrice numerica (righe = campioni, colonne = variabili)
      Var1  Var2  Var3  Var4  Var5
Smp1  12.3  5.6   8.9   3.2   7.1
Smp2  11.8  5.2   9.1   3.5   6.8
Smp3  15.2  6.1   7.8   2.9   8.3
```

## Librerie Richieste

- `FactoMineR`: Funzioni PCA avanzate
- `factoextra`: Visualizzazioni PCA
- `ggplot2`: Grafica
- `plotly`: Plot 3D interattivi
- `dplyr`: Manipolazione dati

## Analisi per Sottogruppi

Gli script includono spesso analisi separate per:
- Pazienti vs Controlli
- Diversi fenotipi
- Time points differenti

Questo permette di valutare pattern specifici di gruppo.

## Post-PCA: Analisi dei Contributi

Usa `dimdesc()` per identificare:
- Quali variabili discriminano i gruppi
- Variabili significativamente correlate con ciascuna PC

```r
descr <- dimdesc(pca_result, axes = 1:3)
```

## Note

- La PCA è **non supervisionata** (non usa labels dei gruppi)
- Utile per esplorare dati, non per classificazione
- Per classificazione supervisionata, usa Random Forest o LDA
- t-SNE migliore per cluster non-lineari ma non preserva distanze globali
