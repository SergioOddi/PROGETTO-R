# Matrice di Correlazione

## Quando utilizzare l'Analisi di Correlazione

L'analisi di correlazione misura la forza e direzione della relazione lineare tra variabili:
- Identificare variabili correlate
- Esplorare relazioni multivariate
- Ridurre ridondanza prima di analisi multivariate

## Script Disponibili

- **01_correlation_matrix_groups.R**: Matrice di correlazione con sottogruppi
- **02_correlation_matrix_groups_v2.R**: Versione aggiornata
- **03_correlation_pvalue.R**: Correlazioni con p-value e significatività

## Coefficiente di Correlazione

### Pearson (r)
Correlazione lineare parametrica:
- **Range**: -1 a +1
- **+1**: Correlazione positiva perfetta
- **0**: Nessuna correlazione
- **-1**: Correlazione negativa perfetta

**Presupposti**: Normalità, relazione lineare

### Spearman (ρ)
Correlazione di rango non parametrica:
- Robusta a outlier
- Non assume normalità
- Cattura relazioni monotone (non necessariamente lineari)

## Workflow

1. **Selezionare variabili numeriche**
   ```r
   dati_numeric <- dati %>% select(where(is.numeric))
   ```

2. **Calcolare correlazioni**
   ```r
   # Pearson
   cor_matrix <- cor(dati_numeric, use = "complete.obs", method = "pearson")

   # Spearman (alternativa non parametrica)
   cor_matrix <- cor(dati_numeric, use = "complete.obs", method = "spearman")
   ```

3. **Calcolare p-value** (usa `Hmisc::rcorr`)
   ```r
   library(Hmisc)
   res <- rcorr(as.matrix(dati_numeric), type = "pearson")

   # Matrice correlazioni
   res$r

   # Matrice p-values
   res$P
   ```

4. **Visualizzazione**
   ```r
   library(corrplot)
   corrplot(cor_matrix, method = "color", type = "upper",
            addCoef.col = "black", tl.col = "black")
   ```

## Visualizzazioni

### Heatmap Corrplot
```r
corrplot(cor_matrix,
         method = "circle",      # o "color", "number"
         type = "upper",         # triangolo superiore
         order = "hclust",       # clustering gerarchico
         tl.col = "black",
         addCoef.col = "black")  # aggiunge coefficienti
```

### Heatmap ggplot2
```r
library(reshape2)
melted <- melt(cor_matrix)

ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1))
```

## Analisi per Sottogruppi

Gli script `01` e `02` permettono analisi separate per gruppo:
```r
# Esempio: Pazienti vs Controlli
pazienti <- dati %>% filter(gruppo == "Paziente")
controlli <- dati %>% filter(gruppo == "Controllo")

cor_pazienti <- cor(pazienti %>% select(where(is.numeric)))
cor_controlli <- cor(controlli %>% select(where(is.numeric)))
```

## Interpretazione

| |r| | Forza |
|-------|-------|
| 0.0-0.2 | Molto debole |
| 0.2-0.4 | Debole |
| 0.4-0.6 | Moderata |
| 0.6-0.8 | Forte |
| 0.8-1.0 | Molto forte |

**Attenzione**: Correlazione ≠ Causalità

## Significatività

Il p-value testa l'ipotesi nulla H₀: ρ = 0

- **p < 0.05**: Correlazione significativa
- Con molte variabili, applica correzione per test multipli (FDR, Bonferroni)

## Formato Dati

```r
# Solo variabili numeriche
      Var1  Var2  Var3  Var4
Smp1  12.3  5.6   8.9   3.2
Smp2  11.8  5.2   9.1   3.5
Smp3  15.2  6.1   7.8   2.9
```

## Librerie Richieste

- `stats` (base R)
- `Hmisc`: Correlazioni con p-value (`rcorr`)
- `corrplot`: Visualizzazione matrice correlazione
- `ggplot2` + `reshape2`: Heatmap personalizzate
- `dplyr`: Manipolazione dati

## Gestione Missing Values

```r
# Pairwise deletion (usa tutte le coppie disponibili)
cor(dati, use = "pairwise.complete.obs")

# Listwise deletion (rimuovi righe con NA)
cor(dati, use = "complete.obs")
```

## Correlazione Parziale

Per controllare effetto di variabili confondenti, usa correlazione parziale:
```r
library(ppcor)
pcor(dati)  # Correlazione parziale
```

## Note

- La correlazione misura solo relazioni **lineari** (Pearson)
- Outlier influenzano fortemente Pearson → usa Spearman se necessario
- Matrice di correlazione è **simmetrica** (corr(X,Y) = corr(Y,X))
- Con molte variabili, la matrice diventa difficile da leggere → usa clustering
