# Boxplot con Confronti Statistici

## Quando utilizzare i Boxplot

I boxplot sono ideali per:
- Visualizzare la distribuzione di variabili continue
- Confrontare gruppi (2 o più)
- Identificare outlier visivamente
- Mostrare mediana, quartili e range dei dati

## Script Disponibili

### Con Reshaping (Long Format)
- **01_boxplot_multiple_reshape_2023.R**: Versione aggiornata 2023, multipli confronti
- **02_boxplot_multiple_march_2023.R**: Variante marzo 2023
- **03_boxplot_reshape_multigroup.R**: Per 3+ gruppi
- **04_boxplot_with_reshape.R**: Template generico con reshape
- **06_boxplot_reshape_ttest.R**: Ottimizzato per t-test (2 gruppi)

### Senza Reshaping (Wide Format)
- **05_boxplot_no_reshape.R**: Dati già in formato lungo

### Per Test Specifici
- **07_boxplot_ttest_anova.R**: Adattato per t-test e ANOVA
- **08_boxplot_time_comparison.R**: Confronti temporali stesso gruppo
- **09_boxplot_anova_two_way.R**: Per ANOVA bidimensionale

### Avanzati
- **10_boxplot_reshape_comparison.R**: Confronto approcci con/senza reshape

## Formato Dati

### Wide Format (richiede reshaping)
```r
ID   Control  Treatment_A  Treatment_B
1    12.3     15.2         18.1
2    11.8     14.9         17.6
```

### Long Format (pronto per plotting)
```r
ID   Group         Value
1    Control       12.3
1    Treatment_A   15.2
1    Treatment_B   18.1
2    Control       11.8
```

## Conversione Wide → Long

Gli script usano `reshape2::melt()`:
```r
library(reshape2)
dati_long <- melt(dati_wide,
                  id.vars = c("ID"),
                  variable.name = "Group",
                  value.name = "Value")
```

## Elementi Visuali

Gli script includono tipicamente:
- **Boxplot**: Box + whiskers (mediana, Q1, Q3, range)
- **Jitter/Dotplot**: Overlay dei dati grezzi
- **Statistiche**: P-value da t-test, ANOVA, Wilcoxon
- **Confronti multipli**: Bracket con significatività

## Confronti Statistici

Usa `ggpubr::stat_compare_means()`:
- **2 gruppi**: t-test o Wilcoxon
- **3+ gruppi**: ANOVA o Kruskal-Wallis + post-hoc

```r
# Esempio
p <- ggboxplot(data, x = "Group", y = "Value") +
  stat_compare_means(method = "anova")  # o "t.test", "wilcox.test"
```

## Scelta dello Script

| Situazione | Script Consigliato |
|------------|-------------------|
| Dati in wide format, 3+ gruppi | `01_boxplot_multiple_reshape_2023.R` |
| Dati già in long format | `05_boxplot_no_reshape.R` |
| Solo 2 gruppi (t-test) | `06_boxplot_reshape_ttest.R` |
| ANOVA a 2 vie | `09_boxplot_anova_two_way.R` |
| Confronti temporali | `08_boxplot_time_comparison.R` |

## Librerie Richieste

- `ggplot2`: Base grafica
- `ggpubr`: Boxplot + statistiche (`ggboxplot`, `stat_compare_means`)
- `reshape2`: Conversione formato dati (`melt`)
- `dplyr`: Manipolazione dati

## Personalizzazione

Elementi comuni da personalizzare:
- Colori: `fill = c("blue", "red", "green")`
- Titoli: `ggtitle()`, `xlab()`, `ylab()`
- Tema: `theme_classic()`, `theme_bw()`, `theme_prism()`
- Dimensioni testo: `theme(axis.text = element_text(size = 12))`

## Note

- Verifica sempre i presupposti statistici prima di interpretare i p-value
- I boxplot mostrano mediana, non media (usa bar plot per medie)
- Outlier sono punti oltre 1.5×IQR dai quartili
