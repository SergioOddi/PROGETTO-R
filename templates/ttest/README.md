# T-Test

## Quando utilizzare il T-Test

Il t-test confronta le medie di **due gruppi** indipendenti. Per 3+ gruppi, usa ANOVA.

## Script Disponibili

- **01_ttest_unpaired.R**: T-test non appaiato (campioni indipendenti)
- **02_ttest_with_boxplot.R**: T-test con visualizzazione boxplot
- **03_nonparametric_wilcox.R**: Test di Wilcoxon (alternativa non parametrica)

## Presupposti

1. **Normalità**: Dati normalmente distribuiti (test di Shapiro-Wilk)
2. **Omogeneità varianze**: Varianze simili tra gruppi (test di Levene)
3. **Indipendenza**: Osservazioni indipendenti

Se i presupposti sono violati, usa il test di Wilcoxon (Mann-Whitney U test).

## Tipi di T-Test

### Unpaired (Indipendente)
Campioni da popolazioni diverse:
```r
t.test(valore ~ gruppo, data = dati, var.equal = TRUE)  # Welch's t-test
```

### Paired (Appaiato)
Misure ripetute sugli stessi soggetti:
```r
t.test(prima, dopo, paired = TRUE)
```

### Welch's t-test
Quando le varianze non sono uguali (default in R):
```r
t.test(valore ~ gruppo, data = dati)  # var.equal = FALSE (default)
```

## Workflow

1. **Verifica normalità** (Shapiro-Wilk)
   ```r
   shapiro.test(dati$valore[dati$gruppo == "Controllo"])
   shapiro.test(dati$valore[dati$gruppo == "Trattamento"])
   ```

2. **Verifica omogeneità varianze** (Levene)
   ```r
   library(car)
   leveneTest(valore ~ gruppo, data = dati)
   ```

3. **Esegui t-test** (se presupposti OK)
   ```r
   risultato <- t.test(valore ~ gruppo, data = dati)
   print(risultato)
   ```

4. **Alternativa non parametrica** (se presupposti violati)
   ```r
   wilcox.test(valore ~ gruppo, data = dati)
   ```

## Visualizzazione con Boxplot

Usa `ggpubr` per boxplot + statistiche:
```r
library(ggpubr)

ggboxplot(dati, x = "gruppo", y = "valore",
          fill = "gruppo", palette = c("blue", "red"),
          add = "jitter") +
  stat_compare_means(method = "t.test")
```

## Interpretazione

- **p < 0.05**: Differenza significativa
- **t-value**: Grandezza della differenza (valore assoluto alto → grande differenza)
- **df**: Gradi di libertà
- **95% CI**: Intervallo di confidenza della differenza

## Formato Dati

```r
ID    Gruppo        Valore
1     Controllo     12.3
2     Controllo     11.8
3     Trattamento   15.2
4     Trattamento   14.9
```

## Librerie Richieste

- `stats` (base R, incluso automaticamente)
- `ggpubr`: Visualizzazione + statistiche
- `car`: Test di Levene (opzionale)
- `dplyr`: Manipolazione dati

## T-Test vs Wilcoxon

| Caratteristica | T-Test | Wilcoxon |
|----------------|--------|----------|
| Presupposti | Normalità, omogeneità varianze | Nessuno (distribution-free) |
| Confronta | Medie | Mediane/Ranghi |
| Potenza | Maggiore (se presupposti OK) | Minore |
| Robustezza outlier | Bassa | Alta |

## Note

- Per campioni piccoli (n < 30), verifica attentamente la normalità
- Il test di Welch è più robusto alle violazioni di omogeneità varianze
- Per confronti multipli (stesso dataset, più t-test), applica correzione Bonferroni
- Il t-test appaiato è più potente del non appaiato (riduce variabilità)
