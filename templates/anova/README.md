# ANOVA (Analysis of Variance)

## Quando utilizzare ANOVA

L'analisi della varianza (ANOVA) è utilizzata per confrontare le medie di **tre o più gruppi** indipendenti. Se hai solo due gruppi, usa un t-test.

## Presupposti Statistici

Prima di eseguire un'ANOVA, verifica questi presupposti:

1. **Normalità**: I residui devono seguire una distribuzione normale (test di Shapiro-Wilk)
2. **Omogeneità delle varianze**: Le varianze tra i gruppi devono essere omogenee (test di Levene)
3. **Indipendenza**: Le osservazioni devono essere indipendenti

Se i presupposti non sono soddisfatti, usa alternative non parametriche (Kruskal-Wallis).

## Script Disponibili

### Analisi Preliminare
- **06_preliminary_anova_assumptions.R**: Verifica dei presupposti statistici (normalità, omogeneità varianze)
- **07_preliminary_multigroup.R**: Preparazione dati per confronti multipli

### ANOVA a Una Via (One-Way)
- **01_anova_one_way.R**: Template principale per ANOVA univariata
- **02_anova_one_way_basic.R**: Versione semplificata

**Quando usare**: Un singolo fattore con 3+ livelli (es. Gruppo: Controllo, Trattamento A, Trattamento B)

### ANOVA a Due Vie (Two-Way)
- **03_anova_two_way_balanced.R**: Disegno bilanciato (stesso n per gruppo)
- **04_anova_two_way_unbalanced.R**: Disegno sbilanciato (n diversi)
- **05_anova_two_way_pairwise.R**: Con confronti multipli post-hoc

**Quando usare**: Due fattori (es. Genotipo × Trattamento) per studiare effetti principali e interazioni

## Workflow Consigliato

1. **Verifica presupposti** → `06_preliminary_anova_assumptions.R`
2. **Se presupposti OK** → ANOVA parametrica (`01_anova_one_way.R` o `03_anova_two_way_balanced.R`)
3. **Se presupposti violati** → Kruskal-Wallis (vedi `projects/finalizzata_napoli/04_kruskal_wallis.R`)
4. **Se ANOVA significativa** → Post-hoc test (Tukey HSD, pairwise comparisons)

## Formato Dati Richiesto

```r
# Long format (preferito)
ID      Group       Response
1       Control     12.3
2       Control     11.8
3       Treatment_A 15.2
4       Treatment_A 14.9
5       Treatment_B 18.1
```

## Librerie Richieste

- `stats` (base R)
- `car` (test di Levene)
- `ggpubr` (visualizzazione)
- `dplyr` (manipolazione dati)

## Post-hoc Tests

Dopo un'ANOVA significativa, usa:
- **Tukey HSD**: Per tutti i confronti pairwise
- **Dunnett**: Per confronti vs controllo
- **Bonferroni**: Più conservativo

## Note

- L'ANOVA testa se **almeno una media** è diversa dalle altre
- Non dice **quali** gruppi differiscono → necessari post-hoc test
- Per confronti ripetuti nel tempo, usa ANOVA a misure ripetute (non inclusa)
