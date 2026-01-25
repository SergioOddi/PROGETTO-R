# Templates - Script Riutilizzabili

Questa directory contiene **script template generici** pronti per essere adattati a nuovi dataset. Ogni script è documentato e segue best practices statistiche.

## Struttura

```
templates/
├── anova/              - Analisi della varianza (3+ gruppi)
├── boxplot/            - Visualizzazioni boxplot con statistiche
├── pca/                - Analisi componenti principali
├── regression/         - Regressione lineare e non lineare
├── ttest/              - T-test (2 gruppi)
├── correlation/        - Matrici di correlazione
├── descriptive_stats/  - Statistiche descrittive
├── normality_tests/    - Test presupposti statistici
├── random_forest/      - Machine learning classificazione
├── volcano_plot/       - Volcano plots (genomica)
├── heatmap/            - Heatmap con clustering
├── power_analysis/     - Analisi potenza e dimensione campione
├── visualization/      - Grafici matematici e visualizzazioni
└── utils/              - Utility varie
```

## Come Usare i Template

1. **Scegli lo script** appropriato per la tua analisi (consulta i README nelle sottocartelle)
2. **Copia** lo script nel tuo progetto o workspace
3. **Modifica** i riferimenti ai dataset con i tuoi dati
4. **Adatta** parametri e variabili secondo le tue esigenze
5. **Verifica** i presupposti statistici prima di interpretare i risultati

## Convenzioni di Naming

Gli script seguono la numerazione:
- `01_`, `02_`, ... : Ordine logico di utilizzo
- Nome descrittivo della funzionalità

## Dipendenze Comuni

Librerie frequentemente utilizzate:
- **Manipolazione dati**: `dplyr`, `tidyr`, `reshape2`
- **Visualizzazione**: `ggplot2`, `ggpubr`, `ggprism`
- **Statistica**: `stats`, `car`, `Hmisc`
- **PCA**: `FactoMineR`, `factoextra`
- **Machine Learning**: `randomForest`

Installazione:
```r
install.packages(c("dplyr", "ggplot2", "ggpubr", "ggprism",
                   "reshape2", "car", "FactoMineR", "factoextra",
                   "randomForest", "Hmisc"))
```

## Workflow Consigliato

1. **Esplorazione dati** → `descriptive_stats/`
2. **Verifica presupposti** → `normality_tests/`
3. **Analisi statistica** → `anova/`, `ttest/`, `regression/`
4. **Visualizzazione** → `boxplot/`, `pca/`, `heatmap/`
5. **Reporting** → Export grafici e risultati

## Supporto

Per esempi applicati a dataset reali, consulta la directory [`projects/`](../projects/)
