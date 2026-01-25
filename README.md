# PROGETTO-R: Collezione Script R per Analisi Statistiche

Repository organizzato di script R per analisi statistiche e bioinformatiche, sviluppato nel corso degli anni per ricerca biomedica.

## Struttura del Repository

```
PROGETTO-R/
├── templates/           # Script riutilizzabili generici
│   ├── anova/          # Analisi della varianza
│   ├── boxplot/        # Visualizzazioni con statistiche
│   ├── pca/            # Analisi componenti principali
│   ├── regression/     # Regressione lineare/non lineare
│   ├── ttest/          # T-test e confronti
│   ├── correlation/    # Matrici di correlazione
│   └── ...             # Altre categorie
│
├── projects/           # Analisi su dataset specifici
│   ├── mild_ad/        # Studio Alzheimer MILD
│   ├── pea_mice/       # Topi transgenici PEA
│   ├── marina_macrophages/  # Macrofagi M0/M1
│   └── ...             # Altri progetti
│
├── SCRIPT SODDI/       # [BACKUP] Collezione originale
├── CLAUDE.md           # Istruzioni per Claude Code
└── README.md           # Questo file
```

## Categorie di Analisi

### Statistica Inferenziale
- **ANOVA**: Confronto 3+ gruppi (parametrico)
- **T-Test**: Confronto 2 gruppi
- **Test Non Parametrici**: Kruskal-Wallis, Wilcoxon/Mann-Whitney
- **Post-hoc**: Tukey HSD, confronti multipli

### Analisi Multivariate
- **PCA**: Riduzione dimensionale, pattern recognition
- **t-SNE**: Clustering non-lineare
- **Correlazione**: Matrici di correlazione, heatmap

### Machine Learning
- **Random Forest**: Classificazione, feature importance
- **Power Analysis**: Calcolo numerosità campione

### Visualizzazione
- **Boxplot**: Con statistiche integrate
- **Volcano Plot**: Analisi differenziali genomiche
- **Heatmap**: Clustering gerarchico
- **Line Plot**: Time series, curve

### Regressione
- **Lineare**: Semplice e multipla
- **Non Lineare**: Curve dose-risposta (IC50/EC50)
- **FRAP**: Fluorescence recovery analysis

## Come Usare Questo Repository

### Per Nuove Analisi
1. Vai in [`templates/`](templates/)
2. Scegli la categoria appropriata
3. Consulta il README della categoria
4. Copia e adatta lo script template ai tuoi dati

### Per Consultare Esempi Reali
1. Vai in [`projects/`](projects/)
2. Trova un progetto simile alla tua analisi
3. Studia il workflow completo applicato

## Librerie R Richieste

### Core
```r
install.packages(c(
  # Manipolazione dati
  "dplyr", "tidyr", "reshape2",

  # Visualizzazione
  "ggplot2", "ggpubr", "ggprism",

  # Statistica
  "car", "Hmisc",

  # Analisi multivariate
  "FactoMineR", "factoextra",

  # Machine learning
  "randomForest",

  # Altro
  "corrplot", "pheatmap", "plotly"
))
```

### Specializzate
```r
# Regressione dose-risposta
install.packages("drc")

# Heatmap avanzate
install.packages("dendextend")

# Power analysis
install.packages("pwr")
```

## Workflow Tipico

1. **Caricamento dati** → Dataset in R environment
2. **Esplorazione** → `templates/descriptive_stats/`
3. **Verifica presupposti** → `templates/normality_tests/`
4. **Analisi statistica** → `templates/anova/` o `templates/ttest/`
5. **Visualizzazione** → `templates/boxplot/` o `templates/pca/`
6. **Reporting** → Export grafici e risultati

## Convenzioni

### Naming Files
- Numerazione sequenziale: `01_`, `02_`, ...
- Nome descrittivo: `anova_one_way.R`
- Underscore separator: `_`

### Formato Dati
La maggior parte degli script assume **long format**:
```r
ID    Group      Variable    Value
1     Control    Var1        12.3
1     Control    Var2        5.6
2     Treatment  Var1        15.2
```

Usa `reshape2::melt()` per conversione da wide a long format.

### Stile Codice
- Commenti in italiano
- Pipe moderno: `%>%` (magrittr/dplyr)
- ggplot2 per visualizzazioni
- Librerie caricate all'inizio dello script

## Area di Applicazione

Questo repository è stato sviluppato per:
- **Ricerca biomedica**: Studi clinici, sperimentazione animale
- **Bioinformatica**: Analisi genomiche, trascrittomiche
- **Neuroscienze**: Modelli Alzheimer, endocannabinoidi
- **Farmacologia**: Curve dose-risposta, screening
- **Imaging**: FRAP, microscopia quantitativa

## Contributi

Per aggiungere nuovi script:
1. Identifica se è un template o un progetto specifico
2. Crea lo script nella directory appropriata
3. Segui le convenzioni di naming
4. Aggiungi documentazione nel README della categoria

## Backup

La directory `SCRIPT SODDI/` contiene la collezione originale completa. **Non modificare** questa directory - serve come backup storico.

## Supporto

Per domande sull'uso di Claude Code con questo repository, consulta [CLAUDE.md](CLAUDE.md).
