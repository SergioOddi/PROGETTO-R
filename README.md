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

## Roadmap: Testing e Refactoring Script

### Obiettivo
Testare sistematicamente ogni script del repository per:
- ✅ **Verificare funzionalità**: Esecuzione senza errori
- 🔧 **Migliorare codice**: Leggibilità, efficienza, best practices
- 📚 **Documentare**: Aggiungere esempi d'uso e spiegazioni dettagliate
- 🎨 **Standardizzare**: Uniformare stile e convenzioni
- 🧪 **Validare**: Verificare correttezza statistica

### Strategia di Testing

#### Fase 1: Templates (Priorità Alta)
Script riutilizzabili da testare per primi:
1. `templates/anova/` - 7 script
2. `templates/ttest/` - 3 script
3. `templates/boxplot/` - 9 script
4. `templates/pca/` - 5 script
5. `templates/regression/` - 10 script
6. `templates/correlation/` - 2 script
7. Altri template (power analysis, random forest, etc.)

#### Fase 2: Projects (Validazione)
Script su progetti reali per validare workflow completi:
- `projects/mild_ad/`
- `projects/pea_mice/`
- `projects/marina_macrophages/`
- Altri progetti specifici

### Processo per Ogni Script

1. **Setup**: Creare dataset di esempio sintetico
2. **Esecuzione**: Testare lo script end-to-end
3. **Debug**: Risolvere errori e warning
4. **Refactoring**:
   - Migliorare nomi variabili
   - Ottimizzare codice ridondante
   - Aggiungere validazione input
   - Separare logica da visualizzazione
5. **Documentazione**:
   - Header con descrizione e autore
   - Commenti inline chiari
   - Esempio d'uso nel README
6. **Testing**: Verificare con dati edge-case

### Metriche di Qualità

Ogni script testato deve avere:
- [ ] Nessun errore di esecuzione
- [ ] Nessun warning critico
- [ ] Commenti esplicativi adeguati
- [ ] Output riproducibile
- [ ] Presupposti statistici verificati
- [ ] Grafici publication-ready

### Status Tracking

Script testati e validati verranno marcati nei README di categoria con badge:
- ✅ **Testato**: Funziona correttamente
- 🔧 **Refactored**: Codice migliorato
- 📚 **Documentato**: README aggiornato
- 🚧 **In Progress**: Attualmente in lavorazione
- ⚠️ **Issues**: Problemi noti da risolvere

### Prossimi Passi

1. Iniziare da `templates/ttest/01_ttest_unpaired.R`
2. Creare dataset sintetico di test
3. Eseguire e documentare risultati
4. Iterare su tutti i template
5. Aggiornare progressivamente i README di categoria

---

## Supporto

Per domande sull'uso di Claude Code con questo repository, consulta [CLAUDE.md](CLAUDE.md).
