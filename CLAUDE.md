# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Panoramica del Progetto

Questo repository contiene una collezione organizzata di script R per analisi statistiche e bioinformatiche, sviluppati nel corso degli anni per ricerca biomedica. Gli script sono ora organizzati in una struttura pulita che separa **template riutilizzabili** da **analisi su progetti specifici**.

## Nuova Struttura del Repository

```
PROGETTO-R/
├── templates/           # Script generici riutilizzabili (50+ script)
│   ├── anova/          # Analisi della varianza
│   ├── boxplot/        # Visualizzazioni con statistiche
│   ├── pca/            # Analisi componenti principali
│   ├── regression/     # Regressione lineare/non lineare
│   ├── ttest/          # T-test e confronti
│   ├── correlation/    # Matrici di correlazione
│   ├── descriptive_stats/
│   ├── normality_tests/
│   ├── random_forest/
│   ├── volcano_plot/
│   ├── heatmap/
│   ├── power_analysis/
│   ├── visualization/
│   └── utils/
│
├── projects/           # Analisi su dataset specifici (40+ script)
│   ├── mild_ad/        # Studio Alzheimer MILD
│   ├── pea_mice/       # Topi transgenici PEA
│   ├── marina_macrophages/
│   ├── finalizzata_napoli/
│   ├── domenico_macrophages/
│   ├── canine_studies/
│   ├── frap_imaging/
│   ├── tgurb_alzheimer/
│   ├── sperm_analysis/
│   ├── prism_data/
│   ├── misc_analyses/
│   └── public_datasets/
│
└── SCRIPT SODDI/       # [BACKUP] Collezione originale
```

## Navigazione del Repository

### Per Template Riutilizzabili
- Ogni categoria in `templates/` ha un README.md con:
  - Quando utilizzare quella analisi
  - Presupposti statistici richiesti
  - Descrizione degli script disponibili
  - Formato dati richiesto
  - Esempi d'uso

### Per Progetti Specifici
- Ogni progetto in `projects/` ha un README.md con:
  - Obiettivo dello studio
  - Descrizione dataset
  - Analisi condotte
  - Risultati principali

### Convenzioni Naming
Gli script seguono numerazione sequenziale:
- `01_nome_descrittivo.R`
- `02_altro_script.R`
- ecc.

L'ordine numerico riflette il workflow logico di analisi.

## Pattern Architetturali Comuni

### 1. Workflow Standardizzato
Gli script seguono tipicamente questo flusso:
```r
# 1. Caricamento e preprocessing
dati <- dataset_precaricato %>%
  rename(nuovi_nomi) %>%
  filter(condizioni)

# 2. Test dei presupposti
shapiro.test()  # Normalità
leveneTest()    # Omogeneità varianze

# 3. Analisi statistica
# Parametrica o non-parametrica in base ai presupposti

# 4. Visualizzazione publication-ready
ggplot2 + ggpubr + ggprism

# 5. Post-hoc e reporting
```

### 2. Librerie Core

**Manipolazione dati:**
- `dplyr`: filter, select, mutate, group_by, summarise, rename
- `reshape2`: melt() per conversione long format
- `tidyr`: pivot operations

**Analisi statistica:**
- `stats`: funzioni base (aov, t.test, kruskal.test)
- `car`: test di Levene
- `FactoMineR`: PCA e analisi multivariate
- `drc`: curve dose-risposta (LL.4 model)
- `randomForest`: machine learning

**Visualizzazione:**
- `ggplot2`: base grafica
- `ggpubr`: boxplot con statistiche (stat_compare_means)
- `ggprism`: tema stile GraphPad Prism
- `factoextra`: visualizzazioni PCA
- `pheatmap`: heatmap con clustering
- `plotly`: grafici 3D interattivi

### 3. Strutture Dati Ricorrenti

- **Long format**: Colonne tipiche: `ID`, `Group/Treatment/Phenotype`, `Response/Value`
- **Wide format**: Una colonna per variabile, conversione con `melt(id.var = c("ID", "Group"))`
- **Matrici numeriche**: Conversione con `as.matrix.data.frame()` per PCA/correlazioni
- **Dataframe con annotazioni**: Metadati separati per annotazioni visuali (heatmap, PCA)

### 4. Caratteristiche Tecniche

**Dipendenze ambientali:**
- Gli script assumono dataset pre-caricati nell'ambiente R globale
- Non utilizzano `setwd()` o path assoluti
- Pensati per uso interattivo in RStudio

**Stile di codifica:**
- Commenti estensivi in italiano con spiegazioni metodologiche
- Versioning manuale nei commenti (es. "Versione 3.0 --- 13 marzo 2023")
- Mix di pipe moderno (`%>%`) e funzioni base R

**Pattern riutilizzabili:**
- Codice template con placeholder per filtri/subset
- Analisi ripetute su sottogruppi (loop impliciti)
- Opzioni commentate per test alternativi (parametrico vs non-parametrico)

## Template vs Projects

### Template (`templates/`)
- Script **generici** adattabili a qualsiasi dataset
- Contengono placeholder e variabili generiche
- Documentati con README metodologici
- Pronti per essere copiati e personalizzati

### Projects (`projects/`)
- Script **specifici** applicati a dataset reali
- Riferimenti a nomi di dataset concreti (es. `MILD_AD`, `marina`, `Campioni_sangue_Topi_PEA`)
- Rappresentano analisi complete su studi reali
- Da consultare come esempi applicativi

## Workflow per Nuove Analisi

1. Identificare il tipo di analisi necessaria
2. Consultare `templates/[categoria]/README.md`
3. Scegliere lo script template appropriato
4. Copiare lo script nel workspace
5. Adattare ai propri dati
6. Verificare presupposti statistici
7. Eseguire e interpretare

## Ricerca Script

Per trovare lo script giusto:
- **Per metodo statistico**: Vai in `templates/[metodo]/`
- **Per esempio simile**: Cerca in `projects/`
- **Consulta README**: Ogni categoria ha documentazione dettagliata

## Considerazioni per Modifiche

1. **Preservare workflow consolidati**: Gli script riflettono procedure validate per ricerca biomedica
2. **Mantenere commenti in italiano**: La documentazione metodologica è essenziale
3. **Rispettare dipendenze implicite**: Dataset pre-caricati e naming conventions esistenti
4. **Non over-engineering**: La semplicità e riproducibilità sono prioritarie rispetto all'astrazione
5. **Verificare presupposti statistici**: Ogni modifica deve mantenere la validità statistica (test normalità, omogeneità varianze)
6. **Non modificare SCRIPT SODDI/**: È il backup originale completo

## Progetti Principali

### Studi Alzheimer
- `projects/mild_ad/`: Pazienti MILD AD, livelli eCB, biomarcatori
- `projects/tgurb_alzheimer/`: Modello murino TgURB, Abeta/APP

### Endocannabinoidi
- `projects/pea_mice/`: Effetti PEA in topi transgenici (7 analisi)
- `projects/mild_ad/`: Livelli eCB plasma pazienti

### Macrofagi
- `projects/marina_macrophages/`: Fenotipizzazione M0/M1
- `projects/domenico_macrophages/`: PCR macrofagi

### Imaging
- `projects/frap_imaging/`: FRAP microscopia (6 analisi)

## Contesto del Progetto

Repository per ricerca biomedica/bioinformatica con focus su:
- Analisi esplorativa dati (EDA)
- Test statistici con verifica presupposti
- Visualizzazioni publication-ready
- Riproducibilità delle analisi
- Neuroscienze (modelli Alzheimer, endocannabinoidi)
- Immunologia (macrofagi, infiammazione)
