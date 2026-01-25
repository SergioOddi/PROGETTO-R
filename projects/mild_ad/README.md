# Progetto MILD AD (Mild Alzheimer's Disease)

## Obiettivo dello Studio

Analisi di pazienti con forma lieve di Alzheimer (MILD AD) confrontati con controlli sani. Focus su:
- Livelli di endocannabinoidi (eCB) nel plasma
- Biomarcatori plasmatici
- Pattern di espressione macrofagi
- Correlazioni con score cognitivi

## Dataset

- **MILD_AD**: Dataset principale pazienti e controlli
- **MILD_AD_senza_outlier**: Dataset pulito da outlier
- **PLASMA_ECS_level**: Livelli eCB nel plasma con score cognitivi

## Analisi Condotte

### 1. PCA su Pazienti MILD AD
**Script**: `01_pca_mild_home.R`

Analisi delle componenti principali su pazienti MILD AD per identificare pattern di variazione nei biomarcatori.

- Sottogruppi analizzati separatamente (pazienti vs controlli)
- Visualizzazioni 2D e 3D
- Identificazione variabili discriminanti

### 2. PCA Macrofagi MILD
**Script**: `02_pca_macrophages_mild.R`

Analisi PCA specifica su popolazione macrofagi in pazienti MILD AD.

- Dataset con e senza outlier
- Focus su fenotipo macrofagico
- Correlazioni con stato patologico

### 3. Boxplot Confronti Statistici
**Script**: `03_boxplot_mild.R`

Confronti statistici tra gruppi (MILD AD vs Controlli) su variabili chiave.

- Test parametrici e non parametrici
- Visualizzazione significatività
- Identificazione biomarcatori differenziali

### 4. PCA con Score Cognitivi
**Script**: `04_pca_ecb_ad_score.R`

Analisi integrativa livelli eCB plasmatici con score cognitivi.

- Correlazione eCB con performance cognitive
- Identificazione pattern associati a declino cognitivo
- Stratificazione per severità

## Variabili Chiave

- **Endocannabinoidi**: AEA, 2-AG, PEA, OEA
- **Biomarcatori infiammatori**: Citokine, markers macrofagi
- **Score cognitivi**: MMSE, MoCA (se disponibili)
- **Fenotipo**: Paziente vs Controllo

## Risultati Attesi

- Separazione gruppi in spazio PCA
- Livelli eCB differenziali tra MILD AD e controlli
- Correlazioni eCB-performance cognitiva
- Signature macrofagiche specifiche

## Note

- Gli outlier sono stati rimossi dopo analisi preliminare
- Analisi condotte separatamente per sottogruppi per maggiore specificità
- Dataset "a casa" indica analisi su pazienti ambulatoriali
