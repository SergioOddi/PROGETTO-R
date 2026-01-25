# Progetto PEA Mice (Palmitoiletanolamide in Topi Transgenici)

## Obiettivo dello Studio

Valutazione degli effetti del trattamento con PEA (Palmitoiletanolamide) in topi transgenici (modello Alzheimer) vs wildtype. Analisi di:
- Livelli endocannabinoidi (eCB) nel sangue
- Espressione genica nell'ippocampo
- Biomarcatori infiammatori
- Isoprostanoidi (stress ossidativo)

## Dataset

- **Campioni_sangue_Topi_FINALIZZATA_2nd_tornata**: Livelli eCB sangue topi PEA (2° tornata campionamento)
- **Geni_HIPP_Topi_Tg_PEA_12_mesi**: Espressione mRNA geni target ippocampo (topi Tg 12 mesi)
- **Dati AEA topi Tg/Wt**: Livelli AEA topi transgenici vs wildtype

## Analisi Condotte

### 1. T-Test Livelli eCB Sangue
**Script**: `01_ttest_blood_levels.R`

Confronto livelli endocannabinoidi nel sangue tra gruppi sperimentali.

- Variabili: PEA, AEA, 2-AG, OEA
- Gruppi: Tg vs Wt, Trattati vs Non-trattati
- Test multipli con correzione

### 2. T-Test Dataset Standard
**Script**: `02_ttest_ecb_standard.R`

Analisi standardizzata su dataset completo eCB.

- Test su multiple variabili simultaneamente
- Visualizzazione con boxplot integrati

### 3. PCA Livelli eCB
**Script**: `03_pca_blood_ecb.R`

Analisi multivariata pattern eCB nel sangue.

- Identificazione pattern associati a trattamento PEA
- Separazione Tg vs Wt
- Contributo di ciascun eCB

### 4. Boxplot eCB 2° Tornata
**Script**: `04_boxplot_ecb_2nd_round.R`

Visualizzazione confronti statistici campioni 2° tornata.

- Validazione risultati prima tornata
- Confronti temporali
- Significatività statistica

### 5. Boxplot Geni Ippocampo
**Script**: `05_boxplot_genes_hippocampus.R`

Analisi espressione mRNA geni target infiammatori nell'ippocampo.

- Geni infiammatori (TNF-α, IL-1β, IL-6, ecc.)
- Effetto PEA su neuroinfiammazione
- Topi Tg 12 mesi

### 6. Boxplot Tg vs Wt
**Script**: `06_boxplot_pea_tg_wt.R`

Confronto diretto topi transgenici vs wildtype.

- Baseline differences
- Effetti del genotipo
- Interazione genotipo × trattamento

### 7. PCA MCI Sani + Isoprostanoidi
**Script**: `07_pca_mci_pea_isoprostanes.R`

Analisi integrativa PEA, eCB e isoprostanoidi (marcatori stress ossidativo).

- Correlazione PEA-isoprostanoidi
- Pattern stress ossidativo
- Potenziale neuroprotettivo PEA

## Gruppi Sperimentali

1. **Tg + PEA**: Topi transgenici trattati
2. **Tg + Veicolo**: Topi transgenici controllo
3. **Wt + PEA**: Wildtype trattati
4. **Wt + Veicolo**: Wildtype controllo

## Variabili Chiave

### Endocannabinoidi
- **PEA**: Palmitoiletanolamide (trattamento)
- **AEA**: Anandamide
- **2-AG**: 2-arachidonoilglicerolo
- **OEA**: Oleiletanolamide

### Geni Infiammatori (Ippocampo)
- Citokine pro-infiammatorie
- Markers microgliali
- Enzimi di sintesi/degradazione eCB

### Stress Ossidativo
- Isoprostanoidi

## Time Points

- **12 mesi**: Analisi topi adulti/anziani
- **2 tornate di campionamento**: Validazione risultati

## Ipotesi

1. PEA riduce livelli pro-infiammatori in topi Tg
2. PEA modula livelli altri eCB (cross-talk)
3. Effetto genotipo-dipendente del trattamento
4. Riduzione stress ossidativo con PEA

## Note

- Modello transgenico Alzheimer (genotipo non specificato, probabilmente APP/PS1 o 3xTg)
- PEA: Lipide endogeno con proprietà anti-infiammatorie e neuroprotettive
- Analisi ippocampo: Regione chiave per memoria e patologia AD
