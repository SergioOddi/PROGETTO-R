# Projects - Analisi su Dataset Specifici

Questa directory contiene **script applicati a dataset reali** di progetti di ricerca. Gli script sono organizzati per studio/progetto e rappresentano analisi complete su dati sperimentali.

## Struttura

```
projects/
├── mild_ad/                - Studio MILD Alzheimer's Disease
├── marina_macrophages/     - Analisi macrofagi M0/M1 (Marina)
├── finalizzata_napoli/     - Progetto Finalizzata Napoli
├── pea_mice/               - Studi PEA su topi transgenici
├── domenico_macrophages/   - PCR macrofagi (Domenico)
├── canine_studies/         - Studi su cani (IBD, Boari)
├── frap_imaging/           - Analisi FRAP microscopia
├── tgurb_alzheimer/        - Analisi Abeta/APP in topi TgURB
├── sperm_analysis/         - Analisi spermatozoi
├── prism_data/             - Dati importati da GraphPad Prism
├── misc_analyses/          - Analisi varie
└── public_datasets/        - Dataset pubblici (Cleveland, Vehicles)
```

## Differenza tra Projects e Templates

| Templates | Projects |
|-----------|----------|
| Script generici riutilizzabili | Script applicati a dataset specifici |
| Dati placeholder | Riferimenti a dataset reali |
| Documentazione metodologica | Contesto sperimentale specifico |
| Pronti per adattamento | Risultati completi |

## Come Consultare i Projects

1. Ogni sottocartella contiene un **README.md** con:
   - Obiettivo dello studio
   - Descrizione dataset
   - Analisi condotte
   - Risultati principali (se disponibili)

2. Gli script sono numerati in ordine logico di analisi

3. Per riutilizzare un'analisi su nuovi dati, consulta i **template** corrispondenti

## Dataset Principali

### Studi Alzheimer
- **mild_ad/**: Pazienti MILD AD vs controlli, analisi eCB e biomarcatori
- **tgurb_alzheimer/**: Modello murino TgURB, livelli Abeta e APP

### Macrofagi
- **marina_macrophages/**: Fenotipizzazione M0/M1, moderate AD
- **domenico_macrophages/**: PCR macrofagi M0/M1

### Endocannabinoidi (eCB)
- **pea_mice/**: Livelli eCB in sangue topi trattati con PEA
- **mild_ad/**: Livelli eCB plasma pazienti AD

### Imaging
- **frap_imaging/**: Fluorescence Recovery After Photobleaching

### Altri
- **finalizzata_napoli/**: Analisi tissutali (muscolo, adiposo)
- **canine_studies/**: Patologie canine
- **sperm_analysis/**: Parametri spermatici

## Note

- Gli script assumono dataset pre-caricati nell'ambiente R
- Path ai file potrebbero richiedere adattamento
- Per domande specifiche sui progetti, consulta i README nelle sottocartelle
