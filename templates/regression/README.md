# Regressione Lineare e Non Lineare

## Quando utilizzare la Regressione

La regressione modella la relazione tra variabili:
- **Lineare**: Relazione proporzionale tra X e Y
- **Non lineare**: Curve dose-risposta, crescita, decadimento

## Script Disponibili

### Regressione Lineare
- **06_linear_regression_basic.R**: Template base regressione semplice
- **07_linear_regression_v2.R**: Versione aggiornata
- **08_linear_regression_multiple.R**: Regressione multipla (più curve)
- **09_linear_plots.R**: Visualizzazione dati lineari
- **10_linear_regression_v3.R**: Versione avanzata

### Regressione Non Lineare
- **02_dose_response_curve.R**: Curve dose-risposta (modello logistico)
- **03_dose_response_final.R**: Versione finale dose-risposta
- **04_multiple_curves_errors.R**: Curve multiple con error bars
- **05_linear_nonlinear_fitting.R**: Fitting combinato

### FRAP (Fluorescence Recovery After Photobleaching)
- **01_frap_general_2023.R**: Template generale FRAP 2023

## Regressione Lineare

### Modello
```r
Y = β₀ + β₁X + ε
```

### Presupposti
1. **Linearità**: Relazione lineare tra X e Y
2. **Normalità residui**: Residui normalmente distribuiti
3. **Omoschedasticità**: Varianza costante
4. **Indipendenza**: Osservazioni indipendenti

### Workflow
```r
# Fit modello
model <- lm(Y ~ X, data = dati)

# Diagnostica
summary(model)  # R², p-value, coefficienti
plot(model)     # Residui, Q-Q plot

# Predizione
predict(model, newdata = nuovi_dati)
```

### Metriche
- **R²**: Varianza spiegata (0-1, più alto meglio)
- **p-value**: Significatività coefficienti
- **RMSE**: Root Mean Square Error

## Regressione Non Lineare (Dose-Risposta)

### Modello Logistico 4-Parametri (LL.4)
```r
Y = Bottom + (Top - Bottom) / (1 + (X/EC50)^HillSlope)
```

**Parametri:**
- **Bottom**: Asintoto inferiore (risposta minima)
- **Top**: Asintoto superiore (risposta massima)
- **EC50**: Concentrazione effettiva al 50% (dose efficace)
- **HillSlope**: Pendenza della curva

### Libreria: drc
```r
library(drc)

# Fit curve dose-risposta
fit <- drm(Response ~ Dose, data = dati,
           fct = LL.4(names = c("HillSlope", "Bottom", "Top", "EC50")))

# Estrarre parametri
summary(fit)
EC50 <- fit$coefficients["EC50"]

# Calcolare IC50 (se necessario)
IC50 <- EC50  # Per inibizione, EC50 = IC50
```

## Curve Multiple con Errori

Gli script `04_multiple_curves_errors.R` gestiscono:
- Replicati tecnici/biologici
- Calcolo media e deviazione standard
- Error bars (SD o SEM)
- Fitting curve su medie

```r
# Calcolo medie per riga
media <- rowMeans(dati[, 2:ncol(dati)], na.rm = TRUE)
sd <- rowSds(as.matrix(dati[, 2:ncol(dati)]), na.rm = TRUE)
```

## FRAP Analysis

Fluorescence Recovery After Photobleaching:
- Modelli esponenziali di recupero
- Calcolo tassi di diffusione
- Normalizzazione baseline

```r
# Modello esponenziale tipico
Y = A * (1 - exp(-k * t))
```

## Formato Dati

### Regressione Lineare
```r
X      Y
1.0    2.3
2.0    4.1
3.0    6.5
```

### Dose-Risposta
```r
Dose      Response
0.001     5.2
0.01      12.8
0.1       45.3
1.0       89.1
10.0      95.8
```

## Visualizzazione

### ggplot2 + ggprism
Gli script usano `ggprism` per grafici publication-ready:
```r
library(ggprism)

ggplot(dati, aes(x = Dose, y = Response)) +
  geom_point() +
  geom_smooth(method = "drm", method.args = list(fct = LL.4())) +
  scale_x_log10() +  # Scala logaritmica per dose
  theme_prism()
```

## Librerie Richieste

### Regressione Lineare
- `stats` (base R)
- `ggplot2`

### Regressione Non Lineare
- `drc`: Dose-response curves
- `ggplot2`, `ggprism`: Visualizzazione
- `matrixStats`: Calcolo statistiche su matrici

### FRAP
- Stesse librerie + eventualmente `minpack.lm` per fit avanzati

## Scelta dello Script

| Situazione | Script Consigliato |
|------------|-------------------|
| Correlazione lineare semplice | `06_linear_regression_basic.R` |
| Curve multiple lineari | `08_linear_regression_multiple.R` |
| Curve dose-risposta (IC50) | `02_dose_response_curve.R` |
| Dati con replicati + errori | `04_multiple_curves_errors.R` |
| Analisi FRAP | `01_frap_general_2023.R` |

## Note

- Verifica sempre i residui per validare il modello
- Per dose-risposta, usa scala logaritmica sull'asse X
- IC50/EC50 sono equivalenti per curve di inibizione
- Per modelli complessi, considera fitting non lineare (nls, nlme)
