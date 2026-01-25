# TODO - Prossima Sessione

## 🎯 Obiettivo: Iniziare Testing Sistematico Script

### Fase 1: Primo Script Template

#### 1. Testare `templates/ttest/01_ttest_unpaired.R`

**Passi da seguire:**
- [ ] Leggere e analizzare lo script
- [ ] Creare dataset sintetico di test (es. 2 gruppi, n=20 per gruppo)
- [ ] Eseguire lo script end-to-end
- [ ] Identificare eventuali errori o warning
- [ ] Documentare assunzioni statistiche (normalità, omogeneità varianze)

**Refactoring da considerare:**
- [ ] Aggiungere header descrittivo
- [ ] Migliorare nomi variabili se necessario
- [ ] Aggiungere commenti esplicativi
- [ ] Verificare best practices R
- [ ] Separare sezioni logiche (setup, analisi, plot, output)

**Documentazione:**
- [ ] Aggiornare `templates/ttest/README.md` con esempio pratico
- [ ] Aggiungere badge ✅ Testato una volta validato
- [ ] Documentare formato dati richiesto
- [ ] Includere interpretazione output

#### 2. Workflow Template Testing

Una volta validato il primo script, replicare il processo per:
1. `templates/ttest/02_ttest_with_boxplot.R`
2. `templates/ttest/03_nonparametric_wilcox.R`
3. Continuare con categoria successiva (ANOVA)

### Note Tecniche

**Dataset Sintetici:**
- Usare `set.seed()` per riproducibilità
- Simulare dati realistici (media, SD, dimensione campione tipici di studi biomedici)
- Includere sia casi "puliti" che edge cases (outlier, violazioni assunzioni)

**Validazione Statistica:**
- Verificare sempre Shapiro-Wilk test (normalità)
- Test di Levene (omogeneità varianze)
- Confermare scelta test parametrico vs non-parametrico

**Output Aspettato:**
- Grafici publication-ready (font size, labels, legenda)
- Statistiche chiaramente leggibili
- P-values formattati correttamente
- Salvataggio output (tabelle, grafici)

---

## 📊 Progress Tracking

**Script Testati:** 0 / 50+
**Categorie Completate:** 0 / 10+
**Progetti Validati:** 0 / 40+

**Prossimo Milestone:** Completare categoria `templates/ttest/` (3 script)

---

_Creato: 2026-01-25_
_Ultima modifica: 2026-01-25_
