# RIVM Dutch-river benchmark inputs: provenance

| Field | Value |
|---|---|
| Repository | https://github.com/rivm-syso/MicroplasticsERA_water |
| Commit SHA | `7729fa66a677aec0a481b57fa22ae15641a481ee` (commit date 2026-09-08) |
| Retrieved | 2026-10-14 |
| Licence | European Union Public Licence v1.2 (EUPL-1.2) |
| Report | Quik, J.T.K., van Breemen, P., Hids, A.R., van Loon, S., Mintenig, S.M., Koelmans, A.A., 2026. Screening level ecological risk assessment of Microplastics in Dutch surface waters (No. 2026-0069). RIVM, Bilthoven. https://doi.org/10.21945/RIVM-2026-0069 |

## What is (and is not) in this repository

The raw RIVM clone is **not** redistributed. It is gitignored at
`data_input/external/rivm/MicroplasticsERA_water/`. Only the derived summaries in
`derived/` are committed:

| File | Content |
|---|---|
| `derived/rivm_alphas_longest.csv` | Power-law fits, `param == "longest"`, from `Output/Data/powerlawFitFTIR_All2026-06-30.csv` and `Output/Data/powerlawFitLDIR_All2026-04-09.csv`; `a_psd = -alpha` |
| `derived/rivm_sample_audit.csv` | Every Meta row of the three readme workbooks with its total concentration (as reported and excluding rubber), inclusion flag and exclusion reason |
| `derived/rivm_mintenig2020_rubber_match.csv` | Mintenig 2020 `Meta$Location_name` to `Corrected_concentrations$Location` match used for the rubber-excluded totals (41/41 matched) |
| `derived/rivm_published_rescaled_W1_by_sample.csv` | Per-sample median and 5–95% of RIVM's own rescaled concentrations (`Output/Data/Rescaled_concentrations_2026-05-13.Rds`, `OptionType == "concentrationOption3"`, 1–5000 µm) |

`R/rivm_utils.R::rivm_load_inputs()` rebuilds these CSVs from the clone when the clone is
present. Without the clone, it reads the committed CSVs, so the benchmark still renders.

## Files read from the clone

- `Output/Data/powerlawFitFTIR_All2026-06-30.csv`, `Output/Data/powerlawFitLDIR_All2026-04-09.csv`
- `rawdata/Mintenig_2020/readme.xlsx` (sheets Meta, Corrected_concentrations, Corrected_concentrations_total)
- `rawdata/Mughini-Gras_2021/readme.xlsx` (sheets Meta, Concentrations)
- `rawdata/Mintenig_2025/readme.xlsx` (Meta) and `rawdata/Mintenig_2025/Concentrations.xlsx` (per-polymer, summed per site)
- `Output/Data/Rescaled_concentrations_2026-05-13.Rds` (Git LFS)

The readme workbooks are byte-identical in size to the copies supplied by the user:
`readme.xlsx` is Mintenig 2020, `readme (2).xlsx` is Mughini-Gras 2021, and
`readme (1).xlsx` is Mintenig 2025.

## Re-fetching

```bash
cd data_input/external/rivm
git clone https://github.com/rivm-syso/MicroplasticsERA_water.git
cd MicroplasticsERA_water
git checkout 7729fa66a677aec0a481b57fa22ae15641a481ee
git lfs install && git lfs pull   # .xlsx and .Rds are Git LFS; without this you get pointer stubs
```

Alternatively, point `RIVM_REPO_DIR` at an existing clone.

## Data limitations

- Bäuerlein et al. (2022, 2023) particle data (Meuse, Overijsselse Vecht, Lek, WWTP canal; LDIR) are not public. RIVM's slopes for these water bodies appear in the slope comparison only. RIVM's derived total-concentration table does contain their totals, but those samples are deliberately excluded from the concentration figures.
- Mughini-Gras 2021 reports totals for samples 1, 2, 5 and 6 only (Lobith A and Vianen A, summer and winter).
- The nine Mintenig 2020 samples with LLm = 300 µm (Maasbommel*, Oijen*) have no total in `Corrected_concentrations_total` and are excluded. Two of them are also WWTP effluent.
- LDIR lower size limit: the sample metadata say 10 µm and the RIVM report text says 20 µm. The metadata value is used, and 20 µm is run as a sensitivity.

## Citation

The repository READMEs ask users to cite the original studies:

- Mintenig, S.M., Kooi, M., Erich, M.W., Primpke, S., Redondo-Hasselerharm, P.E., Dekker, S.C., Koelmans, A.A., van Wezel, A.P., 2020. A systems approach to understand microplastic occurrence and variability in Dutch riverine surface waters. Water Research 176, 115723. https://doi.org/10.1016/j.watres.2020.115723
- Mughini-Gras, L., van der Plaats, R.Q.J., van der Wielen, P.W.J.J., Bauerlein, P.S., de Roda Husman, A.M., 2021. Riverine microplastic and microbial community compositions: A field study in the Netherlands. Water Research 192, 116852. https://doi.org/10.1016/j.watres.2021.116852
- Mintenig, S.M., Hunnestad, A., Koelmans, A.A., 2025. Microplastics in het oppervlaktewater van de Dommel. Wageningen University & Research.
