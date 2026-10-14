# Tracked-change edits for the Savoca et al. drafts (Dutch-river benchmark)

These scripts insert the RIVM benchmark text, figures, table and references into the Word drafts in `docs/` as tracked changes (author "Claude"). Every number is read from the CSVs written by `R/rivm_benchmark.qmd` (`data_output/rivm_benchmark_*_v1.csv`). Interpretive wording, such as "overlapped" or "exceeded our upper quartile", is guarded by assertions on those CSVs.

```bash
# 1. merge fragmented runs (docx skill: scripts/merge_runs.py), then
python tools/rivm_docx/edit_main.py main_merged.docx main_edit.docx
python tools/rivm_docx/edit_si.py   si_merged.docx   si_edit.docx
# 2. add the review comments with the docx skill's comment.py using --id 801/802/803 (main; 803 = reply to 25) and 811/812 (SI)
```

Outputs used for the v1 drafts: `docs/*_v1_sc_rivm.docx`.

## Inputs (all generated, never hand-typed)

- `data_output/rivm_benchmark_*_v1.csv`, written by `R/rivm_benchmark.qmd` (the Dutch benchmark).
- `data_output/psd_rescaling_params.json`, written by Appendix M of `R/probabilistic_risk_characterization.qmd`. It holds the PSD slopes, heterogeneity, two-segment fit, sieve-window CFs, W1/W2 concentrations and headline risk.
- `figures/*.png`. Regenerated pipeline figures are swapped into the drafts (width kept, aspect corrected), and each swap is flagged with a comment.

Re-run the pipeline first, then both scripts, then the `comment.py` calls (comment ids 801-804 main, 811-818 SI).
