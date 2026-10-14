# Tracked-change edits for the Savoca et al. drafts (Dutch-river benchmark)

These scripts insert the RIVM benchmark text, figures, table and references into the Word drafts in `docs/` as tracked changes (author "Claude"). Every number is read from the CSVs written by `R/rivm_benchmark.qmd` (`data_output/rivm_benchmark_*_v1.csv`). Interpretive wording, such as "overlapped" or "exceeded our upper quartile", is guarded by assertions on those CSVs.

```bash
# 1. merge fragmented runs (docx skill: scripts/merge_runs.py), then
python tools/rivm_docx/edit_main.py main_merged.docx main_edit.docx
python tools/rivm_docx/edit_si.py   si_merged.docx   si_edit.docx
# 2. add the review comments with the docx skill's comment.py using --id 801/802/803 (main; 803 = reply to 25) and 811/812 (SI)
```

Outputs used for the v1 drafts: `docs/*_v1_sc_rivm.docx`.
