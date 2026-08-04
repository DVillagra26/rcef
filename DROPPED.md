# Dropped from the old `R/` folder

Verified unreferenced two ways before removal:

1. no call site anywhere in `app.R`, `mod_*.R`, `0_Functions_*.R`,
   `summaries.R`, `session_log.R`;
2. no call site inside the six vendored files that were **kept** — i.e.
   nothing kept depends on anything dropped.

| dropped | held | why |
|---|---|---|
| `1.diagnosis_discards.R` | `diagnosis_discards()` | superseded by `classify_strata()` + `plot_coverage()` |
| `1.diagnosis_discards_WORKED.R` | `diagnosis_discards()` (second copy) | duplicate of the above, different body |
| `cef_add_strata.R` | `cef_add_strata()`, `cef_remove_strata()`, `cef_generate_strata_combinations()`, `cef_expand_strata_full()` | superseded by `build_strata()` |
| `cef_allocate_age_strata.R` | `cef_allocate_strata()` | superseded by `catchAllocationAoL()` |
| `cef_compute_ref_numbers.R` | `cef_compute_ref_numbers()` | unused |
| `discard_raising_functions.R` | `std_catch_cat()`, `normalise_catch_names()`, `add_fleet_levels()`, `prep_census()`, `summarise_strata()`, `diagnose_coverage()`, `raise_discards_ladder()`, `plot_ladder()`, `raise_discards_rules()` | prototype ladder; superseded by `catchRaising()` + the cascade grouping |
| `raise_discards_strata.R` | `raise_discards_strata()` | prototype |
| `strata_coverage_summary.R` | `strata_coverage_summary()` | superseded by `classify_strata()` |
| `split_L_with_without_D.R` | `split_L_with_without_D()` | prototype |
| `funCompareIntercatchCEF.R` | `funCompareIntercatchCEF()` | not wired into any tab |
| `funReadIntercatch.R` | `funReadIntercatch()` | not called; `funIntercatchCEF()` reads for itself |
| `sop.R` | `sop()` | unused |
| `utils.R` | `unpack_list()` | unused |
| `discard_coverage_explorer.Rmd`, `discard_raising_app.Rmd` | — | superseded by the app |
| `.Rhistory`, `README` | — | noise; README content is now `MANIFEST.md` |

Nothing was deleted destructively: the originals stay in git history, and
`funCompareIntercatchCEF()` in particular is worth revisiting if a
CEF-vs-InterCatch reconciliation tab is ever wanted.
