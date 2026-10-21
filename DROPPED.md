# The Shiny application, taken out to work on the package alone

The whole application, and every function only it used, left the working tree
on 2026-10-13. Nothing was lost: the last commit that has it (`a39e29e`) carries
the tag **`app-archive`**.

```sh
git worktree add ../rcef-app app-archive       # the package with the app, as it was, in its own folder
git checkout app-archive -- inst/app R/run_app.R   # or bring single files back into this tree
git show app-archive:R/rcef_taf.R              # or read one
```

A function counted as the app's when the only things that reach it are the
app, its tests and dev scripts: it is not called by the user's TAF scripts, not
by the vignettes or README, not by any of the documented functions, and not by
another function that is. Bringing the app back therefore means bringing these
back with it (the tag has them all), plus `shiny`, `DT` and `rsconnect` in
`Suggests`.

| gone | what it was |
|---|---|
| `inst/app/` (4 files), `R/run_app.R` | the application and its entry point |
| `R/rcef_taf.R`, `R/rcef_emit.R`, `R/rcef_session.R` | the scripts the app wrote (TAF set, single script), the session log and state save / restore |
| `R/rcef_intercatch.R`, `R/rcef_status.R`, `R/rcef_filter.R` | the app's InterCatch staging, availability panel and figure filters |
| `R/rcef_raise_native.R` | `run_native_raising()` and the `cef_*` helpers around `catchRaising()`: the app's bridge from its groups to the engine. Call `catchRaising()` directly |
| in `R/rcef_alloc.R` | `run_allocation()`, the `agg_*` wrappers and the allocation comparison: the app's bridge to `catchAllocationAoL()`. Call `catchAllocationAoL()`, `catchAtAoLAggregation()`, `meanWoLatAoLAggregation()` directly |
| in `R/rcef_length.R`, `R/rcef_groups.R`, `R/rcef_exclude.R`, `R/rcef_summaries.R`, `R/rcef_utils.R` | the app's allocation cascade and rule tables, donor-exclusion pickers, summary tables, key vocabulary and labellers |
| `plot_river_level()`, `status_legend_plot()`, `seed_palettes()` | app plots and palette seeding; the plots no longer carry the `"status_legend"` attribute the app read |
| `dev/check_app.R`, `check_server.R`, `check_sections.R`, `check_parity.R`, `check_cssquotes.R`, `deploy_shinyapps.R`, `dev/tests/legacy/` and 25 suites in `dev/tests/` | their checks. The suites that tested a function that stays were kept and now call the engines directly (`fx_raise()`, `fx_allocate()` in `dev/tests/fixture.R`) |

`define_raising_groups_auto()` and `define_raising_groups_manual()` went before
that, for the same reason: they were the app's way of turning its groups into
conditions. The groups are conditions you write (`strataCond`, `matchedDataCond`)
and check with `check_group_conditions()`.

The 174 definitions, by the file they were in:

```
R/rcef_alloc.R        run_allocation, agg_numbers, agg_meanweight, alloc_pivot_long, alloc_src_col, alloc_src_label,
                      alloc_compare_numbers, alloc_stock_weight, plot_alloc_numbers, plot_alloc_stockweight,
                      alloc_overview, emit_alloc_agg_code, run_allocation_set, compare_allocations,
                      comparison_totals, plot_allocation_comparison; ALLOC_WIDE_RE, ALLOC_SRC_COLS
R/rcef_emit.R         wrap_lines, emit_header, emit_load_code, emit_filter_code, emit_derive_code, emit_area_helper,
                      emit_sets_code, emit_hier_code, emit_rules_code, emit_explore_filter_code
R/rcef_exclude.R      exclusion_choices, exclusion_overview, exclusion_expr, exclusion_report
R/rcef_filter.R       explore_filter_levels, clean_filters, apply_filters, filter_label, filter_coverage; FILTER_ANY
R/rcef_groups.R       fmt_num, clause_string, cond_string, roll_up_excluded_keys, donor_exclusion_expr,
                      source_cond_string, rule_effective_sets, rule_group_summary, conds_from_rules, close_catch_all,
                      rule_decision_table, rule_river_data, rules_progress, rules_unassigned, strata_raise_table,
                      strata_donor_table
R/rcef_intercatch.R   ic_code, ic_stage_mapping, ic_detect_year; OUTPUT_FORMATS, OUTPUT_DESC, IC_FILE_ROLES,
                      IC_FILE_PATTERN, IC_CANONICAL
R/rcef_length.R       distribution_units, dist_without_catch, length_scale_check, length_link_summary, cc_bracket,
                      as_cc_spec, cc_step_value, fmt_cc_spec, as_cc_pair, alloc_steps, alloc_step_scope,
                      alloc_cond_string, alloc_quo, emit_alloc_conditions, emit_alloc_grouping_code,
                      emit_alloc_recipe_code, emit_alloc_code, expand_cc, assign_alloc_groups, plot_alloc_river,
                      assign_alloc_cascade, alloc_step_grouping, alloc_cascade_table, cascade_to_groups,
                      add_sufficiency, measurement_warning, fld, manual_progress, manual_missing,
                      stratum_sparkline, stratum_sparkline_map, alloc_matched_cond, alloc_conds_from_groups
R/rcef_raise_native.R cef_reorder, cef_check, cef_prepare, run_native_raising; CEF_ORDER, CEF_REQUIRED
R/rcef_raising.R      scenarios_to_json, scenarios_from_json, common_keys
R/rcef_session.R      log_new, log_set, log_render, state_to_json, state_from_json, state_updater, state_apply,
                      code_header, code_setup, code_session, code_auto_rule, code_manual_rule, code_raise,
                      code_compare, code_final, code_plots; LOG_ORDER, STATE_SKIP
R/rcef_status.R       rcef_sources, rcef_status; RCEF_OPTIONAL
R/rcef_summaries.R    catches_summary, catches_by_category, biology_coverage, distributions_summary,
                      distributions_by_type, effort_summary
R/rcef_taf.R          taf_header, taf_libs, taf_mkdir, taf_sources, taf_derive, taf_save_plot_helper,
                      taf_plot_blocks, taf_data, taf_model_explore_discards, taf_model_allocate_raise_discards,
                      taf_model_explore_length_age, taf_model_allocate_length_age, taf_output, taf_slug,
                      taf_report, taf_bundle, taf_concat, taf_write, taf_zip, taf_q, taf_call;
                      TAF_FILES, TAF_METHOD_LABEL, TAF_LABEL, TAF_BLURB
R/rcef_utils.R        fmt_t3, grouping_vars, key_vs_strata_warnings, key_order_warnings, key_vocab_grouped,
                      stratum_label, stratum_axis_label, rule_label, facet_scales;
                      KEY_FAMILY_RANK, KEY_FAMILY_OF, FILTER_VARS
R/run_app.R           run_app; RCEF_APP_PKGS
R/viz_helpers.R       status_legend_plot
R/viz_raising.R       plot_river_level
R/viz_theme.R         seed_palettes; PALETTE_VARS
```

---

# Cleared out after the dependency map

The dependency map (every function, who calls whom) showed what only the app
and the old tests needed, what was several functions doing one thing, and what
was a function with exactly one caller. All of it is in git as of `a39e29e`
(plus the working-tree edits made before it was committed).

**Ticked for deletion** (32): `add_stratum_metrics`, `add_tree_totals`, `and_list`,
`apply_raising`, `apply_rules_all`, `apply_rules_first`, `assign_rule_groups`,
`build_strata`, `check_partition`, `combine_thresholds`, `compare_scenarios`,
`comparison_table`, `conds_from_auto`, `decision_tree`, `donor_ok_of`,
`eval_clauses`, `fieldsToStrata`, `flag_donor_exclusion`, `fmt_vals`, `new_rule`,
`plot_scenario_comparison`, `raising_groups`, `raising_steps`, `recommend_raising`,
`reset_palettes`, `row_condition`, `run_scenario`, `scenario_auto`,
`scenario_manual`, `source_match`, `stratum_keys`, `stratum_metrics`; and with
`build_strata` the only thing that still used it, `count_measurements_by_stratum`.
The hierarchical cascade, the scenario machinery and the donor-exclusion rules
went with them: the groups are conditions you write. `rcef_groups.R`,
`rcef_raising.R`, `rcef_exclude.R` and `rcef_classify.R` are gone.

**Merged into one function** (the same job done several times, or a helper with
one caller and no output of its own, joined into that caller):

| now | absorbed |
|---|---|
| `plot_colours()` | `PAL_BASE`, `PAL_CB`, `PAL_GRADUAL`, `var_palette()`, `scale_fill_var()`, `scale_colour_var()`, `group_palette()` |
| `convert_units()` | `to_tonnes()`, `unit_factor()`, `WEIGHT_TO_TONNES`, `LEN_CLASS_TO_MM`, `NUMBER_TO_N`, `WEIGHT_TO_G` |
| `build_coverage_strata()` | `biology_link()`, `stratum_measurements()`, `coverage_extra_cols()`, `DIMS_ALL_CANDIDATES`, `link_dist_to_strata()` as far as coverage needed it |
| `plot_coverage_grid()` | `coverage_grid()`, `plot_coverage_heat()` and `plot_coverage_bubble()` (one function with `type = "heat"` or `"bubble"`), `classify_strata()`, `cat_lookup()`, `status_palette()`, `status_abbr()`, `combo_key()`, `thr_active()`, `coverage_status()`, `thr_lines()`, `thr_guide()`, `shortfall_label()`, `status_key_layer()` |
| `coverage_cells()` | `cov_title()`, `cov_title_what()`, `share_phrase()`, `weight_words()`, `samp_word()`, `default_weight_label()` |
| `biology_dist_prep()` | `link_dist_to_strata()`, `length_numbers()`, `length_meanweight()`, `pool_plus_group()` and `pool_length_bins()` (one pooling now), and the plus-group / bin-width checks that three functions each wrote out |
| `derive_fields()` | `ices_area_levels()`, `distribution_fields()` |
| `mesh_var_levels()` | `mesh_levels()`, `metier4_mesh_levels()` |
| `category_cols()` | `reorder_mesh()`, `discrete_cols()` |
| `facet_layer()` | `maybe_facet()`, `facet_two()` and four inline `facet_wrap()` calls |
| `scenario_list()` | `scenario_factor()` (its `factor` function now), `scenario_levels()`, `level_factor()` |
| `coverage_what()` | `normalize_what()`, `coverage_table_what()` |
| `biology_key()` | `biology_match_keys()` |
| `status_marks()` | `cross_marks()` (its `cross` function now), `status_levels()` (the constant `STATUS_LEVELS`) |
| `save_on_exit()` | `write_plot()` |
| `plot_coverage_map()` | `ices_area_centroid()`, `ICES_AREA_CENTROID` |
| `plot_DR()` | `finest_per_family()` |
| `strata_sentence()` | `oxford_list()` |
| `prep_distributions()` | the exact-repeat check that `link_dist_to_strata()` made (so the coverage table warns about repeated rows too) |

`%||%` is gone (its one use is written out).

**Tests.** Two suites went (`test_tree_merge`, `test_scenario_compare`) and the
fixture no longer builds groups with the cascade: `fx_groups()` hands back
hand-written ones. Suites that called an internal helper now go through the
public function it served (`plot_NaLoA()` / `plot_MWaLoA()` `$data` for the
distributions, the plots for the status of a cell). No function exists only for
a test.

**Vignettes.** `part2b-discard-raising-hierarchical-groups` and
`part3-compare-allocation-schemes` are gone with the cascade and the scenarios;
`part2a` is now `part2-discard-raising` (with the comparison of two ways of
raising that was in part 3); `part3-length-age-allocation` is new.

---

# One plot for before and after the raising and the allocation

The plots that looked at the data before a step and the ones that looked at its
result did the same job on different inputs. They are one function each now,
with the same arguments; what the step added (`importedOrRaised` on the catch,
`sampledOrEstimated` on the distributions) is a variable like any other and
the title says what the data holds: *imported + raised* / *only imported*,
*sampled + estimated* / *only sampled*.

| now | absorbed |
|---|---|
| `plot_DR()` | `plot_raised_DR()` |
| `plot_catches()` (new) | `plot_raised_catches()`, which only ever took a raised catch; it now takes any catch |
| `plot_NaLoA()` | `plot_allocated_NaLoA()` |
| `plot_MWaLoA()` | `plot_allocated_MWaLoA()` |
| `biology_dist_prep()` | `alloc_class_table()` (and with it `rcef_alloc.R`), which shared the plus-group / bin-width checks and the axis labels (`biology_axis_labels()`) with it. The allocated plots no longer run the framework's `catchAtAoLAggregation()` / `meanWoLatAoLAggregation()`; they are built the way the plots before the allocation are, from the allocation result's two tables, and give the same numbers (the two aggregation functions stay, as the way to get the tables) |
| `build_coverage_strata()` | the per-stratum sums `plot_raised_DR()` made itself: the table of a raised catch has `importedOrRaised` per stratum |

`plot_allocated_NaLoA(alloc, ...)` is `plot_NaLoA(alloc$catch, alloc$distribution, ...)`;
`plot_raised_catches(raised_catch = r, ...)` is `plot_catches(catch = r, ...)`;
`plot_raised_DR(raised_catch = r, ...)` is `plot_DR(catch = r, ...)`. The mark -
transparency then, now a cross, a dotted line or a hatched bar - is still
`alpha`; `alpha = NULL` draws none.

---

# One link between the catch and the distributions of an allocation

`alloc_domain()` (commit `88210a6`) gave a catch row with no `domainBiology` the
domain the allocation makes up for the strata it estimates a distribution for
(`"[alloc]_<season>_<area>_<unit>_<fleet>_<category>"`), but only when the
distributions held such domains and no catch row did, and it said so in a
message. It is gone. `catch_domain()` does it for every catch row, always:
`domainBiology` where the row has one, the allocation's own recipe where it has
none. `link_check()` warns - instead of the message - about a catch weight with
no distribution and a distribution with fish and no catch weight. The two
aggregations and `distributionsCombined()` call them, so the allocation's `$catch`
and the raised catch it ran on give the same tables.

---

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
| — (from `rcef_utils.R`) | `apply_custom_var()`, `apply_custom_vars()`, `custom_var_code()` | the custom-variable builder was removed from the app at the author's request; these were its engine and had no remaining call site |
| `.Rhistory`, `README` | — | noise; README content is now `MANIFEST.md` |

Nothing was deleted destructively: the originals stay in git history, and
`funCompareIntercatchCEF()` in particular is worth revisiting if a
CEF-vs-InterCatch reconciliation tab is ever wanted.
