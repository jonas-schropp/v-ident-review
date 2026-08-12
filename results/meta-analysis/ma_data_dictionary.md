# Meta-analysis data dictionary

Inputs: `data/eval.rds`, `data/signal.rds`, `data/auth.rds`, `data/features.rds`, and `data/preproc.rds`. RDS files are preferred because they preserve R types; CSV mirrors are written only as inspection/export artifacts.

Eligible experiment rows are the 726 unique `id` + `experience` rows in `data/eval.rds`; `id` identifies the study and `experience` identifies the experiment within that study. Rows are included only when accuracy can be obtained directly or from valid EER conversion.

`ma_data.rds` is the single canonical analysis-ready meta-analysis data set used for imputation and modeling. Standard-error provenance is written to `standard_error_provenance.rds` and mirrored to `standard_error_provenance.csv`.

| Variable | Source | Definition |
|---|---|---|
| `experiment_id` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `source_row` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `stable_key` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `id` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `experience` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `modality` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `conditions` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `permanence` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `algorithm_family` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `number_of_individuals` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `n` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `logn` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `outcome_source` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `outcome_percent` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `outcome_probability` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `outcome_probability_corrected` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `boundary_epsilon` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `outcome_decision_note` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `uncertainty_reported_source` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `uncertainty_reported_percent` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `se_provenance_category` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `se_conversion_denominator` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `uncertainty_exclusion_flag` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `se_decision_note` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `mean` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `ylogit` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `se` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `se2` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `se_logit` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `has_se` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `se2_logit` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `has_se2` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `ECG` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `PPG` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `Other` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `multimodal` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `algo` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `algo_other` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `intruders` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `activity` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `duration` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `auth_time` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `enrol_time` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `device` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `device_location` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `feature_type` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `feature_dim` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `dim_reduction` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `noise_reduction` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `segmentation_type` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `normalizationyn` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `filteringyn` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
| `external_validation` | canonical meta-analysis data | Analysis-ready variable derived from current cleaned RDS exports; blank/NA/not reported values are treated as missing before modeling-specific coding. |
