# Data Cleaning / Prep

clean_dir <- here::here("code/data-cleaning")
clean_files <- list.files(clean_dir, full.names = TRUE)
lapply(clean_files, source)

inpath <- here::here("data", "data-extraction.xlsx")

dext <- code_dext(inpath)

for (n in names(dext)) {
  
  signal <- dext[['signal']][,c('id', 'experience', 'modality')]
  tmp <- dext[[n]]
  
  if (n %in% c('preproc', 'features', 'auth', 'eval')) {
    tmp <- left_join(signal, tmp)
  }
  
  readr::write_csv(tmp, here::here("data", paste0(n, ".csv")))
  readr::write_rds(tmp, here::here("data", paste0(n, ".rds")))
  
}



# Data Visualization

viz_dir <- here::here("code/data-visualization")
viz_files <- list.files(viz_dir, full.names = TRUE)
lapply(viz_files, source)

plot_modality_over_time(
  general_path = here::here("data", "general.rds"),
  signal_path = here::here("data", "signal.rds"),
  output_path = here::here("results", "figures", "modality_over_time.tiff"),
  dpi = 300
)

# Tables

table_dir <- here::here("code/tables")
table_files <- list.files(table_dir, full.names = TRUE)
lapply(table_files, source)

tables_modality_over_time(
  general_path = here::here("data", "general.rds"),
  signal_path = here::here("data", "signal.rds"),
  output_dir = here::here("results", "tables")
)

get_preprocessing_info(
  preproc_path = here::here("data", "preproc.rds"),
  output_dir = here::here("data")
)

tables_authentication_performance_by_study(
  eval_path = here::here("data", "eval.rds"),
  output_dir = here::here("results", "tables")
)

tables_validation_metrics(
  eval_path = here::here("data", "eval.rds"),
  output_dir = here::here("results", "tables")
)

tables_authentication_algorithms(
  auth_path = here::here("data", "auth.rds"),
  output_dir = here::here("results", "tables")
)

tables_features(
  features_path = here::here("data", "features.rds"),
  output_dir = here::here("results", "tables")
)

get_wearable_devices(
  signal_path = here::here("data", "signal.rds"),
  output_dir = here::here("results", "tables")
)

tables_datasets(
  signal_path = here::here("data", "signal.rds"),
  auth_path = here::here("data", "auth.rds"),
  output_dir = here::here("results", "tables")
)

tables_study_characteristics(
  general_path = here::here("data", "general.rds"),
  extraction_path = here::here("data", "signal.rds"),
  output_path = here::here("results", "tables", "study_characteristics.csv")
)

