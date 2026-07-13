#' Code Extracted Data into Analysis Categories
#' 
#' This function calls the single coding functions and creates a list of 
#' coded data frames for further processing into analysis tables.
#' 
#' @param inpath the path to the data extraction excel file
#' @param outpath the path to the coded data file for results
#' 
code_dext <- function(inpath) {
  
  require(dplyr)
  require(here)
  folder <- "code/data-cleaning"
  funs <- c("code-auth.R", "code-eval.R", "code-features.R", "code-preproc.R",
            "code-signal.R")
  for (i in funs)( source(here(folder, i)) )
  
  dext <- list(
    'general' = readxl::read_xlsx( inpath, sheet = 1 ),
    'signal' = readxl::read_xlsx( inpath, sheet = 2 ),
    'preproc' = readxl::read_xlsx( inpath, sheet = 3 ),
    'features' = readxl::read_xlsx( inpath, sheet = 4 ),
    'auth' = readxl::read_xlsx( inpath, sheet = 5 ),
    'eval' = readxl::read_xlsx( inpath, sheet = 6 ),
    'other' = readxl::read_xlsx( inpath, sheet = 7 )
  )
  
  dext[[2]] <- dext[[2]] %>%
    code_modality() %>%
    code_vital_parameter() %>%
    code_users() %>%
    code_device() %>%
    code_sampling() %>%
    code_channels() %>%
    code_duration() %>%
    code_users() %>%
    code_conditions() %>%
    code_permanence() %>%
    select(-`acquisition environment`)
  
  dext[[3]] <- dext[[3]] %>%
    code_filtering() %>%
    code_segmentation_type() %>%
    code_normalization()

  dext[[4]] <- dext[[4]] %>%
    code_feature_type() %>%
    code_feature_dim() %>%
    code_dim_reduction()
    
  return(dext)
}
