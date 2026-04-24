inpath <- here::here("data",
                   "data-extraction.xlsx")

#' Code Extracted Data into Analysis Categories
#' 
#' @param inpath the path to the data extraction excel file
#' @param outpath the path to the coded data file for results
#' 
code_dext <- function(inpath) {
  
  require(dplyr)
  
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
  
}

