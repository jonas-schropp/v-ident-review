install.packages('renv')

packages <- c("elizagrames/litsearchr", "easyPubMed", "revtools",
              "dplyr", "tidyr", "ggplot2", "ggrepel",
              "rvest", "here", "hrbrmstr/cfhttr",
              "nealhaddaway/GSscraper", "readr",
              "pdftools", "textclean", "tm", "igraph",
              "tidygraph", "ggraph", "quanteda",
              "rscopus", "stringr", "rentrez", "xml2",
              "RefManageR", "stringdist", "fuzzyjoin",
              "reticulate", "tidytext", "tm", "text2vec",
              "topicmodels", "reshape2", "writexl", "readxl",
              "roadoi", "brms", "devtools", "stan-dev/cmdstanr",
              "flextable", "officer",
              "mice", "miceadds",
              # Publication figures: colour-blind pastel forest + raincloud plots.
              "ggdist", "gghalves", "scales")

renv::install(packages)
y
 
cmdstanr::install_cmdstan()
