# OASIS Shiny app dependencies
# Run with: Rscript requirements.R

# CRAN packages
cran_pkgs <- c(
  "shiny",
  "tidyverse",
  "dplyr",
  "tidyr",
  "purrr",
  "forcats",
  "stringr",
  "tibble",
  "data.table",
  "DT",
  "gsheet",
  "tippy",
  "shinyBS",
  "shinyjs",
  "shinythemes",
  "shinycssloaders",
  "shinybusy",
  "shinyhelper",
  "shinyWidgets",
  "feather",
  "bsplus",
  "bslib",
  "plotly",
  "kableExtra",
  "stringdist",
  "readr",
  "stringi",
  "RColorBrewer",
  "ggplot2",
  "htmlwidgets",
  "knitr",
  "rmarkdown"
)

install_if_missing <- function(pkgs) {
  missing <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing)) {
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing(cran_pkgs)

# GitHub-only dependency
if (!requireNamespace("RBedtools", quietly = TRUE)) {
  install_if_missing("remotes")
  remotes::install_github("vinay-swamy/RBedtools")
}

# Note: RBedtools expects the bedtools command-line tool to be available in your PATH.
