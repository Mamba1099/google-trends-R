
load_required_packages <- function(packages) {
  # Load each required package
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of required packages
packages_to_load <- c(
  "tidyverse",
  "knitr",
  "tinytex",
  "moderndive",
  "fivethirtyeight",
  "httr",
  "utils",
  "styler",
  "shiny",
  "openxlsx",
  "httpuv"
)

# Call the function to load required packages
load_required_packages(packages_to_load)

