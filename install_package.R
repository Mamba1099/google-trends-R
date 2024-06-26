
install_required_packages <- function(packages) {
  installed_packages <- rownames(installed.packages())

  packages_to_install <- packages[!(packages %in% installed_packages)]
  
  if (length(packages_to_install) > 0) {
    install.packages(packages_to_install)
  }
}

# List of required packages
packages_to_install <- c(
  "tidyverse",
  "knitr",
  "moderndive",
  "reshape2",
  "shiny",
  "tinytex",
  "styler",
  "utils",
  "fivethirtyeight"

)

# Call the function to install required packages
install_required_packages(packages_to_install)
