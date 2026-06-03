# setup.R — install required packages for the Data Harmonisation Assistant

pkgs <- c("shiny", "bslib", "DT", "dplyr", "haven", "readr", "tidyr", "purrr", "htmltools")

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(to_install) > 0) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
} else {
  message("All required packages already installed.")
}
