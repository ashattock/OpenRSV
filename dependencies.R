###########################################################
# DEPENDENCIES
#
# Deal with all package dependencies in one place.
#
###########################################################

# ---- R version check ----

# R versions for which this project has been tested and is stable
stable_versions = c("4.4.0", "4.4.1")

# R versions for which this project is stable (as a string)
stable_str = paste(stable_versions, collapse = ", ")

# Get details of R version currently running
version_info = R.Version()

# Construct version number from list details
version_num = paste0(version_info$major, ".",  version_info$minor)

# Throw an error if this R version is unsuitable
if (!version_num %in% stable_versions)
  stop("This software is stable with R version(s): ", stable_str,
       " (currently running ", version_num, ")")

# Clear global environment
rm(list = ls())

# ---- Source files ----

# Scripts that should bot be sourced
no_src = c(
  "launch.R", 
  "submit.R",
  "dependencies.R", 
  "copy_project.R")

# All R files, and those to source
all_files = list.files(pattern = ".+\\.R$")
src_files = setdiff(all_files, no_src)

# Source each of these files
for (file in src_files)
  source(file)

# ---- Define packages ----

# Complete list of all packages required for this project
packages = c(
  "tidyverse",      # Includes ggplot2, dplyr, tidyr (www.tidyverse.org/packages/)
  "magrittr",       # Additional pipe operators, such as %<>%
  "data.table",     # Next generation dataframes
  "dtplyr",         # Syntax of dplyr with the speed of datatable
  "useful",         # General helper functions (eg compare.list)
  "rlist",          # List-related helper functions (eg list.remove)
  "gsubfn",         # Output multiple variables from functions
  "wrapr",          # Convenience functions (eg qc)
  "stats",          # Statistical calculations and random number generation
  "matrixStats",    # Matrix row and column operations
  "tgp",            # Latin hypercube sampler
  "hetGP",          # Gaussian Process model and acquisition functions
  # "philentropy",    # Distance measures
  # "smooth",         # Simple moving average model
  # "splines",        # Spline fitting models
  # "forecast",       # Linear regression for time series
  # "imputeTS",       # Imputation for time series
  # "akima",          # Bivariate interpolation
  "tidygraph",      # Out of the box network solutions
  # "widyr",          # Compile network properties
  # "socialmixr",     # Age structured contact matrixes from POLYMOD
  "wrswoR",         # Fast weighted integer sampling without replacement
  # "phonenumber",    # Letter to number conversion
  # "progress",       # Progress bar
  "tictoc",         # Code timer
  # "httr",           # Read data from API endpoint
  # "jsonlite",       # Convert data to/from json format
  # "rio",            # Data loading functionality
  # "readxl",         # Data loading functionality
  "yaml",           # Data loading functionality
  "lubridate",      # Data formatting functionality
  # "naniar",         # Data formatting functionality
  # "coda",           # Plotting functionality
  # "gridExtra",      # Plotting functionality
  # "ggnewscale",     # Plotting functionality
  "ggpubr",         # Plotting functionality
  # "cowplot",        # Plotting functionality
  "scales",         # Plotting functionality
  # "ggh4x",          # Plotting functionality (flexible faceting)
  # "ggtext",         # Plotting functionality (use markdown in labels)
  "pals")           # Colour palettes
# "colorspace",     # Colour palettes
# "RColorBrewer")   # Colour palettes

# List of all packages only available from github
# gh_packages = c("eliocamp/tagger")

# ---- Install and/or load packages with pacman ----

message("* Installing required packages")

# Check whether pacman itself has been installed
pacman_installed = "pacman" %in% rownames(installed.packages())

# If not, install it
if (!pacman_installed) 
  install.packages("pacman")

# Load pacman
library(pacman) 

# Load all required packages, installing them if required
pacman::p_load(char = packages)

# Same for github packages
# pacman::p_load_gh(gh_packages)

# ---- Redefine or unmask particular functions ----

# Unmask certain functions otherwise overwritten
select  = dplyr::select
filter  = dplyr::filter
rename  = dplyr::rename
recode  = dplyr::recode
count   = dplyr::count
union   = dplyr::union
predict = stats::predict

# ---- Clean up ----

# Tidy up console after package loading
if (interactive()) clf()  # Close figures
if (interactive()) clc()  # Clear console

