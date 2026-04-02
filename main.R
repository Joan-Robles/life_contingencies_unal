
# prep
rm(list = ls())
.initial_time <- Sys.time() # set timer
set.seed(123) # Seed

# Auxiliar functions --------------------------------------------------------

source("general_graphs.R") 
source("general_functions.R")


# Packages ----------------------------------------------------------------

# Install pacman if not available
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Load (and install if needed) packages
pacman::p_load(
  readxl,
  dplyr,
  forecast,
  parallel,
  pdftools,
  fanplot,
  plotly
)

# Parameters --------------------------------------------------------------

n_sim <- 1000
file_path <- file.path(getwd(), "res-1555-2010.pdf") # mortality table path
i <- 0.025
inflation <- 0.052

# Read and process mortality table ---------------------------------------------------

.mortality_file <- pdf_text(file_path)[2]
mortality_table <- process_mortality_table(.mortality_file, 0.1) # file

# process functions
tst1 <- Problem1(34, 12, 0.05, .mortality_file, "women", 0.1)
