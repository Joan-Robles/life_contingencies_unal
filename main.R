
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

# Read and process mortality table ---------------------------------------------------

.mortality_file <- pdf_text(file_path)
mortality_table <- .mortality_file[2]

mortality_table <- process_mortality_table(mortality_table)

# keep what matters
mortality_table <- mortality_table[, c(1, 4, 9)]
colnames(mortality_table) <- c("age", "q_men", "q_women")

# get complements
p_men <- 1 - mortality_table[, "q_men"]
p_women <- 1 - mortality_table[, "q_women"]
