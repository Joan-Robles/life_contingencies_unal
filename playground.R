
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

.mortality_file <- pdf_text(file_path)[2]
mortality_table <- process_mortality_table(.mortality_file, 0.1) # file

# # get complements
# p_men <- 1 - mortality_table[, "q_men"]
# p_women <- 1 - mortality_table[, "q_women"]

mortality_table <- as.data.frame(mortality_table)

# Parametros
interest = 0.1
v = 1/(1+interest)

mortality_table["dx_men"] = NA
mortality_table["lx_men"] = NA
mortality_table["dx_women"] = NA
mortality_table["lx_women"] = NA

mortality_table$lx_men[1] = 100000
mortality_table$lx_women[1] = 100000

for (i in 1:nrow(mortality_table)) {
  if (i != 1) {
    mortality_table$lx_men[i] = mortality_table$lx_men[i-1] - mortality_table$dx_men[i-1]
    mortality_table$lx_women[i] = mortality_table$lx_women[i-1] - mortality_table$dx_women[i-1]
  }
  mortality_table$dx_men[i] = mortality_table$lx_men[i] * mortality_table$q_men[i]
  mortality_table$dx_women[i] = mortality_table$lx_women[i] * mortality_table$q_women[i]
}

mortality_table <- mortality_table %>% mutate(
  Dx_men = lx_men*v^age,
  Dx_women = lx_women*v^age,
  Cx_men = Dx_men*v*q_men,
  Cx_women = Dx_women*v*q_women
)

mortality_table$Mx_men <- NA
mortality_table$Mx_women <- NA
sum_men = 0
sum_women = 0
for (i in 1:nrow(mortality_table)) {
  sum_men <- sum_men + mortality_table$Cx_men[nrow(mortality_table)+1-i]
  sum_women <- sum_women + mortality_table$Cx_women[nrow(mortality_table)+1-i]
  mortality_table$Mx_men[nrow(mortality_table)+1-i] <- sum_men
  mortality_table$Mx_women[nrow(mortality_table)+1-i] <- sum_women
}

mortality_table$Ax_men <- mortality_table$Mx_men/mortality_table$Dx_men
mortality_table$Ax_women <- mortality_table$Mx_women/mortality_table$Dx_women
