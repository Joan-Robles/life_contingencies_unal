
# draft for Taller_1.Rmd (the definite file)

# prep
rm(list = ls())
.initial_time <- Sys.time() # set timer
set.seed(123) # Seed

# Auxiliar functions --------------------------------------------------------

source("general_functions.R")
source("life_ensurance.R")
source("specific_functions.R")

# Packages ----------------------------------------------------------------

# Install pacman if not available
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Load (and install if needed) packages
pacman::p_load(
  dplyr,
  parallel,
  pdftools,
  plotly
)

# Parameters --------------------------------------------------------------

n_sim <- 1000
file_path <- file.path(getwd(), "res-1555-2010.pdf") # mortality table path
i <- 0.025
inflation <- 0.052
x_values = 45:90
n_values = 1:30

# Read and process mortality table ---------------------------------------------------

.mortality_file <- pdf_text(file_path)[2]


# workshop --------------------------------------------------------------

# ex 1
A <- get_premium_insurance(mortality_file = .mortality_file,
                           x = 34,
                           n = Inf,
                           i = 0.1,
                           r = 0.05,
                           frac_pay = TRUE,
                           frac_value = FALSE,
                           initial_payment = "a")

tst1 <- price_case_11(34, 12, 0.05, .mortality_file, "women", 0.1)
get_case_index(frac_pay = TRUE, frac_value = FALSE, growth = "arithmetic", r = 0.05, initial_payment = "a")

A$premium; tst1


# ex 2. 
A <- get_premium_insurance(mortality_file = .mortality_file,
                           x = 27,
                           n = Inf,
                           i = 0.1,
                           r = 0.05,
                           frac_pay = TRUE,
                           frac_value = TRUE,
                           initial_payment = "b")

tst2 <- price_case_25(27, 12, 0.05, .mortality_file, "women", 0.1)
get_case_index(frac_pay = TRUE, frac_value = TRUE, growth = "arithmetic", r = 0.05, initial_payment = "b")
A$premium; tst2


# ex 3. n pendiente
A <- get_premium_insurance(
  mortality_file = .mortality_file,
  gender = "women",
  x = 25,
  n = 50,
  i = 0.1,
  r = 0.05,
  frac_pay = TRUE,
  frac_value = TRUE,
  initial_payment = "a",
  growth = "arithmetic"
)

tst_3 <- price_case_09_term(
  x = 25,
  n = 50,
  m = 12,
  r = 0.05,
  .mortality_file = .mortality_file,
  gender = "women",
  interest = 0.1
)

A$premium; tst_3


# ex 5
A <- get_premium_insurance(mortality_file = .mortality_file,
                           x = 26,
                           n = Inf,
                           i = 0.1,
                           r = 0.05,
                           frac_pay = TRUE,
                           frac_value = FALSE,
                           initial_payment = "a",
                           growth = "geometric")

tst5 <- price_case_15(26, 12, 0.05, .mortality_file, "women", 0.1)

get_case_index(frac_pay = TRUE,
               frac_value = FALSE,
               growth = "geometric",
               r = 0.05,
               initial_payment = "a")

A$premium; tst5

# Nice plots --------------------------------------------------------------


premium_table <- evaluate_premium_grid(
  x_values = x_values,
  n_values = n_values,
  pricing_fun = get_premium_insurance,
  mortality_file = .mortality_file,
  i = 0.1,
  r = 0.05,
  frac_pay = TRUE,
  frac_value = FALSE,
  initial_payment = "a"
)


heatmap_xyz(x = premium_table$x, y = premium_table$y, z = premium_table$z,
            xlab = "Edad", ylab = "Años asegurados", main = "Producto ...") 
