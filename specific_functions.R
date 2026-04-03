### TEMPORAL ###

source("general_graphs.R") 
source("general_functions.R")

n_sim <- 1000
file_path <- file.path(getwd(), "res-1555-2010.pdf") # mortality table path
.mortality_file <- pdf_text(file_path)[2]

## START ##

# Problem 1
price_case_11 <- function(x,
                     m,
                     r,
                     .mortality_file,
                     gender = "women",
                     interest) {

  mortality_table <- process_mortality_table(.mortality_file, interest)
  mortality_table <- as.data.frame(mortality_table)

  pos_in_table <- which(mortality_table[, "age"] == x)
  interest_m <- m * ((1 + interest)^(1 / m) - 1)

  if (gender == "women") {
    result1 <- mortality_table$IAx_women[pos_in_table] - mortality_table$Ax_women[pos_in_table]
    result2 <- mortality_table$Ax_women[pos_in_table] + r * result1
  } else if (gender == "men") {
    result1 <- mortality_table$IAx_men[pos_in_table] - mortality_table$Ax_men[pos_in_table]
    result2 <- mortality_table$Ax_men[pos_in_table] + r * result1
  }

  result <- interest / interest_m * result2
  
  # returns
  result
}


#Problem2.5 in workshop
price_case_09 <- function(x,
                     m,
                     r,
                     .mortality_file,
                     gender = "women",
                     interest) {

  mortality_table <- process_mortality_table(.mortality_file, interest)
  mortality_table <- as.data.frame(mortality_table)

  pos_in_table <- which(mortality_table[, "age"] == x)
  interest_m <- m * ((1 + interest)^(1 / m) - 1)

  if (gender == "women") {
    result1 <- price_case_11(x, m, r, .mortality_file,
                        gender, interest)
    result2 <- mortality_table$Ax_women[pos_in_table] * r * (
      (interest - interest_m) / interest_m^2
    )
  } else if (gender == "men") {
    result1 <- price_case_11(x, m, r, .mortality_file,
                        gender, interest)
    result2 <- mortality_table$Ax_men[pos_in_table] * r * (
      (interest - interest_m) / interest_m^2
    )
  }
  
  result <- result1 + result2
  
  result
  
}


# Problem 2 in workshop
price_case_25 <- function(x,
                     m,
                     r,
                     .mortality_file,
                     gender = "women",
                     interest) {
  mortality_table <- process_mortality_table(.mortality_file, interest)
  mortality_table <- as.data.frame(mortality_table)
  
  pos_in_table <- which(mortality_table[, "age"] == x)
  interest_m <- m * ((1 + interest)^(1 / m) - 1)
  result1 <- price_case_09(x, m, r, .mortality_file,
                      gender, interest)
  
  if (gender == "women") {
    result2 <- (r * interest * mortality_table$Ax_women[pos_in_table]) / (m * interest_m)
  } else if (gender == "men") {
    result2 <- (r * interest * mortality_table$Ax_men[pos_in_table]) / (m * interest_m)
  }
  
  result <- result1 + result2
  
  result
}


# NEW finite-term case:
# frac_pay = TRUE, frac_value = TRUE, initial_payment = "a", arithmetic
price_case_09_term <- function(x,
                               m,
                               r,
                               n,
                               .mortality_file,
                               gender = "women",
                               interest) {
  
  terms <- term_Ax_IAx(
    x = x,
    n = n,
    .mortality_file = .mortality_file,
    gender = gender,
    interest = interest
  )
  
  Ax_term  <- terms["Ax_term"]
  IAx_term <- terms["IAx_term"]
  
  interest_m <- m * ((1 + interest)^(1 / m) - 1)
  
  result <- (interest / interest_m) * ((1 - r) * Ax_term + r * IAx_term) +
    r * ((interest - interest_m) / (interest_m^2)) * Ax_term
  
  result
}


Problem4 <- function(x,
                     m,
                     r,
                     n,
                     .mortality_file,
                     gender = "women",
                     interest) {
  
  mortality_table <- process_mortality_table(.mortality_file, interest)
  mortality_table <- as.data.frame(mortality_table)
  
  pos_in_table <- which(mortality_table[, "age"] == x)
  interest_m <- m * ((1 + interest)^(1 / m) - 1)
  
  # Dotal
  
  v <- 1/(1+interest)
  mult <- 1
  if (gender == "women"){
    for (i in 0:x+n-1) {
      mult <- mult * (1 - mortality_table$q_women[pos_in_table + i])
    }
  } else if (gender == "men") {
    for (i in 0:x+n-1) {
      mult <- mult * (1 - mortality_table$q_men[pos_in_table + i])
    }
  }
  Ax1n <- (v^n)*mult
  
  # Life
  
  Axn1 <- price_case_09_term(x, m, r, n, .mortality_file,
                             gender, interest)
  
  
  result <- Ax1n + Axn1
  
  result
  
}

# Problem 5
price_case_15 <- function(x,
                          m,
                          r,
                          .mortality_file,
                          gender = "women",
                          interest) {
  
  # Modified interest rate for the geometric-growth reduction
  interest_star <- (interest - r) / (1 + r)
  
  # Build mortality table using the modified interest rate
  mortality_table <- process_mortality_table(.mortality_file, interest_star)
  mortality_table <- as.data.frame(mortality_table)
  
  # Position of age x in table
  pos_in_table <- which(mortality_table[, "age"] == x)
  
  if (length(pos_in_table) == 0) {
    stop("Age x was not found in mortality_table.")
  }
  
  # Equivalent nominal rate convertible m times per year, at the ORIGINAL rate
  interest_m <- m * ((1 + interest)^(1 / m) - 1)
  
  # Select Ax at the modified rate
  if (gender %in% c("women", "mujer", "female")) {
    Ax_star <- mortality_table$Ax_women[pos_in_table]
    
  } else if (gender %in% c("men", "hombre", "male")) {
    Ax_star <- mortality_table$Ax_men[pos_in_table]
    
  } else {
    stop("`gender` must be one of: women/mujer/female or men/hombre/male.")
  }
  
  # Final result
  result <- (interest / interest_m) * (Ax_star / (1 + r))
  
  # return
  result
}


tst1 <- price_case_11(34, 12, 0.05, .mortality_file, "women", 0.25)
tst1
tst2 <- price_case_25(34, 12, 0.07, .mortality_file, "women", 0.15)
tst2
tst3 <- price_case_09(25, 12, 0.05, .mortality_file, "women", 0.1)
tst3
tst4 <- price_case_09_term(25, 50, 12, 0.05, .mortality_file, "women", 0.1)
tst4

tst5 <- Problem4(25, 12, 0.05, 49, .mortality_file, "women", 0.1)
tst5
