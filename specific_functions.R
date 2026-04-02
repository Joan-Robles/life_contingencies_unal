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

# Problem 2
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


Problem3 <- function(x,
                     m,
                     r,
                     n,
                     .mortality_file,
                     gender = "women",
                     interest) {
  
  mortality_table <- process_mortality_table(.mortality_file, interest)
  mortality_table <- as.data.frame(mortality_table)
  
  pos_in_table1 <- which(mortality_table[, "age"] == x)
  pos_in_table2 <- which(mortality_table[, "age"] == x+n)
  interest_m <- m * ((1 + interest)^(1 / m) - 1)

  
  if (gender == "women") {
    result1 <- (mortality_table$Mx_women[pos_in_table1] 
                - mortality_table$Mx_women[pos_in_table2]
                ) / mortality_table$Dx_women[pos_in_table1]
    result2 <- (mortality_table$Rx_women[pos_in_table1]
                - mortality_table$Rx_women[pos_in_table2]
                - n * mortality_table$Mx_women[pos_in_table2]
                ) / mortality_table$Dx_women[pos_in_table1]
    result3 <- mortality_table$Mx_women[pos_in_table2] / mortality_table$Dx_women[pos_in_table1]
    result4 <- result2 + result3
  } else if (gender == "men"){
    result1 <- (mortality_table$Mx_men[pos_in_table1] 
                - mortality_table$Mx_men[pos_in_table2]
                ) / mortality_table$Dx_men[pos_in_table1]
    result2 <- (mortality_table$Rx_men[pos_in_table1]
                - mortality_table$Rx_men[pos_in_table2]
                - n*mortality_table$Mx_men[pos_in_table2]
                ) / mortality_table$Dx_men[pos_in_table1]
    result3 <- mortality_table$Mx_men[pos_in_table2] / mortality_table$Dx_men[pos_in_table1]
    result4 <- result2 + result3
  }
  
  
  result <- (interest / interest_m) * (
    result1 + r * result4
  )

  result
}

Problem4 <- function(x,
                     m,
                     r,
                     .mortality_file,
                     gender = "women",
                     interest) {
  
  mortality_table <- process_mortality_table(.mortality_file, interest)
  mortality_table <- as.data.frame(mortality_table)
  
  pos_in_table <- which(mortality_table[, "age"] == x)
  interest_m <- m * ((1 + interest)^(1 / m) - 1)
  result1 <- Problem3(x, m, r, .mortality_file,
                      gender, interest)
  
}


tst1 <- price_case_11(34, 12, 0.05, .mortality_file, "women", 0.25)
tst1
tst2 <- price_case_25(34, 12, 0.07, .mortality_file, "women", 0.15)
tst2
tst3 <- price_case_09(25, 12, 0.05, .mortality_file, "women", 0.1)
tst3
tst4 <- Problem3(25, 12, 0.05, 49, .mortality_file, "women", 0.1)
tst4
0.0185161
