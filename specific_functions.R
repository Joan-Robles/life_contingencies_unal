

Problem1 <- function(x,
                     m,
                     r,
                     mortality_table,
                     gender = "women",
                     interest) {
  
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

Problem2 <- function(x,
                     m,
                     r,
                     mortality_table,
                     gender = "women",
                     interest) {
  pos_in_table <- which(mortality_table[, "age"] == x)
  interest_m <- m * ((1 + interest)^(1 / m) - 1)
  
  if (gender == "women") {
    result1 <- Problem1(x, m, r, mortality_table,
                        gender, interest)
    result2 <- mortality_table$Ax_women[pos_in_table] * r * (
      (interest_m - interest) / interest_m^2
    )
  } else if (gender == "men") {
    result1 <- Problem1(x, m, r, mortality_table,
                        gender, interest)
    result2 <- mortality_table$Ax_men[pos_in_table] * r * (
      (interest_m - interest) / interest_m^2
    )
  }
  result <- result1 + result2
  return(result)
}

tst1 <- Problem1(34, 12, 0.05, mortality_table, "women", 0.1)
tst1
tst2 <- Problem2(34, 12, 0.05, mortality_table, "women", 0.1)
tst2

