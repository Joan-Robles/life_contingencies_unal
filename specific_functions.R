

EnteroFraccionarioCrecimientoAritmetico <- function(x,
                                                    m,
                                                    r,
                                                    mortality_table,
                                                    gender = "M",
                                                    interest) {
  
  pos_in_table <- which(mortality_table[, "age"] == x)
  
  interest_m <- m * ((1 + interest)^(1 / m) - 1)
  if (gender == "women") {
    result1 <- mortality_table$IAx_women[pos_in_table] - mortality_table$Ax_women[x -
                                                                              14]
    result2 <- mortality_table$Ax_women[pos_in_table] + r * result1
  } else if (gender == "men") {
    result1 <- mortality_table$IAx_men[pos_in_table] - mortality_table$Ax_men[x -
                                                                          14]
    result2 <- mortality_table$Ax_men[pos_in_table] + r * result1
  }
  
  result <- interest / interest_m * result2
  
  # returns
  result
}
