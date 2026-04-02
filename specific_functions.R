
EnteroFraccionarioCrecimientoAritmetico <- function(x, m, r, mortality_table, 
                                                    gender = "M", interest) {
  interest_m <- m*((1+interest)^(1/m) -1)
  if (gender == "M") {
    result1 <- mortality_table$IAx_women[x-14] - mortality_table$Ax_women[x-14]
    result2 <- mortality_table$Ax_women[x-14] + r*result1
  } else {
    result1 <- mortality_table$IAx_men[x-14] - mortality_table$Ax_men[x-14]
    result2 <- mortality_table$Ax_men[x-14] + r*result1
  }
  result <- interest/interest_m * result2
  return(result)
}

