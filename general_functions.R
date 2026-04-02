
process_mortality_table <- function(text_content, interest, save_copy = TRUE) {
  # Split text into lines
  lines <- unlist(strsplit(text_content, "\n"))
  
  # Find lines that appear to contain table data, based on the presence of numbers
  table_lines <- grep("^[[:space:]]*[0-9]{2,}", lines, value = TRUE)
  
  # Define a function to process each line and extract numeric data
  process_line <- function(line) {
    # Remove commas from numbers
    line <- gsub(",", "", line)
    # Split the line into component values
    values <- unlist(strsplit(line, "[[:space:]]+"))
    # Remove empty strings
    values <- values[values != ""]
    # Convert values to numeric
    as.numeric(values)
  }
  
  # Apply the function to each line to get a list of numeric vectors
  numeric_lines <- lapply(table_lines, process_line)
  
  # Combine numeric vectors into a matrix
  mortality_matrix <- do.call(rbind, numeric_lines)
  
  # keep what matters
  mortality_matrix <- mortality_matrix[, c(1, 4, 9)]
  colnames(mortality_matrix) <- c("age", "q_men", "q_women")
  
  # A: I feel more confortable with dataframes
  mortality_table <- as.data.frame(mortality_matrix)
  
  # Parameters
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
  
  mortality_table$Rx_men <- NA
  mortality_table$Rx_women <- NA
  sum_men = 0
  sum_women = 0
  for (i in 1:nrow(mortality_table)) {
    sum_men <- sum_men + mortality_table$Mx_men[nrow(mortality_table)+1-i]
    sum_women <- sum_women + mortality_table$Mx_women[nrow(mortality_table)+1-i]
    mortality_table$Rx_men[nrow(mortality_table)+1-i] <- sum_men
    mortality_table$Rx_women[nrow(mortality_table)+1-i] <- sum_women
  }
  
  mortality_table$Ax_men <- mortality_table$Mx_men/mortality_table$Dx_men
  mortality_table$Ax_women <- mortality_table$Mx_women/mortality_table$Dx_women
  
  mortality_table$IAx_men <- mortality_table$Rx_men/mortality_table$Dx_men
  mortality_table$IAx_women <- mortality_table$Rx_women/mortality_table$Dx_women
  
  mortality_matrix <- as.matrix(mortality_table)
  
  # Try to save a copy as RDS if requested, without breaking the function
  if (isTRUE(save_copy)) {
    tryCatch(
      {
        saveRDS(mortality_matrix, file = "mortality_table.rds")
      },
      error = function(e) {
        message("Could not save RDS copy: ", e$message)
      }
    )
  }
  
  # Return the matrix
  mortality_matrix
}


get_discount_factors <- function(rate, time, type = "effective") {
  
  # Convert months to years (30/360 → 1 month = 1/12)
  t <- time / 12
  
  if (type == "effective") {
    df <- (1 + rate)^(-t)
    
  } else if (type == "continuous") {
    df <- exp(-rate * t)
    
  } else if (type == "nominal") {
    # assume rate is nominal annual compounded monthly
    df <- (1 + rate/12)^(-time)
    
  } else {
    stop("type must be 'effective', 'continuous', or 'nominal'")
  }
  
  return(df)
}


Ax1n <- function(age, n, mortality_matrix, gender = "M") {
  
  # A: I feel more confortable with dataframes
  mortality_table <- as.data.frame(mortality_matrix)
  
  if (gender == "M") {
    result <- mortality_table$Mx_women[age] - mortality_table$Mx_women[age + n]
    result <- result/mortality_table$Dx_women[age]
  } else {
    result <- mortality_table$Mx_men[age] - mortality_table$Mx_men[age + n]
    result <- result/mortality_table$Dx_men[age]
  }
  
  return(result)
}
