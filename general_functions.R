
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


death_probs_from_x <- function(mortality_table, gender, x) {
  
  # Extract ages
  age_vec <- mortality_table[, "age"]
  
  # Select mortality column by gender
  gender <- tolower(gender)
  if (gender %in% c("male", "hombre", "men")) {
    qx_full <- mortality_table[, "q_men"]
  } else if (gender %in% c("female", "mujer", "women")) {
    qx_full <- mortality_table[, "q_women"]
  } else {
    stop("`gender` must be male/hombre/men or female/mujer/women.")
  }
  
  # Terminal age in the table
  terminal_age <- max(age_vec)
  
  # Ages needed from current age x to terminal age
  needed_ages <- x:terminal_age
  idx <- match(needed_ages, age_vec)
  
  if (any(is.na(idx))) {
    stop("Missing ages between x and terminal age.")
  }
  
  # Mortality vector from age x onward
  qx <- qx_full[idx]
  
  # Survival probabilities p_x = 1 - q_x
  px <- 1 - qx
  
  # k-year survival probabilities: _k p_x for k = 0,1,2,...
  surv_probs <- c(1, cumprod(px[-length(px)]))
  
  # Probability of dying in each year:
  # q_x, p_x q_{x+1}, p_x p_{x+1} q_{x+2}, ...
  death_probs <- surv_probs * qx
  
  return(death_probs)
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

# Helper: compute A_{x:n} and IA_{x:n} directly from the mortality table
term_Ax_IAx <- function(x,
                        n,
                        .mortality_file,
                        gender = "women",
                        interest) {
  
  mortality_table <- process_mortality_table(.mortality_file, interest)
  mortality_table <- as.data.frame(mortality_table)
  
  # annual death probabilities from age x onward:
  # q_x, p_x q_{x+1}, p_x p_{x+1} q_{x+2}, ...
  death_probs <- death_probs_from_x(
    mortality_table = as.matrix(mortality_table),
    gender = gender,
    x = x
  )
  
  # chop n if needed
  n_eff <- if (is.infinite(n)) length(death_probs) else min(n, length(death_probs))
  
  k <- seq_len(n_eff)
  v <- 1 / (1 + interest)
  
  Ax_term  <- sum((v ^ k) * death_probs[1:n_eff])
  IAx_term <- sum(k * (v ^ k) * death_probs[1:n_eff])
  
  c(Ax_term = Ax_term, IAx_term = IAx_term)
}
