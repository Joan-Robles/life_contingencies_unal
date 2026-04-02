
process_mortality_table <- function(text_content, save_copy = TRUE) {
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
  
  # Try to save a copy as RDS if requested, without breaking the function
  if (isTRUE(save_copy)) {
    tryCatch(
      {
        saveRDS(mortality_matrix, file = "mortality_matrix.rds")
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