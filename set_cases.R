# Create a table with cases for get_premium_insurance and try functions

# Build the full case table (deterministic order)
get_case_table <- function() {
  
  cases <- expand.grid(
    frac_pay = c(TRUE, FALSE),
    frac_value = c(TRUE, FALSE),
    growth = c("arithmetic", "geometric"),
    r_case = c("zero", "nonzero"),
    initial_payment = c("a", "b"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  cases <- cases[, c("frac_pay", "frac_value", "growth", "r_case", "initial_payment")]
  cases$index <- seq_len(nrow(cases))
  
  # Put index first
  cases <- cases[, c("index", "frac_pay", "frac_value", "growth", "r_case", "initial_payment")]
  
  rownames(cases) <- NULL
  cases
}


# Return the index of a given case
insurance_case_index <- function(
    frac_pay = TRUE,
    frac_value = FALSE,
    growth,
    r,
    initial_payment = "a"
) {
  
  # Basic checks
  if (!is.logical(frac_pay) || length(frac_pay) != 1 || is.na(frac_pay)) {
    stop("`frac_pay` must be one TRUE/FALSE value.")
  }
  
  if (!is.logical(frac_value) || length(frac_value) != 1 || is.na(frac_value)) {
    stop("`frac_value` must be one TRUE/FALSE value.")
  }
  
  if (!is.character(growth) || length(growth) != 1) {
    stop("`growth` must be one character value.")
  }
  growth <- tolower(growth)
  if (!(growth %in% c("arithmetic", "geometric"))) {
    stop("`growth` must be 'arithmetic' or 'geometric'.")
  }
  
  if (!is.numeric(r) || length(r) != 1 || is.na(r)) {
    stop("`r` must be one numeric value.")
  }
  
  if (!is.character(initial_payment) || length(initial_payment) != 1) {
    stop("`initial_payment` must be one character value.")
  }
  initial_payment <- tolower(initial_payment)
  if (!(initial_payment %in% c("a", "b"))) {
    stop("`initial_payment` must be 'a' or 'b'.")
  }
  
  # Collapse r into the binary case requested
  r_case <- if (r == 0) "zero" else "nonzero"
  
  # Get table and locate row
  tbl <- insurance_case_table()
  
  idx <- tbl$index[
    tbl$frac_pay == frac_pay &
      tbl$frac_value == frac_value &
      tbl$growth == growth &
      tbl$r_case == r_case &
      tbl$initial_payment == initial_payment
  ]
  
  if (length(idx) != 1) {
    stop("Could not identify a unique case.")
  }
  
  idx
}

# save table
case_table <- get_case_table()
saveRDS(case_table, "case_table.rds")
