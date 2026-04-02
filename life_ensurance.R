
get_premium_insurance <- function(
    mortality_table,            # mandatory: mortality matrix
    x,                          # mandatory: issue age(s), integer vector
    n,                          # mandatory: term in years, integer or Inf
    i,                          # mandatory: annual effective rate in decimal
    m = 12,                     # number of fractions per year
    frac_pay = TRUE,            # TRUE: payment at end of fraction; FALSE: end of year
    frac_value = FALSE,         # TRUE: insured amount grows each fraction; FALSE: each year
    growth = "arithmetic",      # "arithmetic", "geometric", or "none"
    r = 0,                      # growth pace
    initial_payment = "a",      # "a" or "b"
    gender = "female"           # scalar or vector
) {
  
  ##########################################################################
  # 1. CHECK MANDATORY INPUTS
  ##########################################################################
  if (missing(mortality_table)) stop("`mortality_table` is mandatory.")
  if (missing(x)) stop("`x` is mandatory.")
  if (missing(n)) stop("`n` is mandatory.")
  if (missing(i)) stop("`i` is mandatory.")
  
  
  ##########################################################################
  # 2. VALIDATE MORTALITY TABLE
  ##########################################################################
  if (!is.matrix(mortality_table)) {
    stop("`mortality_table` must be a matrix.")
  }
  
  required_cols <- c("age", "q_men", "q_women")
  missing_cols <- setdiff(required_cols, colnames(mortality_table))
  
  if (length(missing_cols) > 0) {
    stop(
      "`mortality_table` is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Basic column checks
  if (!is.numeric(mortality_table[, "age"])) {
    stop("Column `age` in `mortality_table` must be numeric/integer.")
  }
  if (!is.numeric(mortality_table[, "q_men"])) {
    stop("Column `q_men` in `mortality_table` must be numeric.")
  }
  if (!is.numeric(mortality_table[, "q_women"])) {
    stop("Column `q_women` in `mortality_table` must be numeric.")
  }
  
  # Mortality probabilities must be in [0, 1]
  if (any(mortality_table[, "q_men"] < 0 | mortality_table[, "q_men"] > 1, na.rm = TRUE)) {
    stop("Column `q_men` must contain probabilities in [0, 1].")
  }
  if (any(mortality_table[, "q_women"] < 0 | mortality_table[, "q_women"] > 1, na.rm = TRUE)) {
    stop("Column `q_women` must contain probabilities in [0, 1].")
  }
  
  
  ##########################################################################
  # 3. VALIDATE INPUTS
  ##########################################################################
  
  # x can now be a vector of integer ages
  if (!is.numeric(x) || any(is.na(x)) || any(x %% 1 != 0)) {
    stop("`x` must be a vector of integer ages.")
  }
  
  # n can be positive integer or Inf
  if (!is.numeric(n) || length(n) != 1 || is.na(n)) {
    stop("`n` must be one positive integer or `Inf`.")
  }
  if (!(is.infinite(n) || (n %% 1 == 0 && n > 0))) {
    stop("`n` must be one positive integer or `Inf`.")
  }
  
  if (!is.numeric(i) || length(i) != 1 || is.na(i)) {
    stop("`i` must be one numeric value (annual effective rate in decimal).")
  }
  
  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m %% 1 != 0 || m <= 0) {
    stop("`m` must be one positive integer value.")
  }
  
  if (!is.logical(frac_pay) || length(frac_pay) != 1 || is.na(frac_pay)) {
    stop("`frac_pay` must be TRUE or FALSE.")
  }
  
  if (!is.logical(frac_value) || length(frac_value) != 1 || is.na(frac_value)) {
    stop("`frac_value` must be TRUE or FALSE.")
  }
  
  if (!is.character(growth) || length(growth) != 1) {
    stop("`growth` must be one of 'arithmetic', 'geometric', or 'none'.")
  }
  
  growth <- tolower(growth)
  if (!(growth %in% c("arithmetic", "geometric", "none"))) {
    stop("`growth` must be one of 'arithmetic', 'geometric', or 'none'.")
  }
  
  if (!is.numeric(r) || length(r) != 1 || is.na(r)) {
    stop("`r` must be one numeric value.")
  }
  
  if (!is.character(initial_payment) || length(initial_payment) != 1) {
    stop("`initial_payment` must be 'a' or 'b'.")
  }
  initial_payment <- tolower(initial_payment)
  if (!(initial_payment %in% c("a", "b"))) {
    stop("`initial_payment` must be either 'a' or 'b'.")
  }
  
  # gender can be scalar or same length as x
  if (!is.character(gender)) {
    stop("`gender` must be a character vector.")
  }
  if (!(length(gender) %in% c(1, length(x)))) {
    stop("`gender` must have length 1 or the same length as `x`.")
  }
  if (length(gender) == 1) {
    gender <- rep(gender, length(x))
  }
  
  
  ##########################################################################
  # 4. PRODUCT WARNING
  ##########################################################################
  if (isTRUE(frac_value) && isFALSE(frac_pay)) {
    warning(
      "You set `frac_value = TRUE` and `frac_pay = FALSE`. ",
      "This is an unusual product: the insured value grows within the year, ",
      "but payment is only at year-end."
    )
  }
  
  
  ##########################################################################
  # 5. AGE SUPPORT + CHOPPING OF n
  ##########################################################################
  # Assumption:
  # - The table goes up to a terminal age.
  # - Beyond that age, pricing does not change anymore because death
  #   probabilities are zero-padded in the matrix.
  #
  # Requested rule:
  # - If n > max_age or n = Inf, chop it to max_age.
  ##########################################################################
  age_vec <- mortality_table[, "age"]
  min_age <- min(age_vec)
  terminal_age <- max(age_vec)
  
  if (any(x < min_age)) {
    stop("Some values in `x` are below the minimum age in the mortality table.")
  }
  if (any(x > terminal_age)) {
    stop("Some values in `x` are above the terminal age in the mortality table.")
  }
  
  # Only require mortality data from min(x) to terminal_age
  needed_ages <- min(x):terminal_age
  missing_ages <- setdiff(needed_ages, age_vec)
  
  if (length(missing_ages) > 0) {
    stop(
      "The mortality table does not contain all required ages from min(x) to terminal age. ",
      "Missing ages: ", paste(missing_ages, collapse = ", ")
    )
  }
  
  # Computational horizon:
  # if n is too large (or infinite), chop to terminal_age
  n_eff <- if (is.infinite(n)) terminal_age else min(as.integer(n), terminal_age)
  
  
  ##########################################################################
  # 6. DISCOUNT SETUP
  ##########################################################################
  dt <- 1 / m
  
  # Build both time grids using the chopped horizon
  times_frac <- seq(dt, n_eff, by = dt)
  times_year <- seq(1, n_eff, by = 1)
  
  # Effective annual discounting
  df_frac <- (1 + i)^(-times_frac)
  df_year <- (1 + i)^(-times_year)
  
  
  ##########################################################################
  # 7. INSURED AMOUNT / BENEFIT GROWTH PATTERN
  ##########################################################################
  
  # Fractional indices
  k_frac_a <- 0:(length(times_frac) - 1)
  k_frac_b <- 1:length(times_frac)
  
  # Yearly indices
  k_year_a <- 0:(length(times_year) - 1)
  k_year_b <- 1:length(times_year)
  
  insured_amount_frac <- NULL
  insured_amount_year <- NULL
  
  if (growth == "none") {
    insured_amount_frac <- rep(1, length(times_frac))
    insured_amount_year <- rep(1, length(times_year))
    
  } else if (growth == "arithmetic") {
    
    if (initial_payment == "a") {
      insured_amount_frac <- 1 + r * (k_frac_a / m)
      insured_amount_year <- 1 + r * k_year_a
    } else {
      insured_amount_frac <- 1 + r * (k_frac_b / m)
      insured_amount_year <- 1 + r * k_year_b
    }
    
  } else if (growth == "geometric") {
    
    if (initial_payment == "a") {
      insured_amount_frac <- (1 + r)^(k_frac_a / m)
      insured_amount_year <- (1 + r)^(k_year_a)
    } else {
      insured_amount_frac <- (1 + r)^(k_frac_b / m)
      insured_amount_year <- (1 + r)^(k_year_b)
    }
  }
  
  
  ##########################################################################
  # 8. SELECT BENEFIT SCHEDULE USED IN PRICING
  ##########################################################################
  # Four cases:
  # 1. frac_pay = TRUE,  frac_value = TRUE
  #    -> fractional payment times and fractional benefit growth
  #
  # 2. frac_pay = TRUE,  frac_value = FALSE
  #    -> fractional payment times, but benefit only changes yearly
  #       so we repeat each yearly amount m times
  #
  # 3. frac_pay = FALSE, frac_value = TRUE
  #    -> weird product, but if user insists, we take the end-of-year
  #       value from the fractional schedule
  #
  # 4. frac_pay = FALSE, frac_value = FALSE
  #    -> standard yearly pricing
  ##########################################################################
  if (isTRUE(frac_pay) && isTRUE(frac_value)) {
    benefit_schedule <- insured_amount_frac
    discount_factors <- df_frac
    
  } else if (isTRUE(frac_pay) && isFALSE(frac_value)) {
    benefit_schedule <- rep(insured_amount_year, each = m)
    discount_factors <- df_frac
    
  } else if (isFALSE(frac_pay) && isTRUE(frac_value)) {
    benefit_schedule <- insured_amount_frac[seq(m, length(insured_amount_frac), by = m)]
    discount_factors <- df_year
    
  } else {
    benefit_schedule <- insured_amount_year
    discount_factors <- df_year
  }
  
  
  ##########################################################################
  # 9. PROBABILITY OF DEATH MATRIX
  ##########################################################################
  # Annual death probabilities:
  # - one column per insured age in x
  # - one row per policy year, up to n_eff
  # - if a life reaches terminal age earlier than n_eff, fill the remaining
  #   rows with zeros so all columns have equal length
  #
  # Fractional case:
  # - assume deaths are uniformly distributed within each year
  # - split each annual death probability equally into m pieces
  ##########################################################################
  
  # Annual death probability matrix: n_eff x length(x)
  prob_death_year <- matrix(
    0,
    nrow = n_eff,
    ncol = length(x)
  )
  
  for (j in seq_along(x)) {
    
    # Annual death probabilities from age x[j] onward
    dp_j <- death_probs_from_x(
      mortality_table = mortality_table,
      gender = gender[j],
      x = x[j]
    )
    
    # Keep only as many rows as the global horizon allows
    take_j <- min(length(dp_j), n_eff)
    prob_death_year[1:take_j, j] <- dp_j[1:take_j]
    
    # Remaining rows stay at zero by construction
  }
  
  # Assign names
  colnames(prob_death_year) <- paste0("x_", x)
  rownames(prob_death_year) <- paste0("year_", seq_len(n_eff))
  
  # If payment is fractional, split each annual probability uniformly
  if (isTRUE(frac_pay)) {
    prob_death <- prob_death_year[rep(seq_len(n_eff), each = m), , drop = FALSE] / m
    rownames(prob_death) <- paste0("frac_", seq_len(nrow(prob_death)))
  } else {
    prob_death <- prob_death_year
  }
  
  
  ##########################################################################
  # 10. PRICING
  ##########################################################################
  # Pure premium / EPV of death benefit:
  #
  # premium = sum_t [ v_t * benefit_t * Pr(death at t) ]
  #
  # Since x can be a vector:
  # - prob_death is a matrix
  # - discount_factors and benefit_schedule are vectors
  # - we build a cash-flow weight vector and multiply it columnwise
  ##########################################################################
  
  # One pricing weight per row/time
  pricing_weights <- discount_factors * benefit_schedule
  
  # Columnwise premium values, one for each insured age
  premium_value <- colSums(prob_death * pricing_weights)
  
  # Nice names
  names(premium_value) <- paste0("x_", x)
  
  # If x has length 1, return a scalar instead of a named vector
  if (length(premium_value) == 1) {
    premium_value <- as.numeric(premium_value)
  }
  
  
  ##########################################################################
  # 11. RETURN
  ##########################################################################
  result <- (list(
    premium = premium_value,
    discount_factors = discount_factors,
    benefit_schedule = benefit_schedule,
    prob_death_year = prob_death_year,
    prob_death = prob_death
  ))
  
  result #return
}

A <- get_premium_insurance(mortality_table = mortality_table,
                          x = 34,
                          n = Inf,
                          i = 0.1,
                          r = 0.05,
                          frac_pay = TRUE,
                          frac_value = TRUE,
                          initial_payment = "a")

A$premium

# Update table cases
