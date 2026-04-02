premium_insurance <- function(
    mortality_table,            # mandatory. Matrix
    x,                          # mandatory: issue age
    n,                          # mandatory: term in years
    i,                          # mandatory: annual rate in decimal
    m = 12,                     # number of fractions per year
    frac_pay = TRUE,            # TRUE: pay at end of each fraction; FALSE: end of each year
    frac_value = FALSE,         # TRUE: insured amount grows each fraction; FALSE: grows each year
    growth = "arithmetic",      # "arithmetic", "geometric", or "none"
    r = 0,                      # growth pace
    initial_payment = "a",       # "a" or "b"
    gender = "F"
) {
  
  ##########################################################################
  # 0. GENERAL PURPOSE
  ##########################################################################
  # Template for pricing / premium calculation of a life insurance product.
  #
  # IMPORTANT:
  # - This is only a skeleton.
  # - Validation is included.
  # - Core actuarial pricing logic is left as TODO sections.
  # - A key missing design choice is SEX / mortality column selection:
  #   your mortality table requires both q_men and q_women, but the inputs
  #   currently do not include a "sex" argument. So, in the pricing block
  #   below, you will need to decide how to choose the mortality column.
  ##########################################################################
  
  
  ##########################################################################
  # 1. CHECK MANDATORY INPUTS
  ##########################################################################
  # In R, missing mandatory arguments are usually caught automatically.
  # Still, explicit checks make error messages clearer.
  ##########################################################################
  if (missing(mortality_table)) {
    stop("`mortality_table` is mandatory.")
  }
  if (missing(x)) {
    stop("`x` is mandatory.")
  }
  if (missing(n)) {
    stop("`n` is mandatory.")
  }
  if (missing(i)) {
    stop("`i` is mandatory.")
  }
  
  
  ##########################################################################
  # 2. VALIDATE MORTALITY TABLE
  ##########################################################################
  # Expected:
  # - data.frame or tibble-like object
  # - must include at least columns:
  #   age, q_men, q_women
  ##########################################################################

  required_cols <- c("age", "q_men", "q_women")
  missing_cols <- setdiff(required_cols, colnames(mortality_table))
  
  if (length(missing_cols) > 0) {
    stop(
      "`mortality_table` is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Basic type checks on mortality columns
  if (!is.numeric(mortality_table[, "age"])) {
    stop("Column `age` in `mortality_table` must be numeric/integer.")
  }
  if (!is.numeric(mortality_table[, "q_men"])) {
    stop("Column `q_men` in `mortality_table` must be numeric.")
  }
  if (!is.numeric(mortality_table[, "q_women"])) {
    stop("Column `q_women` in `mortality_table` must be numeric.")
  }
  
  # check mortality probabilities are in [0, 1]
  if (any(mortality_table[, "q_men"] < 0 | mortality_table[, "q_men"] > 1, na.rm = TRUE)) {
    stop("Column `q_men` must contain probabilities in [0, 1].")
  }
  if (any(mortality_table[, "q_women"] < 0 | mortality_table[, "q_women"] > 1, na.rm = TRUE)) {
    stop("Column `q_women` must contain probabilities in [0, 1].")
  }
  
  
  ##########################################################################
  # 3. VALIDATE SCALAR INPUTS
  ##########################################################################
  # x: integer age
  # n: integer duration in years
  # i: numeric annual rate in decimal
  # m: positive integer number of fractions per year
  ##########################################################################
  if (!is.numeric(x) || is.na(x) || x %% 1 != 0) {
    stop("`x` must be a vector of integer values")
  }
  
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n %% 1 != 0 || n <= 0) {
    stop("`n` must be one positive integer value.")
  }
  
  if (!is.numeric(i) || length(i) != 1 || is.na(i)) {
    stop("`i` must be one numeric value (annual rate in decimal).")
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
    stop("`growth` must be one character value: 'arithmetic', 'geometric', or 'none'.")
  }
  
  growth <- tolower(growth)
  allowed_growth <- c("arithmetic", "geometric", "none")
  if (!(growth %in% allowed_growth)) {
    stop("`growth` must be one of: ", paste(allowed_growth, collapse = ", "))
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
  
  
  ##########################################################################
  # 4. PRODUCT CONSISTENCY WARNINGS
  ##########################################################################
  # User requested:
  # Put an alert if frac_value = TRUE and frac_pay = FALSE
  # because that is a weird product.
  ##########################################################################
  if (isTRUE(frac_value) && isFALSE(frac_pay)) {
    warning(
      "You set `frac_value = TRUE` and `frac_pay = FALSE`. ",
      "This means the insured amount grows within the year, ",
      "but payments / valuation are only yearly. Weird product; check if intended."
    )
  }
  
  
  ##########################################################################
  # 5. CHECK THAT AGES NEEDED EXIST IN THE TABLE
  ##########################################################################
  # Assumption:
  # At the terminal age max(mortality_table[, "age"]), death is certain.
  # Therefore, insurance terms extending beyond that age are allowed.
  # Extending the term further does not change the value after the terminal age.
  ##########################################################################
  # Terminal age in the mortality table
  terminal_age <- max(mortality_table[, "age"])
  
  # Only require ages from issue age up to the terminal age
  needed_ages <- min(x):terminal_age
  
  missing_ages <- setdiff(needed_ages, mortality_table[, "age"])
  
  if (length(missing_ages) > 0) {
    stop(
      "The mortality table does not contain all required ages from issue age to terminal age. ",
      "Missing ages: ", paste(missing_ages, collapse = ", ")
    )
  }
  
  ##########################################################################
  # 6. DISCOUNT SETUP
  ##########################################################################
  # 30/360 convention:
  # - 1 month = 1/12 year
  # - more generally, each fraction = 1/m year
  #
  # These objects are useful later for fractional timing.
  ##########################################################################
  dt <- 1 / m                    # fraction of year
  times_frac <- seq(dt, n, by = dt)   # end of each fraction
  times_year <- seq(1, n, by = 1)     # end of each year
  
  # Example discount factors:
  # If you keep i as effective annual:
  # v(t) = (1 + i)^(-t)
  
  # Placeholder vectors:
  df_frac <- (1 + i)^(-times_frac)
  df_year <- (1 + i)^(-times_year)
  
  
  ##########################################################################
  # 7. PAYMENT / BENEFIT GROWTH PATTERN
  ##########################################################################
  # This block defines the insured amount pattern, not the final premium yet.
  #
  # INTERPRETATION REQUESTED:
  #
  # Arithmetic:
  #   initial_payment = "a":
  #     1, 1 + 1/m, 1 + 2/m, ...
  #
  #   initial_payment = "b":
  #     1 + 1/m, 1 + 2/m, 1 + 3/m, ...
  #
  # Geometric (natural analogous interpretation):
  #   initial_payment = "a":
  #     1, (1 + r)^(1/m), (1 + r)^(2/m), ...
  #
  #   initial_payment = "b":
  #     (1 + r)^(1/m), (1 + r)^(2/m), (1 + r)^(3/m), ...
  #
  # None:
  #   always 1
  #
  # NOTE:
  # - You may later want to decide whether "r" is annual growth rate
  #   or per-step growth rate. The formulas below interpret r as an
  #   ANNUAL geometric growth rate.
  ##########################################################################
  
  # Time index for fractional pattern:
  k_frac_a <- 0:(length(times_frac) - 1)
  k_frac_b <- 1:length(times_frac)
  
  # Time index for annual pattern:
  k_year_a <- 0:(length(times_year) - 1)
  k_year_b <- 1:length(times_year)
  
  # Initialize placeholders
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
  # 8. SELECT THE RELEVANT BENEFIT SCHEDULE
  ##########################################################################
  # If frac_value = TRUE, benefit schedule changes each fraction.
  # If frac_value = FALSE, benefit schedule changes yearly.
  #
  # IMPORTANT:
  # Even if the benefit changes yearly, you may still evaluate payments or
  # death timing fractionally. That alignment is part of the pricing logic
  # to be defined later.
  ##########################################################################
  benefit_schedule <- if (isTRUE(frac_value)) insured_amount_frac else insured_amount_year
  
  
  ##########################################################################
  # 9. MORTALITY EXTRACTION PLACEHOLDER
  ##########################################################################
  # TODO:
  # Decide how to choose between q_men and q_women.
  #
  # Since no `sex` input is present yet, here are some possibilities:
  # - add a new argument `sex = c("men", "women")`
  # - pass the desired q-column directly as an argument
  # - create separate functions for men and women
  #
  # For now, we only leave a placeholder.
  ##########################################################################
  
  # Example placeholder:
  # qx <- mortality_table$q_men[mortality_table$age %in% x:(x + n)]
  #
  # Better future design:
  # sex <- match.arg(sex)
  # q_col <- if (sex == "men") "q_men" else "q_women"
  # qx <- mortality_table[[q_col]][match(x:(x + n), mortality_table$age)]
  
  
  ##########################################################################
  # 10. PRICING LOGIC PLACEHOLDER
  ##########################################################################
  # TODO:
  # Here you will build the actuarial present value / premium formula.
  #
  # Typical steps:
  # 1. Build survival / death probabilities from the selected qx.
  # 2. Decide whether deaths / claims are recognized yearly or fractionally.
  # 3. Match benefit timing with payment timing:
  #    - frac_pay = TRUE  -> benefit/premium cash flows on fractional grid
  #    - frac_pay = FALSE -> benefit/premium cash flows on yearly grid
  # 4. Apply discount factors.
  # 5. Sum expected present values.
  # 6. Return net premium (or gross premium later if expenses are added).
  #
  # This template returns a structured object for debugging and extension.
  ##########################################################################
  
  premium_value <- NA_real_
  
  
  ##########################################################################
  # 11. RETURN
  ##########################################################################
  # Return a list for now, since this is a template. That makes debugging
  # easier while you build the actuarial core.
  ##########################################################################
  return(list(
    premium = premium_value,
    inputs = list(
      x = x,
      n = n,
      i = i,
      m = m,
      frac_pay = frac_pay,
      frac_value = frac_value,
      growth = growth,
      r = r,
      initial_payment = initial_payment
    ),
    grids = list(
      dt = dt,
      times_frac = times_frac,
      times_year = times_year
    ),
    discount = list(
      df_frac = df_frac,
      df_year = df_year
    ),
    benefit_schedule = benefit_schedule
  ))
}