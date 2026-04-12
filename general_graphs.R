
heatmap_xyz <- function(x, y, z, 
                        xlab = "x", 
                        ylab = "y", 
                        main = "Heatmap: x vs. y (color = z)",
                        interpol = TRUE) {
  # Check for and load the lattice package
  if (!requireNamespace("lattice", quietly = TRUE)) {
    stop("The lattice package is required. Please install it using install.packages('lattice').")
  }
  library(lattice)
  
  if (interpol) {
    # Check for and load the akima package for interpolation
    if (!requireNamespace("akima", quietly = TRUE)) {
      stop("The akima package is required for interpolation. Please install it using install.packages('akima').")
    }
    library(akima)
    
    # Remove NA values for interpolation
    valid_idx <- !is.na(z)
    x_valid <- x[valid_idx]
    y_valid <- y[valid_idx]
    z_valid <- z[valid_idx]
    
    # Scale x and y to the [0, 1] interval to avoid scale issues
    x_min <- min(x_valid, na.rm = TRUE)
    x_max <- max(x_valid, na.rm = TRUE)
    y_min <- min(y_valid, na.rm = TRUE)
    y_max <- max(y_valid, na.rm = TRUE)
    
    x_scaled <- (x_valid - x_min) / (x_max - x_min)
    y_scaled <- (y_valid - y_min) / (y_max - y_min)
    
    # Define a grid on the scaled domain
    xo <- seq(0, 1, length.out = length(unique(x_scaled)))
    yo <- seq(0, 1, length.out = length(unique(y_scaled)))
    
    # Perform interpolation on the scaled data using duplicate = "mean" to handle duplicate points
    interp_result <- tryCatch({
      akima::interp(x = x_scaled, y = y_scaled, z = z_valid, 
                    xo = xo, yo = yo, linear = TRUE, extrap = FALSE,
                    duplicate = "mean")
    }, error = function(e) {
      stop("Interpolation error: ", e$message)
    })
    
    # Unscale the grid back to original coordinates
    new_x <- interp_result$x * (x_max - x_min) + x_min
    new_y <- interp_result$y * (y_max - y_min) + y_min
    
    # Create a complete data frame from the interpolated grid.
    df <- data.frame(expand.grid(x = new_x, y = new_y),
                     z = as.vector(interp_result$z))
  } else {
    # Otherwise, simply create a data frame from the provided values.
    df <- data.frame(x = x, y = y, z = z)
  }
  
  # Create the heatmap using levelplot
  levelplot(z ~ x * y, data = df,
            # Define a color ramp from green (low values) to red (high values)
            col.regions = colorRampPalette(c("green", "red"))(100),
            # Custom panel function to overlay NA values (if any remain)
            panel = function(x, y, z, ...) {
              panel.levelplot(x, y, z, ...)
              na.idx <- is.na(z)
              if (any(na.idx)) {
                panel.points(x[na.idx], y[na.idx], pch = 15, col = "grey50", cex = 1.5)
              }
            },
            xlab = xlab,
            ylab = ylab,
            main = main)
}


plot_benefit_comparison <- function(n_values, inflation, m) {
  horizon_years <- max(n_values)
  r <- inflation
  time_grid <- seq(1 / m, horizon_years, by = 1 / m)
  
  benefit_p2 <- 1 + r * (seq_along(time_grid) / m)
  benefit_p5 <- rep((1 + r)^(0:(horizon_years - 1)), each = m)
  
  df_long <- rbind(
    data.frame(
      time = time_grid,
      value = benefit_p2,
      type = "Punto 2: aritmético intra-anual"
    ),
    data.frame(
      time = time_grid,
      value = benefit_p5,
      type = "Punto 5: geométrico anual"
    )
  )
  
  ggplot(df_long, aes(x = time, y = value, linetype = type)) +
    geom_line(linewidth = 1) +
    labs(
      title = "Comparación del crecimiento del valor asegurado",
      x = "Tiempo (años)",
      y = "Valor asegurado",
      linetype = ""
    ) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme_minimal() +
    theme(legend.position = "top")
}


plot_premium_comparison <- function(n_values, age, mortality_file, i, r) {
  premium_p2 <- sapply(n_values, function(n) {
    get_premium_insurance(
      mortality_file = mortality_file,
      x = age,
      n = n,
      i = i,
      r = r,
      frac_pay = TRUE,
      frac_value = TRUE,
      initial_payment = "b"
    )$premium
  })
  
  premium_p5 <- sapply(n_values, function(n) {
    get_premium_insurance(
      mortality_file = mortality_file,
      x = age,
      n = n,
      i = i,
      r = r,
      frac_pay = TRUE,
      frac_value = FALSE,
      initial_payment = "a",
      growth = "geometric"
    )$premium
  })
  
  df_primas <- rbind(
    data.frame(
      n = n_values,
      premium = premium_p2,
      type = "Punto 2: aritmético intra-anual"
    ),
    data.frame(
      n = n_values,
      premium = premium_p5,
      type = "Punto 5: geométrico anual"
    )
  )
  
  ggplot(df_primas, aes(x = n, y = premium, linetype = type)) +
    geom_line(linewidth = 1) +
    labs(
      title = "Comparación del valor de la prima única",
      x = "Años asegurados (n)",
      y = "Prima única",
      linetype = ""
    ) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_minimal() +
    theme(legend.position = "top")
}