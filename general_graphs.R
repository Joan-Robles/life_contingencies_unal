# Funcion para graficar el Fanchart de las curvas (grafico con bandas de certeza)
# Creada por QUantil SAS
# mat: Matriz de datos de las curvas simuladas
# var: 
# titulos: Titulo del grafico
# ylab: Nombre del eje y
# comparar: 
# promedio: TRUE grafica el promedio de las simulaciones, FALSE grafica
# la mediana de las simulaciones
fanChartCurvas <- function(mat,
                           var = NULL,
                           titulos = NULL,
                           ylab = NULL,
                           comparar = NULL,
                           promedio = TRUE) {

  mat <- t(na.omit(mat))
  if (promedio) {
    medias <- colMeans(mat)
    data <- data.frame(media = medias)
  } else {
    medianas <- apply(mat, 2, median)
    data <- data.frame(media = medianas)
  }

  upper_seq <- seq(70, 95, by = 5) / 100
  data <- cbind(data, t(apply(mat, 2, quantile, probs = c(1 - upper_seq, upper_seq))))
  colnames(data) <- c(
    'Promedio', 'Min70', 'Min75', 'Min80', 'Min85', 'Min90',
    'Min95', 'Max70', 'Max75', 'Max80', 'Max85', 'Max90',
    'Max95'
  )
  if (!is.null(comparar)) {
    data <- cbind(data, as.numeric(comparar))
    colnames(data)[ncol(data)] <- 'Comparar'
  }
  rownames(data) <- colnames(mat)

  plot <- if (promedio == TRUE) {
    data %>%
      plotly::plot_ly(x = as.numeric(rownames(data))) %>% ## as. numeric cuando graficamos curvas
      plotly::add_ribbons(
        ymin = ~Min95, ymax = ~Max95,
        line = list(color = adjustcolor('firebrick', alpha.f = 0.05)),
        fillcolor = adjustcolor('firebrick', alpha.f = 0.05),
        name = '95%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min90, ymax = ~Max90,
        line = list(color = adjustcolor('red', alpha.f = 0.1)),
        fillcolor = adjustcolor('red', alpha.f = 0.1),
        name = '90%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min85, ymax = ~Max85,
        line = list(color = adjustcolor('orange', alpha.f = 0.15)),
        fillcolor = adjustcolor('orange', alpha.f = 0.15),
        name = '85%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min80, ymax = ~Max80,
        line = list(color = adjustcolor('gold', alpha.f = 0.2)),
        fillcolor = adjustcolor('gold', alpha.f = 0.2),
        name = '80%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min75, ymax = ~Max75,
        line = list(color = adjustcolor('green', alpha.f = 0.25)),
        fillcolor = adjustcolor('green', alpha.f = 0.25),
        name = '75%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min70, ymax = ~Max70,
        line = list(color = adjustcolor('green4', alpha.f = 0.3)),
        fillcolor = adjustcolor('green4', alpha.f = 0.3),
        name = '70%'
      ) %>%
      plotly::add_trace(
        y = ~Promedio, name = 'Mean', type = 'scatter', mode = 'lines',
        line = list(color = adjustcolor('black'))
      ) %>%
      plotly::layout(
        title = titulos,
        xaxis = list(title = 'Plazo', 1:6), # type = 'date', tickformat = '%b <br> %Y'),
        yaxis = list(title = ylab)
      )
  } else {
    data %>%
      plotly::plot_ly(x = as.numeric(rownames(data))) %>%
      plotly::add_ribbons(
        ymin = ~Min95, ymax = ~Max95,
        line = list(color = adjustcolor('firebrick', alpha.f = 0.05)),
        fillcolor = adjustcolor('firebrick', alpha.f = 0.05),
        name = '95%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min90, ymax = ~Max90,
        line = list(color = adjustcolor('red', alpha.f = 0.1)),
        fillcolor = adjustcolor('red', alpha.f = 0.1),
        name = '90%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min85, ymax = ~Max85,
        line = list(color = adjustcolor('orange', alpha.f = 0.15)),
        fillcolor = adjustcolor('orange', alpha.f = 0.15),
        name = '85%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min80, ymax = ~Max80,
        line = list(color = adjustcolor('gold', alpha.f = 0.2)),
        fillcolor = adjustcolor('gold', alpha.f = 0.2),
        name = '80%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min75, ymax = ~Max75,
        line = list(color = adjustcolor('green', alpha.f = 0.25)),
        fillcolor = adjustcolor('green', alpha.f = 0.25),
        name = '75%'
      ) %>%
      plotly::add_ribbons(
        ymin = ~Min70, ymax = ~Max70,
        line = list(color = adjustcolor('green4', alpha.f = 0.3)),
        fillcolor = adjustcolor('green4', alpha.f = 0.3),
        name = '70%'
      ) %>%
      plotly::add_trace(
        y = ~Promedio, name = 'Median', type = 'scatter', mode = 'lines',
        line = list(color = adjustcolor('black'))
      ) %>%
      plotly::layout(
        title = titulos,
        xaxis = list(title = 'Plazo', type = 'date', tickformat = '%b <br> %Y'),
        yaxis = list(title = ylab)
      )
  }

  if (!is.null(comparar)) {
    plot <- plot %>%
      plotly::add_trace(
        y = ~Comparar,
        name = 'Real',
        type = 'scatter',
        mode = 'lines',
        line = list(color = adjustcolor('red'))
      )
  }
  return(plot)
}


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
