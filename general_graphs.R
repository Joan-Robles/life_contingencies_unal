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
