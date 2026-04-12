# generate_readme_plots.R
# Genera las imágenes estáticas usadas en README.md.
# Ejecutar desde la raíz del repositorio:
# source("generate_readme_plots.R")

rm(list = ls())

# -------------------------------------------------------------------------
# Paquetes
# -------------------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  dplyr,
  pdftools,
  ggplot2,
  lattice,
  akima,
  plotly
)

# -------------------------------------------------------------------------
# Funciones del proyecto
# -------------------------------------------------------------------------

source("general_functions.R")
source("life_ensurance.R")
source("specific_functions.R")
source("general_graphs.R")

# -------------------------------------------------------------------------
# Parámetros base
# -------------------------------------------------------------------------

seed <- 123
set.seed(seed)

inflation <- 0.051
real_interest <- 0.025
nominal_interest <- (1 + inflation) * (1 + real_interest) - 1

file_path <- file.path(getwd(), "res-1555-2010.pdf")
.mortality_file <- pdftools::pdf_text(file_path)[2]

x_values <- 30:70
n_values <- 1:40

m <- 12
gender <- "women"
age <- 60
base_n <- 30

# Para que los gráficos del README sean coherentes con los parámetros del Rmd.
pricing_interest <- nominal_interest
growth_rate <- inflation

if (!dir.exists("plots")) {
  dir.create("plots")
}

# -------------------------------------------------------------------------
# Helpers para guardar gráficos
# -------------------------------------------------------------------------

save_ggplot_png <- function(plot, filename, width = 8, height = 5, dpi = 160) {
  ggplot2::ggsave(
    filename = file.path("plots", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

save_lattice_png <- function(plot, filename, width = 1000, height = 650, res = 140) {
  png(
    filename = file.path("plots", filename),
    width = width,
    height = height,
    res = res
  )
  print(plot)
  dev.off()
}

# -------------------------------------------------------------------------
# 1. Comparación de crecimiento del valor asegurado
# -------------------------------------------------------------------------

horizon_years <- max(n_values)
time_grid <- seq(1 / m, horizon_years, by = 1 / m)

benefit_p2 <- 1 + growth_rate * (seq_along(time_grid) / m)
benefit_p5 <- rep((1 + growth_rate)^(0:(horizon_years - 1)), each = m)

df_benefit <- rbind(
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

p_benefit <- ggplot(df_benefit, aes(x = time, y = value, linetype = type)) +
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

save_ggplot_png(p_benefit, "readme_benefit_comparison.png")

# -------------------------------------------------------------------------
# 2. Comparación de primas: producto 2 vs producto 5
# -------------------------------------------------------------------------

premium_p2 <- sapply(n_values, function(n) {
  get_premium_insurance(
    mortality_file = .mortality_file,
    x = age,
    n = n,
    i = pricing_interest,
    r = growth_rate,
    frac_pay = TRUE,
    frac_value = TRUE,
    initial_payment = "b",
    growth = "arithmetic",
    gender = gender,
    m = m
  )$premium
})

premium_p5 <- sapply(n_values, function(n) {
  get_premium_insurance(
    mortality_file = .mortality_file,
    x = age,
    n = n,
    i = pricing_interest,
    r = growth_rate,
    frac_pay = TRUE,
    frac_value = FALSE,
    initial_payment = "a",
    growth = "geometric",
    gender = gender,
    m = m
  )$premium
})

df_premium <- rbind(
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

p_premium <- ggplot(df_premium, aes(x = n, y = premium, linetype = type)) +
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

save_ggplot_png(p_premium, "readme_premium_comparison.png")

# -------------------------------------------------------------------------
# 3. Mapas de calor por producto
# -------------------------------------------------------------------------

premium_table_p1 <- evaluate_premium_grid(
  x_values = x_values,
  n_values = n_values,
  pricing_fun = get_premium_insurance,
  mortality_file = .mortality_file,
  i = pricing_interest,
  r = growth_rate,
  frac_pay = TRUE,
  frac_value = FALSE,
  initial_payment = "a",
  growth = "arithmetic",
  gender = gender,
  m = m
)

premium_table_p2 <- evaluate_premium_grid(
  x_values = x_values,
  n_values = n_values,
  pricing_fun = get_premium_insurance,
  mortality_file = .mortality_file,
  i = pricing_interest,
  r = growth_rate,
  frac_pay = TRUE,
  frac_value = TRUE,
  initial_payment = "b",
  growth = "arithmetic",
  gender = gender,
  m = m
)

premium_table_p3 <- evaluate_premium_grid(
  x_values = x_values,
  n_values = n_values,
  pricing_fun = get_premium_insurance,
  mortality_file = .mortality_file,
  i = pricing_interest,
  r = growth_rate,
  frac_pay = TRUE,
  frac_value = TRUE,
  initial_payment = "a",
  growth = "arithmetic",
  gender = gender,
  m = m
)

premium_table_p5 <- evaluate_premium_grid(
  x_values = x_values,
  n_values = n_values,
  pricing_fun = get_premium_insurance,
  mortality_file = .mortality_file,
  i = pricing_interest,
  r = growth_rate,
  frac_pay = TRUE,
  frac_value = FALSE,
  initial_payment = "a",
  growth = "geometric",
  gender = gender,
  m = m
)

save_lattice_png(
  heatmap_xyz(
    x = premium_table_p1$x,
    y = premium_table_p1$y,
    z = premium_table_p1$z,
    xlab = "Edad",
    ylab = "Años asegurados",
    main = "Seguro entero fraccionario con crecimiento aritmético cada año"
  ),
  "readme_heatmap_producto_1.png"
)

save_lattice_png(
  heatmap_xyz(
    x = premium_table_p2$x,
    y = premium_table_p2$y,
    z = premium_table_p2$z,
    xlab = "Edad",
    ylab = "Años asegurados",
    main = "Seguro entero fraccionario con crecimiento aritmético dentro de cada año"
  ),
  "readme_heatmap_producto_2.png"
)

save_lattice_png(
  heatmap_xyz(
    x = premium_table_p3$x,
    y = premium_table_p3$y,
    z = premium_table_p3$z,
    xlab = "Edad",
    ylab = "Años asegurados",
    main = "Seguro temporal fraccionario con crecimiento aritmético dentro de cada año"
  ),
  "readme_heatmap_producto_3.png"
)

save_lattice_png(
  heatmap_xyz(
    x = premium_table_p5$x,
    y = premium_table_p5$y,
    z = premium_table_p5$z,
    xlab = "Edad",
    ylab = "Años asegurados",
    main = "Seguro entero fraccionario con crecimiento geométrico cada año"
  ),
  "readme_heatmap_producto_5.png"
)

# -------------------------------------------------------------------------
# 4. Diferencias contra el producto 1
# -------------------------------------------------------------------------

diff_p2_p1 <- premium_table_p2
diff_p2_p1$z <- premium_table_p2$z - premium_table_p1$z

diff_p3_p1 <- premium_table_p3
diff_p3_p1$z <- premium_table_p3$z - premium_table_p1$z

diff_p5_p1 <- premium_table_p5
diff_p5_p1$z <- premium_table_p5$z - premium_table_p1$z

save_lattice_png(
  heatmap_xyz(
    x = diff_p2_p1$x,
    y = diff_p2_p1$y,
    z = diff_p2_p1$z,
    xlab = "Edad",
    ylab = "Años asegurados",
    main = "Diferencia de primas: producto 2 - producto 1"
  ),
  "readme_diff_p2_p1.png"
)

save_lattice_png(
  heatmap_xyz(
    x = diff_p3_p1$x,
    y = diff_p3_p1$y,
    z = diff_p3_p1$z,
    xlab = "Edad",
    ylab = "Años asegurados",
    main = "Diferencia de primas: producto 3 - producto 1"
  ),
  "readme_diff_p3_p1.png"
)

save_lattice_png(
  heatmap_xyz(
    x = diff_p5_p1$x,
    y = diff_p5_p1$y,
    z = diff_p5_p1$z,
    xlab = "Edad",
    ylab = "Años asegurados",
    main = "Diferencia de primas: producto 5 - producto 1"
  ),
  "readme_diff_p5_p1.png"
)

message("README plots generated in plots/.")
