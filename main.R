.initial_time <- Sys.time() # Cronometrar corrida
set.seed(123) # Fijar semilla
source("general_graphs.R") # Traer funciones


# Taller 2 ----------------------------------------------------------------

pacman::p_load(readxl, dplyr, forecast, parallel, pdftools, fanplot, plotly)

# Parámetros --------------------------------------------------------------

n_sim <- 1000
allowance <- 1.8 # En millones de pesos de 2024

# Datos del taller
clientbase <- rbind(c(50, 64, 20000, .5),
                    c(65, 74, 16000, .49),
                    c(75, 82, 12000, .48),
                    c(83, 89, 10000, .47))

# Mujeres
clientbase <- cbind(clientbase, 1 - clientbase[, 4]) 

# Nombres
colnames(clientbase) <- c("from", "to", "affiliates", "men%", "women%")

# Tablas de mortalidad 
file_path <- file.path(getwd(), "res-1555-2010.pdf") # Si está en el mismo directorio
.mortality_file <- pdf_text(file_path)
mortality_table <- .mortality_file[2]

process_mortality_table <- function(text_content) {
  # Divide el texto en líneas
  lines <- unlist(strsplit(text_content, "\n"))
  
  # Encuentra las líneas que contienen datos de la tabla, basado en la presencia de números
  table_lines <- grep("^[[:space:]]*[0-9]{2,}", lines, value = TRUE)
  
  # Prepara una función para procesar cada línea y extraer los datos numéricos
  process_line <- function(line) {
    # Remueve comas de los números
    line <- gsub(",", "", line)
    # Divide la línea en sus valores componentes
    values <- unlist(strsplit(line, "[[:space:]]+"))
    # Filtra los valores para remover espacios vacíos
    values <- values[values != ""]
    # Convierte los valores a numéricos
    as.numeric(values)
  }
  
  # Aplica la función a cada línea para obtener una lista de vectores numéricos
  numeric_lines <- lapply(table_lines, process_line)
  
  # Combina los vectores numéricos en una matriz
  mortality_matrix <- do.call(rbind, numeric_lines)
  
  # Retorna la matriz
  return(mortality_matrix)
}

mortality_table <- process_mortality_table(mortality_table)

# Extraer lo importante
mortality_table <- mortality_table[, c(1, 4, 9)]
colnames(mortality_table) <- c("age", "p(men)", "p(women)")

# 1. Proyecciones muertes y desembolsos -----------------------------------

disaggregate_clientbase <- function(clientbase) {
  # Extrae datos
  men <- clientbase[, "affiliates"] * clientbase[, "men%"]
  women <- clientbase[, "affiliates"] * clientbase[, "women%"]
  unique_age <- clientbase[, "to"] - clientbase[, "from"] + 1
  
  # Encuentra totales
  total_men <- sapply(seq_len(length(unique_age)),
                      function(i)
                        rep(men[i], unique_age[i])) %>% unlist
  total_women <- sapply(seq_len(length(unique_age)),
                        function(i)
                          rep(women[i], unique_age[i])) %>% unlist
  
  result_matrix <-
    cbind(
      age = seq(min(clientbase[, "from"]), max(clientbase[, "to"])),
      total_men = total_men,
      total_women = total_women
    )
}

full_clientbase <- disaggregate_clientbase(clientbase)


simulate_deaths_trajectory <- function(full_clientbase, mortality_table) {
  # Unir ambas para que quede fácil
  # Para eso hay que añadir las edades faltantes en full_clientbas
  
  ages_to_add <- setdiff(mortality_table[, "age"], full_clientbase[, "age"])
  
  # Crea un nuevo data frame con esas edades y el resto de columnas como 0
  new_rows <- matrix(0,
                     ncol = ncol(full_clientbase) - 1,
                     nrow = length(ages_to_add))
  
  new_rows <- cbind(ages_to_add, new_rows)
  
  colnames(new_rows) <- colnames(full_clientbase)
  
  # Combina 'full_clientbase' con 'new_rows'
  full_clientbase <-
    rbind(full_clientbase, new_rows) # Queda en desorden, pero no importa
  
  # Ahora sí combina
  merged_table <-
    merge(full_clientbase, mortality_table, by = "age")
  
  # Agregar una fila con un año más que la edad máxima. EL total de gente es
  # 0 y las probabilidades son irrelevantes.
  impossible_age <- merged_table[nrow(merged_table), "age"] + 1
  merged_table <- rbind(merged_table,
                        c(impossible_age, rep(0, ncol(merged_table) - 1)))
  
  # Contar la gente
  total_people <- sum(merged_table[, c("total_men", "total_women")])
  
  # Ahora sí simular
  
  # Inicializar vector de muertes totales. Cuando llegue a todas, el bucle para
  
  death_per_stage <- c()
  
  while (sum(death_per_stage) < total_people) {
   
     #Inicializar vectores de info
    death_men <- c()
    death_women <- c()
    
    # Simular muertes para hombres y mujeres de forma vectorizada
    death_men <- rbinom( n = nrow(merged_table), 
                         size = merged_table$'total_men',
                         prob = merged_table$'p(men)')
    death_women <- rbinom( n = nrow(merged_table), 
                           size = merged_table$'total_women',
                           prob = merged_table$'p(women)')
    
    # Crear una nueva versión de merged_table para ajustar las poblaciones
    new_merged_table <- merged_table
    
    # Actualizar total de hombres y mujeres restando las muertes, excluyendo la primera fila
    new_merged_table[-1, 'total_men'] <-
      merged_table[-nrow(merged_table), 'total_men'] - death_men[1:(nrow(merged_table) - 1)]
    new_merged_table[-1, 'total_women'] <-
      merged_table[-nrow(merged_table), 'total_women'] - death_women[1:(nrow(merged_table) - 1)]
    
    # Sobreescribir merged_table con new_merged_table para la siguiente iteración
    merged_table <- new_merged_table
    
    # Contar muertos de la etapa
    death_in_stage <- sum(death_men, death_women)
    death_per_stage <- c(death_per_stage, death_in_stage)
  }
  alives_per_stage <- total_people - cumsum(death_per_stage)
  alives_per_stage
}

results <- replicate(1000,
                     simulate_deaths_trajectory(full_clientbase = full_clientbase, 
                                                mortality_table = mortality_table),
                     simplify = FALSE)

# Find the maximum length of the vectors obtained
max_length <- max(sapply(results, length))

# Estandarizar tamaños (en alguna simulación se pueden morir todos muy rápido)
standardized_results <- lapply(results, function(vec) {
  length(vec) <- max_length  # This pads the vector with NAs if it is shorter than max_length
  replace(vec, is.na(vec), 0)  # Replace NAs with zeros
})

# Convert the list of vectors to a matrix
alives_trajectories <- do.call(rbind, standardized_results)

# Resultado final
liabilities_matrix <- alives_trajectories * allowance * 12

# media y desviación
simulations_mean <- colMeans(liabilities_matrix)
simulations_sd <- apply(liabilities_matrix, 2, sd)

plot(simulations_mean); plot(simulations_sd) # Media y varianza
fanChartCurvas(t(liabilities_matrix)) # fanchart

duration <- sum(simulations_mean * seq_len(length(simulations_mean))) / 
  sum(simulations_mean) # Duración esperada: un poco más de 14 años
