
require(gridExtra)
require(broom)
require(stats)
require(dplyr)
require(lubridate)
require(tidyverse)
require(caret)
require(reticulate)
require(devtools)
require(hydroGOF)
require(trend)
require(airGR)
require(Metrics)
require(psych)
require(ggcorrplot)
require(plotly)
require(data.table)
require(ncdf4)
require(Kendall)
require(zoo)
require(raster)
require(ncdf4)
require(sf)
require(stars)
require(ggspatial)
require(prettymapr)
require(deldir)
require(moments)
require(fitdistrplus)
require(raster)
require(sf)


# rstudioapi::restartSession()

# en el archivo base de datos se encuentra el df_estaciones_P que son todos los datos usados y los GCM ya extraidos.
# el codigo corre al 100, cualquier duda escribeme :D Felipe.Rivas5492@gmail.com. La extraccion de datos de la base del cr2 requiere de
# mis archivos, ya que se corrigieron algunas cosas de la base de datos para que pudiera funcionar el codigo. Por favor escribeme si las necesitas
# ya que github no me dejo subir archivos de mas de 25 MB. Se subio df_estaciones_P en dos partes.


#### ZE: I a III REGION OK  #### 


cuenca_salida <- st_read(".../Chile_Regional/Regional.shp") |>
st_transform(4326) |> st_make_valid()
polygon_union <- st_union(cuenca_salida[1:3, ]) # regiones 1 a la 3 
polygon_sf <- st_sf(geometry = polygon_union)
polygon_sp <- as(polygon_sf, "Spatial")  # necesario para raster::crop y mask


#### EXTRACCION DE DATOS CR2 2020 OK #####

#F1
leer_datos <- function(file_path, sep="\t") {
  data <- read.table(file_path, header = TRUE, sep = sep, quote="", fill=TRUE, stringsAsFactors=FALSE)
  return(as.data.frame(data))
}

#F2
extraer_estacion <- function(cadena) {
  strsplit(cadena, ",")[[1]][1]
}

#F3
procesar_estaciones <- function(datos_estaciones) {
  estaciones <- list()
  
  for (i in 1:nrow(datos_estaciones)) {
    fila_actual <- datos_estaciones[i,]
    estaciones[i] <- unlist(strsplit(as.character(fila_actual), ","))[1]
  }
  
  return(as.character(estaciones))
}

#F4
extraer_info_columnas <- function(info) {
  substring <- sub('.*?\\$(.*?)\\s*:.*', '\\1', info)
  datos_separados <- strsplit(substring[2], "\\.")
  datos_separados_df <- as.data.frame(do.call(rbind, datos_separados))
  return(as.character(datos_separados_df[1, ]))
}

#F5
crear_df_estaciones <- function(datos_separados_lista, nombres_columnas) {
  df_estaciones <- as.data.frame(do.call(rbind, datos_separados_lista), stringsAsFactors = FALSE)
  colnames(df_estaciones) <- nombres_columnas
  return(df_estaciones)
}

#F6
filtrar_datos_por_fechas <- function(datos) {
  lista_datos_filtrados <- list()
  fechas <- as.Date(datos[-(1:13), 1], format = "%Y-%m-%d")
  
  for (i in 2:ncol(datos)) {
    estacion <- names(datos)[i]
    inicio <- as.Date(datos[11, i], format = "%Y-%m-%d")
    termino <- as.Date(datos[12, i], format = "%Y-%m-%d")
    
    # Check conversion of dates
    if (is.na(inicio) || is.na(termino)) {
      warning(paste("Date conversion issue for station", estacion))
      next
    }
    
    valores <- as.numeric(datos[-(1:13), i])
    valores[valores == -9999] <- NA
    
    datos_temp <- data.frame(fecha = fechas, valor = valores)
    datos_filtrados <- datos_temp %>%
      filter(fecha >= inicio & fecha <= termino)
    
    encabezados <- datos[1:13, i, drop = FALSE]
    colnames(encabezados) <- estacion
    rownames(encabezados) <- NULL
    
    lista_datos_filtrados[[estacion]] <- list(encabezados = encabezados, datos = datos_filtrados)
  }
  
  return(lista_datos_filtrados)
}

#F7
eliminar_anos_incompletos <- function(lista_datos, umbral_porcentaje = 0.8) {
  lista_datos_filtrados <- list()
  lista_anos_filtrados <- list()
  
  for (estacion in names(lista_datos)) {
    datos <- lista_datos[[estacion]]$datos
    encabezados <- lista_datos[[estacion]]$encabezados
    
    # Check if 'datos' is a data frame
    if (!is.data.frame(datos)) {
      warning(paste("Data for station", estacion, "is not a data frame. Skipping."))
      next
    }
    
    umbral <- 365 * umbral_porcentaje
    datos <- datos %>%
      mutate(Año = year(fecha))
    
    anos_completos <- datos %>%
      group_by(Año) %>%
      filter(n() >= umbral) %>%
      ungroup()
    
    anos_filtrados <- setdiff(unique(datos$Año), unique(anos_completos$Año))
    
    lista_datos_filtrados[[estacion]] <- list(encabezados = encabezados, datos = anos_completos)
    lista_anos_filtrados[[estacion]] <- anos_filtrados
  }
  
  return(list(datos_filtrados = lista_datos_filtrados, anos_filtrados = lista_anos_filtrados))
}


# PROCESAR DATOS DE PRECIPITACION Y TEMPERATURA 

procesar_datos_T <- function(T_daily_dir, T_daily_dir_est) {
  cat("Leyendo datos de temperatura...\n")
  T_mm_day <- leer_datos(T_daily_dir)
  T_mm_day_est <- leer_datos(T_daily_dir_est)
  
  # Procesar estaciones
  cat("Procesando estaciones de temperatura...\n")
  estaciones_T <- procesar_estaciones(T_mm_day_est)
  
  cat("Extrayendo información de columnas de temperatura...\n")
  info_T <- capture.output(str(T_mm_day))
  nombres_columnas_T <- extraer_info_columnas(info_T)
  
  cat("Creando dataframes de estaciones de temperatura..\n")
  datos_separados_lista_T <- lapply(1:nrow(T_mm_day), function(i) unlist(strsplit(as.character(T_mm_day[i,]), ",")))
  df_DGA_estaciones_T <- crear_df_estaciones(datos_separados_lista_T, nombres_columnas_T)
  
  cat("Filtrando datos por fechas para precipitación...\n")
  datos_filtrados_T <- filtrar_datos_por_fechas(df_DGA_estaciones_T)
  
  cat("Eliminando años incompletos para precipitación...\n")
  resultado_T <- eliminar_anos_incompletos(datos_filtrados_T)
  
  datos_filtrados_completos_T <- resultado_T$datos_filtrados
  anos_filtrados_T <- resultado_T$anos_filtrados
  
  # Devolver resultados
  return(list(
    datos_filtrados_completos_T = datos_filtrados_completos_T,
    anos_filtrados_T = anos_filtrados_T
  ))
}


procesar_datos_P <- function(P_daily_dir, P_daily_dir_est) {
  cat("Leyendo datos de precipitación...\n")
  P_mm_day <- leer_datos(P_daily_dir)
  P_mm_day_est <- leer_datos(P_daily_dir_est)
  
  # Procesar estaciones
  cat("Procesando estaciones de precipitación...\n")
  estaciones_P <- procesar_estaciones(P_mm_day_est)
  
  cat("Extrayendo información de columnas de precipitación...\n")
  info_P <- capture.output(str(P_mm_day))
  nombres_columnas_P <- extraer_info_columnas(info_P)
  
  cat("Creando dataframes de estaciones de precipitación...\n")
  datos_separados_lista_P <- lapply(1:nrow(P_mm_day), function(i) unlist(strsplit(as.character(P_mm_day[i,]), ",")))
  df_DGA_estaciones_P <- crear_df_estaciones(datos_separados_lista_P, nombres_columnas_P)
  
  cat("Filtrando datos por fechas para precipitación...\n")
  datos_filtrados_P <- filtrar_datos_por_fechas(df_DGA_estaciones_P)
  
  cat("Eliminando años incompletos para precipitación...\n")
  resultado_P <- eliminar_anos_incompletos(datos_filtrados_P)
  
  datos_filtrados_completos_P <- resultado_P$datos_filtrados
  anos_filtrados_P <- resultado_P$anos_filtrados
  
  # Devolver resultados
  return(list(
    datos_filtrados_completos_P = datos_filtrados_completos_P,
    anos_filtrados_P = anos_filtrados_P
  ))
}


procesar_datos_Q <- function(Q_daily_dir, Q_daily_dir_est) {
  cat("Leyendo datos de caudal...\n")
  Q_mm_day <- leer_datos(Q_daily_dir)
  Q_mm_day_est <- leer_datos(Q_daily_dir_est)
  
  # Procesar estaciones
  cat("Procesando estaciones de caudal...\n")
  estaciones_Q <- procesar_estaciones(Q_mm_day_est)
  
  cat("Extrayendo información de columnas de caudal...\n")
  info_Q <- capture.output(str(Q_mm_day))
  nombres_columnas_Q <- extraer_info_columnas(info_Q)
  
  cat("Creando dataframes de estaciones de caudal...\n")
  datos_separados_lista_Q <- lapply(1:nrow(Q_mm_day), function(i) unlist(strsplit(as.character(Q_mm_day[i,]), ",")))
  df_DGA_estaciones_Q <- crear_df_estaciones(datos_separados_lista_Q, nombres_columnas_Q)
  
  cat("Filtrando datos por fechas para caudal...\n")
  datos_filtrados_Q <- filtrar_datos_por_fechas(df_DGA_estaciones_Q)
  
  cat("Eliminando años incompletos para caudal...\n")
  resultado_Q <- eliminar_anos_incompletos(datos_filtrados_Q)
  
  datos_filtrados_completos_Q <- resultado_Q$datos_filtrados
  anos_filtrados_Q <- resultado_Q$anos_filtrados
  
  # Devolver resultados
  return(list(
    datos_filtrados_completos_Q = datos_filtrados_completos_Q,
    anos_filtrados_Q = anos_filtrados_Q
  ))
}


# EXTRAER  DE ESTACIONES DE INTERES

extraer_estaciones_zona_estudio_1 <- function(zona_estudio_shp, est_fluv_shp, est_met_shp, est_dmc_shp) {
  # Leer shapefile de la zona de estudio
  # DGA_delimitaciones <- st_read(zona_estudio_shp)
  DGA_delimitaciones  <- zona_estudio_shp
  # Filtrar la cuenca de interés
  cuenca <- DGA_delimitaciones 
  
  # Leer y transformar las estaciones
  Est_fluv <- st_read(Est_fluv)
  Est_met <- st_read(Est_met)
  Est_DMC <- st_read(Est_DMC)
  
  crs_cuenca <- st_crs(cuenca)
  Est_fluv <- st_transform(Est_fluv, crs_cuenca)
  Est_met <- st_transform(Est_met, crs_cuenca)
  Est_DMC <- st_transform(Est_DMC, crs_cuenca)
  
  # Simplificar geometría
  cuenca_simplified <- st_simplify(cuenca, dTolerance = 50)
  
  # Filtrar estaciones dentro de la cuenca
  Est_fluv_filtrado <- Est_fluv[st_intersects(Est_fluv, cuenca_simplified, sparse = FALSE),]
  Est_met_filtrado <- Est_met[st_intersects(Est_met, cuenca_simplified, sparse = FALSE),]
  Est_DMC_filtrado <- Est_DMC[st_intersects(Est_DMC, cuenca_simplified, sparse = FALSE),]
  
  # Extraer códigos de estaciones
  codigos_fluv <- Est_fluv_filtrado$COD_BNA
  codigos_met <- Est_met_filtrado$COD_BNA
  codigos_DMC <- Est_DMC_filtrado$COD_BNA
  
  # Modificar los códigos para eliminar los dos últimos caracteres
  codigos_modificados_fluv <- substr(codigos_fluv, 1, nchar(codigos_fluv) - 2)
  codigos_modificados_met <- substr(codigos_met, 1, nchar(codigos_met) - 2)
  codigos_modificados_DMC <- substr(codigos_DMC, 1, nchar(codigos_DMC) - 2)
  
  return(list(
    codigos_modificados_fluv = codigos_modificados_fluv,
    codigos_modificados_met = codigos_modificados_met,
    codigos_modificados_DMC = codigos_modificados_DMC
  ))
}

# EXTRAER INFORMACION DE ESTACIONES DE INTERES

extraer_estaciones_interes_P <- function(codigos_modificados, resultados_P) {
  # Crear una lista para almacenar los datos filtrados
  datos_estaciones_filtradas <- list()
  
  for (codigo in codigos_modificados) {
    if (codigo %in% names(resultados_P$datos_filtrados_completos_P)) {
      datos_estacion <- resultados_P$datos_filtrados_completos_P[[codigo]]
      datos_estaciones_filtradas[[codigo]] <- datos_estacion
    }
  }
  
  return(datos_estaciones_filtradas)
}

extraer_estaciones_interes_Q <- function(codigos_modificados, resultados_Q) {
  # Crear una lista para almacenar los datos filtrados
  datos_estaciones_filtradas <- list()
  
  for (codigo in codigos_modificados) {
    if (codigo %in% names(resultados_Q$datos_filtrados_completos_Q)) {
      datos_estacion <- resultados_Q$datos_filtrados_completos_Q[[codigo]]
      datos_estaciones_filtradas[[codigo]] <- datos_estacion
    }
  }
  
  return(datos_estaciones_filtradas)
}

extraer_estaciones_interes_T <- function(codigos_modificados, resultados_T) {
  # Crear una lista para almacenar los datos filtrados
  datos_estaciones_filtradas <- list()
  
  for (codigo in codigos_modificados) {
    if (codigo %in% names(resultados_T$datos_filtrados_completos_T)) {
      datos_estacion <- resultados_T$datos_filtrados_completos_T[[codigo]]
      datos_estaciones_filtradas[[codigo]] <- datos_estacion
    }
  }
  
  return(datos_estaciones_filtradas)
}

#### LLAMAR FUNCIONES Y EXTRAER DATOS ####

# los archivos _dir han sido manipulados para un correcto funcionamiento
# caudales 

Q_daily_dir = '.../cr2_qflxDaily_2018/cr2_qflxDaily_2018/cr2_qflxDaily_2018.txt' 
Q_daily_dir_est = '.../cr2_qflxDaily_2018/cr2_qflxDaily_2018/cr2_qflxDaily_2018_stations.txt' 

# precipitacion

P_daily_dir = '.../cr2_prDaily_2020_ghcn/cr2_prDaily_2020_ghcn/cr2_prDaily_2020_ghcn.txt' 
P_daily_dir_est = '...cr2_prDaily_2020_ghcn/cr2_prDaily_2020_ghcn/cr2_prDaily_2020_stations_ghcn.txt' 

# temperatura max min 

T_dailymax_dir = '.../cr2_tasmaxDaily_2020/cr2_tasmaxDaily_2020/cr2_tasmaxDaily_2020.txt' 
T_dailymax_dir_est = '.../71C/T4/cr2_tasmaxDaily_2020/cr2_tasmaxDaily_2020/cr2_tasmaxDaily_2020_stations.txt' 

T_dailymin_dir = '.../cr2_tasminDaily_2020/cr2_tasminDaily_2020.txt' 
T_dailymin_dir_est = '.../cr2_tasminDaily_2020/cr2_tasminDaily_2020_stations.txt' 

T_daily_dir = '.../cr2_tasDaily_2020_ghcn/cr2_tasDaily_2020_ghcn/cr2_tasDaily_2020_ghcn.txt' 
T_daily_dir_est = '.../cr2_tasDaily_2020_ghcn/cr2_tasDaily_2020_ghcn/cr2_tasDaily_2020_stations_ghcn.txt' 



#### OBTENCION DATAFRAMES #### 


datos_procesados_P = procesar_datos_P(P_daily_dir, P_daily_dir_est)

# datos_procesados_T = procesar_datos_T(T_daily_dir, T_daily_dir_est)
# 
# datos_procesados_T_max = procesar_datos_T(T_dailymax_dir, T_dailymax_dir_est)
# 
# datos_procesados_T_min = procesar_datos_T(T_dailymin_dir, T_dailymin_dir_est)
# 
# 
# datos_procesados_Q = procesar_datos_Q(Q_daily_dir, Q_daily_dir_est)
# 

# estaciones_ZE = extraer_estaciones_zona_estudio_1(cuenca_salida, Est_fluv, Est_met, Est_DMC)


estaciones_ZE = extraer_estaciones_zona_estudio_1(polygon_sf, Est_fluv, Est_met, Est_DMC)


datos_estaciones_ZE_P = extraer_estaciones_interes_P(estaciones_ZE$codigos_modificados_met, datos_procesados_P)

# datos_estaciones_ZE_T = extraer_estaciones_interes_T(estaciones_ZE$codigos_modificados_met, datos_procesados_T)
# 
# datos_estaciones_ZE_T_max = extraer_estaciones_interes_T(estaciones_ZE$codigos_modificados_met, datos_procesados_T_max)
# 
# datos_estaciones_ZE_T_min = extraer_estaciones_interes_T(estaciones_ZE$codigos_modificados_met, datos_procesados_T_min)
# 
# 
# datos_estaciones_ZE_Q = extraer_estaciones_interes_Q(estaciones_ZE$codigos_modificados_fluv, datos_procesados_Q)
# 

# ESTACIONES CR2 FORMATO DATA FRAME 

combinar_datos_estaciones <- function(lista_datos) {
  df_completo <- data.frame()
  
  for (estacion in names(lista_datos)) {
    encabezados <- lista_datos[[estacion]]$encabezados
    datos <- lista_datos[[estacion]]$datos
    
    # Extraer información del encabezado
    codigo <- estacion
    nombre_estacion <- encabezados[3, 1]
    latitud <- as.numeric(encabezados[5, 1])
    longitud <- as.numeric(encabezados[6, 1])
    fecha_inicio <- as.Date(encabezados[11, 1])
    fecha_termino <- as.Date(encabezados[12, 1])
    altitud <- as.numeric(encabezados[4, 1])
    
    
    # Añadir columnas al dataframe de datos
    datos <- datos %>%
      mutate(
        codigo = codigo,
        nombre_estacion = nombre_estacion,
        latitud = latitud,
        longitud = longitud,
        fecha_inicio = fecha_inicio,
        fecha_termino = fecha_termino,
        altitud = altitud
      )
    
    # Combinar datos en un dataframe completo
    df_completo <- rbind(df_completo, datos)
  }
  
  return(as.data.frame(df_completo))
}

# # Aplicar la función a la lista de estaciones y convertir a data frame
df_estaciones_P <- combinar_datos_estaciones(datos_estaciones_ZE_P)

# df_estaciones_T <- combinar_datos_estaciones(datos_estaciones_ZE_T)
# 
# df_estaciones_T_max <- combinar_datos_estaciones(datos_estaciones_ZE_T_max)
# 
# df_estaciones_T_min <- combinar_datos_estaciones(datos_estaciones_ZE_T_min)
# 
# df_estaciones_Q <- combinar_datos_estaciones(datos_estaciones_ZE_Q)

# 

# # Ver los primeros registros del DataFrame resultante
print(head(df_estaciones_P))

# print(head(df_estaciones_T))
# print(head(df_estaciones_T_max))
# print(head(df_estaciones_T_min))

# print(head(df_estaciones_Q))


#### RESUMEN ESTACIONES ####


df_estaciones_P <- df_estaciones_P %>%
  dplyr::rename(nombre_estacion = nombre_estacion)


colnames(df_estaciones_P)

head(df_estaciones_P$nombre_estacion)


resumen_estaciones <- function(data) {
  data %>%
    dplyr::mutate(
      mes = lubridate::floor_date(fecha, "month"), # Extraer el mes
      anio = lubridate::year(fecha)               # Extraer el año
    ) %>%
    dplyr::group_by(nombre_estacion, mes) %>%
    dplyr::summarise(
      datos_cero = sum(valor == 0, na.rm = TRUE),          # Datos con valor cero
      datos_mayor_cero = sum(valor > 0, na.rm = TRUE),     # Datos con valor mayor a cero
      datos_na = sum(is.na(valor)),                        # Datos NA
      .groups = "drop"                                     # Eliminar agrupamiento
    ) %>%
    dplyr::group_by(nombre_estacion) %>%
    dplyr::summarise(
      anios_totales = dplyr::n_distinct(lubridate::year(mes)),    # Años totales
      meses_25_ceros = sum(datos_cero >= 25),                    # Meses con 25+ datos cero
      meses_25_mayor_cero = sum(datos_mayor_cero >= 25),         # Meses con 25+ datos > 0
      total_na = sum(datos_na),                                  # Total de NAs
      nas_por_mes = sum(datos_na) / dplyr::n(),                  # NAs promedio por mes
      .groups = "drop"                                           # Eliminar agrupamiento
    )
}


resultado_resumen_prefiltro <- resumen_estaciones(df_estaciones_P)
print(resultado_resumen_prefiltro)

resultado_resumen_prefiltro <- resultado_resumen_prefiltro %>%
  dplyr::left_join(
    df_estaciones_P %>%
      dplyr::select(nombre_estacion, fecha_inicio, fecha_termino) %>%
      dplyr::distinct(),
    by = "nombre_estacion"
  )


print(resultado_resumen_prefiltro)

as.data.frame(resultado_resumen_prefiltro)



#### FILTRAR POR CANTIDAD DE AÑOS Y MESES CON NA #### 

filtrar_estaciones <- function(data, x_anios, na_max, porcentaje_datos = 0.85) {
  # Total de días por mes (asumiendo datos diarios)
  dias_por_mes <- data %>%
    dplyr::mutate(mes = lubridate::floor_date(fecha, "month")) %>%
    dplyr::group_by(mes) %>%
    dplyr::summarise(total_dias = dplyr::n_distinct(fecha), .groups = "drop")
  
  # Resumen de las estaciones con datos agrupados por mes
  resumen <- data %>%
    dplyr::mutate(
      mes = lubridate::floor_date(fecha, "month"), # Extraer el mes
      anio = lubridate::year(fecha)               # Extraer el año
    ) %>%
    dplyr::left_join(dias_por_mes, by = "mes") %>% # Agregar total de días por mes
    dplyr::group_by(nombre_estacion, mes) %>%
    dplyr::summarise(
      datos_no_na = sum(!is.na(valor)),                  # Datos no faltantes
      datos_na = sum(is.na(valor)),                      # Datos faltantes (NA)
      porcentaje_completitud = datos_no_na / dplyr::first(total_dias), # Porcentaje de completitud
      .groups = "drop"
    ) %>%
    dplyr::filter(
      datos_na <=na_max,                                   # Excluir meses con más de NA_MAX datos NA 
      porcentaje_completitud >= porcentaje_datos       # Excluir meses con bajo completitud
    ) %>%
    dplyr::group_by(nombre_estacion) %>%
    dplyr::summarise(
      anios_disponibles = dplyr::n_distinct(lubridate::year(mes)), # Años disponibles
      meses_validos = dplyr::n(),                                 # Meses válidos
      fecha_inicio_disponible = min(mes),                         # Fecha mínima disponible
      fecha_fin_disponible = max(mes),                            # Fecha máxima disponible
      .groups = "drop"
    )
  
  # Filtrar estaciones con más de x_anios disponibles y suficientes meses válidos
  estaciones_filtradas <- resumen %>%
    dplyr::filter(anios_disponibles > x_anios, meses_validos > 0)
  
  # Filtrar el DataFrame original para incluir solo las estaciones seleccionadas
  data_filtrada <- data %>%
    dplyr::filter(
      nombre_estacion %in% estaciones_filtradas$nombre_estacion,
      !is.na(valor) # Excluir valores NA
    )
  
  
  return(data_filtrada)
}


df_estaciones_P_f <- filtrar_estaciones(df_estaciones_P, x_anios = 20, na_max= 5)


unique(df_estaciones_P$nombre_estacion)
unique(df_estaciones_P_f$nombre_estacion)


#### FILTRAR POR FECHAS ####

# 
# fecha_inicio <- as.Date("1990-04-01")
# fecha_fin <- as.Date("2018-03-31")


fecha_inicio <- as.Date("1970-01-01")
fecha_fin <- as.Date("1980-01-01")


# Asegurarnos de que las fechas estén en formato Date
df_estaciones_P_f$fecha <- as.Date(df_estaciones_P_f$fecha, format = "%Y-%m-%d")

# Dividir los datos en una lista utilizando split en lugar de group_split
lista_dataframes_min <- split(df_estaciones_P_f, df_estaciones_P_f$nombre_estacion)

# Filtrar cada dataframe en la lista según el rango de fechas
lista_dataframes_min_filtrados <- lapply(lista_dataframes_min, function(df) {
  df_filtrado <- df[df$fecha >= fecha_inicio & df$fecha <= fecha_fin, ]
  return(df_filtrado)
})


df_estaciones_P_f <- as.data.frame(bind_rows(lista_dataframes_min_filtrados, .id = "nombre_estacion"))


# 
# resultados_precip_filtrados <- lapply(resultados_precip, function(df) {
#   df_filtrado <- df[df$Fecha >= fecha_inicio & df$Fecha <= fecha_fin, ]
#   return(df_filtrado)
# })
# 
# 
# names(resultados_precip_filtrados) <- names(resultados_precip)
# 
# # Verifica los datos filtrados
# str(resultados_precip_filtrados)
# 
# resultados_precip = resultados_precip_filtrados
# 
# 
# length(resultados_precip)
# 
# 




















#### MAPA ESTACIONES ####



library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)   # Para annotation_map_tile() y geom_spatraster()
library(terra)       # Para manejo de raster
library(patchwork)   # Para combinar plots


df_estaciones_coords <- df_estaciones_P %>%
  distinct(codigo, nombre_estacion, latitud, longitud)

estaciones_sf <- st_as_sf(df_estaciones_coords,
                          coords = c("longitud", "latitud"),
                          crs = 4326)


cuenca_salida <- st_read(".../01_shapes/Chile_Regional/Regional.shp") |>
  st_transform(4326) |> st_make_valid()

polygon_union <- st_union(cuenca_salida[1:3, ])
polygon_sf <- st_sf(geometry = polygon_union)
polygon_vect <- terra::vect(polygon_sf)


# Contar número de estaciones
nro_estaciones <- nrow(df_estaciones_coords)

# Crear el título con el número de estaciones
titulo_mapa <- paste0("Estaciones y Regiones I a III. ", nro_estaciones," Estaciones")

# Crear el mapa
mapa_estaciones <- ggplot() +
  annotation_map_tile(type = "cartolight") +
  geom_sf(data = polygon_sf, fill = NA, color = "darkgreen", size = 1.2) +
  geom_sf(data = estaciones_sf, color = "blue", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.25, 
                   text_cex = 0.8, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering(), 
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = titulo_mapa)

mapa_estaciones




#### AÑADIR GCM AL DATAFRAME DE OBSERVACIONES TODOS LOS DATOS DEL GCM ####


extraer_variable_modelo <- function(
  modelo_nombre, 
  ruta_base,
  estaciones_df,
  variable_objetivo = "pr"
) {
  ruta_modelo <- base::file.path(ruta_base, modelo_nombre)
  archivos_nc <- base::list.files(ruta_modelo, pattern = "\\.nc$", full.names = TRUE)
  resultados <- base::list()
  
  estaciones_coord <- base::unique(estaciones_df[, c("codigo", "latitud", "longitud")])
  xy <- base::as.matrix(estaciones_coord[, c("longitud", "latitud")])
  
  for (archivo_nc in archivos_nc) {
    r <- base::try(terra::rast(archivo_nc, subds = variable_objetivo), silent = TRUE)
    if (base::inherits(r, "try-error")) {
      warning(paste("No se pudo leer la variable", variable_objetivo, "en el archivo", basename(archivo_nc)))
      next
    }

    fechas <- terra::time(r)
    if (base::is.null(fechas)) {
      terra::tmpFiles(remove = TRUE)  # Limpieza de temporales
      next
    }
    
    valores <- terra::extract(r, xy)

    if (base::ncol(valores) <= 1) {
      warning(paste("Sin valores extraídos en", basename(archivo_nc)))
      terra::tmpFiles(remove = TRUE)
      next
    }

    valores <- valores[, -1, drop = FALSE]
    base::colnames(valores) <- base::paste0("lyr", base::seq_len(base::ncol(valores)))
    df_extraido <- base::cbind(estaciones_coord["codigo"], valores)

    df_largo <- reshape2::melt(df_extraido, id.vars = "codigo", 
                               variable.name = "layer", value.name = "valor")

    df_largo$fecha <- fechas[as.numeric(gsub("lyr", "", df_largo$layer))]
    df_largo$layer <- NULL

    resultados[[length(resultados) + 1]] <- df_largo

    # Liberar todo lo pesado
    rm(r, valores, df_extraido, df_largo, fechas)
    terra::tmpFiles(remove = TRUE)  # Limpia archivos .tmp de terra
    gc()
  }

  if (length(resultados) == 0) {
    warning(paste("No se encontraron datos válidos para el modelo", modelo_nombre))
    return(NULL)
  }

  df_modelo <- do.call(rbind, resultados)
  df_modelo <- stats::aggregate(valor ~ codigo + fecha, data = df_modelo, FUN = mean)
  colnames(df_modelo)[3] <- paste0(variable_objetivo, "_", modelo_nombre)

  # Limpieza final
  rm(resultados)
  gc()
  terra::tmpFiles(remove = TRUE)

  return(df_modelo)
}


extraer_todos_modelos <- function(
  ruta_base,
  estaciones_df,
  variable_objetivo = "pr"
) {
  # Ruta donde se guardan los .rds
  ruta_salida_rds <- file.path(getwd(), "modelos_rds")
  dir.create(ruta_salida_rds, showWarnings = FALSE)

  # Lista de modelos en la carpeta
  modelos <- list.dirs(ruta_base, full.names = FALSE, recursive = FALSE)

  # Separar el modelo problemático
  modelo_excepcion <- "MPI-ESM-MR-RegCM4-10k"
  modelos_validos <- setdiff(modelos, modelo_excepcion)

  # Procesar modelos válidos
  for (mod in modelos_validos) {
    archivo_rds <- file.path(ruta_salida_rds, paste0(mod, ".rds"))

    if (file.exists(archivo_rds)) {
      cat("Ya existe .RDS para modelo:", mod, "- saltando...\n")
      next
    }

    cat("Extrayendo modelo:", mod, "\n")

    df_mod <- extraer_variable_modelo(
      modelo_nombre = mod,
      ruta_base = ruta_base,
      estaciones_df = estaciones_df,
      variable_objetivo = variable_objetivo
    )

    if (!is.null(df_mod)) {
      saveRDS(df_mod, archivo_rds)
    }
  }

  # Leer todos los .rds válidos
  archivos_rds <- list.files(ruta_salida_rds, pattern = "\\.rds$", full.names = TRUE)
  archivos_rds_validos <- archivos_rds[!grepl(modelo_excepcion, archivos_rds)]

  lista_modelos_validos <- lapply(archivos_rds_validos, readRDS)

  # Determinar el largo esperado (mayoría)
  largos <- sapply(lista_modelos_validos, nrow)
  largo_dominante <- as.numeric(names(sort(table(largos), decreasing = TRUE))[1])
  
  # Filtrar modelos con mismo largo
  lista_modelos_comunes <- lista_modelos_validos[largos == largo_dominante]

  if (length(lista_modelos_comunes) == 0) {
    warning("No hay modelos con datos consistentes para combinar.")
    return(NULL)
  }

  # Combinar modelos con mismo largo
  df_final <- Reduce(function(x, y) merge(x, y, by = c("codigo", "fecha"), all = TRUE), lista_modelos_comunes)

  # Leer y agregar el modelo de excepción al final si existe
  archivo_excepcion <- file.path(ruta_salida_rds, paste0(modelo_excepcion, ".rds"))
  if (file.exists(archivo_excepcion)) {
    df_excepcion <- readRDS(archivo_excepcion)

    cat("Agregando modelo con diferente largo:", modelo_excepcion, "\n")

    # Unir con all.x = TRUE para no agregar fechas nuevas
    df_final <- merge(df_final, df_excepcion, by = c("codigo", "fecha"), all.x = TRUE)
  }

  return(df_final)
}

# SOLO FUNCIONA CON EL DISCO EXTERNO CONECTADO SI ES QUE SE OCUPA UNO

df_modelos_todos <- extraer_todos_modelos(
  ruta_base = "D:/CMIP6",
  estaciones_df = df_estaciones_P,
  variable_objetivo = "pr"
)

info_estatica <- df_estaciones_P %>%
  dplyr::select(codigo, nombre_estacion, latitud, longitud, altitud, fecha_inicio, fecha_termino) %>%
  distinct()

df_resultado<- df_modelos_todos %>%
  left_join(info_estatica, by = "codigo")

df_estaciones_P  <- df_estaciones_P %>%
  dplyr::rename(P_obs = valor)

df_resultado <- df_resultado %>%
  left_join(df_estaciones_P %>% dplyr::select(codigo, fecha, P_obs), by = c("codigo", "fecha"))

df_estaciones_P = df_resultado


#### VERIFICACION SCATTERPLOT DIARIOS MENSUALES ANUALES SIN CORRECCION #### 


df_estaciones_P_f <- df_estaciones_completa %>%
  rename(P_obs = valor)

df_estaciones_P <- df_estaciones_P %>%
  rename(P_obs = valor)

require(patchwork)

# anuales calendario 

plot_gcm_vs_obs <- function(df, fecha_col = "fecha", obs_col = "P_obs", gcm_col = "P_ACCESS-CM2") {
  
  require(dplyr)
  require(ggplot2)
  require(lubridate)
  require(patchwork)  # para combinar gráficos

  df <- df %>%
    mutate(
      fecha = as.Date(.data[[fecha_col]]),
      P_obs = .data[[obs_col]],
      P_CM2 = .data[[gcm_col]]
    )
  
  fecha_inicio <- min(df$fecha, na.rm = TRUE)
  fecha_termino <- max(df$fecha, na.rm = TRUE)
  titulo_general <- paste0("Datos desde ", fecha_inicio, " hasta ", fecha_termino)


  df_diario <- df %>% filter(!is.na(P_obs), !is.na(P_CM2))
  r2_diario <- round(cor(df_diario$P_obs, df_diario$P_CM2)^2, 2)
  n_pares_diario <- nrow(df_diario)
  n_estaciones <- n_distinct(df_diario$codigo)
  label_diario <- paste0("R² = ", r2_diario, "\nEstaciones = ", n_estaciones, "\nN pares = ", n_pares_diario)

  p_diario <- ggplot(df_diario, aes(x = P_obs, y = P_CM2)) +
    geom_point(alpha = 0.4) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    geom_label(
      data = data.frame(P_obs = max(df_diario$P_obs, na.rm = TRUE),
                        P_CM2 = min(df_diario$P_CM2, na.rm = TRUE),
                        label = label_diario),
      aes(x = P_obs, y = P_CM2, label = label),
      hjust = 1.1, vjust = -0.5, size = 3.5,
      fill = "white", color = "black", label.size = 0.3
    ) +
    labs(title = "Diario", x = "P_obs", y = gcm_col) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black", fill = NA))

  df_mensual <- df %>%
    filter(!is.na(P_obs), !is.na(P_CM2)) %>%
    mutate(anio = year(fecha), mes = month(fecha)) %>%
    group_by(codigo, nombre_estacion, anio, mes) %>%
    summarise(P_obs = sum(P_obs, na.rm = TRUE),
              P_CM2 = sum(P_CM2, na.rm = TRUE), .groups = "drop")

  r2_mensual <- round(cor(df_mensual$P_obs, df_mensual$P_CM2)^2, 2)
  n_pares_mensual <- nrow(df_mensual)
  n_est_mensual <- n_distinct(df_mensual$codigo)
  label_mensual <- paste0("R² = ", r2_mensual, "\nEstaciones = ", n_est_mensual, "\nN pares = ", n_pares_mensual)

  p_mensual <- ggplot(df_mensual, aes(x = P_obs, y = P_CM2)) +
    geom_point(alpha = 0.7, color = "black") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    geom_label(
      data = data.frame(P_obs = max(df_mensual$P_obs, na.rm = TRUE),
                        P_CM2 = min(df_mensual$P_CM2, na.rm = TRUE),
                        label = label_mensual),
      aes(x = P_obs, y = P_CM2, label = label),
      hjust = 1.1, vjust = -0.5, size = 3.5,
      fill = "white", color = "black", label.size = 0.3
    ) +
    labs(title = "Mensual", x = "P_obs", y = gcm_col) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black", fill = NA))

  # --- Anual ---
  df_anual <- df %>%
    filter(!is.na(P_obs), !is.na(P_CM2)) %>%
    mutate(anio = year(fecha)) %>%
    group_by(codigo, nombre_estacion, anio) %>%
    summarise(P_obs = sum(P_obs, na.rm = TRUE),
              P_CM2 = sum(P_CM2, na.rm = TRUE), .groups = "drop")

  r2_anual <- round(cor(df_anual$P_obs, df_anual$P_CM2)^2, 2)
  n_pares_anual <- nrow(df_anual)
  n_est_anual <- n_distinct(df_anual$codigo)
  label_anual <- paste0("R² = ", r2_anual, "\nEstaciones = ", n_est_anual, "\nN pares = ", n_pares_anual)

  p_anual <- ggplot(df_anual, aes(x = P_obs, y = P_CM2)) +
    geom_point(alpha = 0.9, color = "black") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    geom_label(
      data = data.frame(P_obs = max(df_anual$P_obs, na.rm = TRUE),
                        P_CM2 = min(df_anual$P_CM2, na.rm = TRUE),
                        label = label_anual),
      aes(x = P_obs, y = P_CM2, label = label),
      hjust = 1.1, vjust = -0.5, size = 3.5,
      fill = "white", color = "black", label.size = 0.3
    ) +
    labs(title = "Anual", x = "P_obs", y = gcm_col) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black", fill = NA))

  # --- Panel final ---
  panel <- p_diario + p_mensual + p_anual +
    plot_annotation(title = titulo_general) &
    theme(plot.title = element_text(hjust = 0.5))

  return(panel)
}

plot_gcm = plot_gcm_vs_obs (df_estaciones_P_f)

print(plot_gcm)



#### VERIFICACION SERIES TEMPORALES ANUALES HISTORICA ####



df_estaciones_P <- df_estaciones_P %>%
  dplyr::mutate(
    fecha = as.Date(fecha),
    Año = lubridate::year(fecha)
  )

# estaciones ya filtradas para 1990-01-01 a 2018-01-01 

obtener_maximos_anuales <- function(df) {
  df <- df %>% dplyr::filter(!is.na(P_obs))  # Elimina filas donde valor es NA

  df_extremos <- df %>%
    dplyr::group_by(codigo, nombre_estacion, Año) %>%
    dplyr::summarise(
      maximo = max(P_obs, na.rm = TRUE),
      anual = sum(P_obs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()

  return(df_extremos)
}

f_ma_msd_name <- function(df_name, order, name) {
  require(forecast)
  require(zoo)
  require(Kendall)
  require(ggplot2)
  require(reshape2)
  require(dplyr)
  require(patchwork)

  # --- Preparar datos para máximos ---
  df_in <- df_name %>%
    dplyr::filter(nombre_estacion == name) %>%
    dplyr::select(Año, maximo, anual)

  df_ma_msd_max <- data.frame(
    Year = df_in$Año,
    SVE = as.numeric(df_in$maximo),
    ma = as.numeric(ma(df_in$maximo, order = order)),
    msd = as.numeric(rollapply(df_in$maximo, width = order, FUN = sd, fill = NA, align = "center"))
  )

  df_max_long <- reshape2::melt(df_ma_msd_max, id.vars = "Year")
  df_max_long$variable <- factor(df_max_long$variable,
                                 levels = c("SVE", "ma", "msd"),
                                 labels = c("Máximos anuales",
                                            "Media móvil máximos",
                                            "Desviación móvil máximos"))

  df_test_max <- data.frame(
    variable = levels(df_max_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_max$SVE)$sl,
      MannKendall(df_ma_msd_max$ma)$sl,
      MannKendall(df_ma_msd_max$msd)$sl
    ),
    x = quantile(df_ma_msd_max$Year, 0.05),
    y = c(
      quantile(na.omit(df_ma_msd_max$SVE), 0.975),
      quantile(na.omit(df_ma_msd_max$ma), 0.975),
      quantile(na.omit(df_ma_msd_max$msd), 0.975)
    )
  )
  df_test_max$variable <- factor(df_test_max$variable, levels = levels(df_max_long$variable))

  fig_max <- ggplot(df_max_long, aes(x = Year, y = value)) +
    geom_line(linewidth = 0.5) +
    geom_point() +
    facet_grid(variable ~ ., scales = "free_y") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
    geom_label(data = df_test_max,
               inherit.aes = FALSE,
               aes(x = x, y = y, label = paste0("p-valor M-K: ", round(p_valor, 3))),
               hjust = 0, size = 3,
               fill = "white", color = "black", label.size = 0.3) +
    labs(x = "Año", y = "[mm]") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))


  # --- Preparar datos para anuales ---
  df_ma_msd_anual <- data.frame(
    Year = df_in$Año,
    anual = as.numeric(df_in$anual),
    ma = as.numeric(ma(df_in$anual, order = order)),
    msd = as.numeric(rollapply(df_in$anual, width = order, FUN = sd, fill = NA, align = "center"))
  )

  df_anual_long <- reshape2::melt(df_ma_msd_anual, id.vars = "Year")
  df_anual_long$variable <- factor(df_anual_long$variable,
                                   levels = c("anual", "ma", "msd"),
                                   labels = c("Precipitación anual",
                                              "Media móvil anual",
                                              "Desviación móvil anual"))

  df_test_anual <- data.frame(
    variable = levels(df_anual_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_anual$anual)$sl,
      MannKendall(df_ma_msd_anual$ma)$sl,
      MannKendall(df_ma_msd_anual$msd)$sl
    ),
    x = quantile(df_ma_msd_anual$Year, 0.05),
    y = c(
      quantile(na.omit(df_ma_msd_anual$anual), 0.975),
      quantile(na.omit(df_ma_msd_anual$ma), 0.975),
      quantile(na.omit(df_ma_msd_anual$msd), 0.975)
    )
  )
  df_test_anual$variable <- factor(df_test_anual$variable, levels = levels(df_anual_long$variable))

  fig_anual <- ggplot(df_anual_long, aes(x = Year, y = value)) +
    geom_line(linewidth = 0.5) +
    geom_point() +
    facet_grid(variable ~ ., scales = "free_y") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
    geom_label(data = df_test_anual,
               inherit.aes = FALSE,
               aes(x = x, y = y, label = paste0("p-valor M-K: ", round(p_valor, 3))),
               hjust = 0, size = 3,
               fill = "white", color = "black", label.size = 0.3) +
    labs(x = "Año", y = "[mm]") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

  # --- Unir ambos gráficos en dos columnas ---
  final_plot <- fig_max + fig_anual +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(title = paste0("Series de Precipitacion Anual y Maxima Anual para Estación ", name)) &
    theme(plot.title = element_text(hjust = 0.5))

  return(list(
    df_max = df_ma_msd_max,
    df_anual = df_ma_msd_anual,
    fig = final_plot
  ))
}

df_maximos_anuales <- obtener_maximos_anuales(df_estaciones_P)

plot_tendencias_estacion <- f_ma_msd_name(df_maximos_anuales, order = 5, name = "Visviri")

print(plot_tendencias_estacion$fig)





#### VERIFICACION SERIES TEMPORALES ANUALES HISTORICA Y FUTURA ANTES CORRECCION #### 


obtener_maximos_anuales_gcm <- function(df, gcm, modo) {
  
  if (modo == "solo_obs") {
    # Filtrar solo años con datos observados
    df_filtrado <- df %>% dplyr::filter(!is.na(P_obs))
  } else if (modo == "todas") {
    # No se filtra por datos observados (se usan todos los años disponibles del GCM)
    df_filtrado <- df
  } else {
    stop("El parámetro 'modo' debe ser 'solo_obs' o 'todas'.")
  }

  # Cálculo de resumen
  df_obs <- df %>%
    dplyr::filter(!is.na(P_obs)) %>%
    dplyr::group_by(codigo, nombre_estacion, Año) %>%
    dplyr::summarise(
      maximo = max(P_obs, na.rm = TRUE),
      anual = sum(P_obs, na.rm = TRUE),
      .groups = "drop"
    )

  df_gcm <- df_filtrado %>%
    dplyr::group_by(codigo, nombre_estacion, Año) %>%
    dplyr::summarise(
      maximo_gcm = max(.data[[gcm]], na.rm = TRUE),
      anual_gcm = sum(.data[[gcm]], na.rm = TRUE),
      .groups = "drop"
    )

  # Unir: mantener todos los años del GCM si "todas", o los de obs si "solo_obs"
  df_extremos <- if (modo == "todas") {
    dplyr::left_join(df_gcm, df_obs, by = c("codigo", "nombre_estacion", "Año"))
  } else {
    dplyr::inner_join(df_gcm, df_obs, by = c("codigo", "nombre_estacion", "Año"))
  }

  return(as.data.frame(df_extremos))
}

f_ma_msd_name_gcm <- function(df_name, order, name) {
  require(forecast)
  require(zoo)
  require(Kendall)
  require(ggplot2)
  require(reshape2)
  require(dplyr)
  require(patchwork)

  # --- Filtrar datos por estación ---
  df_in <- df_name %>%
    dplyr::filter(nombre_estacion == name) %>%
    dplyr::select(Año, maximo, anual)

  df_in_gcm <- df_name %>%
    dplyr::filter(nombre_estacion == name) %>%
    dplyr::select(Año, maximo_gcm, anual_gcm)

  # --- Procesar máximos históricos ---
  df_ma_msd_max <- data.frame(
    Year = df_in$Año,
    SVE = as.numeric(df_in$maximo),
    ma = as.numeric(ma(df_in$maximo, order = order)),
    msd = as.numeric(rollapply(df_in$maximo, width = order, FUN = sd, fill = NA, align = "center"))
  )

  df_ma_msd_max_gcm <- data.frame(
    Year = df_in_gcm$Año,
    SVE = as.numeric(df_in_gcm$maximo_gcm),
    ma = as.numeric(ma(df_in_gcm$maximo_gcm, order = order)),
    msd = as.numeric(rollapply(df_in_gcm$maximo_gcm, width = order, FUN = sd, fill = NA, align = "center"))
  )

  # --- Datos en formato largo ---
  df_max_long <- melt(df_ma_msd_max, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("SVE", "ma", "msd"),
                             labels = c("Máximos anuales",
                                        "Media móvil máximos",
                                        "Desviación móvil máximos")))

  df_max_long_gcm <- melt(df_ma_msd_max_gcm, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("SVE", "ma", "msd"),
                             labels = c("Máximos anuales",
                                        "Media móvil máximos",
                                        "Desviación móvil máximos")))

  # --- Test de tendencia máximos ---
  df_test_max <- data.frame(
    variable = levels(df_max_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_max$SVE)$sl,
      MannKendall(df_ma_msd_max$ma)$sl,
      MannKendall(df_ma_msd_max$msd)$sl
    ),
    x = quantile(df_ma_msd_max$Year, 0.05),
    y = c(
      quantile(na.omit(df_ma_msd_max$SVE), 0.975),
      quantile(na.omit(df_ma_msd_max$ma), 0.975),
      quantile(na.omit(df_ma_msd_max$msd), 0.975)
    )
  )

  df_test_max_gcm <- data.frame(
    variable = levels(df_max_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_max_gcm$SVE)$sl,
      MannKendall(df_ma_msd_max_gcm$ma)$sl,
      MannKendall(df_ma_msd_max_gcm$msd)$sl
    ),
    x = quantile(df_ma_msd_max_gcm$Year, 0.95),
    y = c(
      quantile(na.omit(df_ma_msd_max_gcm$SVE), 0.90),
      quantile(na.omit(df_ma_msd_max_gcm$ma), 0.90),
      quantile(na.omit(df_ma_msd_max_gcm$msd), 0.90)
    )
  )

  df_test_max$variable <- factor(df_test_max$variable, levels = levels(df_max_long$variable))
  df_test_max_gcm$variable <- factor(df_test_max_gcm$variable, levels = levels(df_max_long$variable))

  # --- Gráfico máximos ---
  fig_max <- ggplot(df_max_long, aes(x = Year, y = value)) +
    geom_line(linewidth = 0.5) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
    geom_line(data = df_max_long_gcm, aes(x = Year, y = value), color = "red", linewidth = 0.5, inherit.aes = FALSE) +
    geom_point(data = df_max_long_gcm, aes(x = Year, y = value), color = "red", size = 1, alpha = 0.6, inherit.aes = FALSE) +
    geom_smooth(data = df_max_long_gcm, aes(x = Year, y = value), method = "lm", se = FALSE, color = "red", linewidth = 0.5, inherit.aes = FALSE) +
    facet_grid(variable ~ ., scales = "free_y") +
    geom_label(data = df_test_max,
               aes(x = x, y = y, label = paste0("p-valor M-K: ", round(p_valor, 3))),
               hjust = 0, size = 3, fill = "white", color = "black", label.size = 0.3, inherit.aes = FALSE) +
    geom_label(data = df_test_max_gcm,
               aes(x = x, y = y, label = paste0("p-valor GCM: ", round(p_valor, 3))),
               hjust = 1, size = 3, fill = "white", color = "red", label.size = 0.3, inherit.aes = FALSE) +
    labs(x = "Año", y = "[mm]") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

  # --- Procesar anuales ---
  df_ma_msd_anual <- data.frame(
    Year = df_in$Año,
    anual = as.numeric(df_in$anual),
    ma = as.numeric(ma(df_in$anual, order = order)),
    msd = as.numeric(rollapply(df_in$anual, width = order, FUN = sd, fill = NA, align = "center"))
  )

  df_ma_msd_anual_gcm <- data.frame(
    Year = df_in_gcm$Año,
    anual = as.numeric(df_in_gcm$anual_gcm),
    ma = as.numeric(ma(df_in_gcm$anual_gcm, order = order)),
    msd = as.numeric(rollapply(df_in_gcm$anual_gcm, width = order, FUN = sd, fill = NA, align = "center"))
  )

  # --- Datos en formato largo ---
  df_anual_long <- melt(df_ma_msd_anual, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("anual", "ma", "msd"),
                             labels = c("Precipitación anual",
                                        "Media móvil anual",
                                        "Desviación móvil anual")))

  df_anual_long_gcm <- melt(df_ma_msd_anual_gcm, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("anual", "ma", "msd"),
                             labels = c("Precipitación anual",
                                        "Media móvil anual",
                                        "Desviación móvil anual")))

  # --- Test de tendencia anual ---
  df_test_anual <- data.frame(
    variable = levels(df_anual_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_anual$anual)$sl,
      MannKendall(df_ma_msd_anual$ma)$sl,
      MannKendall(df_ma_msd_anual$msd)$sl
    ),
    x = quantile(df_ma_msd_anual$Year, 0.05),
    y = c(
      quantile(na.omit(df_ma_msd_anual$anual), 0.975),
      quantile(na.omit(df_ma_msd_anual$ma), 0.975),
      quantile(na.omit(df_ma_msd_anual$msd), 0.975)
    )
  )

  df_test_anual_gcm <- data.frame(
    variable = levels(df_anual_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_anual_gcm$anual)$sl,
      MannKendall(df_ma_msd_anual_gcm$ma)$sl,
      MannKendall(df_ma_msd_anual_gcm$msd)$sl
    ),
    x = quantile(df_ma_msd_anual_gcm$Year, 0.95),
    y = c(
      quantile(na.omit(df_ma_msd_anual_gcm$anual), 0.90),
quantile(na.omit(df_ma_msd_anual_gcm$ma), 0.90),
quantile(na.omit(df_ma_msd_anual_gcm$msd), 0.90)
)
)

df_test_anual$variable <- factor(df_test_anual$variable, levels = levels(df_anual_long$variable))
df_test_anual_gcm$variable <- factor(df_test_anual_gcm$variable, levels = levels(df_anual_long$variable))


fig_anual <- ggplot(df_anual_long, aes(x = Year, y = value)) +
geom_line(linewidth = 0.5) +
geom_point() +
geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
geom_line(data = df_anual_long_gcm, aes(x = Year, y = value), color = "red", linewidth = 0.5, inherit.aes = FALSE) +
geom_point(data = df_anual_long_gcm, aes(x = Year, y = value), color = "red", size = 1, alpha = 0.6, inherit.aes = FALSE) +
geom_smooth(data = df_anual_long_gcm, aes(x = Year, y = value), method = "lm", se = FALSE, color = "red", linewidth = 0.5, inherit.aes = FALSE) +
facet_grid(variable ~ ., scales = "free_y") +
geom_label(data = df_test_anual,
aes(x = x, y = y, label = paste0("p-valor M-K: ", round(p_valor, 3))),
hjust = 0, size = 3, fill = "white", color = "black", label.size = 0.3, inherit.aes = FALSE) +
geom_label(data = df_test_anual_gcm,
aes(x = x, y = y, label = paste0("p-valor GCM: ", round(p_valor, 3))),
hjust = 1, size = 3, fill = "white", color = "red", label.size = 0.3, inherit.aes = FALSE) +
labs(x = "Año", y = "[mm]") +
theme_bw() +
theme(strip.background = element_rect(fill = "white"))

final_plot <- fig_max + fig_anual +
plot_layout(ncol = 2, guides = "collect") +
plot_annotation(title = paste0("Series de Precipitación Anual y Máxima Anual para Estación ", name)) &
theme(plot.title = element_text(hjust = 0.5))

return(list(
df_max = df_ma_msd_max,
df_anual = df_ma_msd_anual,
fig = final_plot
))
}


df_maximos_anuales_gcm <- obtener_maximos_anuales_gcm(df_estaciones_P, "pr_MIROC-ES2L" , "solo_obs")


df_maximos_anuales_gcm <- obtener_maximos_anuales_gcm(df_estaciones_P, "pr_MIROC-ES2L" , "todas")


plot_tendencias_estacion <- f_ma_msd_name_gcm(df_maximos_anuales_gcm, order = 5, name = "Visviri")


print(plot_tendencias_estacion$fig)


#### DATOS TOPOGRAFICOS EN PIXEL POR ESTACION #### 


# DESCARGAR DEM 

# 
# library(sf)
# library(raster)
# library(terra)
# library(rgee)
# library(exactextractr)
# 
# # Inicializar GEE
# # ee_Initialize()
# 
# ee_Initialize(user = 'ndef', drive = TRUE)
# 
# 
# # ee_Authenticate()
# 
# cuenca_ee <- sf_as_ee(polygon_sf)
# 
# # Cargar y recortar DEM SRTM
# 
# dem <- ee$Image("USGS/SRTMGL1_003")
# dem_clip <- dem$clip(cuenca_ee)
# 
# # Descargar el DEM como raster "SpatRaster" de terra y guardar en disco
# dem_rast <- ee_as_rast(
#   image = dem_clip,
#   region = cuenca_ee$geometry(),
#   scale = 30
# )
# 
# # Guardar el SpatRaster como TIFF local
# 
# writeRaster(dem_rast, filename = "dem_polygon_sp.tif", overwrite = TRUE)
# 
# # Confirmar
# print(dem_rast)



# # LEER EL RASTER YA DESCARGADO 
# Obtener directorio de trabajo actual
dir_actual <- getwd()

# Nombre del archivo raster
nombre_raster <- "dem_polygon_sp.tif"

# Ruta completa
ruta_completa <- file.path(dir_actual, nombre_raster)

# Leer el raster
dem_rast <- terra::rast(ruta_completa)  # usa 'raster(ruta_completa)' si usas el paquete 'raster'

# # Confirmar
# print(dem_rast_leido)
# plot(dem_rast_leido)  # opcional: visualizar el raster




require(terra)
head(df_estaciones_P)


# Asegurar que latitud y longitud estén como numéricos

df_estaciones_coords <- df_estaciones_P %>%
  distinct(codigo, nombre_estacion, latitud, longitud) %>%
  mutate(
    latitud = as.numeric(latitud),
    longitud = as.numeric(longitud)
  )

# 1. Crear puntos espaciales (SpatVector)

estaciones_pts <- vect(df_estaciones_coords, geom = c("longitud", "latitud"), crs = crs(dem_rast))

# 2. Calcular pendiente y aspecto del DEM
# Calcular pendiente y aspecto en grados
slope_rast  <- terrain(dem_rast, v = "slope", unit = "degrees")
aspect_rast <- terrain(dem_rast, v = "aspect", unit = "degrees")

# 3. Extraer valores para cada estación (por pixel)
altitud_vals <- extract(dem_rast, estaciones_pts)[, 2]   # segunda columna = valor
slope_vals   <- extract(slope_rast, estaciones_pts)[, 2]
aspect_vals  <- extract(aspect_rast, estaciones_pts)[, 2]

# 4. Añadir al dataframe original
df_estaciones_coords <- df_estaciones_coords %>%
  mutate(
    altitud_dem = altitud_vals,
    pendiente   = slope_vals,
    aspecto     = aspect_vals
  )

# Mostrar resultado
print(df_estaciones_coords)



df_estaciones_P <- df_estaciones_P %>%
  left_join(df_estaciones_coords %>% 
              dplyr::select(nombre_estacion, altitud_dem, pendiente, aspecto),
            by = "nombre_estacion")


#### DISTANCIA A COSTA  ####


library(rnaturalearth)
library(sf)
library(terra)

# Descargar línea costera de Chile (como sf)
costa_chile <- ne_countries(scale = "medium", country = "Chile", returnclass = "sf")

# Extraer la geometría de la costa como objeto sf (borde del polígono)
costa_linea <- st_cast(st_geometry(costa_chile), "MULTILINESTRING")

# Asegurar que están en sf con CRS compatible (WGS84)
estaciones_sf <- st_as_sf(df_estaciones_coords, coords = c("longitud", "latitud"), crs = 4326)

# Distancia en metros (usa haversine al usar crs 4326)
distancias_m <- st_distance(estaciones_sf, costa_linea)

# Convertir a vector numérico y agregar al dataframe
df_estaciones_coords$distancia_costa_m <- as.numeric(distancias_m)

df_estaciones_P <- df_estaciones_P %>%
  left_join(df_estaciones_coords %>% 
              dplyr::select(nombre_estacion, distancia_costa_m),
            by = "nombre_estacion")

head(df_estaciones_P %>% 
        dplyr::select(nombre_estacion, latitud, longitud, distancia_costa_m))



#### METRICAS PARA TODOS LOS GCM ####


library(dplyr)
library(tidyr)
library(lubridate)

procesar_periodo <- function(df, inicio, fin) {
  
  # Filtrar rango de fechas y asignar año hidrológico
  df_sub <- df %>%
    dplyr::filter(fecha >= as.Date(paste0(inicio, "-04-01")) &
             fecha <= as.Date(paste0(fin, "-03-31"))) %>%
    mutate(
      anio_hidrologico = if_else(month(fecha) >= 4,
                                year(fecha),
                                year(fecha) - 1),
      mes = month(fecha)
    )
  
  # Pivotear columnas de pr_ y P_obs a formato largo
  df_long <- df_sub %>%
    pivot_longer(
      cols = starts_with("pr_") | starts_with("P_obs"),
      names_to = "modelo",
      values_to = "prec"
    )
  
  # Contar días válidos por mes (>=20 días)
  valid_dias_mes <- df_long %>%
    group_by(codigo, anio_hidrologico, mes, modelo) %>%
    summarise(dias_validos = sum(!is.na(prec)), .groups = "drop") %>%
     dplyr::filter(dias_validos >= 20)
  
  # Identificar años hidrológicos completos (12 meses válidos)
  valid_years <- valid_dias_mes %>%
    group_by(codigo, anio_hidrologico, modelo) %>%
    summarise(meses_validos = n(), .groups = "drop") %>%
     dplyr::filter(meses_validos == 12)
  
  # Suma mensual solo para meses válidos
  df_mes_valido <- df_long %>%
    inner_join(valid_dias_mes, by = c("codigo", "anio_hidrologico", "mes", "modelo")) %>%
    group_by(codigo, nombre_estacion, latitud, longitud, altitud,
             fecha_inicio, fecha_termino, anio_hidrologico, mes, modelo) %>%
    summarise(prec_mensual = sum(prec, na.rm = TRUE), .groups = "drop")
  
  # Suma anual solo para años completos y meses válidos
  df_anio_valido <- df_mes_valido %>%
    inner_join(valid_years %>% dplyr::select(codigo, anio_hidrologico, modelo),
               by = c("codigo", "anio_hidrologico", "modelo")) %>%
    group_by(codigo, nombre_estacion, latitud, longitud, altitud,
             fecha_inicio, fecha_termino, anio_hidrologico, modelo) %>%
    summarise(prec_anual = sum(prec_mensual, na.rm = TRUE), .groups = "drop")
  
  # Completitud observada anual (para P_obs)
  completitud_obs <- df_sub %>%
    group_by(codigo, nombre_estacion, latitud, longitud, altitud,
             fecha_inicio, fecha_termino, anio_hidrologico) %>%
    summarise(
      dias_con_dato = sum(!is.na(P_obs)),
      dias_total = n(),
      completitud_P_obs = dias_con_dato / dias_total,
      .groups = "drop"
    )
  
  # Pivotear anual a formato ancho
  df_anio_wide <- df_anio_valido %>%
    pivot_wider(names_from = modelo, values_from = prec_anual)
  
  # Sufijo _anual a columnas de precipitación
  col_base <- c("codigo", "nombre_estacion", "latitud", "longitud", "altitud",
                "fecha_inicio", "fecha_termino", "anio_hidrologico")
  cols_prec <- setdiff(names(df_anio_wide), col_base)
  names(df_anio_wide)[names(df_anio_wide) %in% cols_prec] <- paste0(cols_prec, "_anual")
  
  # Unir completitud
  df_final_anual <- left_join(df_anio_wide, completitud_obs,
                              by = c("codigo", "nombre_estacion", "latitud", "longitud",
                                     "altitud", "fecha_inicio", "fecha_termino", "anio_hidrologico"))
  
  # Limpiar nombres (guiones a guion bajo)
  names(df_final_anual) <- gsub("-", "_", names(df_final_anual))
  
  return(list(
    df_mensual = df_mes_valido,
    df_anual = df_final_anual
  ))
}


calcular_metricas <- function(df_anual) {
  
  metricas <- df_anual %>%
    tidyr::pivot_longer(
      cols = ends_with("_anual") & !ends_with("P_obs_anual"),
      names_to = "modelo",
      values_to = "prec_modelo"
    ) %>%
    mutate(modelo = gsub("_anual$", "", modelo)) %>%
     dplyr::filter(!is.na(P_obs_anual), !is.na(prec_modelo)) %>%
    group_by(codigo, nombre_estacion, modelo) %>%
    summarise(
      R2 = cor(P_obs_anual, prec_modelo, use = "complete.obs")^2,
      PBIAS = 100 * sum(prec_modelo - P_obs_anual, na.rm = TRUE) / sum(P_obs_anual, na.rm = TRUE),
      n_anios = n(),
      .groups = "drop"
    )
  
  return(metricas)
}


# Procesar periodos

resultados <- procesar_periodo(df_estaciones_P, inicio = 1970, fin = 1980)
df_anual <- resultados$df_anual
df_mensual <- resultados$df_mensual


df_anual <- resultados$df_anual %>%
  dplyr::filter(!is.na(P_obs_anual))

df_mensual <- resultados$df_mensual %>%
 dplyr::filter(!is.na(P_obs_anual))

df_mensual<- resultados$df_mensual %>%
   dplyr::filter(modelo == "P_obs", !is.na(prec_mensual))


# Calcular métricas anuales
metricas <- calcular_metricas(df_anual)

print(metricas)


resumen_R2_por_estacion <- metricas %>%
  group_by(codigo, nombre_estacion) %>%
  summarise(
    R2_promedio = mean(R2, na.rm = TRUE),
    R2_mediana = median(R2, na.rm = TRUE),
    n_modelos = n(),
    .groups = "drop"
  )

print(resumen_R2_por_estacion)







#### PROCESAMIENTO ANUAL ####


# 
# saveRDS(df_estaciones_P, file = "df_estaciones_P.rds") # si hay prblemas al correr los procesos de ram con cannot allocate vector of size
# se recomienda ir guardando resultados en RDS e ir leyendo estos resultados solo cuando se necesiten y correr el codigo parceladamente


df_estaciones_P <- readRDS("df_estaciones_P.rds")

head(df_estaciones_P)



# 1. Convertir fecha a Date y crear año hidrológico
df_estaciones_P <- df_estaciones_P %>%
  mutate(
    fecha = as.Date(fecha),
    anio_hidrologico = ifelse(month(fecha) >= 4, year(fecha), year(fecha) - 1)
  )

# 2. Función para filtrar y calcular anuales por subperiodo




# Extract static station data once
estaticos_estacion <- df_estaciones_P %>%
  distinct(codigo, nombre_estacion, latitud, longitud, altitud, fecha_inicio,
           fecha_termino, altitud_dem, pendiente, aspecto, distancia_costa_m)

# 2. Función para filtrar y calcular anuales por subperiodo


procesar_periodo <- function(df, inicio, fin, nro_comp, estaticos_estacion_df) {

  columnas_modelos <- base::names(df)[base::grepl("^pr_|^P_obs$", base::names(df))]
  columnas_estaticas <- base::setdiff(base::names(df), c("fecha", columnas_modelos, "anio_hidrologico", "mes"))
  if (!"nombre_estacion" %in% columnas_estaticas && "nombre_estacion" %in% base::names(df)) {
    columnas_estaticas <- c(columnas_estaticas, "nombre_estacion")
  }

  df_sub <- dplyr::filter(
    df,
    fecha >= base::as.Date(base::paste0(inicio, "-04-01")) &
      fecha <= base::as.Date(base::paste0(fin, "-03-31"))
  ) %>%
    dplyr::mutate(
      anio_hidrologico = dplyr::if_else(lubridate::month(fecha) >= 4,
                                        lubridate::year(fecha),
                                        lubridate::year(fecha) - 1),
      mes = lubridate::month(fecha)
    )

  df_long <- tidyr::pivot_longer(df_sub, cols = dplyr::all_of(columnas_modelos),
                                 names_to = "modelo", values_to = "prec")

  valid_dias_mes <- df_long %>%
    dplyr::group_by(codigo, anio_hidrologico, mes, modelo) %>%
    dplyr::summarise(dias_validos = sum(!is.na(prec)), .groups = "drop") %>%
    dplyr::filter(dias_validos >= 15)

  valid_years <- valid_dias_mes %>%
    dplyr::group_by(codigo, anio_hidrologico, modelo) %>%
    dplyr::summarise(meses_validos = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(meses_validos == 12)

  df_valido <- df_long %>%
    dplyr::inner_join(valid_dias_mes, by = c("codigo", "anio_hidrologico", "mes", "modelo")) %>%
    dplyr::inner_join(valid_years %>% dplyr::select(codigo, anio_hidrologico, modelo),
                      by = c("codigo", "anio_hidrologico", "modelo")) %>%
    dplyr::group_by(codigo, anio_hidrologico, modelo) %>%
    dplyr::summarise(prec_anual = sum(prec, na.rm = TRUE), .groups = "drop")

  completitud_obs <- df_sub %>%
    dplyr::group_by(codigo, anio_hidrologico) %>%
    dplyr::summarise(
      dias_con_dato = sum(!is.na(P_obs)),
      dias_total = dplyr::n(),
      completitud_P_obs = dias_con_dato / dias_total,
      .groups = "drop"
    ) %>%
    dplyr::left_join(estaticos_estacion_df, by = "codigo")

  df_prec <- tidyr::pivot_wider(df_valido, names_from = "modelo", values_from = "prec_anual")

  col_renombrar <- names(df_prec)[grepl("^pr_|^P_obs$", names(df_prec))]
  if (length(col_renombrar) > 0) {
    nombres_limpios <- gsub("-", "_", col_renombrar)
    nombres_limpios <- gsub("\\.", "_", nombres_limpios)
    nombres_finales <- paste0(nombres_limpios, "_anual")
    names(df_prec)[match(col_renombrar, names(df_prec))] <- nombres_finales
  }

  df_prec <- dplyr::distinct(df_prec, codigo, anio_hidrologico, .keep_all = TRUE)

  df_final <- dplyr::left_join(df_prec, completitud_obs, by = c("codigo", "anio_hidrologico"))

  df_con_na <- dplyr::filter(df_final, is.na(completitud_P_obs))
  df_sin_na <- dplyr::filter(df_final, !is.na(completitud_P_obs))

  if (!"nombre_estacion" %in% names(df_sin_na)) {
    stop("Falta columna 'nombre_estacion' en df_sin_na")
  }

  estaciones_validas <- dplyr::distinct(df_sin_na, codigo, nombre_estacion)
  na_por_estacion <- df_con_na %>%
    dplyr::semi_join(estaciones_validas, by = c("codigo", "nombre_estacion")) %>%
    dplyr::count(codigo, nombre_estacion, name = "anios_con_completitud_P_obs_NA")

  # Seleccionar solo columnas de modelos pr_, excluir P_obs_anual del PCA
  gcm_cols <- df_sin_na %>%
    dplyr::select(dplyr::ends_with("_anual")) %>%
    names()
  gcm_cols <- gcm_cols[grepl("^pr_", gsub("_anual$", "", gcm_cols))]

  df_pca_input <- df_sin_na %>%
    dplyr::filter(dplyr::if_all(all_of(gcm_cols), ~ !is.na(.) & !is.nan(.) & !is.infinite(.)))

  all_static_cols_present_in_final <- union(
    intersect(names(estaticos_estacion_df), names(df_sin_na)),
    c("codigo", "anio_hidrologico", "completitud_P_obs", "P_obs_anual")
  )

  if (nrow(df_pca_input) > 2) {
    pca_result <- tryCatch({
      stats::prcomp(df_pca_input[, gcm_cols], center = TRUE, scale. = TRUE)
    }, error = function(e) {
      message("Error en PCA: ", e$message)
      return(NULL)
    })

    if (!is.null(pca_result)) {
      nro_comp_ajust <- min(nro_comp, ncol(pca_result$x))
      pca_scores <- as.data.frame(pca_result$x[, seq_len(nro_comp_ajust)])
      names(pca_scores) <- paste0("PC", seq_len(ncol(pca_scores)))

      df_anual_PCA <- dplyr::bind_cols(
        dplyr::select(df_pca_input, dplyr::all_of(all_static_cols_present_in_final)),
        tibble::as_tibble(pca_scores)
      )

      df_anual_PCA <- df_anual_PCA %>% dplyr::filter(!is.na(P_obs_anual))

      var_exp <- pca_result$sdev^2
      var_exp_prop <- var_exp / sum(var_exp)
      var_exp_df <- data.frame(
        Componente = paste0("PC", seq_along(var_exp_prop)),
        Varianza = var_exp_prop,
        Acumulada = cumsum(var_exp_prop)
      )

      pca_plot <- ggplot2::ggplot(var_exp_df, ggplot2::aes(x = seq_along(Varianza), y = Acumulada)) +
        ggplot2::geom_line(color = "black") +
        ggplot2::geom_point(color = "black") +
        ggplot2::scale_x_continuous(breaks = seq_along(var_exp_prop)) +
        ggplot2::labs(
          x = "Componente Principal",
          y = "Varianza Explicada Acumulada",
          title = "Varianza Acumulada por Componente"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8)
        )
    } else {
      df_anual_PCA <- NULL
      pca_plot <- ggplot2::ggplot() + ggplot2::labs(title = "Error en PCA")
    }
  } else {
    df_anual_PCA <- NULL
    pca_result <- NULL
    pca_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No hay suficientes datos para PCA")
  }

  df_anual <- df_sin_na %>%
    dplyr::select(all_of(all_static_cols_present_in_final), all_of(gcm_cols)) %>%
    dplyr::filter(!is.na(P_obs_anual))

  return(list(
    df_sin_na = df_sin_na,
    df_con_na = df_con_na,
    anios_na = na_por_estacion,
    pca = pca_result,
    df_anual_PCA = df_anual_PCA,
    df_anual = df_anual,
    pca_plot = pca_plot
  ))
}


# Crear copia del dataframe original
df_estaciones_P_f <- df_estaciones_P

# Quitar completamente la columna del modelo específico
df_estaciones_P_f  <- df_estaciones_P_f %>%
  dplyr::select(-`pr_MPI-ESM-MR-RegCM4-10k`)


# 3. Aplicar la función a cada subperiodo

df_periodo_1_anual <- procesar_periodo(df_estaciones_P_f, 1970, 1980, 10, estaticos_estacion)
df_periodo_2_anual <- procesar_periodo(df_estaciones_P, 1980, 1990, 10, estaticos_estacion)
df_periodo_3_anual <- procesar_periodo(df_estaciones_P, 1990, 2000, 10, estaticos_estacion)
df_periodo_4_anual <- procesar_periodo(df_estaciones_P, 2000, 2010, 10, estaticos_estacion)
df_periodo_5_anual <- procesar_periodo(df_estaciones_P, 2010, 2020, 10, estaticos_estacion)

View(df_estaciones_P)

# tomar anuales 

df_estaciones_P_1_anual <- df_periodo_1_anual$df_anual
df_estaciones_P_2_anual <- df_periodo_2_anual$df_anual
df_estaciones_P_3_anual <- df_periodo_3_anual$df_anual
df_estaciones_P_4_anual <- df_periodo_4_anual$df_anual
df_estaciones_P_5_anual <- df_periodo_5_anual$df_anual


# tomar anuales pca

df_estaciones_P_1_anual_pca <- df_periodo_1_anual$df_anual_PCA
df_estaciones_P_2_anual_pca <- df_periodo_2_anual$df_anual_PCA
df_estaciones_P_3_anual_pca <- df_periodo_3_anual$df_anual_PCA
df_estaciones_P_4_anual_pca <- df_periodo_4_anual$df_anual_PCA
df_estaciones_P_5_anual_pca <- df_periodo_5_anual$df_anual_PCA


View(df_estaciones_P_1_anual_pca)


df_list <- list(
  P1 = list(df_anual_PCA = df_estaciones_P_1_anual_pca),
  P2 = list(df_anual_PCA = df_estaciones_P_2_anual_pca),
  P3 = list(df_anual_PCA = df_estaciones_P_3_anual_pca),
  P4 = list(df_anual_PCA = df_estaciones_P_4_anual_pca),
  P5 = list(df_anual_PCA = df_estaciones_P_5_anual_pca)
)


# Guardar como archivo .rds
saveRDS(df_list, file = "df_list_PCA.rds")





#### PCA ANUALES POR PERIODO Y REPORT FALTANTES #### 

# df_periodo_1_anual$df_mensual
# 
# df_periodo_1_anual$na_por_estacion
# 

# df_periodo_1_anual$pca_plot
# df_periodo_1_anual$pca_modelo

# View(df_periodo_1_anual)

# PCA ANUALES POR PERIODO 

get_components_needed <- function(df_data, threshold = 0.975, columna_varianza = "Acumulada") {
  sum(df_data[[columna_varianza]] < threshold) + 1
}

n_comp_1 <- get_components_needed(df_periodo_1_anual$pca_plot$data)
n_comp_2 <- get_components_needed(df_periodo_2_anual$pca_plot$data)
n_comp_3 <- get_components_needed(df_periodo_3_anual$pca_plot$data)
n_comp_4 <- get_components_needed(df_periodo_4_anual$pca_plot$data)
n_comp_5 <- get_components_needed(df_periodo_5_anual$pca_plot$data)

columna_varianza <- "Acumulada"

str(df_periodo_1_anual$pca_plot$data)

y_vals <- c(
  df_periodo_1_anual$pca_plot$data[[columna_varianza]],
  df_periodo_2_anual$pca_plot$data[[columna_varianza]],
  df_periodo_3_anual$pca_plot$data[[columna_varianza]],
  df_periodo_4_anual$pca_plot$data[[columna_varianza]],
  df_periodo_5_anual$pca_plot$data[[columna_varianza]]
)

y_limits <- c(min(y_vals, na.rm = TRUE), max(y_vals, na.rm = TRUE))


common_margin <- ggplot2::margin(5, 5, 5, 5)


max_components <- max(
  nrow(df_periodo_1_anual$pca_plot$data),
  nrow(df_periodo_2_anual$pca_plot$data),
  nrow(df_periodo_3_anual$pca_plot$data),
  nrow(df_periodo_4_anual$pca_plot$data),
  nrow(df_periodo_5_anual$pca_plot$data)
)

x_breaks <- seq(1, max_components, by = 4)

p1 <- df_periodo_1_anual$pca_plot +
  coord_cartesian(ylim = y_limits) +
  scale_x_continuous(breaks = x_breaks) +  # <-- Aquí defines ticks cada 4
  theme(
    axis.title.x = element_blank(),     
    axis.text.x = element_text(),
    plot.margin = common_margin
  ) +
  labs(title = paste("K comp. | var_acum(K) ≥ 97.5%. K = ", n_comp_1))

p2 <- df_periodo_2_anual$pca_plot +
  coord_cartesian(ylim = y_limits) +
  scale_x_continuous(breaks = x_breaks) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(),
    axis.text.y = element_blank(),
    plot.margin = common_margin
  ) +
  labs(title = paste("K comp. | var_acum(K) ≥ 97.5%. K = ", n_comp_2))

p3 <- df_periodo_3_anual$pca_plot +
  coord_cartesian(ylim = y_limits) +
  scale_x_continuous(breaks = x_breaks) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(),
    axis.text.x = element_text(),
    plot.margin = common_margin
  ) +
  labs(title = paste("K comp. | var_acum(K) ≥ 97.5%. K = ", n_comp_3),
       x = "Componente Principal")

p4 <- df_periodo_4_anual$pca_plot +
  coord_cartesian(ylim = y_limits) +
  scale_x_continuous(breaks = x_breaks) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(),
    axis.text.y = element_blank(),
    plot.margin = common_margin
  ) +
  labs(title = paste("K comp. | var_acum(K) ≥ 97.5%. K = ", n_comp_4))

p5 <- df_periodo_5_anual$pca_plot +
  coord_cartesian(ylim = y_limits) +
  scale_x_continuous(breaks = x_breaks) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(),
    axis.text.y = element_blank(),
    plot.margin = common_margin
  ) +
  labs(title = paste("K comp. | var_acum(K) ≥ 97.5%. K = ", n_comp_5))

panel_final <- (p1 | p2 | p3 | p4 | p5) +
  patchwork::plot_layout(ncol = 5, guides = "collect") +
  patchwork::plot_annotation(
    title = "PCA por periodo. 10 años hidrológicos por periodo. 1970 - 2020 de izquierda a derecha",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14)
    )
  )

print(panel_final)







file_path <- "C:/GITHUB/00_imagenes/PCA.png"

width_px <- 1870
height_px <- 714

dpi <- 96

ggsave(
  filename = file_path,
  plot = panel_final,
  width = width_px,
  height = height_px,
  units = "px",
  dpi = dpi
)

print(paste("Plot saved to:", file_path))













#### AJUSTAR RF CASO BASE SOLO P1 ####


df_periodo_1_anual$df_anual

require(randomForest)

# Dividir datos en train (40%) y test (60%)

dividir_train_test <- function(df, prop_entrenamiento = 0.4, seed = 123) {
  
  # 1. Filtrar filas con datos en la variable objetivo y sin NA en predictores
  # df_filtrado <- df %>% 
  #   dplyr::filter(!is.na(P_obs_anual)) %>%
  #   tidyr::drop_na()  # Elimina filas con cualquier NA YA SE FILTRO ANTERIORMENTE

  # 2. Obtener lista de estaciones únicas
  estaciones <- df %>%
    dplyr::distinct(nombre_estacion) %>%
    dplyr::pull(nombre_estacion)

  # 3. Seleccionar aleatoriamente estaciones para entrenamiento
  
  set.seed(seed)
  
  estaciones_train <- sample(estaciones, size = floor(prop_entrenamiento * length(estaciones)))

  # 4. Crear datasets
  
  train_data <- df %>%
    dplyr::filter(nombre_estacion %in% estaciones_train)

  test_data <- df %>%
    dplyr::filter(!nombre_estacion %in% estaciones_train)

  return(list(train = train_data, test = test_data))
  
}

# Usar la función

listas_partidas <- dividir_train_test(df_estaciones_P_1_anual_pca, prop_entrenamiento = 0.4)

# Resultado final:

train_data <- listas_partidas$train
test_data  <- listas_partidas$test


# Entrenar modelo de Random Forest


rf_model <- randomForest::randomForest(
  P_obs_anual ~ altitud_dem + pendiente + aspecto + distancia_costa_m + 
    latitud + longitud + completitud_P_obs +
    PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
  data = train_data,
  importance = TRUE,
  ntree = 500
)


# Predecir sobre test

test_data$pred_rf <- predict(rf_model, newdata = test_data)
train_data$pred_rf <- predict(rf_model, newdata = train_data)


# Evaluación del modelo

r2_test <- cor(test_data$P_obs_anual, test_data$pred_rf)^2
cat("R² en conjunto de prueba:", round(r2_test, 3), "\n")

# Importancia de variables

print(randomForest::importance(rf_model))

train_data$pred_rf <- predict(rf_model, newdata = train_data)
train_data$conjunto <- "train"
test_data$conjunto  <- "test"


# R² crudo (sin corrección)

r2_crudo <- cor(df_estaciones_P_1_anual$P_obs_anual,df_estaciones_P_1_anual$pr_ACCESS_CM2_anual, use = "complete.obs")^2

# R² test
r2_test <- cor(test_data$P_obs_anual, test_data$pred_rf, use = "complete.obs")^2

# R² train
r2_train <- cor(train_data$P_obs_anual, train_data$pred_rf, use = "complete.obs")^2


r2_test_2 <- cor(train_data$P_obs_anual, train_data$pred_rf, use = "complete.obs")^2

r2_test_3 <- cor(train_data$P_obs_anual, train_data$pred_rf, use = "complete.obs")^2

r2_test_4 <- cor(train_data$P_obs_anual, train_data$pred_rf, use = "complete.obs")^2

r2_test_5 <- cor(train_data$P_obs_anual, train_data$pred_rf, use = "complete.obs")^2



# Calcular PBIAS
pbias_crudo <- 100 * sum(df_estaciones_P_1_anual$pr_ACCESS_CM2_anual - df_estaciones_P_1_anual$P_obs_anual, na.rm = TRUE) /
  sum(df_estaciones_P_1_anual$P_obs_anual, na.rm = TRUE)

pbias_train <- 100 * sum(train_data$pred_rf - train_data$P_obs_anual, na.rm = TRUE) /
  sum(train_data$P_obs_anual, na.rm = TRUE)

pbias_test <- 100 * sum(test_data$pred_rf - test_data$P_obs_anual, na.rm = TRUE) /
  sum(test_data$P_obs_anual, na.rm = TRUE)

# Estilo común para bordes
tema_con_borde <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# Gráfico 1: Crudo (sin corrección)
plot_crudo <- ggplot(df_estaciones_P_1_anual) +
  geom_point(aes(x = P_obs_anual, y = pr_ACCESS_CM2_anual), color = "black", alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "GCM sin corrección: ACCESS-CM2",
    subtitle = paste0("R² = ", round(r2_crudo, 3), " | PBIAS = ", round(pbias_crudo, 1), "%"),
    x = "P_obs_anual",
    y = "P_GCM_anual"
  ) +
  tema_con_borde

# Gráfico 2: Train
plot_train <- ggplot(train_data) +
  geom_point(aes(x = P_obs_anual, y = pred_rf), color = "black", alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "GCM corregido - Train",
    subtitle = paste0("R² = ", round(r2_train, 3), " | PBIAS = ", round(pbias_train, 1), "%"),
    x = "P_obs_anual",
    y = "P_GCM_anual_corr"
  ) +
  tema_con_borde

# Gráfico 3: Test
plot_test <- ggplot(test_data) +
  geom_point(aes(x = P_obs_anual, y = pred_rf), color = "black", alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "GCM corregido - Test",
    subtitle = paste0("R² = ", round(r2_test, 3), " | PBIAS = ", round(pbias_test, 1), "%"),
    x = "P_obs_anual",
    y = "P_GCM_anual_corr"
  ) +
  tema_con_borde


plot_test <- ggplot(test_data) +
  geom_point(aes(x = P_obs_anual, y = pred_rf), color = "black", alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "GCM corregido - Test",
    subtitle = paste0("R² = ", round(r2_test, 3), " | PBIAS = ", round(pbias_test, 1), "%"),
    x = "P_obs_anual",
    y = "P_GCM_anual_corr"
  ) +
  tema_con_borde


library(patchwork)

panel <- plot_crudo + plot_train + plot_test +
  patchwork::plot_layout(ncol = 3) +
  patchwork::plot_annotation(
    title = "GCM corregido años hidrológicos 1970 - 1980"
  ) & theme(
    plot.title = element_text(hjust = 0.5)
  )

print(panel)





#### VALIDACION ESPACIOTEMPORAL SIN VARIAR ENTRENAMIENTO #### 



dividir_train_test <- function(df, prop, seed) {
  set.seed(seed)
  est <- unique(df$nombre_estacion)
  train_est <- sample(est, floor(prop * length(est)))
  list(
    train = dplyr::filter(df, nombre_estacion %in% train_est),
    test  = dplyr::filter(df, !nombre_estacion %in% train_est)
  )
}

calcular_metricas <- function(obs, pred) {
  r2 <- cor(obs, pred, use = "complete.obs")^2
  pbias <- 100 * sum(pred - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)
  list(r2 = r2, pbias = pbias)
}

grafico_scatter <- function(df, x, y, titulo, metricas) {
  ggplot(df, aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "black", alpha = 0.7, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = titulo,
      subtitle = paste0("R² = ", round(metricas$r2, 3), 
                        " | PBIAS = ", round(metricas$pbias, 1), "%"),
      x = "P_obs_anual", y = "P_GCM_anual"
    ) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA),
          plot.title = element_text(hjust = 0.5))
}

evaluar_modelo_corregido <- function(df, estaciones, modelo, periodo) {
  df <- dplyr::filter(df, nombre_estacion %in% estaciones)
  df$pred_rf <- predict(modelo, newdata = df)
  met <- calcular_metricas(df$P_obs_anual, df$pred_rf)
  grafico_scatter(df, P_obs_anual, pred_rf, paste("GCMcorr -", periodo), met)
}

evaluar_modelo_crudo <- function(df, estaciones, variable_gcm, periodo, nombre_gcm) {
  df <- dplyr::filter(df, nombre_estacion %in% estaciones)
  met <- calcular_metricas(df$P_obs_anual, df[[variable_gcm]])
  grafico_scatter(df, P_obs_anual, !!sym(variable_gcm), paste0("GCM: ", nombre_gcm, " ", periodo), met)
}


split <- dividir_train_test(df_estaciones_P_1_anual_pca, prop = 0.4, seed = 123)
train <- split$train
test  <- split$test

set.seed(123)

rf_model <- randomForest::randomForest(P_obs_anual ~ . - nombre_estacion - codigo - fecha_inicio -fecha_termino -altitud , data = train, ntree = 5000)

importance_rf_model <- randomForest::importance(rf_model)


# Predecir
train$pred_rf <- predict(rf_model, newdata = train)
test$pred_rf  <- predict(rf_model, newdata = test)

# Estaciones test para aplicar a otros periodos
estaciones_test <- unique(test$nombre_estacion)

estaciones_train <- unique(train$nombre_estacion)


# GCM

nombre_gcm <- "MIROC6"
nombre_variable_gcm <- "pr_MIROC6_anual"  # Esta columna debe existir en todos los dataframes P1–P5

#

# P1

g_train <- grafico_scatter(train, P_obs_anual, pred_rf, "GCMcorr - Entrenamiento P1", calcular_metricas(train$P_obs_anual, train$pred_rf))
g_test  <- grafico_scatter(test,  P_obs_anual, pred_rf, "GCMcorr - Test P1", calcular_metricas(test$P_obs_anual, test$pred_rf))
g_crudo_P1 <- evaluar_modelo_crudo(df_estaciones_P_1_anual, estaciones_test, nombre_variable_gcm, "P1", nombre_gcm)

# P2–P5

g_corr_P2 <- evaluar_modelo_corregido(df_estaciones_P_2_anual_pca, estaciones_test, rf_model, "Test P2")
g_corr_P3 <- evaluar_modelo_corregido(df_estaciones_P_3_anual_pca, estaciones_test, rf_model, "Test P3")
g_corr_P4 <- evaluar_modelo_corregido(df_estaciones_P_4_anual_pca, estaciones_test, rf_model, "Test P4")
g_corr_P5 <- evaluar_modelo_corregido(df_estaciones_P_5_anual_pca, estaciones_test, rf_model, "Test P5")

g_crudo_P2 <- evaluar_modelo_crudo(df_estaciones_P_2_anual, estaciones_test, nombre_variable_gcm, "P2", nombre_gcm)
g_crudo_P3 <- evaluar_modelo_crudo(df_estaciones_P_3_anual, estaciones_test, nombre_variable_gcm, "P3", nombre_gcm)
g_crudo_P4 <- evaluar_modelo_crudo(df_estaciones_P_4_anual, estaciones_test, nombre_variable_gcm, "P4", nombre_gcm)
g_crudo_P5 <- evaluar_modelo_crudo(df_estaciones_P_5_anual, estaciones_test, nombre_variable_gcm, "P5", nombre_gcm)

# --- PANEL FINAL: FILA 1 = CORREGIDO, FILA 2 = CRUDO ---

# PANEL FINAL: 2 filas x 5 columnas (corregido arriba, crudo abajo)

panel <- (
  (g_test | g_corr_P2 | g_corr_P3 | g_corr_P4 | g_corr_P5) /
  (g_crudo_P1 | g_crudo_P2 | g_crudo_P3 | g_crudo_P4 | g_crudo_P5)
) +
  plot_annotation(
    title = paste0("Modelo RF entrenado en P1. Test en estaciones no entrenadas y periodos futuros no entrenados. Comparación con GCM: ", nombre_gcm)
  ) &
  theme(plot.title = element_text(hjust = 0.5))

print(panel)




file_path <- ".../00_imagenes/PANEL_SCATTERPLOT1.png"

width_px <- 1870
height_px <- 714

dpi <- 96

ggsave(
  filename = file_path,
  plot = panel,
  width = width_px,
  height = height_px,
  units = "px",
  dpi = dpi
)

print(paste("Plot saved to:", file_path))








#### VALIDACION ESPACIOTEMPORAL MODELO RF ENTRENADO EN DISTINTOS PERIODOS: ITERACIONES ####


library(randomForest)
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

n_iter <- 1000
ntree <- 500

df_list <- list(
  P1 = list(df_anual_PCA = df_estaciones_P_1_anual_pca),
  P2 = list(df_anual_PCA = df_estaciones_P_2_anual_pca),
  P3 = list(df_anual_PCA = df_estaciones_P_3_anual_pca),
  P4 = list(df_anual_PCA = df_estaciones_P_4_anual_pca),
  P5 = list(df_anual_PCA = df_estaciones_P_5_anual_pca)
)

calc_r2 <- function(obs, pred) cor(obs, pred, use = "complete.obs")^2
calc_pbias <- function(obs, pred) 100 * sum(pred - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)


entrenar_iteracion <- function(seed) {
  
  # 1. Fijar la semilla para que el muestreo aleatorio sea reproducible
  set.seed(seed)
  message("Iteración: ", seed)
  
  # 2. Tomar los datos del periodo P1 (primer periodo)
  df <- df_estaciones_P_1_anual_pca
  
  # 3. Obtener el listado único de estaciones disponibles en P1
  estaciones <- unique(df$nombre_estacion)
  
  # 4. Seleccionar aleatoriamente el 40% de las estaciones para entrenamiento
  estaciones_train <- sample(estaciones, floor(0.4 * length(estaciones)))
  
  # 5. Crear el conjunto de entrenamiento solo con esas estaciones seleccionadas
  df_train <- df_list$P1$df_anual_PCA %>%
    dplyr::filter(nombre_estacion %in% estaciones_train)
  
  # 6. Entrenar un modelo Random Forest con ntree árboles, usando como predictores
  #    todas las variables menos las que no aportan información útil al modelo
  rf_model <- randomForest(
    P_obs_anual ~ ., 
    data = df_train %>%
      dplyr::select(-codigo, -nombre_estacion, -fecha_inicio, -fecha_termino, -altitud),
    ntree = ntree
  )
  
  # 7. Para cada uno de los periodos P1 a P5 (contenidos en df_list):
  map2_dfr(df_list, names(df_list), function(p, periodo) {
    
    # a. Filtrar solo las estaciones que NO se usaron en entrenamiento (estaciones "test")
    p$df_anual_PCA %>%
      dplyr::filter(!(nombre_estacion %in% estaciones_train)) %>%
      
      # b. Aplicar el modelo entrenado y generar predicciones
      mutate(
        pred_rf = predict(rf_model, newdata = .),
        
        # c. Añadir columnas con el nombre del periodo y la semilla usada
        periodo = periodo,
        seed = seed
      )
  })
}

resultados_all <- map_dfr(1:n_iter, entrenar_iteracion)

tema_con_borde <- theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )


df_list_P <- list(
  P1 = list(df_anual = df_estaciones_P_1_anual),
  P2 = list(df_anual = df_estaciones_P_2_anual),
  P3 = list(df_anual = df_estaciones_P_3_anual),
  P4 = list(df_anual = df_estaciones_P_4_anual),
  P5 = list(df_anual = df_estaciones_P_5_anual)
)


graficar_rf_global1 <- function(df) {
  r2 <- calc_r2(df$P_obs_anual, df$pred_rf)
  pbias <- calc_pbias(df$P_obs_anual, df$pred_rf)
  
  ggplot(df, aes(x = P_obs_anual, y = pred_rf)) +
    geom_point(alpha = 0.6, color = "black", size = 1.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Exp 1. 1000 Iteraciones para distintas estaciones.",
      subtitle = paste0("R² = ", round(r2, 3), " | PBIAS = ", round(pbias, 1), "%"),
      x = "P_obs_anual",
      y = "P_GCM_anual_corr"
    ) +
    tema_con_borde
}

plot_rf_100 <- graficar_rf_global1(resultados_all)



entrenar_iteracion_multi <- function(seed) {
  set.seed(seed)
  message("Iteración: ", seed)
  # Todas las estaciones posibles (se asume que todas están en P1)
  estaciones <- unique(df_list$P1$df_anual_PCA$nombre_estacion)
  
  # Seleccionar 40% al azar para entrenamiento
  estaciones_train <- sample(estaciones, floor(0.4 * length(estaciones)))
  
  # Para cada periodo de entrenamiento (P1 a P5)
  map2_dfr(df_list, names(df_list), function(p_train, periodo_entrenamiento) {
    
    # Crear conjunto de entrenamiento en ese periodo
    df_train <- p_train$df_anual_PCA %>%
      dplyr::filter(nombre_estacion %in% estaciones_train)
    
    # Entrenar modelo RF
    rf_model <- randomForest(
      P_obs_anual ~ .,
      data = df_train %>%
        dplyr::select(-codigo, -nombre_estacion, -fecha_inicio, -fecha_termino, -altitud),
      ntree = ntree
    )
    
    # Ahora evaluar en los 4 periodos restantes
    map2_dfr(df_list, names(df_list), function(p_test, periodo_evaluacion) {
      
      if (periodo_evaluacion != periodo_entrenamiento) {
        
        p_test$df_anual_PCA %>%
          dplyr::filter(!(nombre_estacion %in% estaciones_train)) %>%
          mutate(
            pred_rf = predict(rf_model, newdata = .),
            periodo_entrenamiento = periodo_entrenamiento,
            periodo_evaluacion = periodo_evaluacion,
            seed = seed
          )
        
      } else {
        NULL  # No evaluamos en el mismo periodo de entrenamiento
      }
    })
  })
}


resultados_multi_all <- map_dfr(1:n_iter, entrenar_iteracion_multi)

graficar_rf_global2 <- function(df) {
  r2 <- calc_r2(df$P_obs_anual, df$pred_rf)
  pbias <- calc_pbias(df$P_obs_anual, df$pred_rf)
  
  ggplot(df, aes(x = P_obs_anual, y = pred_rf)) +
    geom_point(alpha = 0.6, color = "black", size = 1.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Exp 2. 1000 Iteraciones para distintas estaciones y distintos periodos.",
      subtitle = paste0("R² = ", round(r2, 3), " | PBIAS = ", round(pbias, 1), "%"),
      x = "P_obs_anual",
      y = "P_GCM_anual_corr"
    ) +
    tema_con_borde
}


plot_multi_all = graficar_rf_global2(resultados_multi_all)


graficar_gcm_crudo_global <- function(df_list_P, nombre_modelo) {
  df_total <- map2_dfr(
    df_list_P,
    names(df_list_P),
    ~ .x$df_anual %>%
      dplyr::select(P_obs_anual, !!sym(nombre_modelo)) %>%
      rename(pred_modelo = !!sym(nombre_modelo)) %>%
      mutate(periodo = .y)
  )
  
  r2 <- calc_r2(df_total$P_obs_anual, df_total$pred_modelo)
  pbias <- calc_pbias(df_total$P_obs_anual, df_total$pred_modelo)
  
  ggplot(df_total, aes(x = P_obs_anual, y = pred_modelo)) +
    geom_point(alpha = 0.6, color = "black", size = 1.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "GCM sin corrección. P1 a P5: MIROC6",
      subtitle = paste0("R² = ", round(r2, 3), " | PBIAS = ", round(pbias, 1), "%"),
      x = "P_obs_anual", y = "P_GCM_anual"
    ) +
    tema_con_borde
}


plot_gcm_global <- graficar_gcm_crudo_global(df_list_P, "pr_MIROC6_anual")


panel_scatter_global <- (
  plot_rf_100 | plot_multi_all | plot_gcm_global
) +
  plot_layout(widths = c(1, 1, 1)) +
  plot_annotation(
    title = "Validación espaciotemporal modelo RF y GCM sin corrección para todos los periodos.",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )




file_path <- ".../00_imagenes/PANEL_SCATTERPLOT2.png"

width_px <- 1870
height_px <- 714

dpi <- 96

ggsave(
  filename = file_path,
  plot = panel_scatter_global,
  width = width_px,
  height = height_px,
  units = "px",
  dpi = dpi
)

print(paste("Plot saved to:", file_path))





#### OBTENER SERIE ANUAL CORREGIDA POR ESTACION ####


# saveRDS(df_estaciones_P, file = "df_estaciones_P.rds")

df_estaciones_P <- readRDS("df_estaciones_P.rds")


df_estaciones_P <- df_estaciones_P %>%
  mutate(
    fecha = as.Date(fecha),
    anio_hidrologico = ifelse(month(fecha) >= 4, year(fecha), year(fecha) - 1)
  )


estaticos_estacion <- df_estaciones_P %>%
  distinct(codigo, nombre_estacion, latitud, longitud, altitud, fecha_inicio,
           fecha_termino, altitud_dem, pendiente, aspecto, distancia_costa_m)



procesar_periodo_f <- function(df, inicio, fin, nro_comp, estaticos_estacion_df) {
  columnas_modelos <- base::names(df)[base::grepl("^pr_", base::names(df))]
  columnas_estaticas <- base::setdiff(base::names(df), c("fecha", columnas_modelos, "anio_hidrologico", "mes"))
  if (!"nombre_estacion" %in% columnas_estaticas && "nombre_estacion" %in% base::names(df)) {
    columnas_estaticas <- c(columnas_estaticas, "nombre_estacion")
  }

  df_sub <- dplyr::filter(
    df,
    fecha >= base::as.Date(paste0(inicio, "-04-01")) &
      fecha <= base::as.Date(paste0(fin, "-03-31"))
  ) %>%
    dplyr::mutate(
      anio_hidrologico = dplyr::if_else(lubridate::month(fecha) >= 4,
                                        lubridate::year(fecha),
                                        lubridate::year(fecha) - 1),
      mes = lubridate::month(fecha)
    )

  df_long <- tidyr::pivot_longer(df_sub, cols = dplyr::all_of(columnas_modelos),
                                 names_to = "modelo", values_to = "prec")

  valid_dias_mes <- df_long %>%
    dplyr::group_by(codigo, anio_hidrologico, mes, modelo) %>%
    dplyr::summarise(dias_validos = sum(!is.na(prec)), .groups = "drop") %>%
    dplyr::filter(dias_validos >= 20)

  valid_years <- valid_dias_mes %>%
    dplyr::group_by(codigo, anio_hidrologico, modelo) %>%
    dplyr::summarise(meses_validos = n(), .groups = "drop") %>%
    dplyr::filter(meses_validos == 12)

  df_valido <- df_long %>%
    dplyr::inner_join(valid_dias_mes, by = c("codigo", "anio_hidrologico", "mes", "modelo")) %>%
    dplyr::inner_join(valid_years, by = c("codigo", "anio_hidrologico", "modelo")) %>%
    dplyr::group_by(codigo, anio_hidrologico, modelo) %>%
    dplyr::summarise(prec_anual = sum(prec, na.rm = TRUE), .groups = "drop")

  df_prec <- tidyr::pivot_wider(df_valido, names_from = "modelo", values_from = "prec_anual")

  col_renombrar <- names(df_prec)[grepl("^pr_", names(df_prec))]
  if (length(col_renombrar) > 0) {
    nombres_limpios <- gsub("-", "_", col_renombrar)
    nombres_limpios <- gsub("\\.", "_", nombres_limpios)
    nombres_finales <- paste0(nombres_limpios, "_anual")
    names(df_prec)[match(col_renombrar, names(df_prec))] <- nombres_finales
  }

  df_prec <- dplyr::distinct(df_prec, codigo, anio_hidrologico, .keep_all = TRUE)

  # Validación: si no hay columnas anuales o todos los valores son NA
  anuales_cols <- names(df_prec)[grepl("_anual$", names(df_prec))]
  sin_datos_validos <- length(anuales_cols) == 0 || all(df_prec[, anuales_cols] %>% dplyr::mutate_all(~ all(is.na(.))))

  if (sin_datos_validos) {
    warning(paste("No hay datos válidos de modelos entre", inicio, "y", fin))

    df_anual_PCA <- tibble::tibble(
      codigo = NA_character_,
      anio_hidrologico = NA_integer_
    )

    return(list(
      df_sin_na = NULL,
      pca = NULL,
      df_anual_PCA = df_anual_PCA,
      df_anual = NULL,
      pca_plot = ggplot2::ggplot() + ggplot2::labs(title = "Sin datos suficientes")
    ))
  }

  df_sin_na <- dplyr::left_join(df_prec, estaticos_estacion_df, by = "codigo")

  gcm_cols <- df_sin_na %>%
    dplyr::select(dplyr::ends_with("_anual")) %>%
    names()

  df_pca_input <- df_sin_na %>%
    dplyr::filter(dplyr::if_all(all_of(gcm_cols), ~ !is.na(.) & !is.nan(.) & !is.infinite(.)))

  all_static_cols_present_in_final <- union(
    intersect(names(estaticos_estacion_df), names(df_sin_na)),
    c("codigo", "anio_hidrologico")
  )

  if (nrow(df_pca_input) > 2) {
    pca_result <- tryCatch({
      stats::prcomp(df_pca_input[, gcm_cols], center = TRUE, scale. = TRUE)
    }, error = function(e) {
      message("Error en PCA: ", e$message)
      return(NULL)
    })

    if (!is.null(pca_result)) {
      nro_comp_ajust <- min(nro_comp, ncol(pca_result$x))
      pca_scores <- as.data.frame(pca_result$x[, seq_len(nro_comp_ajust)])
      names(pca_scores) <- paste0("PC", seq_len(ncol(pca_scores)))

      df_anual_PCA <- dplyr::bind_cols(
        dplyr::select(df_pca_input, dplyr::all_of(all_static_cols_present_in_final)),
        tibble::as_tibble(pca_scores)
      )

      var_exp <- pca_result$sdev^2
      var_exp_prop <- var_exp / sum(var_exp)
      var_exp_df <- data.frame(
        Componente = paste0("PC", seq_along(var_exp_prop)),
        Varianza = var_exp_prop,
        Acumulada = cumsum(var_exp_prop)
      )

      pca_plot <- ggplot2::ggplot(var_exp_df, ggplot2::aes(x = seq_along(Varianza), y = Acumulada)) +
        ggplot2::geom_line(color = "black") +
        ggplot2::geom_point(color = "black") +
        ggplot2::scale_x_continuous(breaks = seq_along(var_exp_prop)) +
        ggplot2::labs(
          x = "Componente Principal",
          y = "Varianza Explicada Acumulada",
          title = "Varianza Acumulada por Componente"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8))
    } else {
      df_anual_PCA <- NULL
      pca_plot <- ggplot2::ggplot() + ggplot2::labs(title = "Error en PCA")
    }
  } else {
    df_anual_PCA <- NULL
    pca_result <- NULL
    pca_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No hay suficientes datos para PCA")
  }

  df_anual <- df_sin_na %>%
    dplyr::select(all_of(all_static_cols_present_in_final), all_of(gcm_cols))

  return(list(
    df_sin_na = df_sin_na,
    pca = pca_result,
    df_anual_PCA = df_anual_PCA,
    df_anual = df_anual,
    pca_plot = pca_plot
  ))
}

df_estaciones_P_f <- df_estaciones_P
df_estaciones_P_f  <- df_estaciones_P_f %>%
  dplyr::select(-`pr_MPI-ESM-MR-RegCM4-10k`)



df_periodo_1_anual <- procesar_periodo_f(df_estaciones_P_f, 1950, 1960, 10, estaticos_estacion)
df_periodo_2_anual <- procesar_periodo_f(df_estaciones_P_f, 1960, 1970, 10, estaticos_estacion)
df_periodo_3_anual <- procesar_periodo_f(df_estaciones_P_f, 1970, 1980, 10, estaticos_estacion)
df_periodo_4_anual <- procesar_periodo_f(df_estaciones_P, 1980, 1990, 10, estaticos_estacion)
df_periodo_5_anual <- procesar_periodo_f(df_estaciones_P, 1990, 2000, 10, estaticos_estacion)
df_periodo_6_anual <- procesar_periodo_f(df_estaciones_P, 2000, 2010, 10, estaticos_estacion)
df_periodo_7_anual <- procesar_periodo_f(df_estaciones_P, 2010, 2020, 10, estaticos_estacion)



# tomar anuales pca

df_estaciones_P_1_anual_pca <- df_periodo_1_anual$df_anual_PCA
df_estaciones_P_2_anual_pca <- df_periodo_2_anual$df_anual_PCA
df_estaciones_P_3_anual_pca <- df_periodo_3_anual$df_anual_PCA
df_estaciones_P_4_anual_pca <- df_periodo_4_anual$df_anual_PCA
df_estaciones_P_5_anual_pca <- df_periodo_5_anual$df_anual_PCA
df_estaciones_P_6_anual_pca <- df_periodo_6_anual$df_anual_PCA
df_estaciones_P_7_anual_pca <- df_periodo_7_anual$df_anual_PCA



df_list <- list(
  P1 = list(df_anual_PCA = df_estaciones_P_1_anual_pca),
  P2 = list(df_anual_PCA = df_estaciones_P_2_anual_pca),
  P3 = list(df_anual_PCA = df_estaciones_P_3_anual_pca),
  P4 = list(df_anual_PCA = df_estaciones_P_4_anual_pca),
  P5 = list(df_anual_PCA = df_estaciones_P_5_anual_pca),
  P6 = list(df_anual_PCA = df_estaciones_P_6_anual_pca),
  P7 = list(df_anual_PCA = df_estaciones_P_7_anual_pca)
  
)



# Guardar como archivo .rds
saveRDS(df_list, file = "df_list_PCA_1950_2020.rds")


procesar_periodo_f <- function(df, inicio, fin, nro_comp, estaticos_estacion_df) {
  columnas_modelos <- base::names(df)[base::grepl("^pr_", base::names(df))]
  columnas_estaticas <- base::setdiff(base::names(df), c("fecha", columnas_modelos, "anio_hidrologico", "mes"))
  if (!"nombre_estacion" %in% columnas_estaticas && "nombre_estacion" %in% base::names(df)) {
    columnas_estaticas <- c(columnas_estaticas, "nombre_estacion")
  }

  df_sub <- dplyr::filter(
    df,
    fecha >= base::as.Date(paste0(inicio, "-04-01")) &
      fecha <= base::as.Date(paste0(fin, "-03-31"))
  ) %>%
    dplyr::mutate(
      anio_hidrologico = dplyr::if_else(lubridate::month(fecha) >= 4,
                                        lubridate::year(fecha),
                                        lubridate::year(fecha) - 1),
      mes = lubridate::month(fecha)
    )

  df_long <- tidyr::pivot_longer(df_sub, cols = dplyr::all_of(columnas_modelos),
                                 names_to = "modelo", values_to = "prec")

  valid_dias_mes <- df_long %>%
    dplyr::group_by(codigo, anio_hidrologico, mes, modelo) %>%
    dplyr::summarise(dias_validos = sum(!is.na(prec)), .groups = "drop") %>%
    dplyr::filter(dias_validos >= 20)

  valid_years <- valid_dias_mes %>%
    dplyr::group_by(codigo, anio_hidrologico, modelo) %>%
    dplyr::summarise(meses_validos = n(), .groups = "drop") %>%
    dplyr::filter(meses_validos == 12)

  df_valido <- df_long %>%
    dplyr::inner_join(valid_dias_mes, by = c("codigo", "anio_hidrologico", "mes", "modelo")) %>%
    dplyr::inner_join(valid_years, by = c("codigo", "anio_hidrologico", "modelo")) %>%
    dplyr::group_by(codigo, anio_hidrologico, modelo) %>%
    dplyr::summarise(prec_anual = sum(prec, na.rm = TRUE), .groups = "drop")

  df_prec <- tidyr::pivot_wider(df_valido, names_from = "modelo", values_from = "prec_anual")

  col_renombrar <- names(df_prec)[grepl("^pr_", names(df_prec))]
  if (length(col_renombrar) > 0) {
    nombres_limpios <- gsub("-", "_", col_renombrar)
    nombres_limpios <- gsub("\\.", "_", nombres_limpios)
    nombres_finales <- paste0(nombres_limpios, "_anual")
    names(df_prec)[match(col_renombrar, names(df_prec))] <- nombres_finales
  }

  df_prec <- dplyr::distinct(df_prec, codigo, anio_hidrologico, .keep_all = TRUE)

  # Validación: si no hay columnas anuales o todos los valores son NA
  anuales_cols <- names(df_prec)[grepl("_anual$", names(df_prec))]
  sin_datos_validos <- length(anuales_cols) == 0 || all(df_prec[, anuales_cols] %>% dplyr::mutate_all(~ all(is.na(.))))

  if (sin_datos_validos) {
    warning(paste("No hay datos válidos de modelos entre", inicio, "y", fin))

    df_anual_PCA <- tibble::tibble(
      codigo = NA_character_,
      anio_hidrologico = NA_integer_
    )

    return(list(
      df_sin_na = NULL,
      pca = NULL,
      df_anual_PCA = df_anual_PCA,
      df_anual = NULL,
      pca_plot = ggplot2::ggplot() + ggplot2::labs(title = "Sin datos suficientes")
    ))
  }

  df_sin_na <- dplyr::left_join(df_prec, estaticos_estacion_df, by = "codigo")

  gcm_cols <- df_sin_na %>%
    dplyr::select(dplyr::ends_with("_anual")) %>%
    names()

  df_pca_input <- df_sin_na %>%
    dplyr::filter(dplyr::if_all(all_of(gcm_cols), ~ !is.na(.) & !is.nan(.) & !is.infinite(.)))

  all_static_cols_present_in_final <- union(
    intersect(names(estaticos_estacion_df), names(df_sin_na)),
    c("codigo", "anio_hidrologico")
  )

  if (nrow(df_pca_input) > 2) {
    pca_result <- tryCatch({
      stats::prcomp(df_pca_input[, gcm_cols], center = TRUE, scale. = TRUE)
    }, error = function(e) {
      message("Error en PCA: ", e$message)
      return(NULL)
    })

    if (!is.null(pca_result)) {
      nro_comp_ajust <- min(nro_comp, ncol(pca_result$x))
      pca_scores <- as.data.frame(pca_result$x[, seq_len(nro_comp_ajust)])
      names(pca_scores) <- paste0("PC", seq_len(ncol(pca_scores)))

      df_anual_PCA <- dplyr::bind_cols(
        dplyr::select(df_pca_input, dplyr::all_of(all_static_cols_present_in_final)),
        tibble::as_tibble(pca_scores)
      )

      var_exp <- pca_result$sdev^2
      var_exp_prop <- var_exp / sum(var_exp)
      var_exp_df <- data.frame(
        Componente = paste0("PC", seq_along(var_exp_prop)),
        Varianza = var_exp_prop,
        Acumulada = cumsum(var_exp_prop)
      )

      pca_plot <- ggplot2::ggplot(var_exp_df, ggplot2::aes(x = seq_along(Varianza), y = Acumulada)) +
        ggplot2::geom_line(color = "black") +
        ggplot2::geom_point(color = "black") +
        ggplot2::scale_x_continuous(breaks = seq_along(var_exp_prop)) +
        ggplot2::labs(
          x = "Componente Principal",
          y = "Varianza Explicada Acumulada",
          title = "Varianza Acumulada por Componente"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.8))
    } else {
      df_anual_PCA <- NULL
      pca_plot <- ggplot2::ggplot() + ggplot2::labs(title = "Error en PCA")
    }
  } else {
    df_anual_PCA <- NULL
    pca_result <- NULL
    pca_plot <- ggplot2::ggplot() + ggplot2::labs(title = "No hay suficientes datos para PCA")
  }

  df_anual <- df_sin_na %>%
    dplyr::select(all_of(all_static_cols_present_in_final), all_of(gcm_cols))

  return(list(
    df_sin_na = df_sin_na,
    pca = pca_result,
    df_anual_PCA = df_anual_PCA,
    df_anual = df_anual,
    pca_plot = pca_plot
  ))
}

df_periodo_1_anual_f <- procesar_periodo_f(df_estaciones_P, 2020, 2030, 10, estaticos_estacion)
df_periodo_2_anual_f <- procesar_periodo_f(df_estaciones_P, 2030, 2040, 10, estaticos_estacion)
df_periodo_3_anual_f <- procesar_periodo_f(df_estaciones_P, 2040, 2050, 10, estaticos_estacion)
df_periodo_4_anual_f <- procesar_periodo_f(df_estaciones_P, 2050, 2060, 10, estaticos_estacion)


df_periodo_5_anual_f <- procesar_periodo_f(df_estaciones_P, 2060, 2070, 10, estaticos_estacion)
df_periodo_6_anual_f <- procesar_periodo_f(df_estaciones_P, 2070, 2080, 10, estaticos_estacion)


df_periodo_7_anual_f <- procesar_periodo_f(df_estaciones_P, 2080, 2090, 10, estaticos_estacion)
df_periodo_8_anual_f <- procesar_periodo_f(df_estaciones_P, 2090, 2099, 10, estaticos_estacion)


df_estaciones_P_1_f_anual_pca <- df_periodo_1_anual_f$df_anual_PCA
df_estaciones_P_2_f_anual_pca <- df_periodo_2_anual_f$df_anual_PCA
df_estaciones_P_3_f_anual_pca <- df_periodo_3_anual_f$df_anual_PCA
df_estaciones_P_4_f_anual_pca <- df_periodo_4_anual_f$df_anual_PCA
df_estaciones_P_5_f_anual_pca <- df_periodo_5_anual_f$df_anual_PCA
df_estaciones_P_6_f_anual_pca <- df_periodo_6_anual_f$df_anual_PCA
df_estaciones_P_7_f_anual_pca <- df_periodo_7_anual_f$df_anual_PCA
df_estaciones_P_8_f_anual_pca <- df_periodo_8_anual_f$df_anual_PCA


df_list_f <- list(
  P1 = list(df_estaciones_P_1_f_anual_pca),
  P2 = list(df_estaciones_P_2_f_anual_pca),
  P3 = list(df_estaciones_P_3_f_anual_pca),
  P4 = list(df_estaciones_P_4_f_anual_pca),
  P5 = list(df_estaciones_P_5_f_anual_pca),
  P6 = list(df_estaciones_P_6_f_anual_pca),
  P7 = list(df_estaciones_P_7_f_anual_pca),
  P8 = list(df_estaciones_P_8_f_anual_pca)
)

# # # Guardar como archivo .rds
# saveRDS(df_list_f, file = "df_list_PCA_2020_2099.rds")
# 



df_list <- readRDS("df_list_PCA_1950_2020.rds")

df_list_f <- readRDS("df_list_PCA_2020_2099.rds")

df_list_entrenamiento <- readRDS("df_list_PCA.rds")


df_estaciones_P <- readRDS("df_estaciones_P.rds")

df_estaciones_P <- df_estaciones_P %>%
  mutate(
    fecha = as.Date(fecha),
    anio_hidrologico = ifelse(month(fecha) >= 4, year(fecha), year(fecha) - 1)
  ) %>%
  dplyr::filter(anio_hidrologico >= 1950, anio_hidrologico <= 2099)

head(df_estaciones_P)




estaticos_estacion <- df_estaciones_P %>%
  distinct(codigo, nombre_estacion, latitud, longitud, altitud, fecha_inicio,
           fecha_termino, altitud_dem, pendiente, aspecto, distancia_costa_m)


# ENTRENAMIENTO 

df_periodos_entrenamiento <- map2_dfr(df_list_entrenamiento, names(df_list_entrenamiento), ~ .x$df_anual_PCA %>% dplyr::mutate(periodo = .y))


df_periodos_entrenamiento <- df_periodos_entrenamiento %>% dplyr::select(-completitud_P_obs)


View(df_periodos_entrenamiento)



# PASADO 

df_periodos_pasado <- map2_dfr(df_list, names(df_list), ~ .x$df_anual_PCA %>% dplyr::mutate(periodo = .y))

# df_periodos_pasado <- df_periodos_entrenamiento %>% dplyr::select(-completitud_P_obs)


# FUTURO 

df_periodos_futuro  <- df_list_f %>%
  map(~ .x[[1]]) %>%  # 
  bind_rows(.id = "periodo")  # 

# ENTRENAR RF 

# 
# # Paso 1: seleccionar 10 estaciones aleatorias
# set.seed(1)
# 
# estaciones_aleatorias <- df_periodos_entrenamiento %>%
#   distinct(nombre_estacion) %>%
#   sample_n(10)
# estaciones_aleatorias
# 
# 
# # Crear una lista única de estaciones
# estaciones_unicas <- df_periodos_entrenamiento %>%
#   distinct(nombre_estacion)
# 
# # Paso 2: guardar los datos de esas estaciones en un dataframe aparte
# 
# df_estaciones_10 <- df_periodos_entrenamiento %>%
#   dplyr::filter(nombre_estacion %in% estaciones_aleatorias$nombre_estacion)
# 
# # Paso 3: quitar esas estaciones del dataset principal
# 
# df_periodos_entrenamiento <- df_periodos_entrenamiento %>%
#   dplyr::filter(!(nombre_estacion %in% estaciones_aleatorias$nombre_estacion))
# 
# 



set.seed(123)

rf_model_periodo_entrenamiento <- randomForest::randomForest(
  P_obs_anual ~ . - nombre_estacion - altitud - fecha_inicio - fecha_termino - anio_hidrologico - periodo, 
  data = df_periodos_entrenamiento,
  importance = TRUE,
  ntree = 5000
)


importancia = randomForest::importance(rf_model_periodo_entrenamiento)


# ... aquí trabajas con df_periodos_entrenamiento_sin ...

# Paso 4: volver a agregarlas cuando sea necesario

# df_periodos_entrenamiento <- bind_rows(df_periodos_entrenamiento, df_estaciones_10)
# # 
# 
# Filtrar estación Visviri



df_periodos_entrenamiento$pred_anual_rf <- predict(rf_model_periodo_entrenamiento, newdata = df_periodos_entrenamiento)

df_periodos_pasado$pred_anual_rf <- predict(rf_model_periodo_entrenamiento, newdata = df_periodos_pasado)

df_periodos_futuro$pred_anual_rf <- predict(rf_model_periodo_entrenamiento, newdata = df_periodos_futuro)





# GRAFICAR RESULTADOS 


df_estaciones_P <- df_estaciones_P %>%
  dplyr::mutate(
    fecha = as.Date(fecha),
    Año = lubridate::year(fecha)
  )



obtener_maximos_anuales_gcm <- function(df, gcm, modo) {

  # Verificar que 'fecha' exista
  if (!"fecha" %in% names(df)) stop("La columna 'fecha' no existe en el dataframe.")

  # Asegurar formato de fecha y calcular año hidrológico
  df <- df %>%
    mutate(
      fecha = as.Date(fecha),
      anio_hidrologico = ifelse(month(fecha) >= 4, year(fecha), year(fecha) - 1)
    ) %>%
    dplyr::filter(anio_hidrologico >= 1950, anio_hidrologico <= 2099)

  # Filtrado según modo
  df_filtrado <- if (modo == "solo_obs") {
    df %>%  dplyr::filter(!is.na(P_obs))
  } else if (modo == "todas") {
    df
  } else {
    stop("El parámetro 'modo' debe ser 'solo_obs' o 'todas'.")
  }

  # Resumen observados por año hidrológico
  df_obs <- df %>%
    dplyr::filter(!is.na(P_obs)) %>%
    group_by(codigo, nombre_estacion, anio_hidrologico) %>%
    summarise(
      maximo = max(P_obs, na.rm = TRUE),
      anual = sum(P_obs, na.rm = TRUE),
      .groups = "drop"
    )

  # Resumen GCM por año hidrológico
  df_gcm <- df_filtrado %>%
    group_by(codigo, nombre_estacion, anio_hidrologico) %>%
    summarise(
      maximo_gcm = max(.data[[gcm]], na.rm = TRUE),
      anual_gcm = sum(.data[[gcm]], na.rm = TRUE),
      .groups = "drop"
    )

  # Unir según modo
  df_extremos <- if (modo == "todas") {
    left_join(df_gcm, df_obs, by = c("codigo", "nombre_estacion", "anio_hidrologico"))
  } else {
    inner_join(df_gcm, df_obs, by = c("codigo", "nombre_estacion", "anio_hidrologico"))
  }

  return(as.data.frame(df_extremos))
}

df_maximos_anuales_gcm <- obtener_maximos_anuales_gcm(df_estaciones_P, "pr_MIROC-ES2L" , "todas")

head(df_maximos_anuales_gcm)



# para los anio_hidrolgoicos de 1950 a 1969 hay que añadir los anuales de df_periodos_pasado a df_maximos_anuales_gcm contando el 1969

# para los anio_hidrolgoicos de 1970 a 2019 hay que añadir los anuales de df_periodos_entrenamiento a df_maximos_anuales_gcm contando el 2019

# para los anio_hidrolgoicos de 2020 a 2099 hay que añadir los anuales de df_periodos_futuro a df_maximos_anuales_gcm
# entonces la idea es ir recorriendo lista por lista y dato por dato nombvre de estacion y si coinciden con_df_maximos anuales
# hay que agregarlo, notar que los datos pueden estar desordenados entonces el enfoque tiene que ser robusto e ir recorriendo y verificando
# dato por dato 


head(df_maximos_anuales_gcm)
# 
head(df_periodos_pasado)

head(df_periodos_futuro)

head(df_periodos_entrenamiento)




# CON VISVIRI

# 1. Extraer las columnas necesarias desde cada fuente
pasado_anual <- df_periodos_pasado %>%
  dplyr::select(codigo, nombre_estacion, anio_hidrologico, pred_anual_rf)

entrenamiento_anual <- df_periodos_entrenamiento %>%
  dplyr::select(codigo, nombre_estacion, anio_hidrologico, pred_anual_rf)

futuro_anual <- df_periodos_futuro %>%
  dplyr::select(codigo, nombre_estacion, anio_hidrologico, pred_anual_rf)

# 2. Filtrar por rangos de años
pasado_50_69 <- pasado_anual %>% dplyr::filter(anio_hidrologico >= 1950, anio_hidrologico <= 1969)
entrenamiento_70_19 <- entrenamiento_anual %>% dplyr::filter(anio_hidrologico >= 1970, anio_hidrologico <= 2019)
futuro_20_99 <- futuro_anual %>% dplyr::filter(anio_hidrologico >= 2020, anio_hidrologico <= 2099)

# 3. Agregar datos al df_maximos_anuales_gcm

# A. Para 1950–1969 desde df_periodos_pasado
df_maximos_anuales_gcm <- df_maximos_anuales_gcm %>%
  left_join(pasado_50_69, by = c("codigo", "nombre_estacion", "anio_hidrologico")) %>%
  mutate(anual_gcm_corr = if_else(anio_hidrologico >= 1950 & anio_hidrologico <= 1969,
                                  pred_anual_rf, NA_real_)) %>%
  dplyr::select(-pred_anual_rf)

# B. Para 1970–2019 desde df_periodos_entrenamiento
df_maximos_anuales_gcm <- df_maximos_anuales_gcm %>%
  left_join(entrenamiento_70_19, by = c("codigo", "nombre_estacion", "anio_hidrologico")) %>%
  mutate(anual_gcm_corr = if_else(anio_hidrologico >= 1970 & anio_hidrologico <= 2019,
                                  coalesce(anual_gcm_corr, pred_anual_rf),
                                  anual_gcm_corr)) %>%
  dplyr::select(-pred_anual_rf)

# C. Para 2020–2099 desde df_periodos_futuro
df_maximos_anuales_gcm <- df_maximos_anuales_gcm %>%
  left_join(futuro_20_99, by = c("codigo", "nombre_estacion", "anio_hidrologico")) %>%
  mutate(anual_gcm_corr = if_else(anio_hidrologico >= 2020 & anio_hidrologico <= 2099,
                                  coalesce(anual_gcm_corr, pred_anual_rf),
                                  anual_gcm_corr)) %>%
  dplyr::select(-pred_anual_rf)


View(df_maximos_anuales_gcm)



# 
# # SIN VISVIRI
# 
# 
# # 1. Extraer columnas necesarias desde las fuentes
# pasado_anual <- df_periodos_pasado %>%
#   dplyr::select(codigo, nombre_estacion, anio_hidrologico, pred_anual_rf)
# 
# futuro_anual <- df_periodos_futuro %>%
#   dplyr::select(codigo, nombre_estacion, anio_hidrologico, pred_anual_rf)
# 
# # 2. Filtrar por rangos
# pasado_50_69 <- pasado_anual %>% dplyr::filter(anio_hidrologico >= 1950, anio_hidrologico <= 1969)
# pasado_70_19 <- pasado_anual %>% dplyr::filter(anio_hidrologico >= 1970, anio_hidrologico <= 2019)
# futuro_20_99 <- futuro_anual %>% dplyr::filter(anio_hidrologico >= 2020, anio_hidrologico <= 2099)
# 
# # 3. Agregar datos a df_maximos_anuales_gcm
# 
# # A. Para 1950–1969 desde df_periodos_pasado
# df_maximos_anuales_gcm <- df_maximos_anuales_gcm %>%
#   left_join(pasado_50_69, by = c("codigo", "nombre_estacion", "anio_hidrologico")) %>%
#   mutate(anual_gcm_corr = if_else(anio_hidrologico >= 1950 & anio_hidrologico <= 1969,
#                                   pred_anual_rf, NA_real_)) %>%
#   dplyr::select(-pred_anual_rf)
# 
# 
# 
# # B. Para 1970–2019 también desde df_periodos_pasado
# df_maximos_anuales_gcm <- df_maximos_anuales_gcm %>%
#   left_join(pasado_70_19, by = c("codigo", "nombre_estacion", "anio_hidrologico")) %>%
#   mutate(anual_gcm_corr = if_else(anio_hidrologico >= 1970 & anio_hidrologico <= 2019,
#                                   coalesce(anual_gcm_corr, pred_anual_rf),
#                                   anual_gcm_corr)) %>%
#   dplyr::select(-pred_anual_rf)
# 
# # C. Para 2020–2099 desde df_periodos_futuro
# df_maximos_anuales_gcm <- df_maximos_anuales_gcm %>%
#   left_join(futuro_20_99, by = c("codigo", "nombre_estacion", "anio_hidrologico")) %>%
#   mutate(anual_gcm_corr = if_else(anio_hidrologico >= 2020 & anio_hidrologico <= 2099,
#                                   coalesce(anual_gcm_corr, pred_anual_rf),
#                                   anual_gcm_corr)) %>%
#   dplyr::select(-pred_anual_rf)
# 
# 



View(df_maximos_anuales_gcm)

View(pred_anuales)


# Funciones de R² y PBIAS
calc_r2 <- function(obs, pred) {
  ss_res <- sum((obs - pred)^2, na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  1 - (ss_res / ss_tot)
}

calc_pbias <- function(obs, pred) {
  100 * sum(pred - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)
}

df_maximos_anuales_gcm <- df_maximos_anuales_gcm  %>%
  mutate(anual = if_else(anual == 0, 0.01, anual))

# Calcular por estación

df_metrics <-  df_maximos_anuales_gcm %>%
  group_by(nombre_estacion) %>%
  summarise(
    R2 = calc_r2(anual, anual_gcm),
    PBIAS = calc_pbias(anual, anual_gcm),
    R2_corr = calc_r2(anual, anual_gcm_corr),
    PBIAS_corr = calc_pbias(anual, anual_gcm_corr),
    .groups = "drop"
  )


View(df_metrics)



# CORRECCION RELATIVA A LOS ANUALES CORREGIDOS Y SIN CORREGIR PARA LOS MAXIMOS 

df_maximos_anuales_gcm <- df_maximos_anuales_gcm %>%
  mutate(
   maximo_gcm_corr = maximo_gcm * (anual_gcm_corr / anual_gcm)
  )


df_maximos_anuales_gcm <- df_maximos_anuales_gcm  %>%
  rename(Año = anio_hidrologico)


f_ma_msd_name_gcm <- function(df_name, order, name, df_metrics) {
  require(forecast)
  require(zoo)
  require(Kendall)
  require(ggplot2)
  require(reshape2)
  require(dplyr)
  require(patchwork)

  # --- Filtrar datos por estación ---
  df_in <- df_name %>%
    dplyr::filter(nombre_estacion == name) %>%
    dplyr::select(Año, maximo, anual)

  df_in_gcm <- df_name %>%
    dplyr::filter(nombre_estacion == name) %>%
    dplyr::select(Año, maximo_gcm, anual_gcm)

  
    df_in_gcm_corr <- df_name %>%
    dplyr::filter(nombre_estacion == name) %>%
    dplyr::select(Año, maximo_gcm_corr, anual_gcm_corr)


  # --- Procesar máximos históricos ---
    
  df_ma_msd_max <- data.frame(
    Year = df_in$Año,
    SVE = as.numeric(df_in$maximo),
    ma = as.numeric(ma(df_in$maximo, order = order)),
    msd = as.numeric(rollapply(df_in$maximo, width = order,  na.rm = TRUE, FUN = sd, fill = NA, align = "center"))
  )

  df_ma_msd_max_gcm <- data.frame(
    Year = df_in_gcm$Año,
    SVE = as.numeric(df_in_gcm$maximo_gcm),
    ma = as.numeric(ma(df_in_gcm$maximo_gcm, order = order)),
    msd = as.numeric(rollapply(df_in_gcm$maximo_gcm, width = order, na.rm = TRUE,  FUN = sd, fill = NA, align = "center"))
  )
  
  df_ma_msd_max_gcm_corr <- data.frame(
    Year = df_in_gcm_corr$Año,
    SVE = as.numeric(df_in_gcm_corr$maximo_gcm_corr),
    ma = as.numeric(ma(df_in_gcm_corr$maximo_gcm_corr, order = order)),
    msd = as.numeric(rollapply(df_in_gcm_corr$maximo_gcm_corr, width = order, na.rm = TRUE, FUN = sd, fill = NA, align = "center"))
  )
  
  
  # --- Datos en formato largo ---
  
  df_max_long <- melt(df_ma_msd_max, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("SVE", "ma", "msd"),
                             labels = c("Máximos anuales",
                                        "Media móvil máximos",
                                        "Desviación móvil máximos")))

  df_max_long_gcm <- melt(df_ma_msd_max_gcm, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("SVE", "ma", "msd"),
                             labels = c("Máximos anuales",
                                        "Media móvil máximos",
                                        "Desviación móvil máximos")))

    df_max_long_gcm_corr <- melt(df_ma_msd_max_gcm_corr, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("SVE", "ma", "msd"),
                             labels = c("Máximos anuales",
                                        "Media móvil máximos",
                                        "Desviación móvil máximos")))

  
  # --- Test de tendencia máximos ---
    
  df_test_max <- data.frame(
    variable = levels(df_max_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_max$SVE)$sl,
      MannKendall(df_ma_msd_max$ma)$sl,
      MannKendall(df_ma_msd_max$msd)$sl
    ),
    x = quantile(df_ma_msd_max$Year, 0.05),
    y = c(
      quantile(na.omit(df_ma_msd_max$SVE), 0.975),
      quantile(na.omit(df_ma_msd_max$ma), 0.975),
      quantile(na.omit(df_ma_msd_max$msd), 0.975)
    )
  )

  df_test_max_gcm <- data.frame(
    variable = levels(df_max_long_gcm$variable),
    p_valor = c(
      MannKendall(df_ma_msd_max_gcm$SVE)$sl,
      MannKendall(df_ma_msd_max_gcm$ma)$sl,
      MannKendall(df_ma_msd_max_gcm$msd)$sl
    ),
    x = quantile(df_ma_msd_max_gcm$Year, 0.95),
    y = c(
      quantile(na.omit(df_ma_msd_max_gcm$SVE), 0.90),
      quantile(na.omit(df_ma_msd_max_gcm$ma), 0.90),
      quantile(na.omit(df_ma_msd_max_gcm$msd), 0.90)
    )
  )

  
    df_test_max_gcm_corr <- data.frame(
    variable = levels(df_max_long_gcm_corr$variable),
    p_valor = c(
      MannKendall(df_ma_msd_max_gcm_corr$SVE)$sl,
      MannKendall(df_ma_msd_max_gcm_corr$ma)$sl,
      MannKendall(df_ma_msd_max_gcm_corr$msd)$sl
    ),
    x = quantile(df_ma_msd_max_gcm_corr$Year, 0.7),
    y = c(
      quantile(na.omit(df_ma_msd_max_gcm_corr$SVE), 0.7),
      quantile(na.omit(df_ma_msd_max_gcm_corr$ma), 0.7),
      quantile(na.omit(df_ma_msd_max_gcm_corr$msd), 0.7)
    )
  )
  
  

  df_test_max$variable <- factor(df_test_max$variable, levels = levels(df_max_long$variable))
  
  df_test_max_gcm$variable <- factor(df_test_max_gcm$variable, levels = levels(df_max_long_gcm$variable))
  
  df_test_max_gcm_corr$variable <- factor(df_test_max_gcm_corr$variable, levels = levels(df_max_long_gcm_corr$variable))


  
  # --- Gráfico máximos ---
  

  fig_max <- ggplot(df_max_long, aes(x = Year, y = value)) +
    geom_line(linewidth = 0.5) +
    geom_point() +
    
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
    geom_line(data = df_max_long_gcm, aes(x = Year, y = value), color = "red", linewidth = 0.5, inherit.aes = FALSE) +
    geom_point(data = df_max_long_gcm, aes(x = Year, y = value), color = "red", size = 1, alpha = 0.6, inherit.aes = FALSE) +
    geom_smooth(data = df_max_long_gcm, aes(x = Year, y = value), method = "lm", se = FALSE, color = "red", linewidth = 0.5, inherit.aes = FALSE) +
    
    
    
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
    geom_line(data = df_max_long_gcm_corr, aes(x = Year, y = value), color = "darkgreen", linewidth = 0.5, inherit.aes = FALSE) +
    geom_point(data = df_max_long_gcm_corr, aes(x = Year, y = value), color = "darkgreen", size = 1, alpha = 0.6, inherit.aes = FALSE) +
    geom_smooth(data = df_max_long_gcm_corr, aes(x = Year, y = value), method = "lm", se = FALSE, color = "darkgreen", linewidth = 0.5, inherit.aes = FALSE) +
    
    facet_grid(variable ~ ., scales = "free_y") +
    
    geom_label(data = df_test_max,
               aes(x = x, y = y, label = paste0("p-valor M-K: ", round(p_valor, 3))),
               hjust = 0, size = 3, fill = "white", color = "black", label.size = 0.3, inherit.aes = FALSE) +
    geom_label(data = df_test_max_gcm,
               aes(x = x, y = y, label = paste0("p-valor GCM: ", round(p_valor, 3))),
               hjust = 1, size = 3, fill = "white", color = "red", label.size = 0.3, inherit.aes = FALSE) +
    
    geom_label(data = df_test_max_gcm_corr,
               aes(x = x, y = y, label = paste0("p-valor GCM corr: ", round(p_valor, 3))),
               hjust = 1, size = 3, fill = "white", color = "darkgreen", label.size = 0.3, inherit.aes = FALSE) +

    labs(x = "Año", y = "[mm]") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white"))

  # --- Procesar anuales

  df_ma_msd_anual <- data.frame(
    Year = df_in$Año,
    anual = as.numeric(df_in$anual),
    ma = as.numeric(ma(df_in$anual, order = order)),
    msd = as.numeric(rollapply(df_in$anual, width = order, na.rm = TRUE, FUN = sd, fill = NA, align = "center"))
  )

  df_ma_msd_anual_gcm <- data.frame(
    Year = df_in_gcm$Año,
    anual = as.numeric(df_in_gcm$anual_gcm),
    ma = as.numeric(ma(df_in_gcm$anual_gcm, order = order)),
    msd = as.numeric(rollapply(df_in_gcm$anual_gcm, width = order, na.rm = TRUE, FUN = sd, fill = NA, align = "center"))
  )

  df_ma_msd_anual_gcm_corr <- data.frame(
    Year = df_in_gcm_corr$Año,
    anual = as.numeric(df_in_gcm_corr$anual_gcm_corr),
    ma = as.numeric(ma(df_in_gcm_corr$anual_gcm_corr, order = order)),
    msd = as.numeric(rollapply(df_in_gcm_corr$anual_gcm_corr, width = order,na.rm = TRUE,  FUN = sd, fill = NA, align = "center"))
  )
  
  # --- Datos en formato largo ---
  
  df_anual_long <- melt(df_ma_msd_anual, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("anual", "ma", "msd"),
                             labels = c("Precipitación anual",
                                        "Media móvil anual",
                                        "Desviación móvil anual")))

   df_anual_long_gcm <- melt(df_ma_msd_anual_gcm, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("anual", "ma", "msd"),
                             labels = c("Precipitación anual",
                                        "Media móvil anual",
                                        "Desviación móvil anual")))
  
  
  df_anual_long_gcm_corr <- melt(df_ma_msd_anual_gcm_corr, id.vars = "Year") %>%
    mutate(variable = factor(variable,
                             levels = c("anual", "ma", "msd"),
                             labels = c("Precipitación anual",
                                        "Media móvil anual",
                                        "Desviación móvil anual")))

  # --- Test de tendencia anual ---
  df_test_anual <- data.frame(
    variable = levels(df_anual_long$variable),
    p_valor = c(
      MannKendall(df_ma_msd_anual$anual)$sl,
      MannKendall(df_ma_msd_anual$ma)$sl,
      MannKendall(df_ma_msd_anual$msd)$sl
    ),
    x = quantile(df_ma_msd_anual$Year, 0.05),
    y = c(
      quantile(na.omit(df_ma_msd_anual$anual), 0.975),
      quantile(na.omit(df_ma_msd_anual$ma), 0.975),
      quantile(na.omit(df_ma_msd_anual$msd), 0.975)
    )
  )

  df_test_anual_gcm <- data.frame(
    variable = levels(df_anual_long_gcm$variable),
    p_valor = c(
      MannKendall(df_ma_msd_anual_gcm$anual)$sl,
      MannKendall(df_ma_msd_anual_gcm$ma)$sl,
      MannKendall(df_ma_msd_anual_gcm$msd)$sl
    ),
    x = quantile(df_ma_msd_anual_gcm$Year, 0.95),
    y = c(
      quantile(na.omit(df_ma_msd_anual_gcm$anual), 0.90),
      quantile(na.omit(df_ma_msd_anual_gcm$ma), 0.90),
      quantile(na.omit(df_ma_msd_anual_gcm$msd), 0.90)
      )
      )
  
  
df_test_anual_gcm_corr <- data.frame(
  variable = levels(df_anual_long_gcm_corr$variable),
  p_valor = c(
    MannKendall(df_ma_msd_anual_gcm_corr$anual)$sl,
    MannKendall(df_ma_msd_anual_gcm_corr$ma)$sl,
    MannKendall(df_ma_msd_anual_gcm_corr$msd)$sl
  ),
  x = quantile(df_ma_msd_anual_gcm_corr$Year, 0.7),
  y = c(
    quantile(na.omit(df_ma_msd_anual_gcm_corr$anual), 0.7),
    quantile(na.omit(df_ma_msd_anual_gcm_corr$ma), 0.7),
    quantile(na.omit(df_ma_msd_anual_gcm_corr$msd), 0.7)
  )
)
  
  

df_test_anual$variable <- factor(df_test_anual$variable, levels = levels(df_anual_long$variable))
df_test_anual_gcm$variable <- factor(df_test_anual_gcm$variable, levels = levels(df_anual_long$variable))

df_test_anual_gcm_corr$variable <- factor(df_test_anual_gcm_corr$variable, levels = levels(df_anual_long_gcm_corr$variable))



fig_anual <- ggplot(df_anual_long, aes(x = Year, y = value)) +
  geom_line(linewidth = 0.5) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  
  geom_line(data = df_anual_long_gcm, aes(x = Year, y = value), color = "red", linewidth = 0.5, inherit.aes = FALSE) +
  geom_point(data = df_anual_long_gcm, aes(x = Year, y = value), color = "red", size = 1, alpha = 0.6, inherit.aes = FALSE) +
  geom_smooth(data = df_anual_long_gcm, aes(x = Year, y = value), method = "lm", se = FALSE, color = "red", linewidth = 0.5, inherit.aes = FALSE) +

  geom_line(data = df_anual_long_gcm_corr, aes(x = Year, y = value), color = "darkgreen", linewidth = 0.5, inherit.aes = FALSE) +
  geom_point(data = df_anual_long_gcm_corr, aes(x = Year, y = value), color = "darkgreen", size = 1, alpha = 0.6, inherit.aes = FALSE) +
  geom_smooth(data = df_anual_long_gcm_corr, aes(x = Year, y = value), method = "lm", se = FALSE, color = "darkgreen", linewidth = 0.5, inherit.aes = FALSE) +

  facet_grid(variable ~ ., scales = "free_y") +
  
  geom_label(data = df_test_anual,
             aes(x = x, y = y, label = paste0("p-valor M-K Obs: ", round(p_valor, 3))),
             hjust = 0, size = 3, fill = "white", color = "black", label.size = 0.3, inherit.aes = FALSE) +
  geom_label(data = df_test_anual_gcm,
             aes(x = x, y = y, label = paste0("p-valor M-K GCM: ", round(p_valor, 3))),
             hjust = 1, size = 3, fill = "white", color = "red", label.size = 0.3, inherit.aes = FALSE) +
  geom_label(data = df_test_anual_gcm_corr,
             aes(x = x, y = y, label = paste0("p-valor M-K GCM corr: ", round(p_valor, 3))),
             hjust = 1, size = 3, fill = "white", color = "darkgreen", label.size = 0.3, inherit.aes = FALSE) +
  
  labs(x = "Año", y = "[mm]") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))


# final_plot <- fig_max + fig_anual +
# plot_layout(ncol = 2, guides = "collect") +
# plot_annotation(title = paste0("Series de Precipitación Anual y Máxima Anual para Estación ", name)) &
# theme(plot.title = element_text(hjust = 0.5))


R2 <- df_metrics %>%
  dplyr::filter(nombre_estacion == name) %>%
  pull(R2)


PBIAS <- df_metrics %>%
  filter(nombre_estacion == name) %>%
  pull(PBIAS)


R2_corr <- df_metrics %>%
  dplyr::filter(nombre_estacion == name) %>%
  pull(R2_corr)


PBIAS_corr <- df_metrics %>%
  filter(nombre_estacion == name) %>%
  pull(PBIAS_corr)



final_plot_anual <- fig_anual +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = bquote(
      "Estación " * .(name) ~ 
      "- " * R^2 == .(round(R2, 2)) * 
      " | " ~ PBIAS == .(round(PBIAS, 2)) * "%" ~ 
      "| " * R^2[corr] == .(round(R2_corr, 2)) * 
      " | " ~ PBIAS[corr] == .(round(PBIAS_corr, 2)) * "%"
    )
  ) &
  theme(plot.title = element_text(hjust = 0.5))


return(list(
df_max = df_ma_msd_max,
df_anual = df_ma_msd_anual,
fig = final_plot_anual
))
}


plot_tendencias_estacion <- f_ma_msd_name_gcm(df_maximos_anuales_gcm, order = 5, name = "Visviri", df_metrics)



visviri_plot_sin_visviri  = plot_tendencias_estacion$fig


visviri_plot_con_visviri  =  plot_tendencias_estacion$fig


panel_visviri <- wrap_elements(visviri_plot_sin_visviri ) + wrap_elements(visviri_plot_con_visviri) +
  plot_layout(ncol = 2, guides = "collect")

panel_visviri


















