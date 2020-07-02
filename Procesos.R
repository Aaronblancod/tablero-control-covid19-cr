#Paquetes
library(flexdashboard)
library(plotly)
library(dygraphs)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)

#-------------------- Colores ---------------------

color_positivos <- 'darkblue'
color_activos <- 'darkred'
color_recuperados <- 'green'
color_fallecidos <- 'grey'

color_nuevos_positivos <- 'lightblue'

color_hospitalizados <- 'lightblue'
color_salon <- 'lightblue'
color_uci <- 'lightblue'

#--------------------- Íconos ---------------------

icono_positivos <- 'fas fa-user-md'
icono_activos <- 'fas fa-user-md'
icono_recuperados <- 'fas fa-user-md'
icono_fallecidos <- 'fas fa-user-md'

icono_nuevos_positivos <- 'fas fa-user-md'

icono_hospitalizados <- 'fas fa-user-md'
icono_salon <- 'fas fa-user-md'
icono_uci <- 'fas fa-user-md'

#--------------- Otros parámetros -----------------

# Separador para lectura de datos CSV
caracter_separador <- ','

#--------------- Archivos de datos ----------------

archivo_general_pais <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_GENERAL.csv'

archivo_positivos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_POSITIVOS.csv'
archivo_activos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_ACTIVOS.csv'
archivo_recuperados_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_RECUP.csv'
archivo_fallecidos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_FALLECIDOS.csv'

#---------------------- Datos ---------------------

# Data frame de datos generales por país
df_general_pais <- read.csv(archivo_general_pais, sep = caracter_separador)
df_general_pais$FECHA <- as.Date(df_general_pais$FECHA, "%d/%m/%Y")

# Data frame de datos generales por país en la última fecha
df_general_pais_ultima_fecha <- 
  df_general_pais %>%
  filter(FECHA == max(FECHA, na.rm = TRUE))

# Data frame de casos positivos por cantón
df_positivos_cantones_ancho <- read.csv(archivo_positivos_cantones, sep = caracter_separador)
df_positivos_cantones <-
  df_positivos_cantones_ancho %>%
  pivot_longer(cols = c(-cod_provin, -provincia, -cod_canton, -canton), names_to = "fecha", values_to = "positivos")
df_positivos_cantones$fecha <- as.Date(df_positivos_cantones$fecha, "X%d.%m.%Y")

# Data frame de casos positivos por cantón en la última fecha
df_positivos_cantones_ultima_fecha <- 
  df_positivos_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, positivos)


# Objeto sf de cantones
url_base_wfs_ign_5mil <- "http://geos.snitcr.go.cr/be/IGN_5/wfs?"
solicitud_wfs_ign_5mil_cantones <- "request=GetFeature&service=WFS&version=2.0.0&typeName=IGN_5:limitecantonal_5k&outputFormat=application/json"
sf_cantones <-
  st_read(paste0(url_base_wfs_ign_5mil, solicitud_wfs_ign_5mil_cantones)) %>%
  st_simplify(dTolerance = 1000) %>%
  st_transform(4326)

# Objeto sf de casos positivos en cantones en la última fecha
sf_positivos_cantones_ultima_fecha <-
  left_join(sf_cantones, df_positivos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(positivos))

##########################################################################################################
