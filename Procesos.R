#-----------------------------------------------------------------------------------------------------------

#Paquetes
library(flexdashboard)
library(plotly)
library(dygraphs)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)

#---------------------------------------------------------------------------------------------------------

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

#--------------------------------------------------------------------------------------------------------

#--------------- Otros parámetros -----------------

# Separador para lectura de datos CSV
caracter_separador <- ','

#--------------- Archivos de datos ----------------

archivo_general_pais <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_GENERAL.csv'

archivo_positivos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_POSITIVOS.csv'
archivo_activos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_ACTIVOS.csv'
archivo_recuperados_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_RECUP.csv'
archivo_fallecidos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_01_CSV_FALLECIDOS.csv'

#---------------------------------------------------------------------------------------------------------

#---------------------- Datos Generales---------------------

# Data frame de datos generales por país
df_general_pais <- read.csv(archivo_general_pais, sep = caracter_separador)
df_general_pais$FECHA <- as.Date(df_general_pais$FECHA, "%d/%m/%Y")

# Data frame de datos generales por país en la última fecha
df_general_pais_ultima_fecha <- 
  df_general_pais %>%
  filter(FECHA == max(FECHA, na.rm = TRUE))

# Objeto sf de cantones
url_base_wfs_ign_5mil <- "http://geos.snitcr.go.cr/be/IGN_5/wfs?"
solicitud_wfs_ign_5mil_cantones <- "request=GetFeature&service=WFS&version=2.0.0&typeName=IGN_5:limitecantonal_5k&outputFormat=application/json"
sf_cantones <-
  st_read(paste0(url_base_wfs_ign_5mil, solicitud_wfs_ign_5mil_cantones)) %>%
  st_simplify(dTolerance = 1000) %>%
  st_transform(4326)

#---------------------------------------------------------------------------------------------------------

#---------------------- Datos positivos ---------------------

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

# Objeto sf de casos positivos en cantones en la última fecha
sf_positivos_cantones_ultima_fecha <-
  left_join(sf_cantones, df_positivos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(positivos))

#---------------------------------------------------------------------------------------------------------
#---------------------- Datos activos ---------------------

# Data frame de casos activos por cantón
df_activos_cantones_ancho <- read.csv(archivo_activos_cantones, sep = caracter_separador)
df_activos_cantones <- df_activos_cantones_ancho %>%
                       pivot_longer(cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
                                    names_to = "fecha", values_to = "activos")
df_activos_cantones$fecha <- as.Date(df_activos_cantones$fecha, "X%d.%m.%Y")

# Data frame de casos activos por cantón en la última fecha
df_activos_cantones_ultima_fecha <- 
  df_activos_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, activos)

# Objeto sf de casos activos en cantones en la última fecha
sf_activos_cantones_ultima_fecha <-
  left_join(sf_cantones, df_activos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(activos))

#---------------------- Datos recuperados -----------------
# Data frame de casos recuperados por cantón
df_recuperados_cantones_ancho <- read.csv(archivo_recuperados_cantones, sep = caracter_separador)
df_recuperdos_cantones <- df_recuperados_cantones_ancho %>%
  pivot_longer(cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
               names_to = "fecha", values_to = "recuperados")
df_activos_cantones$fecha <- as.Date(df_activos_cantones$fecha, "X%d.%m.%Y")

# Data frame de casos recuperados por cantón en la última fecha
df_recuperados_cantones_ultima_fecha <- 
  df_recuperados_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, recuperados)

# Objeto sf de casos recuperados en cantones en la última fecha
sf_recuperados_cantones_ultima_fecha <-
  left_join(sf_cantones, df_recuperados_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(recuperados))

#---------------------- Datos fallecidos ------------------
# Data frame de casos fallecidos por cantón
df_fallecidos_cantones_ancho <- read.csv(archivo_fallecidos_cantones, sep = caracter_separador)
df_fallecidos_cantones <- df_fallecidos_cantones_ancho %>%
  pivot_longer(cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
               names_to = "fecha", values_to = "fallecidos")
df_fallecidos_cantones$fecha <- as.Date(df_fallecidos_cantones$fecha, "X%d.%m.%Y")

# Data frame de casos fallecidos por cantón en la última fecha
df_fallecidos_cantones_ultima_fecha <- 
  df_fallecidos_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, fallecidos)

# Objeto sf de casos fallecidos en cantones en la última fecha
sf_fallecidos_cantones_ultima_fecha <-
  left_join(sf_cantones, df_fallecidos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(fallecidos))

#---------------------- Datos distritos -------------------
#--------------------------------------------------------------------------------------------------------
#---------------------- Value boxes -------------------
  Row
-----------------------------------------------------------------------
  
  
  ### Casos positivos {.value-box}
  ```{r}
valueBox(value = paste(format(df_general_pais_ultima_fecha$positivos, big.mark = ","), "", sep = " "), 
         caption = "Total de casos positivos", 
         icon = icono_positivos, 
         color = color_positivos
)
```

### Casos mujeres {.value-box}
```{r}
valueBox(value = paste(format(df_general_pais_ultima_fecha$muj_posi, big.mark = ","), " (",
                       round(100 * df_general_pais_ultima_fecha$muj_posi / df_general_pais_ultima_fecha$positivos, 1), 
                       "%)", sep = ""), 
         caption = "Total de mujeres",
         icon = icono_mujeres, 
         color = color_positivos
)
```


### Casos hombres {.value-box}
```{r}
valueBox(value = paste(format(df_general_pais_ultima_fecha$hom_posi, big.mark = ","), " (",
                       round(100 * df_general_pais_ultima_fecha$hom_posi / df_general_pais_ultima_fecha$positivos, 1), 
                       "%)", sep = ""), 
         caption = "Total de hombres",
         icon = icono_hombres, 
         color = color_positivos
)
```

Row
-----------------------------------------------------------------------
  
  
  ### Casos menores de edad {.value-box}
  ```{r}
valueBox(value = paste(format(df_general_pais_ultima_fecha$menor_posi, big.mark = ","), " (",
                       round(100 * df_general_pais_ultima_fecha$menor_posi / df_general_pais_ultima_fecha$positivos, 1), 
                       "%)", sep = ""), 
         caption = "Total de menores de edad",
         icon = icono_menor, 
         color = color_positivos
)
```


### Casos adultos {.value-box}
```{r}
valueBox(value = paste(format(df_general_pais_ultima_fecha$adul_posi, big.mark = ","), "(",                          round(100 * df_general_pais_ultima_fecha$adul_posi / df_general_pais_ultima_fecha$positivos, 1), 
                       "%)", sep = ""), 
         caption = "Total de Adultos", 
         icon = icono_positivos,
         color = color_positivos
)
```


### Casos adultos mayores
```{r}
valueBox(value = paste(format(df_general_pais_ultima_fecha$am_posi, big.mark = ","), "(",                          round(100 * df_general_pais_ultima_fecha$am_posi / df_general_pais_ultima_fecha$positivos, 1), 
                       "%)", sep = ""), 
         caption = "Total de Adultos", 
         icon = icono_adultom,
         color = color_positivos
)
```

icono_positivos <- 'ion-android-person-add'
icono_activos <- 'fas fa-user-md'
icono_recuperados <- 'fas fa-user-md'
icono_fallecidos <- 'fas fa-user-md'
icono_mujeres <- 'fas fa-user-md'
icono_hombres <- 'fas fa-user-md'
icono_menor <- 'fas fa-user-md'
icono_adultom <- 'fas fa-user-md'
icono_nuevos_positivos <- 'fas fa-user-md'
icono_hospitalizados <- 'fas fa-user-md'
icono_salon <- 'fas fa-user-md'
icono_uci <- 'fas fa-user-md'