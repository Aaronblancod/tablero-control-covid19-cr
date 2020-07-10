#-------------------------------------- Paquetes ---------------------------------------------------------

#Paquetes
library(flexdashboard)
library(plotly)
library(dygraphs)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)

#------------------- Heatmap Distritos -----------------------------------------
#Ejemplo mapa
paleta_roja <- colorBin(palette = "Reds", 
                        domain = sf_activos_cantones_ultima_fecha$activos, bins = 10)
m2 <- leaflet(sf_activos_cantones_ultima_fecha) %>% 
  fitBounds(lng1 = -86, lng2 = -82, lat1 = 8, lat2 = 11) %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World TopoMap") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
  addPolygons(fillColor = ~paleta_roja (activos), stroke=T, fillOpacity = 1,
              color="black", weight=0.2, opacity= 0.5,
              group = "Cantones",
              popup = paste("Provincia: ", sf_activos_cantones_ultima_fecha$provincia, "<br>",
                            "Cantón: ", sf_activos_cantones_ultima_fecha$canton, "<br>",
                            "Activos: ", sf_activos_cantones_ultima_fecha$activos)) %>%
  addLegend("bottomright", pal = paleta_roja, values = ~activos,
            title = "Casos activos",opacity = 1) %>%  
  addLayersControl(baseGroups = c("Esri World TopoMap", "CartoDB Dark"), overlayGroups = c("Cantones"),
                   options = layersControlOptions(collapsed = TRUE))%>% 
  addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldImagery, position = "bottomleft")
m2

#------------------------ Datos previos --------------------------------------
caracter_separador <- ','

archivo_general_pais <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_02_CSV_GENERAL.csv'
archivo_positivos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_02_CSV_POSITIVOS.csv'
archivo_activos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_02_CSV_ACTIVOS.csv'
archivo_recuperados_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_02_CSV_RECUP.csv'
archivo_fallecidos_cantones <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_02_CSV_FALLECIDOS.csv'
archivo_activos_distritos <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_02_CSV_GENERAL_DISTRITOS.csv'


# Data frame de datos generales por país
df_general_pais <- read.csv(archivo_general_pais, sep = caracter_separador)
df_general_pais$FECHA <- as.Date(df_general_pais$FECHA, "%d/%m/%Y")
# Data frame de datos generales por país en la última fecha
df_general_pais_ultima_fecha <- 
  df_general_pais %>%
  filter(FECHA == max(FECHA, na.rm = TRUE))

# Data frame de datos generales por distrtito
df_general_distrito <- read.csv(archivo_activos_distritos, sep = caracter_separador)
df_general_distrito$FECHA <- as.Date(df_general_distrito$X.2, "%d/%m/%Y")
# Data frame de datos generales por distrito en la última fecha
df_general_distrito_ultima_fecha <- 
  df_general_distrito %>%
  filter(X.2 == max(X.2, na.rm = TRUE))





# Objeto sf de distritos
url_base_wfs_ign_5mil <- "http://geos.snitcr.go.cr/be/IGN_5/wfs?"
solicitud_wfs_ign_5mil_distrito <- "request=GetFeature&service=WFS&version=2.0.0&typeName=limitedistrital_5k&outputFormat=application/json"
sf_disitrito <-
  st_read(paste0(url_base_wfs_ign_5mil, solicitud_wfs_ign_5mil_distrito)) %>%
  st_simplify(dTolerance = 1000) %>%
  st_transform(4326)


#--------------- Datos activos por distrito --------------------------------

# Data frame de casos activos por cantón
df_activos_distritos_ancho <- read.csv(archivo_activos_distritos, sep = caracter_separador)
df_activos_distritos <- df_activos_distritos_ancho %>%
  pivot_longer(cols = c(-cod_provin, -provincia, -codigo_dta, -distrito), 
               names_to = "fecha", values_to = "activos")
df_activos_distritos$fecha <- as.Date(df_activos_distritos$fecha, "X%d.%m.%Y")

# Data frame de casos activos por cantón en la última fecha
df_activos_cantones_ultima_fecha <- 
  df_activos_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, activos)

# Objeto sf de casos activos en cantones en la última fecha
sf_activos_cantones_ultima_fecha <-
  left_join(sf_cantones, df_activos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(activos))




