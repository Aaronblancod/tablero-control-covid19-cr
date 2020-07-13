#-------------------------------------- Paquetes ---------------------------------------------------------

#Paquetes
library(flexdashboard)
library(plotly)
library(dygraphs)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(leaflet.extras)

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
archivo_general_distritos <- 'https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/covid19/ms/07_02_CSV_GENERAL_DISTRITOS.csv'


# Data frame de datos generales por país
df_general_pais <- read.csv(archivo_general_pais, sep = caracter_separador)
df_general_pais$FECHA <- as.Date(df_general_pais$FECHA, "%d/%m/%Y")
# Data frame de datos generales por país en la última fecha
df_general_pais_ultima_fecha <- 
  df_general_pais %>%
  filter(FECHA == max(FECHA, na.rm = TRUE))

#--------------- Datos activos por distrito --------------------------------

# Carga del archivo CSV en un data frame
df_general_distritos_sucio <- read.csv(archivo_general_distritos)
# Eliminación de filas y columnas que corresponden a encabezados, totales, etc.
df_general_distritos_ultima_fecha <- df_general_distritos_sucio[-c(1:5), -c(1, 3, 10, 11)]
# Cambio de nombre de las columnas
df_general_distritos_ultima_fecha <- 
  df_general_distritos_ultima_fecha %>%
  rename(provincia = X.1, canton = X.3,  distrito = X.4,
         positivos = X.5, recuperados = X.6, 
         fallecidos = X.7, activos = X.8) %>%  
  mutate_all(funs(sub("^\\s*$", NA, .))) %>% 
  mutate(distrito = if_else(distrito == "El Carmen", "Carmen", distrito)) %>%
  mutate(distrito = if_else(distrito == "Valle de La Estrella", "Valle La Estrella", distrito)) %>%
  mutate(distrito = if_else(distrito == "La Amistad", "La  Amistad", distrito)) %>%
  fill(c(1,2))
# Borrado de las filas con valor de NA o de "Sin información de distrito" en la columna de distrito
df_general_distritos_ultima_fecha <- df_general_distritos_ultima_fecha[!is.na(df_general_distritos_ultima_fecha$distrito), ]
df_general_distritos_ultima_fecha <- df_general_distritos_ultima_fecha[df_general_distritos_ultima_fecha$distrito != 'Sin información de distrito', ]
# Conversión a integer de los tipos de datos de las columnas con cifras
df_general_distritos_ultima_fecha$positivos <- as.integer(df_general_distritos_ultima_fecha$positivos)
df_general_distritos_ultima_fecha$recuperados <- as.integer(df_general_distritos_ultima_fecha$recuperados)
df_general_distritos_ultima_fecha$fallecidos <- as.integer(df_general_distritos_ultima_fecha$fallecidos)
df_general_distritos_ultima_fecha$activos <- as.integer(df_general_distritos_ultima_fecha$activos)
# Objeto sf de distritos
sf_distritos <- st_read('https://raw.githubusercontent.com/geoprocesamiento-2020i/datos/master/delimitacion-territorial-administrativa/cr/ign/cr_limite_distrital_ign_wgs84.geojson')
# Objeto sf de casos positivos en distritos en la última fecha
sf_general_distritos_ultima_fecha <-
left_join(sf_distritos, df_general_distritos_ultima_fecha, by = c('provincia', 'canton', 'distrito'))

#------------------------------------- Mapa de calor -------------------------------------------------------
paleta_azul <- colorBin(palette = "Blues", domain = sf_positivos_cantones_ultima_fecha$positivos, 
                        bins = 10)
m1 <- leaflet(sf_positivos_cantones_ultima_fecha) %>% 
  fitBounds(lng1 = -86, lng2 = -82, lat1 = 8, lat2 = 11) %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri World TopoMap") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark") %>%
  addPolygons(fillColor = ~paleta_azul(positivos), stroke=T, fillOpacity = 1,
              color="black", weight=0.2, opacity= 0.5, group = "Cantones",
              popup = paste("Provincia: ", sf_positivos_cantones_ultima_fecha$provincia, "<br>",
                            "Cantón: ", sf_positivos_cantones_ultima_fecha$canton, "<br>",
                            "Positivos: ", sf_positivos_cantones_ultima_fecha$positivos)) %>%
  addLegend("bottomright", pal = paleta_azul, values = ~positivos, title = "Casos positivos", 
            opacity = 1)%>%  
  addLayersControl(baseGroups = c("Esri World TopoMap", "CartoDB Dark"), overlayGroups = c("Cantones"),
                   options = layersControlOptions(collapsed = TRUE))%>%  
  addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldImagery, position = "bottomleft")
m1

leaflet(spdf) %>%
  addProviderTiles(providers$Thunderforest.TransportDark) %>%
  addWebGLHeatmap(size=25,units='px')

library(leaflet.extras)

heatmap <- leaflet(sf_general_distritos_ultima_fecha) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addWebGLHeatmap(lng=~long, lat=~lat, intensity = ~mag, size=60000)
heatmap


# mapaa
sf_activos_distritos <- sf_general_distritos_ultima_fecha[, "activos"]

sf_activos_distritos_puntos <- st_cast(sf_activos_distritos$geometry, "MULTIPOINT")


coordenadas <- do.call(rbind, st_geometry(sf_activos_distritos_puntos)) %>% 
  as_tibble() %>% setNames(c("long","lat"))

# Mapa web
m5 <- leaflet(coordenadas) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addHeatmap( lng = ~long, lat = ~lat,  blur = 30, max = 0.005, radius = 12)
m5



sf_activos_distritos <- sf_general_distritos_ultima_fecha[, c("activos", "distrito")]

sf_activos_distritos_puntos <- st_cast(sf_activos_distritos$geometry, "MULTIPOINT")

coordenadas <- do.call(rbind, st_geometry(sf_activos_distritos_puntos)) %>% 
  as_tibble() %>% setNames(c("long","lat"))