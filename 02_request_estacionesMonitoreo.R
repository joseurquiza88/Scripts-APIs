
# Este codigo permite optener desde diversas fuentes infomracion sobre la calidad del aire
# a traves de distintas APIs. Algunas es posible contar con informacion historica y otras no

###############################################################################
#                    ----    API aqicn.org        -----
# Formas de obtener datos de la API https://aqicn.org/map/argentina/es/


# ----    Por ciudad
url <- "https://api.waqi.info/feed/Paris/?token=3a1671d7bc5576694e2d76e9fa12e8ee15913b0f"
url <- "https://api.waqi.info/feed/BuenosAires/?token=3a1671d7bc5576694e2d76e9fa12e8ee15913b0f"
response <- GET(url)
resp_json <- fromJSON(content(response, as = "text"))


# ----   Por coordenadas
# Buenos Aires
url_3 <-"https://api.waqi.info/feed//geo:-34.652736726195336;-58.34336303374917/?token=3a1671d7bc5576694e2d76e9fa12e8ee15913b0f"
# Mendoza. Ojo no devuelve estaciones de Mendoza sino de Chile.
# Entonces siempre devuelve los datos de la estacion mas cercana a las coordenadas  ingresadas
url_4 <-"https://api.waqi.info/feed//geo:-32.898346;-68.849693/?token=3a1671d7bc5576694e2d76e9fa12e8ee15913b0f"
response <- GET(url_4)
resp_json <- fromJSON(content(response, as = "text"))

# ----   Bounding Box
# Esta request devuelve las estaciones que estan disponibles en ese bounding box
# (latitude, longitude) bottom left, (latitude, longitude) top right
#BA
url <-"https://api.waqi.info/v2/map/bounds?latlng=-34.7942359128351,-58.888460968407635,-34.374067211490434,-58.493219406400435&networks=all&token=3a1671d7bc5576694e2d76e9fa12e8ee15913b0f"
# Villa constitucion
#url <-"https://api.waqi.info/v2/map/bounds?latlng=-33.27091780220122,-60.35509544999683,-33.222316165014895,-60.32960534693428&networks=all&token=3a1671d7bc5576694e2d76e9fa12e8ee15913b0f"
response <- GET(url)
resp_json <- fromJSON(content(response, as = "text"))

# Recorremos la salida y generamos un DF para saber las estaciones desponibles,
# fecha del ultimo dato disponible y su correspondiente AQI
df_rbind = data.frame()
for (i in 1:length(resp_json[["data"]][["lat"]])){
  print(i)
  df <- data.frame( station_name = resp_json[["data"]][["station"]][["name"]][i],
                    time = resp_json[["data"]][["station"]][["time"]][i],
                    lat = resp_json[["data"]][["lat"]][i],
                    lon = resp_json[["data"]][["lon"]][i],
                    aqi = resp_json[["data"]][["aqi"]][i])
  df_rbind <- rbind(df_rbind,df)
}
#Vemos que estaciones no tiene datos
for (p in 1:nrow(df_rbind)){
  if(df_rbind$aqi[p] == "-"){
    print(df_rbind$station_name[p])
  }
}

# Idem que lo anterior - similar
df_rbdind_comp <- df_rbind[df_rbind$aqi != "-",]
station_rbind <- data.frame()
for (j in 1:nrow(df_rbdind_comp)){
  coords_x <- df_rbdind_comp$lon[j]
  coords_y <- df_rbdind_comp$lat[j]
  url_estaciones <- paste("https://api.waqi.info/feed//geo:",coords_y,";",coords_x,"/?token=3a1671d7bc5576694e2d76e9fa12e8ee15913b0f",sep="")
  response_estaciones <- GET(url_estaciones)
  resp_json_estaciones <- fromJSON(content(response_estaciones , as = "text"))
  
  df_station <- data.frame (station_name = resp_json_estaciones[["data"]][["city"]][["name"]],
                            iaqi_pm25 = resp_json_estaciones[["data"]][["iaqi"]][["pm25"]][["v"]],
                            iaqi_pm10 = resp_json_estaciones[["data"]][["iaqi"]][["pm10"]][["v"]],
                            local_time = resp_json_estaciones[["data"]][["time"]][["s"]],
                            lat =resp_json_estaciones[["data"]][["city"]][["geo"]][1],
                            lon = resp_json_estaciones[["data"]][["city"]][["geo"]][2])
  station_rbind <- rbind(station_rbind,df_station)
}

View(station_rbind)
# Como esta API solo nos devuelve datos de AQI, y nosotras necesitamos valores de concentraciones
# transformamos el aqi en un PM aproximado
valores_aqi <- read.csv("D:/Josefina/Proyectos/salud/movilidad_10/geo_data/aqi_pm25.csv")
df_aqi_rbind <- data.frame()
for (z in 1:nrow(station_rbind)){
  match<- which(valores_aqi$AQI == station_rbind$iaqi_pm25[z])
  
  table <- valores_aqi[match,] 
  dim_table <- dim(table)
  if (dim_table[1] == 0){
    match_min<- which(valores_aqi$AQI == (station_rbind$iaqi_pm25[z]+1))
    match_max<- which(valores_aqi$AQI == (station_rbind$iaqi_pm25[z]-1))
    table_min <- valores_aqi[match_min,] 
    table_max <- valores_aqi[match_max,]
    df_aqi <- data.frame(station_name = station_rbind$station_name[z], 
                         iaqi_pm25 = station_rbind$iaqi_pm25[z],
                         local_time = station_rbind$local_time[z], 
                         lat = station_rbind$lat[z], lon = station_rbind$lon[z],
                         conc = ((table_min$conc+table_max$conc)/2), aqi_table = "mean")
    
    
  }
  else{
    df_aqi <- data.frame(station_name = station_rbind$station_name[z], 
                         iaqi_pm25 = station_rbind$iaqi_pm25[z],
                         local_time = station_rbind$local_time[z], 
                         lat = station_rbind$lat[z], lon = station_rbind$lon[z],
                         conc = table$conc, aqi_table = table$AQI)
    
  }
  df_aqi_rbind <- rbind(df_aqi_rbind, df_aqi)
}


###############################################################################
#                    ----    API iqair.com        -----

# Da el AQI pero de la ciudad, no devuelve el nombre o valores de las estaciones
url <- "http://api.airvisual.com/v2/nearest_city?lat=-32.88995120772317&lon=-68.91162148248097&key=5ab7b7c4-4a11-46e3-bf5c-70a01ba0fa3f"

# Request similar a la anterior, la diferencia es que colocamos el nombre de las ciudades y no ponemos coordenadas
# al igual que lo anterior no devuelve el nombre o valores de las estaciones
url2 <- "http://api.airvisual.com/v2/city?city=Los%Angeles&state=California&country=USA&key=5ab7b7c4-4a11-46e3-bf5c-70a01ba0fa3f"
url3 <- "http://api.airvisual.com/v2/city?city=Mendoza&state=Mendoza&country=Argentina&key=5ab7b7c4-4a11-46e3-bf5c-70a01ba0fa3f"

response_estacion<- GET(url2)
resp_json_estacion <- fromJSON(content(response_estacion , as = "text"))

###################################################################################
#                    ----    API purple air        -----
# En el ultimo tiempo cambiaron las condiciones de uso, y con la api hay que pagar
# Hay muchas quejas en el foro. Posiblemente en el futuro cambie.
# De igual forma se pueden descarar algunos datos desde su pagina en formato csv
# se puede hacer web scrapping?
url <-"https://api.purpleair.com/v1/sensors?fields=name%2Clatitude%2Clongitude&location_type=0&nwlng=38&nwlat=-122.5&selng=37&selat=-121.5&api_key=3CA0A82A-06D0-11EE-BD21-42010A800008"
url <-"https://api.purpleair.com/v1/sensors?fields=name%2Clatitude%2Clongitude&location_type=0&nwlng=38&nwlat=-122.5&selng=37&selat=-121.5&api_key=3CA0A82A-06D0-11EE-BD21-42010A800008"

response <- GET(url)
resp_json <- fromJSON(content(response, as = "text"))
resp_json


#################################################################################
#                    ----    API sensor community        -----
#https://sensor.community/es/
#https://github.com/opendata-stuttgart/meta/wiki/EN-APIs
# Se pueden descargar datos historicos
#http://archive.sensor.community/2023-01-07/
# Ejemplo berlin, alemania
url <- "https://data.sensor.community/airrohr/v1/filter/box=52.1,13.0,53.5,13.5" #berlin
# Aparecen datos de concentraciones
url <- "https://data.sensor.community/airrohr/v1/filter/box=-34.7942359128351,-58.888460968407635,-34.374067211490434,-58.493219406400435" 

url <- "https://data.sensor.community/static/v2/data.1h.json"
response <- GET(url)
resp_json <- fromJSON(content(response, as = "text"))
resp_json
