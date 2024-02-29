# Uso de la API de enrutamiento de TOM-TOM
# El enrutamiento consta de calcular una ruta entre un origen y un destino, 
# pasando por puntos de referencia. 
# Se pueden tener en cuenta multiples parámetros adicionales de enrutamiento, 
# aqui solo se especifican algunos como:
#             - origen
#             - destino
#             - modo: "car", "pedestrian", "bicyle", "motorcycle", "bus"
#             - hora_salida
# Para agregar otros parametros se debe visualizar la pagina web donde especifican
# para agregarlos correctamente, se deben agregar en la url como se observa en la funcion 
# a continuacion
# https://developer.tomtom.com/routing-api/api-explorer
trayectorias_tom_tom<- function(origen,destino,modo,hora_salida){
  df_rbind <- data.frame()
  num_alternativas <- 5

  #---  hora de partida del hogar
  dia<- substr (hora_salida,1,10)
  hora <- substr(hora_salida,12,13)
  minutos <- substr(hora_salida,15,16)
  formato_hora <- paste(dia,"T",hora,"%3A",minutos,"%3A00-03%3A00",sep = "")
  df_rbind_output <- data.frame()
  #---  Se ponen en formato la longitud y latitud de origen. Esta informacion debe estar
  # en WGS84 como se muestra en el ejemplo
  latitud_origen  <- strsplit(origen, ",")[[1]][1]
  longitud_origen<- strsplit(origen, ",")[[1]][2]
  latitud_destino  <- strsplit(destino, ",")[[1]][1]
  longitud_destino<- strsplit(destino, ",")[[1]][2]
  #--- URL para generar la request a la API de TOM-TOM.
  # Si se quieren agregar otros parametros se deben colocar aqui como se muestra a continuacion:
  url_parte_1 <- "https://api.tomtom.com/routing/1/calculateRoute/" 
  url_parte_2 <- paste("/json?maxAlternatives=",num_alternativas,"&departAt=",formato_hora,"&routeRepresentation=polyline&computeTravelTimeFor=all&traffic=true&travelMode=",sep="" )
  url_parte_3="&vehicleEngineType=combustion&key="
  url<- paste0(url_parte_1,latitud_origen,"%2C",longitud_origen,"%3A",latitud_destino,"%2C",longitud_destino,url_parte_2,modo,url_parte_3,key)
  #---  Request en la API

  response <- GET(url)
  # la salida no nos da la informacion que pedimos a la API, por lo que la tenemos que 
  # transformar a formato JSON para poder visualizar correctamente
  resp_json <- fromJSON(content(response, as = "text"))
  #-- Creamos un data.frame para poder trabajar los datos de una forma mas simple.
  # Corroborar que datos de interes queremos en nuestro DF, aca se ponen algunos
  for (j in 1:length(resp_json[["routes"]][["legs"]])){
    resp<- data.frame( long = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["longitude"]],
                       lat = resp_json[["routes"]][["legs"]][[j]][["points"]][[1]][["latitude"]],
                       # --- Arrival and departure time --
                       departureTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["departureTime"]],
                       arrivalTime= resp_json[["routes"]][["legs"]][[j]][["summary"]][["arrivalTime"]],
                       # --   Distance  ---
                       lengthInMeters = (resp_json[["routes"]][["legs"]][[j]][["summary"]][["lengthInMeters"]]/1000),
                       trafficLengthInMeters=resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficLengthInMeters"]],
                       travelMode=resp_json[["routes"]][["sections"]][[1]][["travelMode"]][1],
                       # --- Delay Time
                       trafficDelayInSeconds=resp_json[["routes"]][["legs"]][[j]][["summary"]][["trafficDelayInSeconds"]],
                       
                       # ---  Real Time with traffic ---
                       travelTimeInSeconds = round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["travelTimeInSeconds"]]/60),2),
                       liveTrafficIncidentsTravelTimeInSeconds=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["liveTrafficIncidentsTravelTimeInSeconds"]]/60),2),
                       # ---  Historic Traffic time  ---
                       historicTrafficTravelTimeInSeconds=round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["historicTrafficTravelTimeInSeconds"]]/60),2),
                       #   ---  Time without traffic  ---
                       noTrafficTravelTimeInSeconds= round((resp_json[["routes"]][["legs"]][[j]][["summary"]][["noTrafficTravelTimeInSeconds"]]/60),2),
                       alternative = paste("alternative_",j,sep=""))
    # Le ponemos un ID para que los puntos esten en orden
    num_rows<-  nrow(resp)
    ID <- c(1:num_rows)
    data_frame_resp <- cbind(ID , resp)
    df_rbind <- rbind(data_frame_resp,df_rbind)  
    
  }
  
  df_rbind_output<- rbind(df_rbind,df_rbind_output)  
  names(df_rbind_output) <- c("ID" , "long","lat" ,"departureTime", 
                              "arrivalTime", "lengthInMeters", 
                              "trafficLengthInMeters","travelMode", 
                              "trafficDelayInSeconds","travelTimeInSeconds" ,                   
                              "liveTrafficIncidentsTravelTimeInSeconds",
                              "historicTrafficTravelTimeInSeconds",
                              "noTrafficTravelTimeInSeconds",           
                              "alternative")
  return(df_rbind_output)
}
#Ejemplos para correr esta funcion
#Para obtener la clave gratuita ingresar a:
#https://developer.tomtom.com/how-to-get-tomtom-api-key. 
#key <- "YOdvX5qKwpk9YRl9v0JzqC5qSYNOwbDc"
trayectoria_mendoza<- trayectorias_tom_tom(origen="-32.79679,-68.816",destino="-32.90212,-68.761",modo = "car", hora_salida="2018-08-01 07:50:00 -03" )
trayectoria_cordoba<- trayectorias_tom_tom(origen="-31.41073462665463,-64.21257199036828",destino="-31.424320317370437,-64.20671135936355",modo = "motorcycle", hora_salida="2023-07-06 07:50:00 -03" )
trayectoria_buenos_aires<- trayectorias_tom_tom(origen="-34.63687488221078,-58.4624003932349",destino="-34.63987523888976,-58.44806601730178",modo = "pedestrian", hora_salida="2018-08-01 07:50:00 -03" )

