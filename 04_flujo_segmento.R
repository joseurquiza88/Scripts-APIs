# Request de la api de tom-tom velocidades por segmento

# Informacion en: https://developer.tomtom.com/traffic-api/documentation/traffic-flow/flow-segment-data

# Este servicio proporciona información sobre las velocidades y tiempos de viaje del fragmento 
# de las rutas más cercano a las coordenadas proporcionadas. 

flow_segment_data<- function(lat,lon){
  
  url_part_1 <- 'https://api.tomtom.com/traffic/services/4/flowSegmentData/absolute/20/json?point='
  coords <- paste(lat,"%2C",lon,sep="")
  url_part_2 <-paste("&unit=KMPH&openLr=false&key=",key,sep="")
  url <- paste(url_part_1,coords,url_part_2,sep="")
  #---  Request en la API
  response <- GET(url)
  resp_json <- fromJSON(content(response, as = "text"))
  df = data.frame ( frc = resp_json[["flowSegmentData"]][["frc"]],
                    currentSpeed = resp_json[["flowSegmentData"]][["currentSpeed"]],
                    freeFlowSpeed = resp_json[["flowSegmentData"]][["freeFlowSpeed"]],
                    currentTravelTime = resp_json[["flowSegmentData"]][["currentTravelTime"]],
                    freeFlowTravelTime = resp_json[["flowSegmentData"]][["freeFlowTravelTime"]],
                    confidence = resp_json[["flowSegmentData"]][["confidence"]],
                    resp_json[["flowSegmentData"]][["coordinates"]][["coordinate"]])


  return (df)
}


key <- "YOdvX5qKwpk9YRl9v0JzqC5qSYNOwbDc"
prueba <- flow_segment_data (lat=-32.930139044601574, lon = -68.84509878961236)
View(prueba)
prueba2 <- flow_segment_data (lat=-32.931441413755614, lon = -68.84550706080101)
View(prueba2)

#GUardamos en csv para visualizarlo por ej. en QGIS
write.csv(prueba,"prueba.csv")


#  --------- Salida de la funcion segun la documentacion de TOM-TOM
# ---- 01.frc: Functional R oad C lass. This indicates the road type:
#FRC0 : Motorway, freeway or other major road
#FRC1 : Major road, less important than a motorway
#FRC2 : Other major road
#FRC3 : Secondary road
#FRC4 : Local connecting road
#FRC5 : Local road of high importance
#FRC6 : Local road
#The unit of speed.kmph (kilometers per hour)

# ---- 02. currentSpeed: La velocidad promedio actual en el punto seleccionado,
# en la unidad solicitada. Esto se calcula a partir del tiempo de viaje actual y
# la longitud del segmento seleccionado. Ojo con esto porque generalmente el trayecto
# considerado es muy amplio

# ---- 03. freeFlowSpeed: La velocidad de flujo libre esperada en condiciones ideales, 
#expresada en la unidad solicitada. 

# ---- 04. currentTravelTime: Tiempo de viaje actual en segundos basado en mediciones en tiempo real 
# entre las ubicaciones definidas en la dirección especificada. 

# ---- 05. freeFlowTravelTime: El tiempo de viaje en segundos que se esperaría bajo condiciones ideales de flujo libre

# ---- 06. confidence: La confianza es una medida de la calidad del tiempo de viaje y 
# la velocidad proporcionados. Varía entre 0 y 1, donde 1 significa confianza total,
#lo que indica que la respuesta contiene datos de la más alta calidad. 
#Los valores más bajos indican el grado en que la respuesta puede variar de las
#condiciones reales en la carretera.