# Codigo para obtener informacion de los sensores de contamiantes
# con la API de OpenAQ

# ARG - prueba
# URL para solicitar informacion sobre todas las estacion del pais. 
# En este caso "AR"
url <- "https://api.openaq.org/v2/locations?country=AR"
# Solicitud
response <- GET(url)
# Transformamos de JSON a TEXTO
resp_json <- fromJSON(content(response, as = "text"))
df_contaminante_rbind <- data.frame()
# Recorremos la respuesta y generamos un dataframe con la informacion
#necesaria
for (i in 1:length(resp_json[["results"]][["parameters"]])){
  print(c("esto es i", i))
  df_contaminante <- data.frame(name = resp_json[["results"]][["name"]][i],
                                city = resp_json[["results"]][["city"]][i],
                                latitude=  resp_json[["results"]][["coordinates"]][["latitude"]][i],
                                longitude = resp_json[["results"]][["coordinates"]][["longitude"]][i],
                                manufacturers = resp_json[["results"]][["manufacturers"]][[i]][["manufacturerName"]],
                                id =resp_json[["results"]][["parameters"]][[i]][["id"]],
                                unit = resp_json[["results"]][["parameters"]][[i]][["unit"]],
                                lastValue = resp_json[["results"]][["parameters"]][[i]][["lastValue"]],
                                average = resp_json[["results"]][["parameters"]][[i]][["average"]],
                                count = resp_json[["results"]][["parameters"]][[i]][["count"]],
                                parameter = resp_json[["results"]][["parameters"]][[i]][["parameter"]],
                                displayName = resp_json[["results"]][["parameters"]][[i]][["displayName"]],
                                lastUpdatedUTC = resp_json[["results"]][["parameters"]][[i]][["lastUpdated"]],
                                
                                firstUpdatedUTC = resp_json[["results"]][["parameters"]][[i]][["firstUpdated"]])#
    df_contaminante_rbind <- rbind (df_contaminante_rbind,df_contaminante)                               
  }
# Guardamos
write.csv(df_contaminante_rbind,"D:/Josefina/Congresos/Air Pollution Conference 2024/OpenAQ/sensores_arg.csv")


###-----------------------------------------------------------
# A partir de la prueba anterior, generamos una funcion para automatizar
# el proceso
sensor_search <- function(city_ID){
  # ARG - prueba
  url <- paste("https://api.openaq.org/v2/locations?country=",city_ID,sep = "")
  response <- GET(url)
  resp_json <- fromJSON(content(response, as = "text"))
  df_contaminante_rbind <- data.frame()
  for (i in 1:length(resp_json[["results"]][["parameters"]])){
    #print(c("esto es i", i))
    df_contaminante <- data.frame(name = resp_json[["results"]][["name"]][i],
                                  city = resp_json[["results"]][["city"]][i],
                                  latitude=  resp_json[["results"]][["coordinates"]][["latitude"]][i],
                                  longitude = resp_json[["results"]][["coordinates"]][["longitude"]][i],
                                  manufacturers = resp_json[["results"]][["manufacturers"]][[i]][["manufacturerName"]],
                                  id =resp_json[["results"]][["parameters"]][[i]][["id"]],
                                  unit = resp_json[["results"]][["parameters"]][[i]][["unit"]],
                                  lastValue = resp_json[["results"]][["parameters"]][[i]][["lastValue"]],
                                  average = resp_json[["results"]][["parameters"]][[i]][["average"]],
                                  count = resp_json[["results"]][["parameters"]][[i]][["count"]],
                                  parameter = resp_json[["results"]][["parameters"]][[i]][["parameter"]],
                                  displayName = resp_json[["results"]][["parameters"]][[i]][["displayName"]],
                                  lastUpdatedUTC = resp_json[["results"]][["parameters"]][[i]][["lastUpdated"]],
                                  
                                  firstUpdatedUTC = resp_json[["results"]][["parameters"]][[i]][["firstUpdated"]])#
    df_contaminante_rbind <- rbind (df_contaminante_rbind,df_contaminante)                               
  }
  # Guardmos
  #save_name <-paste("D:/Josefina/Congresos/Air Pollution Conference 2024/OpenAQ/sensor_",city_ID,".csv",sep="")
  #write.csv(df_contaminante_rbind,save_name)
  print("Save")
  # Tambien retoramos la informacion
  return(df_contaminante_rbind)
  
}
#######################################################################
# Hacemos la prueba para cada pais de 
# Todo LataAM
Argentina <- sensor_search("AR")  
Argentina$pais <- "Argentina"
Belize <- sensor_search("BZ")  
Belize$pais <- "Belize"
Bolivia <- sensor_search("BO")
Bolivia$pais <- "Bolivia"
Brazil<- sensor_search("BR")
Brazil$pais <- "Brazil"
Chile<- sensor_search("CL")
Chile$pais <- "Chile"
Colombia<- sensor_search("CO")
Colombia$pais <- "Colombia"
Costa_Rica <- sensor_search("CR")
Costa_Rica$pais <- "Costa_Rica"
Cuba<- sensor_search("---")# NO HAY SENSORES
Dominican_Republic<- sensor_search("DR")# ultima actualizacion 2 años
Dominican_Republic$pais <- "Dominican_Republic"
Ecuador <- sensor_search("EC")
Ecuador$pais <- "Ecuador"
El_Salvador<- sensor_search("SV")
El_Salvador$pais <- "El_Salvador"
Guatemala<- sensor_search("GT")
Guatemala$pais <- "Guatemala"
Haiti<- sensor_search("--")# NOHAY
Honduras<- sensor_search("HN")
Honduras$pais <- "Honduras"
Mexico<- sensor_search("MX")
Mexico$pais <- "Mexico"
Nicaragua<- sensor_search("----") #NO HAY
Nicaragua$pais <-"Nicaragua"
Panama<- sensor_search("PA")
Panama$pais <- "Panama"
Paraguay<- sensor_search("PY")
Paraguay$pais <- "Paraguay"
Peru<- sensor_search("PE")
Peru$pais <- "Peru"
Puerto_Rico<- sensor_search("PR")
Puerto_Rico$pais <- "Puerto_Rico"
Uruguay<- sensor_search("UY")
Uruguay$pais <- "Uruguay"
Venezuela<- sensor_search("VE")
Venezuela$pais <- "Venezuela"

# Unimos todos los archivos.
tot <- rbind(Argentina, Belize, Bolivia, Brazil, Chile, Colombia, 
             Costa_Rica,#Cuba,Haiti Dominican_Republic,
              Ecuador, El_Salvador,
             Guatemala, Honduras, Mexico, #Nicaragua,
             Panama, Paraguay, Peru,Puerto_Rico, Uruguay,
             Venezuela)

#Guardamos toda la infoormacion en un mismo archivo
write.csv(tot,"D:/Josefina/Congresos/Air Pollution Conference 2024/OpenAQ/sensores_LatAm.csv")

