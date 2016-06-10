library("iptools")
library("roxygen2")

#' Funcion que genera fichero parseado de log
#' 
#' This funcion parse a (\emph{TMG}) log File placed in ./data/LOG_WEB.w3c
#' @param N/A
#' @return DataFrame whit content parsed
#' @export
#' @example 
#' MyDataFrame <- getDF()
getDF <- function(log = "./data/LOG_WEB.w3c") {
  LogTMG <- read.csv(log, header = TRUE, sep = "\t")
  LogTMG$ip_string <- as.character(LogTMG$X.c.ip)
  
  # Generamos columna con valor numérico de IP
  LogTMG$ip_numero <- ip_to_numeric(LogTMG$ip_string)
  saveRDS(LogTMG, file="./data/dataframelog.rds")
  return(LogTMG)
}

#' Funcion que genera fichero con rangos de IP y pais asignado
#' 
#' This funcion create a (\emph{Data Frame}) with IP range and country assigned
#' @param N/A
#' @return DataFrame whit IP range and country assigned
#' @export
#' @example 
#' MyDataFrame <- getAgregados()
getAgregados <- function(){
  # Bajamos ficheros ubicación
  url <- "http://geolite.maxmind.com/download/geoip/database/GeoLite2-Country-CSV.zip"
  localizaciones <- download.file(url=url, destfile="./data/GeoLite2-Country-CSV.zip")
  unzip(zipfile="./data//GeoLite2-Country-CSV.zip")
  # Cargamos fichero en DF
  localizaciones <- read.csv(file="GeoLite2-Country-CSV_20160607/GeoLite2-Country-Blocks-IPv4.csv")
  # Creamos columna con valor id de red en caracter
  localizaciones$red_string <- as.character(localizaciones$network)  
  
  # Creamos DF con limites de localizaciones
  limites <- range_boundaries(localizaciones$red_string)
  
  # Añado limites a lista localizaciones
  Agregado <- merge(x = localizaciones, y = limites, by.x = "red_string", by.y = "range", all = TRUE)
  
  # Cargamos DF de paises
  NombreLoc <- read.csv(file="GeoLite2-Country-CSV_20160607/GeoLite2-Country-Locations-es.csv")
  
  # Agregamos información del pais a la anterior
  Agregado <- merge(x = Agregado, y = NombreLoc, by = "geoname_id", all = TRUE)
  
  return(Agregado)
}


#' Funcion que devuelve el iso code asignado del pais de un IP dada
#' 
#' This funcion return the (\emph{iso code}) assigned to an (\emph{IP})
#' @param ip, data.frame
#' @return Char with the iso code 
#' @export
#' @example 
#' getPaisbyIP(1371073096, countries)
getIdPaisbyIP <- function(ip , paises) {
  pp <- character(0)
  ip <- as.numeric(ip)
  pp <- as.character(paises[(ip >= paises$min_numeric) & (ip <= paises$max_numeric) ,c("country_iso_code")])
  if (identical(pp, character(0))) {
    return("")
  } else {
    return(pp)
  }
}

#' Funcion que devuelve el nombre del pais asignado de un IP dada
#' 
#' This funcion return the (\emph{country}) assigned to an (\emph{IP})
#' @param ip, data.frame
#' @return Char with the country name 
#' @export
#' @example 
#' getPaisbyIP(1371073096, countries)
getNombrePaisbyIP <- function(ip , paises) {
  pp <- character(0)
  ip <- as.numeric(ip)
  pp <- as.character(paises[(ip >= paises$min_numeric) & (ip <= paises$max_numeric) ,c("country_name")])
  if (identical(pp, character(0))) {
    return("")
  } else {
    return(pp)
  }
}

DataFrame_Nombre_Pais <- apply(X = LogTMG, MARGIN = 1, FUN = function(x) getPaisbyIP(x["ip_numero"], paises))
DataFrame_codigo_Pais <- apply(X = LogTMG, MARGIN = 1, FUN = function(x) getIdbyIP(x["ip_numero"], paises))
LogTMG <- cbind(LogTMG, DataFrame_codigo_Pais, DataFrame_Nombre_Pais)