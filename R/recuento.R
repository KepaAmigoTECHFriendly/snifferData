#' @title Calcula el valor Recuento de los últimos 30 minutos de macs
#'
#' @description Calcula el valor Recuento de los últimos 30 minutos de macs
#'
#' @return json
#'
#' @examples  recuento()
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

recuento <- function(){
  # ------------------------------------------------------------------------------
  # PETICIÓN TOKENs THB
  # ------------------------------------------------------------------------------

  cuerpo <- '{"username":"kepa@produccion.es","password":"kepatech"}'
  post <- httr::POST(url = "https://plataforma.destinosalnes.com/api/auth/login",
                     add_headers("Content-Type"="application/json","Accept"="application/json"),
                     body = cuerpo,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  resultado_peticion_token <- httr::content(post)
  auth_thb <- paste("Bearer",resultado_peticion_token$token)


  # ------------------------------------------------------------------------------
  # GET DATOS
  # ------------------------------------------------------------------------------

  # GET IDs Dispositivos C11
  url_thb <- "https://plataforma.destinosalnes.com/api/tenant/devices?pageSize=20000&page=0"
  peticion <- GET(url_thb, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  df <- as.data.frame(df)
  df_dispositivos_conteo <- df[df$data.type == "Conteo",]



  ids_dispositivos_conteo <- df_dispositivos_conteo$data.id$id
  fecha_1 <- Sys.time() - 60*30 # Timestamp últimos 5 minutos
  fecha_2 <- Sys.time()
  fecha_1 <- format(as.numeric(as.POSIXct(fecha_1))*1000,scientific = F)
  fecha_2 <- format(as.numeric(as.POSIXct(fecha_2))*1000,scientific = F)
  keys <- URLencode(c("mac"))


  for(i in 1:length(ids_dispositivos_conteo)){
    id_dispositivo <- ids_dispositivos_conteo[i]

    url_thb_fechas <- paste("https://plataforma.destinosalnes.com/api/plugins/telemetry/DEVICE/",id_dispositivo,"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
    peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

    # Tratamiento datos. De raw a dataframe
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    df <- as.data.frame(df)
    if(nrow(df) == 0){next}
    colnames(df) <- c("ts","mac")
    df$fecha_time <- as.POSIXct(as.numeric(df$ts)/1000, origin = "1970-01-01")

    recuento <- nrow(df)










    #ENVÍO DE DATOS A PLATAFORMA
    url <- paste("https://plataforma.destinosalnes.com/api/plugins/telemetry/DEVICE/",id_dispositivo,"/timeseries/ANY?scope=ANY",sep = "")
    json_envio_plataforma <- paste('{"Recuento":', recuento,'}',sep = "")
    post <- httr::POST(url = url,
                       add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                       body = json_envio_plataforma,
                       verify= FALSE,
                       encode = "json",verbose()
    )

    #ACTUALIZACIÓN ATRIBUTOS
    url <- paste("https://plataforma.destinosalnes.com/api/plugins/telemetry/DEVICE/",id_dispositivo,"/attributes/SERVER_SCOPE",sep = "")
    json_envio_plataforma <- paste('{"active":', TRUE,'}',sep = "")
    post <- httr::POST(url = url,
                       add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                       body = json_envio_plataforma,
                       verify= FALSE,
                       encode = "json",verbose()
    )
    fecha_inactividad <- Sys.time()
    fecha_inactividad <- ymd_hms(fecha_inactividad)
    fecha_inactividad <- as.numeric(fecha_inactividad) * 1000
    json_envio_plataforma <- paste('{"lastActivityTime":', fecha_inactividad,'}',sep = "")
    post <- httr::POST(url = url,
                       add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                       body = json_envio_plataforma,
                       verify= FALSE,
                       encode = "json",verbose()
    )
  }

  return(1)
}
