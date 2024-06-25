#' @title Calcula el valor promedio del último día
#'
#' @description Calcula el valor proemdio del último día
#'
#' @return json
#'
#' @examples  tiempoPromedio()
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

tiempoPromedio <- function(){

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


  # GET IDs Dispositivos C11

  url_thb <- "https://plataforma.destinosalnes.com/api/tenant/devices?pageSize=20000&page=0"
  peticion <- GET(url_thb, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  df <- as.data.frame(df)
  df_dispositivos_conteo <- df[df$data.type == "Conteo",]


  ids_dispositivos_conteo <- df_dispositivos_conteo$data.id$id
  fecha_1 <- Sys.time() - 60*60*12 # Timestamp último día
  fecha_2 <- Sys.time()
  fecha_1 <- format(as.numeric(as.POSIXct(fecha_1))*1000,scientific = F)
  fecha_2 <- format(as.numeric(as.POSIXct(fecha_2))*1000,scientific = F)
  keys <- URLencode(c("mac"))

  for(i in 1:length(ids_dispositivos_conteo)){
    id_dispositivo <- ids_dispositivos_conteo[i]


    if(id_dispositivo == "47a5e0d1-2006-11ee-ae58-bfdaa26a158a"){
      ping <- system(paste("ping -c 1", "77.211.27.131"), intern = FALSE)
      if(ping != 0){
        next
      }else{
        options <- c(2, 0)
        probabilities <- c(0.88, 0.12)
        prob <- sample(options, 1, prob = probabilities)
        if(prob == 0){
          next
        }
      }
    }

    if(id_dispositivo == "47a51d80-2006-11ee-ae58-bfdaa26a158a"){
      ping <- system(paste("ping -c 1", "81.60.227.141"), intern = FALSE)
      if(ping != 0){
        next
      }else{
        options <- c(2, 0)
        probabilities <- c(0.77, 0.23)
        prob <- sample(options, 1, prob = probabilities)
        if(prob == 0){
          next
        }
      }
    }

    if(id_dispositivo == "47a51d81-2006-11ee-ae58-bfdaa26a158a"){
      ping <- system(paste("ping -c 1", "81.60.227.199"), intern = FALSE)
      if(ping != 0){
        next
      }else{
        options <- c(2, 0)
        probabilities <- c(0.60, 0.40)
        prob <- sample(options, 1, prob = probabilities)
        if(prob == 0){
          next
        }
      }
    }

    if(id_dispositivo == "47a607e0-2006-11ee-ae58-bfdaa26a158a"){
      ping <- system(paste("ping -c 1", "81.60.227.123"), intern = FALSE)
      if(ping != 0){
        next
      }else{
        options <- c(2, 0)
        probabilities <- c(0.88, 0.12)
        prob <- sample(options, 1, prob = probabilities)
        if(prob == 0){
          next
        }
      }
    }

    if(id_dispositivo == "47a592b0-2006-11ee-ae58-bfdaa26a158a"){
      ping <- system(paste("ping -c 1", "81.60.227.143"), intern = FALSE)
      if(ping != 0){
        next
      }else{
        options <- c(2, 0)
        probabilities <- c(0.80, 0.20)
        prob <- sample(options, 1, prob = probabilities)
        if(prob == 0){
          next
        }
      }
    }


    url_thb_fechas <- paste("https://plataforma.destinosalnes.com/api/plugins/telemetry/DEVICE/",id_dispositivo,"/values/timeseries?limit=10000&keys=",keys,"&startTs=",fecha_1,"&endTs=",fecha_2,sep = "")
    peticion <- GET(url_thb_fechas, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

    # Tratamiento datos. De raw a dataframe
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    df <- as.data.frame(df)
    if(nrow(df) == 0){next}
    colnames(df) <- c("ts","mac")
    df$fecha_time <- as.POSIXct(as.numeric(df$ts)/1000, origin = "1970-01-01")

    df_filtrado <- df %>%
      filter(!grepl("^.([2|6|A|E]).*$", mac, ignore.case = TRUE))

    #df <- df_filtrado

    # Agrupar por la dirección MAC y calcular la primera y última aparición
    diferencias <- df %>%
      group_by(mac) %>%
      summarise(
        primera_aparicion = min(fecha_time),
        ultima_aparicion = max(fecha_time)
      ) %>%
      ungroup()  # Desagrupar para calcular el promedio

    # Calcular la diferencia en minutos y luego el promedio
    diferencias$permanencia_minutos <- as.numeric(difftime(diferencias$ultima_aparicion, diferencias$primera_aparicion, units = "mins"))
    # Filtrar las direcciones MAC que permanecen 5 horas o menos (300 minutos)
    diferencias_filtradas <- filter(diferencias, permanencia_minutos <= 300)
    tiempo_promedio_permanencia <- round(mean(diferencias_filtradas$permanencia_minutos))

    print(tiempo_promedio_permanencia)

    #ENVÍO DE DATOS A PLATAFORMA
    url <- paste("https://plataforma.destinosalnes.com/api/plugins/telemetry/DEVICE/",id_dispositivo,"/timeseries/ANY?scope=ANY",sep = "")
    json_envio_plataforma <- paste('{"tiempo_promedio_diario":', tiempo_promedio_permanencia,'}',sep = "")
    post <- httr::POST(url = url,
                       add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                       body = json_envio_plataforma,
                       verify= FALSE,
                       encode = "json",verbose()
    )
  }

  return(1)

}
