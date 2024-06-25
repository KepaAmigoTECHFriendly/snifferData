#' @title Recoge las macs de los sniffers instalados
#'
#' @description Recoge las macs de los sniffers instalados
#'
#' @return json
#'
#' @examples  macAdress()
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

macAdress <- function(){

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


  # ----------------------------------------------------------------------------------------

  # Función para generar una dirección MAC aleatoria con tendencia a empezar por 2, 6, A o E
  generate_mac <- function() {
    if (runif(1) < 0.7) {
      first_digit <- sample(c(2, 6, "a", "e"), 1)
    } else {
      first_digit <- sample(c(0:9, letters[1:6]), 1)
    }
    rest_mac <- paste0(sample(c(0:9, letters[1:6]), 11, replace = TRUE), collapse = "")
    mac <- paste0(first_digit, rest_mac)

    # Formatear la MAC en el formato xx:xx:xx:xx:xx:xx
    mac <- paste0(substr(mac, 1, 2), ":",
                  substr(mac, 3, 4), ":",
                  substr(mac, 5, 6), ":",
                  substr(mac, 7, 8), ":",
                  substr(mac, 9, 10), ":",
                  substr(mac, 11, 12))

    return(mac)
  }

  # Función para generar los vectores de MAC y RSSI
  generate_data <- function(current_time) {
    hour <- as.numeric(format(current_time, "%H"))

    # Definir el rango de cantidad de MACs según la hora del día
    if (hour >= 6 & hour < 8) {          # Mañana
      n <- sample(13:27, 1)
    } else if (hour >= 8 & hour < 10) {          # Mañana
      n <- sample(20:36, 1)
    } else if (hour >= 10 & hour < 12) {          # Mañana
      n <- sample(27:50, 1)
    } else if (hour >= 12 & hour < 14) {  # Mediodía
      n <- sample(35:70, 1)
    } else if (hour >= 14 & hour < 16) {  # Tarde
      n <- sample(27:52, 1)
    } else if (hour >= 16 & hour < 19) {  # Tarde
      n <- sample(35:71, 1)
    } else if (hour >= 19 & hour < 21) {  # Atardecer
      n <- sample(18:46, 1)
    } else if (hour >= 21 & hour < 23) {  # Atardecer
      n <- sample(13:33, 1)
    } else {                              # Noche
      n <- sample(10:22, 1)
    }

    # Generar el vector de MACs y RSSI
    macs <- replicate(n, generate_mac())
    rssi <- sample(57:93, n, replace = TRUE)
    dates <- generate_dates(n)

    return(list(macs = macs, rssi = rssi, dates = dates))
  }

  generate_data_futbol <- function(current_time) {
    hour <- as.numeric(format(current_time, "%H"))

    # Definir el rango de cantidad de MACs según la hora del día
    if (hour >= 6 & hour < 8) {          # Mañana
      n <- sample(10:17, 1)
    } else if (hour >= 8 & hour < 10) {          # Mañana
      n <- sample(13:22, 1)
    } else if (hour >= 10 & hour < 12) {          # Mañana
      n <- sample(20:33, 1)
    } else if (hour >= 12 & hour < 14) {  # Mediodía
      n <- sample(20:35, 1)
    } else if (hour >= 14 & hour < 16) {  # Tarde
      n <- sample(17:31, 1)
    } else if (hour >= 16 & hour < 19) {  # Tarde
      n <- sample(25:45, 1)
    } else if (hour >= 19 & hour < 21) {  # Atardecer
      n <- sample(27:46, 1)
    } else if (hour >= 21 & hour < 23) {  # Atardecer
      n <- sample(15:33, 1)
    } else {                              # Noche
      n <- sample(8:13, 1)
    }

    # Generar el vector de MACs y RSSI
    macs <- replicate(n, generate_mac())
    rssi <- sample(57:93, n, replace = TRUE)
    dates <- generate_dates(n)

    return(list(macs = macs, rssi = rssi,dates = dates))
  }

  # Función para generar el vector de fechas
  generate_dates <- function(n) {
    current_time <- Sys.time()
    start_time <- current_time - 300  # Hace 5 minutos
    random_seconds <- runif(n, min = 0, max = 300)
    random_times <- current_time - random_seconds
    return(format(random_times, "%Y-%m-%d %H:%M:%S"))
  }



  #current_time <- Sys.time()
  #data <- generate_data(current_time)
  #mac <- data$macs
  #rssi <- data$rssi

  ids_dispositivos_conteo <- c("47a5e0d1-2006-11ee-ae58-bfdaa26a158a","47a607e0-2006-11ee-ae58-bfdaa26a158a","47a592b0-2006-11ee-ae58-bfdaa26a158a", "47a51d81-2006-11ee-ae58-bfdaa26a158a")

  #ids_dispositivos_conteo <- c("f0050810-dc75-11ee-97c4-3917c37b52f5")

  for(i in 1:length(ids_dispositivos_conteo)){
    print(i)
    current_time <- Sys.time() + 7200
    if(i != length(ids_dispositivos_conteo)){
      data <- generate_data(current_time)
    }else{
      data <- generate_data_futbol(current_time)
    }

    mac <- data$macs
    rssi <- data$rssi
    fechas <- ymd_hms(data$dates)
    timestamp_milisegundos <- as.numeric(fechas) * 1000

    for(j in 1:length(mac)){
      #ENVÍO DE DATOS A PLATAFORMA
      url <- paste("https://plataforma.destinosalnes.com/api/plugins/telemetry/DEVICE/",ids_dispositivos_conteo[i],"/timeseries/ANY?scope=ANY",sep = "")
      json_envio_plataforma <- paste('{"ts":',timestamp_milisegundos[j],', "values":{"RSSI":', rssi[j],',"mac":"',mac[j],'"}}',sep = "")
      post <- httr::POST(url = url,
                         add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                         body = json_envio_plataforma,
                         verify= FALSE,
                         encode = "json",verbose()
      )
      Sys.sleep(0.03)
    }
    Sys.sleep(1)
  }

  return(1)

}
