
# #####################
# CABECERA
# #####################
rm(list = ls()) # Limpiamos variables
assign("last.warning", NULL, envir = baseenv()) # Limpiamos warnings
options(warn = -1) # Desactivamos warnings
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # Fijamos directorio

# Cargamos librerías y paquetes
repos <- "http://cran.us.r-project.org"
if(!require(pdftools)) install.packages("pdftools", repos = repos)
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(lubridate)) install.packages("lubridate", repos = repos)
if(!require(textreadr)) install.packages("textreadr", repos = repos)
# if(!require(rvest)) install.packages("rvest", repos = repos)



# ##################################################
# 1. DESCARGA PDF DE VACUNAS + CARGA DE DATOS
# ##################################################

# Cargamos datos ya descargados
load("./DATOS/pdf_bruto.RData")
fechas_descargadas <- names(pdf_bruto)

# Secuencia de fechas a leer (ampliar fechas si fuese necesario)
# le quitamos las fechas ya bajadas
fechas <-
  setdiff(as.character(seq(as.Date("2021-01-05"),
                           as.Date(Sys.time()), by = 1)),
          fechas_descargadas)
pdf_nuevo <- pdf_bruto # los antiguos
nombres <- names(pdf_bruto) # los antiguos
idx_aux <- 1
for (i in 1:length(fechas)) {
  
  # Con tryCatch, la orden se ejecuta y sigue el proceso aunque
  # devuelva error (que lo marcamos con un -1)
  url_vacunas <-
    paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/",
           "alertasActual/nCov/documentos/Informe_GIV_comunicacion_",
           format(as.Date(fechas[i]), "%Y%m%d"), ".pdf")
  
  intento_pdf <-
    tryCatch(read_pdf(url_vacunas), error = function(e) { -1 })
  
  # Si devuelve error, será un numeric
  # Si lo ha leído bien, lo guarda como una lista la metainfo
  if (typeof(intento_pdf) == "list") {
    
    # Guardamos fecha que ha leído, lo vamos añadiendo a una lista
    nombres <- c(nombres, as.character(fechas[i]))
    pdf_nuevo[[idx_aux + length(pdf_bruto)]] <- intento_pdf
    idx_aux <- idx_aux + 1
    
  }
}
names(pdf_nuevo) <- nombres

# Añadimos
pdf_bruto <- pdf_nuevo

# Guardamos
save(pdf_bruto, file = "./DATOS/pdf_bruto.RData")

# CCAA con sus nombres y códigos ISO
ccaa <-
  data.frame("NOMBRES" =
               c("ANDALUCÍA", "ARAGÓN", "ASTURIAS", "BALEARES", "CANARIAS",
                 "CANTABRIA", "CASTILLA Y LEÓN", "CASTILLA-LA MANCHA",
                 "CATALUNYA", "C. VALENCIANA", "EXTREMADURA",
                 "GALICIA", "LA RIOJA", "C. MADRID", "REGIÓN DE MURCIA",
                 "NAVARRA", "EUSKADI", "CEUTA", "MELILLA"),
             "ISO" = c("AN", "AR", "AS", "IB", "CN", "CB", "CL", "CM",
                       "CT", "VC", "EX", "GA", "RI", "MD", "MC", "NC",
                       "PV", "CE", "ML"))

# Poblacion por edad y ccaa
poblacion_ccaa_edad <-
  read.csv(file = "./DATOS/poblacion_INE_ccaa_edad.csv")
poblacion <-
  data.frame("ISO" = names(poblacion_ccaa_edad)[-(1:2)],
             "poblacion" =
               colSums(poblacion_ccaa_edad[-dim(poblacion_ccaa_edad)[1],
                                           -(1:2)]),
             "poblacion_mayor_16a" =  # Incluyendo los 16 años
               colSums(poblacion_ccaa_edad[-c(0:16,
                                              dim(poblacion_ccaa_edad)[1]),
                                           -(1:2)]),
             "poblacion_mayor_18a" = # Incluyendo los 18 años
               colSums(poblacion_ccaa_edad[-c(0:18,
                                              dim(poblacion_ccaa_edad)[1]),
                                           -(1:2)]))
poblacion$porc_pobl_total <- 100 * poblacion$poblacion / sum(poblacion$poblacion)
poblacion$porc_pobl_total_mayor_16a <-
  100 * poblacion$poblacion_mayor_16a / sum(poblacion$poblacion_mayor_16a)
poblacion$porc_pobl_total_mayor_18a <-
  100 * poblacion$poblacion_mayor_18a / sum(poblacion$poblacion_mayor_18a)
poblacion[, -1] <- round(poblacion[, -1], 3)

# ########################
# DATOS POR FECHA
# # ########################

# Cálculo de datos por fecha
source("./datos_por_fecha.R")

# Cálculo de datos por ccaa
source("./datos_por_ccaa.R")

# Cálculo resumen global España
source("./resumen_global.R")

# ######################################
# 1. Lectura de meta información
#    del documento PDF
# ######################################

# Hasta 400 informes (ampliar bucle si hubiera más de 400 informes)
# Empiezan numerados en 31 los informes
idx_pdf <- 31:4e2 
paginas_pdf <- fecha_pdf <- hora_pdf <- c()
for (i in idx_pdf) {
  
  # Con tryCatch, la orden se ejecuta y sigue el proceso aunque
  # devuelva error (que lo marcamos con un -1)

  url_sanidad <- paste0("https://www.mscbs.gob.es/profesionales/",
                        "saludPublica/ccayes/alertasActual/nCov/",
                        "documentos/Actualizacion_", i, "_COVID-19.pdf")
  intento_pdf <-
    tryCatch(pdf_info(url_sanidad), error = function(e) { -1 })
  
  # Si devuelve error, será un numeric
  # Si lo ha leído bien, lo guarda como una lista la metainfo
  if (typeof(intento_pdf) == "list") {
    
    paginas_pdf <- c(paginas_pdf, intento_pdf$pages)
    fecha_pdf <- c(fecha_pdf, as.character(as.Date(intento_pdf$modified)))
    hora_pdf <-
      c(hora_pdf, as.character(format(intento_pdf$modified, "%H:%M:%S")))
    
  }
     
}        
cat(paste0("Se han exportado páginas/fecha/hora de ",
           length(fecha_pdf), " informes de Sanidad\n"))
cat(paste0("Media de páginas: ", round(mean(paginas_pdf), 2), "\n"))
cat(paste0("Mediana de páginas: ", round(median(paginas_pdf), 2), "\n"))
cat(paste0("Hora media de última modificación: ",
           format(mean(as.POSIXct(hora_pdf,
                                  format = "%H:%M:%S")), "%H:%M:%S\n")))
cat(paste0("Hora media de última modificación en el último mes: ",
           format(mean(as.POSIXct(hora_pdf[fecha_pdf >=
                                             as.Date(Sys.time()) - 30],
                                  format = "%H:%M:%S")), "%H:%M:%S\n")))


     
# ##########################################
# 2. Scrapping del canal de Telegram
#    de Sanidad para saber la hora subida
#    del documento (!= última modificación
#    del pdf)
# ##########################################

# La exportación del historial del canal desde Telegram
# nos devuelve la información en dos html
html_telegram <- list()
html_telegram[[1]] <- read_html(file = "./messages.html")
html_telegram[[2]] <- read_html(file = "./messages2.html")

# Convertimos a mayúscula, quitamos acentos y artículos
for (i in 1:length(html_telegram)) {
  
  # Todo a mayúscula
  html_telegram[[i]] <- toupper(html_telegram[[i]])

  # Quitamos acentos
  html_telegram[[i]] <-
    gsub("'", "", iconv(html_telegram[[i]], to = "ASCII//TRANSLIT"))
  
  # Quitamos puntos, comas y hashtag
  html_telegram[[i]] <- gsub(",", "", html_telegram[[i]])
  html_telegram[[i]] <- gsub(".", "", html_telegram[[i]])
  html_telegram[[i]] <- gsub("#", "", html_telegram[[i]])
  
  # Convertimos todo lo que sea «coronavirus» a COVID para unificar
  # y eliminamos "-19"
  html_telegram[[i]] <- gsub("CORONAVIRUS", "COVID", html_telegram[[i]])
  html_telegram[[i]] <- gsub("-19", "", html_telegram[[i]])
  html_telegram[[i]] <- gsub("-", "", html_telegram[[i]])
  
  # Quitamos artículos (con espacios para distinguirlo de trozos de palabras)
  html_telegram[[i]] <- gsub(" DE ", " ", toupper(html_telegram[[i]]))
  html_telegram[[i]] <- gsub(" DEL ", " ", toupper(html_telegram[[i]]))
  html_telegram[[i]] <- gsub(" DE LA ", " ", toupper(html_telegram[[i]]))
  html_telegram[[i]] <- gsub(" AL ", " ", toupper(html_telegram[[i]]))
  html_telegram[[i]] <- gsub(" A LA ", " ", toupper(html_telegram[[i]]))
  
}

# Buscamos en los mensajes los patrones localizados
# asociados a cuando se publica el mensaje
# (el índice dentro del vector de mensajes)
# Buscar el link del ministerio no siempre vale ya que 
# muchas publican con un bit.ly que no se puede identificar a priori
idx <- which(grepl("ACTUALIZACION DATOS COVID", tolower(aux)) |
               grepl("ACTUALIZACION COVID", tolower(aux)) |
               grepl("DATOS ACTUALIZADOS COVID", tolower(aux))  |
               grepl("DATOS COVID", tolower(aux)) ||
               grepl("INFORMACION ACTUALIZADA COVID", tolower(aux)) |
               grepl("INFORMACION COVID", tolower(aux)) |
               grepl("INFORMACION ACTUALIZADA EVOLUCION COVID", tolower(aux)) |
               grepl("DATOS COVID", tolower(aux)) |
               grepl("ACTUALIZACION DATOS RELATIVOS COVID", tolower(aux)) |
               grepl("ACCEDE A MAS INFORMACION DETALLADA POR CCAA", tolower(aux)) |
               grepl("PUEDES ENCONTRAR INFORMACION MAS AMPLIA", tolower(aux)) |
               grepl("DOCUMENTOS/ACTUALIZACION_", tolower(aux)) |
               grepl("NCOVCHINA/SITUACIONACTUAL", tolower(aux)))





