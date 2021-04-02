
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
# DESCARGA PDF DE VACUNAS + CARGA DE DATOS
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
# RESUMEN
# # ########################

# Cálculo de datos por fecha
source("./datos_por_fecha.R")

# Cálculo de datos por ccaa
source("./datos_por_ccaa.R")

# Cálculo resumen global España
source("./resumen_global.R")
