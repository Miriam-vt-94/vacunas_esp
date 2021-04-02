# CAMPAÑA DE VACUNACIÓN EN ESPAÑA POR CCAA/FECHA

[cran]: https://www.r-pkg.org/badges/version/zeallot "green means go!"
![alt text][cran]

Repositorio con la actualización de vacunas en España, así como estadísticas y gráficas generadas a partir de ellas

## DATOS FUENTE Y SCRAPPING DE LOS PDF DE SANIDAD

Además de los códigos `.R`, el repositorio proporciona una serie de archivos, tanto datos fuente que se importarán para generar las tablas resumen, como datos exportados.

Dichos archivos se encuentran en la carpeta [DATOS](https://github.com/JavierAlvarezLiebana/vacunas_esp/tree/main/DATOS)

- **pdf_bruto.RData** contiene, en formato `RData`, los [pdf del Miniterio de Sanidad, relativos a la campaña de vacunación](https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/situacionActual.htm) descargados y guardados en formato lista, aún sin procesar para la lectura.

```R
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

```
Además de scrapear los PDF de Sanidad que no hayan sido leídos, se genera un pequeño `data.frame` con los nombres y códigos ISO de las comunidades autónomas.

- **poblacion_INE_ccaa_edad.csv** contiene la población del [último censo del INE](https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p08/l0/&file=02003.px&L=0) procesada para proporcionar la población de cada edad (de año en año) por cada una de las comunidades autónomas, en formato `nºedades x ccaa`.

```R

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

```

### CANAL TELEGRAM

Los datos en la carpeta [CANAL TELEGRAM](https://github.com/JavierAlvarezLiebana/vacunas_esp/tree/main/DATOS/CANAL_TELEGRAM) se encuentran descargados en html los mensajes enviados por el Ministerio de Sanidad en su [canal de Telegram](https://t.me/sanidadgob)

## CÓDIGOS R
