
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
if(!require(plotly)) install.packages("plotly", repos = repos)
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = repos)
if(!require(waffle)) install.packages("waffle", repos = repos)
if(!require(emojifont)) install.packages("emojifont", repos = repos)
if(!require(extrafont)) install.packages("extrafont", repos = repos)
if(!require(gganimate)) install.packages("gganimate", repos = repos)
if(!require(animation)) install.packages("animation", repos = repos)
if(!require(imputeTS)) install.packages("imputeTS", repos = repos)
if(!require(sf)) install.packages("sf", repos = repos)
if(!require(mapSpain)) install.packages("mapSpain", repos = repos)
if(!require(readr)) install.packages("readr", repos = repos)
if(!require(dplyr)) install.packages("dplyr", repos = repos)
if(!require(glue)) install.packages("glue", repos = repos)
if(!require(ggplot2)) install.packages("ggplot2", repos = repos)
if(!require(ggtext)) install.packages("ggtext", repos = repos)
if(!require(ragg)) install.packages("ragg", repos = repos)




# ---------
animaciones <- FALSE # poner TRUE si se quieren generar animaciones
gofres <- FALSE # poner TRUE si se quieren generar gofres
mapas <- FALSE # TRUE para generar mapas (1ª vez lento, luego solo genera los no creados)
# ##################################################
# DESCARGA PDF DE VACUNAS + CARGA DE DATOS
# ##################################################

# Cargamos datos ya descargados (para no descargarlos de nuevo)
load("./DATOS/pdf_bruto.RData")
fechas_descargadas <- names(pdf_bruto)

# Secuencia de fechas a leer (sin las fechas ya bajadas
fechas <-
  setdiff(as.character(seq(as.Date("2021-01-05"), as.Date(Sys.time()), by = 1)),
          fechas_descargadas)
pdf_nuevo <- pdf_bruto # en pdf_nuevo guardamos los antiguos que modificaremos
nombres <- names(pdf_bruto)

# Lista de posibles url (vacunas)
url_vacunas <-
  paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/",
         "alertasActual/nCov/documentos/Informe_GIV_comunicacion_",
         format(as.Date(fechas), "%Y%m%d"), ".pdf")

# Con tryCatch, la orden se ejecuta y sigue el proceso aunque
# devuelva error (que lo marcamos con un -1)
intento_pdf <- # lista
  sapply(url_vacunas, # para cada valor de 
         FUN = function(x) { tryCatch(read_pdf(x),
                                      error = function(e) { -1 }) })

# Me quedo solo con los que hayan sido bien leídos
# Si deuelve error, será un numeric
# Si lo ha leído bien, lo guarda como una lista la metainfo
idx_leidos <- which(intento_pdf %>% map(typeof) == "list")
pdf_leido <- intento_pdf[idx_leidos]

# Añado nombres
nombres <- c(nombres, as.character(fechas[idx_leidos]))
pdf_nuevo <- append(pdf_nuevo, pdf_leido)
names(pdf_nuevo) <- nombres

# Añadimos
pdf_bruto <- pdf_nuevo

# Guardamos
save(pdf_bruto, file = "./DATOS/pdf_bruto.RData")

# CCAA con sus nombres y códigos ISO: si da problemas de codificación,
# quitar tildes
ccaa <-
  data.frame("NOMBRES" =
               c("ANDALUCÍA", "ARAGON", "ASTURIAS", "BALEARES", "CANARIAS",
                 "CANTABRIA", "CASTILLA Y LEÓN", "CASTILLA-LA MANCHA",
                 "CATALUNYA", "C. VALENCIANA", "EXTREMADURA",
                 "GALICIA", "LA RIOJA", "C. MADRID", "REGIÓN DE MURCIA",
                 "NAVARRA", "EUSKADI", "CEUTA", "MELILLA", "FFAA", "ESPAÑA"),
             "ISO" = c("AN", "AR", "AS", "IB", "CN", "CB", "CL", "CM",
                       "CT", "VC", "EX", "GA", "RI", "MD", "MC", "NC",
                       "PV", "CE", "ML", "FFAA", "ES"))

# Poblacion por edad y ccaa
poblacion_ccaa_edad <-
  read.csv(file = "./DATOS/poblacion_INE_ccaa_edad.csv", sep = ";",
           stringsAsFactors = FALSE)
poblacion <-
  data.frame("ISO" = names(poblacion_ccaa_edad)[-(1:2)],
             "poblacion" =
               colSums(poblacion_ccaa_edad[-dim(poblacion_ccaa_edad)[1],
                                           -(1:2)]),
             "poblacion_mayor_16a" = # Incluyendo los 16 años
               colSums(poblacion_ccaa_edad[-c(0:16,
                                              dim(poblacion_ccaa_edad)[1]),
                                           -(1:2)]))
poblacion$porc_pobl_total <- 100 * poblacion$poblacion /
  sum(poblacion$poblacion)
poblacion$porc_pobl_total_mayor_16a <- 100 * poblacion$poblacion_mayor_16a /
  sum(poblacion$poblacion_mayor_16a)
poblacion[, -1] <- round(poblacion[, -1], 3)



# ########################
# RESUMEN
# ########################

# Cálculo de datos por fecha
source("./datos_por_ccaa.R")

# Cálculo de datos por fecha
source("./datos_por_fecha.R")

# Cálculo por variables
source("./datos_por_variables.R")

# ########################
# GRÁFICOS
# ########################


# Gráficas España
source("./graficas_nacional.R")


# ##########################################################
# LECTURA DE META INFO DE LOS PDF SUBIDOS DE COVID
# ##########################################################

# Lectura de horas de subida de los pdf
source("./meta_info.R")

# ######################
# RESUMEN
# ######################


cat("\n\n ======================\n")
cat("GRÁFICAS CREADAS:\n")
cat("1. Dosis entregadas acum. [barras verticales]\n")
cat("--> fig_dosis_entregadas_vertical\n")
cat("2. Dosis entregadas diarias con huecos vacíos [barras verticales]\n")
cat("--> fig_dosis_entregadas_diarias_vertical\n")
cat("3. Dosis entregadas diarias sin huecos vacíos [barras verticales]\n")
cat("--> fig_dosis_entregadas_diarias_vertical_sin_huecos\n")
cat("4. Dosis entregadas acum. [barras horizontales]\n")
cat("--> fig_dosis_entregadas_horizontal\n")
#
cat("5. Dosis admin. acum. [barras verticales + relleno + tendencia]\n")
cat("--> fig_dosis_admin_acum\n")
cat("6. Dosis admin. diarias [barras verticales]\n")
cat("--> fig_dosis_admin_diarias\n")
#
cat("7. Personas vacunadas acum. [barras verticales + línea tendencia]\n")
cat("--> fig_vacunados\n")
#
cat("8. Dosis entregadas acum. por farma [diagrama de rosa]\n")
cat("--> fig_dosis_entregadas_rosa\n")
cat("9. Dosis admin (general vs pauta completa) [diagrama de rosa]\n")
cat("--> fig_dosis_admin_rosa\n")
cat("10. Personas vacunadas (1 dosis vs 2 dosis) [diagrama de rosa]\n")
cat("--> fig_personas_vacunadas_rosa\n\n")
#
cat("11. Dosis entregadas acum. por farma [gráfico de gofre + animación]\n")
cat("--> fig_waffle_dosis_entregadas[[i]] para la fecha i\n")
cat("12. Dosis administradas acum. por hab. [gráfico de gofre + animación]\n")
cat("--> fig_waffle_dosis_admin[[i]] para la fecha i\n")
cat("13. Personas vacunadas acum. [gráfico de gofre + animación]\n")
cat("--> fig_waffle_personas_vacunadas[[i]] para la fecha i\n\n")
#
cat("14. Mapa relleno con personas vacunadas [mapa ggplot2\n")
cat("--> mapa_relleno\n\n")
# Resumen
n_dias <- length(panel_vacunas$ES$dosis_entrega)
cat(paste0("\nDosis entregadas acum.: ", panel_vacunas$ES$dosis_entrega[n_dias]))
cat(paste0("\nDosis administradas acum.: ",
           panel_vacunas$ES$dosis_admin[n_dias],
           " (", panel_vacunas$ES$porc_admin_sobre_ccaa[n_dias], "%)"))
cat(paste0("\nDosis administradas por cada 100 hab: ",
           panel_vacunas$ES$dosis_admin_100hab[n_dias]))
cat(paste0("\nDosis administradas últimos 7D.: ",
           panel_vacunas$ES$dosis_7D_admin[n_dias]))
cat(paste0("\nDosis administradas diarias: ",
           panel_vacunas$ES$dosis_diarias_admin[n_dias]))
cat(paste0("\nPersonas vacunadas acum.: ",
           panel_vacunas$ES$personas_vacunadas[n_dias],
           " (", panel_vacunas$ES$porc_personas_vacunadas[n_dias],
           "%, ", panel_vacunas$ES$porc_personas_vacunadas_16a[n_dias],
           "% \u226516 años)"))
cat(paste0("\nPersonas vacunadas pauta completa acum.: ",
           panel_vacunas$ES$personas_pauta_completa[n_dias],
           " (", panel_vacunas$ES$porc_personas_pauta_completa[n_dias],
           "%, ", panel_vacunas$ES$porc_personas_pauta_completa_16a[n_dias],
           "% \u226516 años)"))
cat(paste0("\nPersonas vacunadas esperando 2ª dosis: ",
           panel_vacunas$ES$personas_1dosis[n_dias]))
cat(paste0("\nACTUALIZADO A FECHA DE ",
           format(max(as.Date(panel_vacunas$ES$fechas)), "%d-%m-%Y"), "\n"))
# ################
# GITHUB
# ################
# git add -A
# git commit -m "Actualización"
# git push


