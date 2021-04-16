# CAMPAÑA DE VACUNACIÓN EN ESPAÑA POR CCAA/FECHA

[cran]: https://www.r-pkg.org/badges/version/zeallot "green means go!"
![alt text][cran]

Repositorio con los datos actualizados de vacunas en España (por comunidades autónomas y por fechas), así como estadísticas y gráficas generadas a partir de ellas

## DATOS EXPORTADOS

Además de los códigos `.R` (**ver más abajo como instalar R**) el repositorio proporciona una serie de [archivos resumen exportados](https://github.com/JavierAlvarezLiebana/vacunas_esp/tree/main/EXPORTADO) de la campaña de vacunación en formato `.csv` (se pueden abrir con un Excel), para ser usados libremente (ver más abajo la [descripción de las variables](https://github.com/JavierAlvarezLiebana/vacunas_esp#descripcion-de-variables))

- **DATOS POR COMUNIDAD AUTÓNOMA**: archivos contenidos en la carpeta [POR_CCAA](https://github.com/JavierAlvarezLiebana/vacunas_esp/tree/main/EXPORTADO/POR_CCAA): se proporciona un archivo por cada comunidad autónoma (nombradas con el sufijo correspondiente a su [código ISO](https://es.wikipedia.org/wiki/ISO_3166-2:ES)), que contiene una tabla cuyas **columnas** son las distintas **variables calculadas**, con una **fila por fecha**.

- **DATOS POR FECHA**: archivos contenidos en la carpeta [POR_FECHAS](https://github.com/JavierAlvarezLiebana/vacunas_esp/tree/main/EXPORTADO/POR_FECHAS): se proporciona un archivo por cada fecha, que contiene una tabla cuyas **columnas** son las distintas **variables calculadas**, con una **fila por comunidad autónoma** (nombradas con el sufijo correspondiente a su [código ISO](https://es.wikipedia.org/wiki/ISO_3166-2:ES)).

### Datos exportados en .RData:

Además se proporcionan **dos ficheros `.RData` (para ser abiertos con `R`)**

- **panel_vacunas_ccaa.RData** contiene el resumen de la campaña de vacunación en una lista: cada elemento de la lista corresponde a una comunidad autónoma y contiene un `data.frame`  cuyas columnas son las variables, para cada una de las fechas (filas).

- **panel_vacunas_fecha.RData** contiene el resumen de la campaña de vacunación en una lista: cada elemento de la lista corresponde a una fecha y contiene un `data.frame` cuyas columnas son las variables, para cada una de las comunidades autónomas (filas).

## DESCRIPCIÓN DE VARIABLES
dasda

## DATOS FUENTE Y DATOS EXPORTADOS

Además de los códigos `.R` (**ver más abajo como instalar R**) el repositorio proporciona una serie de archivos, tanto [DATOS FUENTE](https://github.com/JavierAlvarezLiebana/vacunas_esp/tree/main/DATOS) que se importarán para generar las tablas resumen , como datos exportados.

### PDF DE SANIDAD EN BRUTO GUARDADOS EN RDATA (SCRAPPEADOS)

Dichos archivos se encuentran en la carpeta [DATOS](https://github.com/JavierAlvarezLiebana/vacunas_esp/tree/main/DATOS)

- **pdf_bruto.RData** contiene, en formato `RData`, los [pdf del Ministerio de Sanidad, relativos a la campaña de vacunación](https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/situacionActual.htm) descargados y guardados en formato lista, aún sin procesar para la lectura.

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
Además de scrapear los PDF de Sanidad que no hayan sido leídos, se genera un pequeño `data.frame` (tabla de `R`) con los nombres y códigos ISO de las comunidades autónomas.

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

### Instalación

Para poder ejecutar tú mismo/a el código, será necesario que te instales [R](https://cran.r-project.org/)

#### Opción 1 (recomendada): con RStudio

Es recomendable, en especial personas que se inicien en `R` (el lenguaje), la instalación del entorno de desarrollo [RStudio](https://www.rstudio.com/products/rstudio/download/#download) (el equivalente a un procesador de texto, usando el idioma `R`)

Tras ello, **descarga TODOS los archivos (en un archivo zip, que luego tendrás que descomprimir en tu ordenador)** de este [repositorio](https://github.com/JavierAlvarezLiebana/vacunas_esp/archive/refs/heads/main.zip), guárdalo en la ruta que desees de tu ordenador y abre al archivo principal de código [codigo_resumen.R](https://github.com/JavierAlvarezLiebana/vacunas_esp/blob/main/codigo_resumen.R)

![Descarga del zip](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/download_zip.jpg)

Al inicio del archivo [codigo_resumen.R](https://github.com/JavierAlvarezLiebana/vacunas_esp/blob/main/codigo_resumen.R), **todas las variables serán automáticamente eliminadas**, todos los paquetes necesarios serán cargados o instalados (si es la primera que ejecutas un código R, tardará unos minutos en función de tu conexión a internet), y se fijará de forma automática el directorio de trabajo en la carpeta donde tengas el código, para que no tengas que configurar nada más.

Puedes ejecutar el código completo simplemente presionando el botón `save` en RStudio, teniendo con un **tick activado la opción** `source on save` (a la derecha del botón de guardar).

![Source on save](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/head_code_source_on_save.jpg)


**IMPORTANTE**: si se produce algún error durante la instalación de los paquetes de `R`, por favor chequea si el **menú amarillo** de la iamgen aparece.

![error](https://github.com/JavierAlvarezLiebana/COVID19/blob/master/figures/error_installing.jpg)

Si fuera el caso, **haz click en el botón** `install` y espera hasta que los paquetes se instalen.


#### Opción 2: ejecutar en la terminal de R (solo si no has ejecutado la opción 1).

Si eres un nostálgico, puedes ejecutar el código directamente desde la consola de comandos.
 
```R
source('ruta_donde_hayas_guardado_el_codigo/codigo_resumen.R')
```


### Variables generadas en los archivos exportados

- **NOMBRES**: nombre de la comunidad autónoma
- **ISO**: abreviatura según normas ISO.
- **poblacion**, **poblacion_mayor_16a**, **poblacion_mayor_18a**: población de la comunidad autonónoma según [último censo del INE]: global, mayor de 16 años (inclusive) y mayor de 18 años (inclusive).
- **porc_pobl_total**, **porc_pobl_mayor_16a**, **porc_pobl_mayor_18a**: % de población (peso poblacional) que representa dicha comunidad respecto al total de España.
- **DOSIS_ENTREGADAS_PFIZER**, **DOSIS_ENTREGADAS_ASTRA_ZENECA**, **DOSIS_ENTREGADAS_MODERNA**: dosis entregadas a las comunidades autonónomas de Pfizer, Moderna y AstraZeneca.
- **DOSIS_ENTREGADAS**: dosis totales entregadas (`DOSIS_ENTREGADAS_PFIZER + DOSIS_ENTREGADAS_MODERNA + DOSIS_ENTREGADAS_ASTRA_ZENECA`).
- **PORC_ENTREGADAS_SOBRE_TOTAL**: % de dosis entregadas en cada comunidad respecto al total de dosis entregadas en España.
- **DOSIS_ADMIN**: dosis administradas en cada comunidad.
- **PAUTA_COMPLETA**, **PORC_PAUTA_COMPLETA**, **PORC_PAUTA_COMPLETA_16a**, **PORC_PAUTA_COMPLETA_18a**: personas con pauta completa (dos dosis salvo las vacunas que sean monodosis), así como el % de población (global, mayores de 18 años y de 16 años, ambos inclusive) que ya ha recibido dicha pauta vacunal completa.
- **PORC_ADMIN_SOBRE_CCAA**: % de dosis administradas del total de dosis entregadas a la comunidad.
- **PORC_ADMIN_SOBRE_TOTAL**: % de dosis administradas respecto al total de dosis administradas en España.
- **DOSIS_ADMIN_100HAB**: dosis administradas por cada 100 habitantes.
- **DESV_DOSIS_ENTREGADAS**, **DESV_DOSIS_ADMIN**: % de exceso/defecto de dosis entregadas y administradas de cada comunidad respecto a la media nacional (% de exceso/defecto del % de dosis entregadas respecto al total de España, en comparación con su peso poblacional).
- **DESV_PORC_PAUTA_COMPLETA**, **DESV_PORC_PAUTA_COMPLETA_16a**, **DESV_PORC_PAUTA_COMPLETA_18a**: % de exceso/defecto de proporción de población con pauta completa de cada comunidad respecto a la media nacional.

### Scripts de R

- **codigo_resumen.R**: código principal a ejecutar. Además del código citado anteriormente, en [codigo_resumen.R](https://github.com/JavierAlvarezLiebana/vacunas_esp/blob/main/codigo_resumen.R) se hace una llamada a tres script de R: `datos_por_fecha.R` (datos de vacunación en formato lista, una por fecha, y en cada una, los datos de todas las comunidades para cada una de ellas), `datos_por_ccaa.R` (datos de vacunación en formato lista, una por comunidad, y en cada una, la evolución por fecha de todas las variables) y `resumen_global.R` (datos de vacunación globales en España, con la evolución tempoeral de cada variable)


```R
# ########################
# RESUMEN
# # ########################

# Cálculo de datos por fecha
source("./datos_por_fecha.R")

# Cálculo de datos por ccaa
source("./datos_por_ccaa.R")

# Cálculo resumen global España
source("./resumen_global.R")
```

- **datos_por_fecha.R**: código para resumir los valores por fecha. Devuelve una lista: cada elemento es el resumen de dicha fecha, que es a su vez un `data.frame` con las variables por comunidades de dicha fecha.

```R
# #######################################################
# SCRAPPING PDF VACUNAS: HASTA 13 DE ENERO INCLUIDO
# #######################################################

# Páginas dónde estén las tabla a leer
pagina_tabla <- 2

# Datos filtrados
pdf_filtrado <- list()
idx_fechas <- which(names(pdf_bruto) <= "2021-01-13")
idx_aux <- 1
for (i in idx_fechas) {
  
  pdf_filtrado[[idx_aux]] <- pdf_bruto[[i]]
  idx_aux <- idx_aux + 1
}
names(pdf_filtrado) <- names(pdf_bruto)[idx_fechas]

# A cada elemento de la lista (cada pdf, seleccionamos su texto
# y solo el de la pagina guardada)
tabla_bruto <- lapply(pdf_filtrado, function(x, pagina) { x$text[pagina] },
                      pagina_tabla)
tabla_bruto <- lapply(tabla_bruto, FUN = "toupper") # Todo a mayúsculas

# En cada fecha (elemento de la lista), cada línea (salto de línea = "\n")
# la guardamos por separado
tabla_sep_lineas <- lapply(lapply(tabla_bruto, FUN = "str_split", "\n"),
                           "unlist")

# Fila de cada tabla donde empieza a dar datos
idx <- lapply(tabla_sep_lineas,
              FUN = function(x, marca) { which(grepl(marca, x))[1] },
              "ANDALUCÍA")

# Tabla de tamaño n_ccaa x n_fechas
tabla_valores <- mapply(tabla_sep_lineas,
                        FUN = function (x, idx, l) { x[idx:(idx + l - 1)] },
                        idx, length(ccaa$NOMBRES))

# Panel vacunas por fecha
idx_fechas <- which(names(pdf_bruto) <= "2021-01-13")
panel_vacunas_fecha <- list()
for (i in idx_fechas) {
  
  # Datos de esa fecha
  datos <- tabla_valores[, i]
  
  # Separamos cada casilla y cambiamos puntos decimales
  datos_split <-
    lapply(lapply(lapply(sapply(datos, FUN = "str_split", " "),
                         FUN = function(x) { gsub("\\.", "", x) }),
                  FUN = function(x) { gsub(",", ".", x) }),
           FUN = function(x) { gsub("%", "", x) })
  
  # Eliminamos vacíos
  datos_depurados <- 
    sapply(datos_split,
           FUN = function(x) { as.numeric(x[x != "" &
                                              !is.na(as.numeric(x))]) })
  
  # Formato tabla (data.frame) con poblacion
  panel_vacunas <- left_join(ccaa, poblacion, by = "ISO")
  nombres_panel <- names(panel_vacunas)
  panel_vacunas <- data.frame(cbind(panel_vacunas, t(datos_depurados)),
                              row.names = NULL)
  names(panel_vacunas) <- c(nombres_panel, "DOSIS_ENTREGADAS_PFIZER",
                            "DOSIS_ADMIN", "PORC_ADMIN_SOBRE_CCAA")
  
  # En estas fechas no había pauta completa y solo había Pfizer
  panel_vacunas$DOSIS_ENTREGADAS_MODERNA <-
    panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA <-
    panel_vacunas$PAUTA_COMPLETA <- 0
  
  # Calculamos dosis entregadas en total
  panel_vacunas$DOSIS_ENTREGADAS <-
    panel_vacunas$DOSIS_ENTREGADAS_PFIZER +
    panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA +
    panel_vacunas$DOSIS_ENTREGADAS_MODERNA
  
  # % ENTREGADAS RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL <-
    panel_vacunas$DOSIS_ENTREGADAS / sum(panel_vacunas$DOSIS_ENTREGADAS)
  
  # % ADMIN RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ADMIN_SOBRE_TOTAL <-
    panel_vacunas$DOSIS_ADMIN / sum(panel_vacunas$DOSIS_ADMIN)
  
  # Calculamos global España
  panel_vacunas <-
    rbind(panel_vacunas,
          data.frame(NOMBRES = "ESPAÑA", ISO = "ES",
                     poblacion = sum(poblacion$poblacion),
                     poblacion_mayor_16a = sum(poblacion$poblacion_mayor_16a),
                     poblacion_mayor_18a = sum(poblacion$poblacion_mayor_18a),
                     porc_pobl_total = 100,
                     porc_pobl_total_mayor_16a =
                       sum(poblacion$poblacion_mayor_16a) / sum(poblacion$poblacion),
                     porc_pobl_total_mayor_18a =
                       sum(poblacion$poblacion_mayor_18a) / sum(poblacion$poblacion),
                     DOSIS_ENTREGADAS_PFIZER =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_PFIZER),
                     DOSIS_ENTREGADAS_MODERNA =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_MODERNA),
                     DOSIS_ENTREGADAS_ASTRA_ZENECA  =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA),
                     DOSIS_ENTREGADAS = sum(panel_vacunas$DOSIS_ENTREGADAS),
                     DOSIS_ADMIN = sum(panel_vacunas$DOSIS_ADMIN),
                     PORC_ADMIN_SOBRE_CCAA = 
                       sum(panel_vacunas$DOSIS_ADMIN) /
                       sum(panel_vacunas$DOSIS_ENTREGADAS),
                     PAUTA_COMPLETA = sum(panel_vacunas$PAUTA_COMPLETA),
                     PORC_ENTREGADAS_SOBRE_TOTAL = 100.000,
                     PORC_ADMIN_SOBRE_TOTAL = 100.000))
  
  # Cálculos extras poblacionales
  
  # Desviación de % de entregadas vs % peso poblacional
  panel_vacunas$DESV_DOSIS_ENTREGADAS <- 
    100 * (panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL /
             panel_vacunas$porc_pobl_total - 1)
  
  # % pauta completa
  panel_vacunas$PORC_PAUTA_COMPLETA <-
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion
  panel_vacunas$PORC_PAUTA_COMPLETA_16a <- 
    panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion_mayor_16a
  panel_vacunas$PORC_PAUTA_COMPLETA_18a <-
    panel_vacunas$PAUTA_COMPLETA /
    panel_vacunas$poblacion_mayor_18a
  
  # Desviación respecto a la media de % pauta completa
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA <-
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA /
             panel_vacunas$PORC_PAUTA_COMPLETA[length(panel_vacunas$PORC_PAUTA_COMPLETA)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_16a <-
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA_16a /
             panel_vacunas$PORC_PAUTA_COMPLETA_16a[length(panel_vacunas$PORC_PAUTA_COMPLETA_16a)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_18a <-
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA_18a /
             panel_vacunas$PORC_PAUTA_COMPLETA_18a[length(panel_vacunas$PORC_PAUTA_COMPLETA_18a)] - 1)
  
  # Dosis por cada 100 hab
  panel_vacunas$DOSIS_ADMIN_100HAB <-
    100 * panel_vacunas$DOSIS_ADMIN / panel_vacunas$poblacion
  panel_vacunas$DESV_DOSIS_ADMIN <-
    100 * (panel_vacunas$DOSIS_ADMIN_100HAB /
             panel_vacunas$DOSIS_ADMIN_100HAB[length(panel_vacunas$DOSIS_ADMIN_100HAB)] - 1)
  
  # Guardamos (con cols reordenadas)
  panel_vacunas_fecha[[i]] <- panel_vacunas[, c(1:9, 13:15, 16, 10:12, 17:26)]
  panel_vacunas_fecha[[i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[i]],
                                                              FUN = "typeof"))) == "double")] <-
    round(panel_vacunas_fecha[[i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[i]],
                                                                      FUN = "typeof"))) == "double")], 3)
  
}
names(panel_vacunas_fecha) <- nombre_cols <- names(pdf_filtrado)
nombre_vars <- names(panel_vacunas_fecha[[1]])











# #######################################################
# SCRAPPING PDF VACUNAS: DESDE 14 ENERO HASTA 15 ENERO INCLUIDOS
# #######################################################

# Páginas dónde estén las tabla a leer
pagina_tabla <- 2

# Datos filtrados
pdf_filtrado <- list()
idx_fechas <- which(names(pdf_bruto) %in% c("2021-01-14", "2021-01-15"))
idx_aux <- 1
for (i in idx_fechas) {
  
  pdf_filtrado[[idx_aux]] <- pdf_bruto[[i]]
  idx_aux <- idx_aux + 1
}
names(pdf_filtrado) <- names(pdf_bruto)[idx_fechas]

# A cada elemento de la lista (cada pdf, seleccionamos su texto
# y solo el de la pagina guardada)
tabla_bruto <- lapply(pdf_filtrado, function(x, pagina) { x$text[pagina] },
                      pagina_tabla)
tabla_bruto <- lapply(tabla_bruto, FUN = "toupper") # Todo a mayúsculas

# En cada fecha (elemento de la lista), cada línea (salto de línea = "\n")
# la guardamos por separado
tabla_sep_lineas <- lapply(lapply(tabla_bruto, FUN = "str_split", "\n"),
                           "unlist")

# Fila de cada tabla donde empieza a dar datos
idx <- lapply(tabla_sep_lineas,
              FUN = function(x, marca) { which(grepl(marca, x))[1] },
              "ANDALUCÍA")

# Tabla de tamaño n_ccaa x n_fechas
tabla_valores <- mapply(tabla_sep_lineas,
                        FUN = function (x, idx, l) { x[idx:(idx + l - 1)] },
                        idx, length(ccaa$NOMBRES))

# Panel vacunas por fecha
idx_aux <- length(panel_vacunas_fecha)
for (i in 1:length(idx_fechas)) {
  
  # Datos de esa fecha
  datos <- tabla_valores[, i]
  
  # Separamos cada casilla y cambiamos puntos decimales
  datos_split <-
    lapply(lapply(lapply(sapply(datos, FUN = "str_split", " "),
                         FUN = function(x) { gsub("\\.", "", x) }),
                  FUN = function(x) { gsub(",", ".", x) }),
           FUN = function(x) { gsub("%", "", x) })
  
  # Eliminamos vacíos
  datos_depurados <- 
    sapply(datos_split,
           FUN = function(x) { as.numeric(x[x != "" &
                                              !is.na(as.numeric(x))]) })
  
  # Formato tabla (data.frame) con poblacion
  panel_vacunas <- left_join(ccaa, poblacion, by = "ISO")
  nombres_panel <- names(panel_vacunas)
  panel_vacunas <- data.frame(cbind(panel_vacunas, t(datos_depurados)),
                              row.names = NULL)
  names(panel_vacunas) <- c(nombres_panel, "DOSIS_ENTREGADAS_PFIZER",
                            "DOSIS_ENTREGADAS_MODERNA",
                            "DOSIS_ENTREGADAS",
                            "DOSIS_ADMIN", "PORC_ADMIN_SOBRE_CCAA")
  
  # En estas fechas no había pauta completa y solo había Pfizer
  panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA <-
    panel_vacunas$PAUTA_COMPLETA <- 0
  
  
  # % ENTREGADAS RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL <-
    100 * panel_vacunas$DOSIS_ENTREGADAS /
    sum(panel_vacunas$DOSIS_ENTREGADAS)
  
  # % ADMIN RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ADMIN_SOBRE_TOTAL <-
    100 * panel_vacunas$DOSIS_ADMIN / sum(panel_vacunas$DOSIS_ADMIN)
  
  
  # Calculamos global España
  panel_vacunas <-
    rbind(panel_vacunas,
          data.frame(NOMBRES = "ESPAÑA", ISO = "ES",
                     poblacion = sum(poblacion$poblacion),
                     poblacion_mayor_16a = sum(poblacion$poblacion_mayor_16a),
                     poblacion_mayor_18a = sum(poblacion$poblacion_mayor_18a),
                     porc_pobl_total = 100,
                     porc_pobl_total_mayor_16a =
                       100 * sum(poblacion$poblacion_mayor_16a) /
                       sum(poblacion$poblacion),
                     porc_pobl_total_mayor_18a =
                       100 * sum(poblacion$poblacion_mayor_18a) /
                       sum(poblacion$poblacion),
                     DOSIS_ENTREGADAS_PFIZER =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_PFIZER),
                     DOSIS_ENTREGADAS_MODERNA =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_MODERNA),
                     DOSIS_ENTREGADAS_ASTRA_ZENECA  =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA),
                     DOSIS_ENTREGADAS = sum(panel_vacunas$DOSIS_ENTREGADAS),
                     DOSIS_ADMIN = sum(panel_vacunas$DOSIS_ADMIN),
                     PORC_ADMIN_SOBRE_CCAA =
                       100 * sum(panel_vacunas$DOSIS_ADMIN) /
                       sum(panel_vacunas$DOSIS_ENTREGADAS),
                     PAUTA_COMPLETA = sum(panel_vacunas$PAUTA_COMPLETA),
                     PORC_ENTREGADAS_SOBRE_TOTAL = 100.0,
                     PORC_ADMIN_SOBRE_TOTAL = 100.0))
  
  # Cálculos extras poblacionales
  
  # Desviación de % de entregadas vs % peso poblacional
  panel_vacunas$DESV_DOSIS_ENTREGADAS <-
    100 * (panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL /
             panel_vacunas$porc_pobl_total - 1)
  
  # % pauta completa
  panel_vacunas$PORC_PAUTA_COMPLETA <-
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion
  panel_vacunas$PORC_PAUTA_COMPLETA_16a <- 
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion_mayor_16a
  panel_vacunas$PORC_PAUTA_COMPLETA_18a <- 
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion_mayor_18a
  
  # Desviación respecto a la media de % pauta completa
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA <-
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA /
             panel_vacunas$PORC_PAUTA_COMPLETA[length(panel_vacunas$PORC_PAUTA_COMPLETA)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_16a <- 
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA_16a /
             panel_vacunas$PORC_PAUTA_COMPLETA_16a[length(panel_vacunas$PORC_PAUTA_COMPLETA_16a)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_18a <-
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA_18a /
             panel_vacunas$PORC_PAUTA_COMPLETA_18a[length(panel_vacunas$PORC_PAUTA_COMPLETA_18a)] - 1)
  
  # Dosis por cada 100 hab
  panel_vacunas$DOSIS_ADMIN_100HAB <-
    100 * panel_vacunas$DOSIS_ADMIN / panel_vacunas$poblacion
  panel_vacunas$DESV_DOSIS_ADMIN <-
    100 * (panel_vacunas$DOSIS_ADMIN_100HAB /
             panel_vacunas$DOSIS_ADMIN_100HAB[length(panel_vacunas$DOSIS_ADMIN_100HAB)] - 1)
  
  # Guardamos
  panel_vacunas_fecha[[idx_aux + i]] <- panel_vacunas[, nombre_vars]
  panel_vacunas_fecha[[idx_aux + i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[idx_aux + i]],
                                                                        FUN = "typeof"))) == "double")] <-
    round(panel_vacunas_fecha[[idx_aux + i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[idx_aux + i]],
                                                                                FUN = "typeof"))) == "double")], 3)
  
  
}
names(panel_vacunas_fecha) <- c(nombre_cols, names(pdf_filtrado))
nombre_cols <- names(panel_vacunas_fecha)











# #######################################################
# SCRAPPING PDF VACUNAS: DESDE 16 ENERO HASTA 8 FEBRERO INCLUIDOS
# #######################################################

# Páginas dónde estén las tabla a leer
pagina_tabla <- 2

# Datos filtrados
pdf_filtrado <- list()
idx_fechas <- which(names(pdf_bruto) %in%
                      as.character(seq(as.Date("2021-01-16"),
                                       as.Date("2021-02-08"), by = 1)))
idx_aux <- 1
for (i in idx_fechas) {
  
  pdf_filtrado[[idx_aux]] <- pdf_bruto[[i]]
  idx_aux <- idx_aux + 1
}
names(pdf_filtrado) <- names(pdf_bruto)[idx_fechas]

# A cada elemento de la lista (cada pdf, seleccionamos su texto
# y solo el de la pagina guardada)
tabla_bruto <- lapply(pdf_filtrado, function(x, pagina) { x$text[pagina] },
                      pagina_tabla)
tabla_bruto <- lapply(tabla_bruto, FUN = "toupper") # Todo a mayúsculas

# En cada fecha (elemento de la lista), cada línea (salto de línea = "\n")
# la guardamos por separado
tabla_sep_lineas <- lapply(lapply(tabla_bruto, FUN = "str_split", "\n"),
                           "unlist")

# Fila de cada tabla donde empieza a dar datos
idx <- lapply(tabla_sep_lineas,
              FUN = function(x, marca) { which(grepl(marca, x))[1] },
              "ANDALUCÍA")

# Tabla de tamaño n_ccaa x n_fechas
tabla_valores <- mapply(tabla_sep_lineas,
                        FUN = function (x, idx, l) { x[idx:(idx + l - 1)] },
                        idx, length(ccaa$NOMBRES))

# Panel vacunas por fecha
idx_aux <- length(panel_vacunas_fecha)
for (i in 1:length(idx_fechas)) {
  
  # Datos de esa fecha
  datos <- tabla_valores[, i]
  
  # Separamos cada casilla y cambiamos puntos decimales
  datos_split <-
    lapply(lapply(lapply(sapply(datos, FUN = "str_split", " "),
                         FUN = function(x) { gsub("\\.", "", x) }),
                  FUN = function(x) { gsub(",", ".", x) }),
           FUN = function(x) { gsub("%", "", x) })
  
  # Eliminamos vacíos
  datos_depurados <- 
    sapply(datos_split,
           FUN = function(x) { as.numeric(x[x != "" &
                                              !is.na(as.numeric(x))]) })
  datos_depurados <-
    matrix(unlist(datos_depurados), ncol = length(ccaa$NOMBRES))
  
  # Formato tabla (data.frame) con poblacion
  panel_vacunas <- left_join(ccaa, poblacion, by = "ISO")
  nombres_panel <- names(panel_vacunas)
  panel_vacunas <- data.frame(cbind(panel_vacunas, t(datos_depurados)),
                              row.names = NULL)
  names(panel_vacunas) <- c(nombres_panel, "DOSIS_ENTREGADAS_PFIZER",
                            "DOSIS_ENTREGADAS_MODERNA",
                            "DOSIS_ENTREGADAS",
                            "DOSIS_ADMIN", "PORC_ADMIN_SOBRE_CCAA",
                            "PAUTA_COMPLETA")
  
  panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA <- 0
  
  
  # % ENTREGADAS RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL <-
    100 * panel_vacunas$DOSIS_ENTREGADAS / sum(panel_vacunas$DOSIS_ENTREGADAS)
  
  # % ADMIN RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ADMIN_SOBRE_TOTAL <-
    100 * panel_vacunas$DOSIS_ADMIN / sum(panel_vacunas$DOSIS_ADMIN)
  
  
  # Calculamos global España
  panel_vacunas <-
    rbind(panel_vacunas,
          data.frame(NOMBRES = "ESPAÑA", ISO = "ES",
                     poblacion = sum(poblacion$poblacion),
                     poblacion_mayor_16a = sum(poblacion$poblacion_mayor_16a),
                     poblacion_mayor_18a = sum(poblacion$poblacion_mayor_18a),
                     porc_pobl_total = 100,
                     porc_pobl_total_mayor_16a =
                       100 * sum(poblacion$poblacion_mayor_16a) /
                       sum(poblacion$poblacion),
                     porc_pobl_total_mayor_18a =
                       100 * sum(poblacion$poblacion_mayor_18a) /
                       sum(poblacion$poblacion),
                     DOSIS_ENTREGADAS_PFIZER =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_PFIZER),
                     DOSIS_ENTREGADAS_MODERNA =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_MODERNA),
                     DOSIS_ENTREGADAS_ASTRA_ZENECA  =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA),
                     DOSIS_ENTREGADAS = sum(panel_vacunas$DOSIS_ENTREGADAS),
                     DOSIS_ADMIN = sum(panel_vacunas$DOSIS_ADMIN),
                     PORC_ADMIN_SOBRE_CCAA =
                       100 * sum(panel_vacunas$DOSIS_ADMIN) /
                       sum(panel_vacunas$DOSIS_ENTREGADAS),
                     PAUTA_COMPLETA = sum(panel_vacunas$PAUTA_COMPLETA),
                     PORC_ENTREGADAS_SOBRE_TOTAL = 100.0,
                     PORC_ADMIN_SOBRE_TOTAL = 100.0))
  
  # Cálculos extras poblacionales
  
  # Desviación de % de entregadas vs % peso poblacional
  panel_vacunas$DESV_DOSIS_ENTREGADAS <-
    100 * (panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL /
             panel_vacunas$porc_pobl_total - 1)
  
  # % pauta completa
  panel_vacunas$PORC_PAUTA_COMPLETA <-
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion
  panel_vacunas$PORC_PAUTA_COMPLETA_16a <- 
    100 * panel_vacunas$PAUTA_COMPLETA /  panel_vacunas$poblacion_mayor_16a
  panel_vacunas$PORC_PAUTA_COMPLETA_18a <-
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion_mayor_18a
  
  # Desviación respecto a la media de % pauta completa
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA <- 
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA /
             panel_vacunas$PORC_PAUTA_COMPLETA[length(panel_vacunas$PORC_PAUTA_COMPLETA)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_16a <- 
    100 *
    (panel_vacunas$PORC_PAUTA_COMPLETA_16a /
       panel_vacunas$PORC_PAUTA_COMPLETA_16a[length(panel_vacunas$PORC_PAUTA_COMPLETA_16a)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_18a <- 
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA_18a /
             panel_vacunas$PORC_PAUTA_COMPLETA_18a[length(panel_vacunas$PORC_PAUTA_COMPLETA_18a)] - 1)
  
  # Dosis por cada 100 hab
  panel_vacunas$DOSIS_ADMIN_100HAB <-
    100 * panel_vacunas$DOSIS_ADMIN / panel_vacunas$poblacion
  panel_vacunas$DESV_DOSIS_ADMIN <-
    100 * (panel_vacunas$DOSIS_ADMIN_100HAB /
             panel_vacunas$DOSIS_ADMIN_100HAB[length(panel_vacunas$DOSIS_ADMIN_100HAB)] - 1)
  
  # Guardamos
  panel_vacunas_fecha[[idx_aux + i]] <- panel_vacunas[, nombre_vars]
  panel_vacunas_fecha[[idx_aux + i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[idx_aux + i]],
                                                                        FUN = "typeof"))) == "double")] <-
    round(panel_vacunas_fecha[[idx_aux + i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[idx_aux + i]],
                                                                                FUN = "typeof"))) == "double")], 3)
  
  
}
names(panel_vacunas_fecha) <- c(nombre_cols, names(pdf_filtrado))
nombre_cols <- names(panel_vacunas_fecha)
















# #######################################################
# SCRAPPING PDF VACUNAS: DESDE 9 FEBRERO HASTA ACTUALIDAD INCLUIDOS
# #######################################################

# Páginas dónde estén las tabla a leer
pagina_tabla <- 2

# Datos filtrados
pdf_filtrado <- list()
idx_fechas <- which(names(pdf_bruto) %in%
                      as.character(seq(as.Date("2021-02-09"),
                                       as.Date(Sys.time()), by = 1)))
idx_aux <- 1
for (i in idx_fechas) {
  
  pdf_filtrado[[idx_aux]] <- pdf_bruto[[i]]
  idx_aux <- idx_aux + 1
}
names(pdf_filtrado) <- names(pdf_bruto)[idx_fechas]

# A cada elemento de la lista (cada pdf, seleccionamos su texto
# y solo el de la pagina guardada)
tabla_bruto <- lapply(pdf_filtrado, function(x, pagina) { x$text[pagina] },
                      pagina_tabla)
tabla_bruto <- lapply(tabla_bruto, FUN = "toupper") # Todo a mayúsculas

# En cada fecha (elemento de la lista), cada línea (salto de línea = "\n")
# la guardamos por separado
tabla_sep_lineas <- lapply(lapply(tabla_bruto, FUN = "str_split", "\n"),
                           "unlist")

# Fila de cada tabla donde empieza a dar datos
idx <- lapply(tabla_sep_lineas,
              FUN = function(x, marca) { which(grepl(marca, x))[1] },
              "ANDALUCÍA")

# Tabla de tamaño n_ccaa x n_fechas
tabla_valores <- mapply(tabla_sep_lineas,
                        FUN = function (x, idx, l) { x[idx:(idx + l - 1)] },
                        idx, length(ccaa$NOMBRES))

# Panel vacunas por fecha
idx_aux <- length(panel_vacunas_fecha)
for (i in 1:length(idx_fechas)) {
  
  # Datos de esa fecha
  datos <- tabla_valores[, i]
  
  # Separamos cada casilla y cambiamos puntos decimales
  datos_split <-
    lapply(lapply(lapply(sapply(datos, FUN = "str_split", " "),
                         FUN = function(x) { gsub("\\.", "", x) }),
                  FUN = function(x) { gsub(",", ".", x) }),
           FUN = function(x) { gsub("%", "", x) })
  
  # Eliminamos vacíos
  datos_depurados <- 
    sapply(datos_split,
           FUN = function(x) { as.numeric(x[x != "" &
                                              !is.na(as.numeric(x))]) })
  datos_depurados <-
    matrix(unlist(datos_depurados), ncol = length(ccaa$NOMBRES))
  
  # Formato tabla (data.frame) con poblacion
  panel_vacunas <- left_join(ccaa, poblacion, by = "ISO")
  nombres_panel <- names(panel_vacunas)
  panel_vacunas <- data.frame(cbind(panel_vacunas, t(datos_depurados)),
                              row.names = NULL)
  names(panel_vacunas) <- c(nombres_panel, "DOSIS_ENTREGADAS_PFIZER",
                            "DOSIS_ENTREGADAS_MODERNA",
                            "DOSIS_ENTREGADAS_ASTRA_ZENECA",
                            "DOSIS_ENTREGADAS",
                            "DOSIS_ADMIN", "PORC_ADMIN_SOBRE_CCAA",
                            "PAUTA_COMPLETA")
  
  
  
  # % ENTREGADAS RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL <-
    100 * panel_vacunas$DOSIS_ENTREGADAS / sum(panel_vacunas$DOSIS_ENTREGADAS)
  
  # % ADMIN RESPECTO AL TOTAL DE ESPAÑA
  panel_vacunas$PORC_ADMIN_SOBRE_TOTAL <-
    100 * panel_vacunas$DOSIS_ADMIN / sum(panel_vacunas$DOSIS_ADMIN)
  
  
  # Calculamos global España
  panel_vacunas <-
    rbind(panel_vacunas,
          data.frame(NOMBRES = "ESPAÑA", ISO = "ES",
                     poblacion = sum(poblacion$poblacion),
                     poblacion_mayor_16a = sum(poblacion$poblacion_mayor_16a),
                     poblacion_mayor_18a = sum(poblacion$poblacion_mayor_18a),
                     porc_pobl_total = 100,
                     porc_pobl_total_mayor_16a =
                       100 * sum(poblacion$poblacion_mayor_16a) /
                       sum(poblacion$poblacion),
                     porc_pobl_total_mayor_18a =
                       100 * sum(poblacion$poblacion_mayor_18a) /
                       sum(poblacion$poblacion),
                     DOSIS_ENTREGADAS_PFIZER =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_PFIZER),
                     DOSIS_ENTREGADAS_MODERNA =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_MODERNA),
                     DOSIS_ENTREGADAS_ASTRA_ZENECA  =
                       sum(panel_vacunas$DOSIS_ENTREGADAS_ASTRA_ZENECA),
                     DOSIS_ENTREGADAS = sum(panel_vacunas$DOSIS_ENTREGADAS),
                     DOSIS_ADMIN = sum(panel_vacunas$DOSIS_ADMIN),
                     PORC_ADMIN_SOBRE_CCAA =
                       100 * sum(panel_vacunas$DOSIS_ADMIN) /
                       sum(panel_vacunas$DOSIS_ENTREGADAS),
                     PAUTA_COMPLETA = sum(panel_vacunas$PAUTA_COMPLETA),
                     PORC_ENTREGADAS_SOBRE_TOTAL = 100.0,
                     PORC_ADMIN_SOBRE_TOTAL = 100.0))
  
  # Cálculos extras poblacionales
  
  # Desviación de % de entregadas vs % peso poblacional
  panel_vacunas$DESV_DOSIS_ENTREGADAS <-
    100 * (panel_vacunas$PORC_ENTREGADAS_SOBRE_TOTAL /
             panel_vacunas$porc_pobl_total - 1)
  
  # % pauta completa
  panel_vacunas$PORC_PAUTA_COMPLETA <-
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion
  panel_vacunas$PORC_PAUTA_COMPLETA_16a <- 
    100 * panel_vacunas$PAUTA_COMPLETA /  panel_vacunas$poblacion_mayor_16a
  panel_vacunas$PORC_PAUTA_COMPLETA_18a <-
    100 * panel_vacunas$PAUTA_COMPLETA / panel_vacunas$poblacion_mayor_18a
  
  # Desviación respecto a la media de % pauta completa
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA <- 
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA /
             panel_vacunas$PORC_PAUTA_COMPLETA[length(panel_vacunas$PORC_PAUTA_COMPLETA)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_16a <- 
    100 *
    (panel_vacunas$PORC_PAUTA_COMPLETA_16a /
       panel_vacunas$PORC_PAUTA_COMPLETA_16a[length(panel_vacunas$PORC_PAUTA_COMPLETA_16a)] - 1)
  panel_vacunas$DESV_PORC_PAUTA_COMPLETA_18a <- 
    100 * (panel_vacunas$PORC_PAUTA_COMPLETA_18a /
             panel_vacunas$PORC_PAUTA_COMPLETA_18a[length(panel_vacunas$PORC_PAUTA_COMPLETA_18a)] - 1)
  
  # Dosis por cada 100 hab
  panel_vacunas$DOSIS_ADMIN_100HAB <-
    100 * panel_vacunas$DOSIS_ADMIN / panel_vacunas$poblacion
  panel_vacunas$DESV_DOSIS_ADMIN <-
    100 * (panel_vacunas$DOSIS_ADMIN_100HAB /
             panel_vacunas$DOSIS_ADMIN_100HAB[length(panel_vacunas$DOSIS_ADMIN_100HAB)] - 1)
  
  # Guardamos
  panel_vacunas_fecha[[idx_aux + i]] <- panel_vacunas[, nombre_vars]
  panel_vacunas_fecha[[idx_aux + i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[idx_aux + i]],
                                                                        FUN = "typeof"))) == "double")] <-
    round(panel_vacunas_fecha[[idx_aux + i]][, which(as.character(unlist(lapply(panel_vacunas_fecha[[idx_aux + i]],
                                                                                FUN = "typeof"))) == "double")], 3)
  
  
}
names(panel_vacunas_fecha) <- c(nombre_cols, names(pdf_filtrado))
nombre_cols <- names(panel_vacunas_fecha)

















# ################
# EXPORTACIÓN
##################

save(panel_vacunas_fecha, file = "./EXPORTADO/panel_vacunas_fecha.RData")
for (i in 1:length(panel_vacunas_fecha)) {
  
  panel_vacunas_fecha[[i]]$NOMBRES <-
    as.character(panel_vacunas_fecha[[i]]$NOMBRES)
  panel_vacunas_fecha[[i]]$ISO <- as.character(panel_vacunas_fecha[[i]]$ISO)
  write.csv(panel_vacunas_fecha[[i]],
            file =
              paste0("./EXPORTADO/POR_FECHAS/datos_",
                     as.character(format(as.Date(names(panel_vacunas_fecha)[i]),
                                         "%d_%m_%Y")), ".csv"))
  
}


```
- **datos_por_ccaa.R**: código para resumir los valores por comunidad. Devuelve una lista: cada elemento es el resumen de dicha comunidad que es a su vez un `data.frame` con las variables por fechas.

```R
# ########################
# DATOS POR CCAA
# ########################

panel_vacunas_ccaa <- list()
for (i in 1:length(ccaa$NOMBRES)) {

  panel_vacunas_ccaa[[i]] <-
    data.frame(matrix(as.numeric(unlist(lapply(panel_vacunas_fecha,
                                      FUN = function(x, i) { x[i, -c(1:8)] }, 1))),
                        ncol = length(panel_vacunas_fecha)))
  names(panel_vacunas_ccaa[[i]]) <- as.character(names(panel_vacunas_fecha))
  row.names(panel_vacunas_ccaa[[i]]) <-
    names(panel_vacunas_fecha[[1]])[-c(1:8)]
}
names(panel_vacunas_ccaa) <- ccaa$ISO

# ################
# EXPORTACIÓN
##################

save(panel_vacunas_ccaa, file = "./EXPORTADO/panel_vacunas_ccaa.RData")
for (i in 1:length(panel_vacunas_ccaa)) {
  
  write.csv(panel_vacunas_ccaa[[i]],
            file = paste0("./EXPORTADO/POR_CCAA/datos_",
                          names(panel_vacunas_ccaa)[[i]], ".csv"))
  
}
```
- **resumen_global.R**: código para resumir los valores a nivel global de España.

```R
# ########################
# DATOS ESPAÑA
# ########################

panel_vacunas_global <-
  data.frame(matrix(as.numeric(unlist(lapply(panel_vacunas_fecha,
                                             FUN = function(x) { x[20, -c(1:8)] }))),
                               ncol = length(panel_vacunas_fecha)))
names(panel_vacunas_global) <- as.character(names(panel_vacunas_fecha))
row.names(panel_vacunas_global) <- names(panel_vacunas_fecha[[1]])[-c(1:8)]


# ################
# EXPORTACIÓN
##################

save(panel_vacunas_global, file = "./EXPORTADO/panel_vacunas_global.RData")
write.csv(panel_vacunas_global,
          file = paste0("./EXPORTADO/POR_CCAA/resumen_global.csv"))

```
