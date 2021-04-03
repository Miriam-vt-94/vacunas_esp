
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
  
  # Añadimos FFAA
  panel_vacunas <- panel_vacunas %>%
    add_row(NOMBRES = "FUERZAS ARMADAS", ISO = "FFAA", poblacion = NA,
            poblacion_mayor_16a = NA, poblacion_mayor_18a = NA,
            porc_pobl_total = NA, porc_pobl_total_mayor_16a = NA,
            porc_pobl_total_mayor_18a = NA)
  nombres_panel <- names(panel_vacunas)
  aux <- t(datos_depurados)
  panel_vacunas <- data.frame(cbind(panel_vacunas,
                          rbind(aux, rep(0, dim(aux)[2]))),
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
  
  # Añadimos FFAA
  panel_vacunas <- panel_vacunas %>%
    add_row(NOMBRES = "FUERZAS ARMADAS", ISO = "FFAA", poblacion = NA,
            poblacion_mayor_16a = NA, poblacion_mayor_18a = NA,
            porc_pobl_total = NA, porc_pobl_total_mayor_16a = NA,
            porc_pobl_total_mayor_18a = NA)
  nombres_panel <- names(panel_vacunas)
  aux <- t(datos_depurados)
  panel_vacunas <- data.frame(cbind(panel_vacunas,
                                    rbind(aux, rep(0, dim(aux)[2]))),
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

  # Añadimos FFAA
  panel_vacunas <- panel_vacunas %>%
    add_row(NOMBRES = "FUERZAS ARMADAS", ISO = "FFAA", poblacion = NA,
            poblacion_mayor_16a = NA, poblacion_mayor_18a = NA,
            porc_pobl_total = NA, porc_pobl_total_mayor_16a = NA,
            porc_pobl_total_mayor_18a = NA)
  nombres_panel <- names(panel_vacunas)
  aux <- t(datos_depurados)
  panel_vacunas <- data.frame(cbind(panel_vacunas,
                                    rbind(aux, rep(0, dim(aux)[2]))),
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
# SCRAPPING PDF VACUNAS: DESDE 9 FEBRERO HASTA 23 DE MARZO INCLUIDOS
# #######################################################

# Páginas dónde estén las tabla a leer
pagina_tabla <- 2

# Datos filtrados
pdf_filtrado <- list()
idx_fechas <- which(names(pdf_bruto) %in%
                      as.character(seq(as.Date("2021-02-09"),
                                       as.Date("2021-03-23"), by = 1)))
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
  
  # Añadimos FFAA
  panel_vacunas <- panel_vacunas %>%
    add_row(NOMBRES = "FUERZAS ARMADAS", ISO = "FFAA", poblacion = NA,
            poblacion_mayor_16a = NA, poblacion_mayor_18a = NA,
            porc_pobl_total = NA, porc_pobl_total_mayor_16a = NA,
            porc_pobl_total_mayor_18a = NA)
  nombres_panel <- names(panel_vacunas)
  aux <- t(datos_depurados)
  panel_vacunas <- data.frame(cbind(panel_vacunas,
                                    rbind(aux, rep(0, dim(aux)[2]))),
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


















# #######################################################
# SCRAPPING PDF VACUNAS: DESDE 24 MARZO HASTA ACTUALIDAD INCLUIDOS
# #######################################################

# Páginas dónde estén las tabla a leer
pagina_tabla <- 2

# Datos filtrados
pdf_filtrado <- list()
idx_fechas <- which(names(pdf_bruto) %in%
                      as.character(seq(as.Date("2021-03-24"),
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
                        FUN = function (x, idx, l) { x[idx:(idx + l)] },
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
    matrix(unlist(datos_depurados), ncol = length(ccaa$NOMBRES) + 1)
  
  # Formato tabla (data.frame) con poblacion
  panel_vacunas <- left_join(ccaa, poblacion, by = "ISO")
  
  # Añadimos FFAA
  panel_vacunas <- panel_vacunas %>%
    add_row(NOMBRES = "FUERZAS ARMADAS", ISO = "FFAA", poblacion = NA,
            poblacion_mayor_16a = NA, poblacion_mayor_18a = NA,
            porc_pobl_total = NA, porc_pobl_total_mayor_16a = NA,
            porc_pobl_total_mayor_18a = NA)
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

