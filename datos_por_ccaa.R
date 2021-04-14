

# ###########################
# FUNCIONES AUXILIARES
# ###########################

fechas <- as.Date(names(pdf_bruto))
panel_vacunas <- list()

poblacion <-
  rbind(poblacion,
        data.frame("ISO" = c("FFAA", "ES"),
                   "poblacion" = c(NA, sum(poblacion$poblacion)),
                   "poblacion_mayor_16a" =
                     c(NA, sum(poblacion$poblacion_mayor_16a)),
                   "porc_pobl_total" = c(NA, 100),
                   "porc_pobl_total_mayor_16a" = c(NA, 100)))
row.names(poblacion) <- ccaa$ISO


# Filtramos por fecha, extraemos datos y guardamos en
# una tabla  de tamaño n_ccaa x n_fechas
filtrado_extraccion_pdf <-
  function(pdf, fechas_filtrado, marca = "ANDALUCÍA",
           nombres_ccaa, idx2 = 1) {
    
    # Guardamos datos filtrados por fecha tal cual
    pdf_filtrado <- pdf[as.character(fechas_filtrado)]
    names(pdf_filtrado) <- as.character(fechas_filtrado)
    
    # Leemos la segunda página
    # En tabla_bruto se guarda un vector de 6 elementos, cada uno
    # contiene la página 2 entera
    tabla_bruto <- pdf_filtrado %>% map("text") %>%
      map_chr(function(x) { x[2]} )
    tabla_bruto <- toupper(tabla_bruto) # A mayúsculas
    
    # En cada fecha (elemento de la lista), cada línea (salto de línea = "\n")
    # la guardamos por separado
    tabla_sep_lineas <- str_split(tabla_bruto, "\n") # lista, una por fecha
    
    # Fila de cada tabla donde empieza a dar datos
    idx <- tabla_sep_lineas %>% map(grepl, pattern = marca) %>%
      map(which) %>% map_int(function(x) { x[1] })
    
    # Tabla de tamaño n_ccaa x n_fechas
    tabla_valores <-
      mapply(tabla_sep_lineas,
             FUN = function (x, idx, l) { x[idx:(idx + l - idx2)] },
             idx, length(nombres_ccaa))
    
    # Output
    return(tabla_valores)
    
  }

# ###########################
# SCRAPPING PDF VACUNAS
# ###########################
# ###########################
# Filtramos HASTA 13 ENERO INCLUIDO y extraemos
# ###########################
fechas_filtro <- fechas[fechas <= as.Date("2021-01-13")]
nombres_ccaa <- ccaa$NOMBRES
tabla_valores <- # Matriz de nº ccaa x fechas
  filtrado_extraccion_pdf(pdf_bruto, fechas_filtro, nombres_ccaa = nombres_ccaa)

for (i in 1:length(ccaa$ISO)) {
  
  if (ccaa$ISO[i] == "FFAA") {
    
    panel_vacunas[[i]] <-
      data.frame("fechas" = fechas_filtro,
                 "ISO" = as.character(poblacion$ISO[i]),
                 "poblacion" = poblacion$poblacion[i],
                 "porc_pobl_total" = poblacion$porc_pobl_total[i],
                 "poblacion_mayor_16a" =
                   poblacion$poblacion_mayor_16a[i],
                 "porc_pobl_total_mayor_16a" =
                   poblacion$porc_pobl_total_mayor_16a[i],
                 matrix(0, nrow = length(fechas_filtro), ncol = 3))
    
  } else {
    
    aux <- str_split(str_split(pattern =
                                 as.character(ccaa$NOMBRES[i]),
                               tabla_valores[min(i, 20), ]), pattern = " ")
    aux <- aux %>% map(function(x) { x[x != ""] }) %>%
      map(gsub, pattern = "%", replacement = "") %>%
      map(gsub, pattern = "\\.", replacement = "") %>%
      map(gsub, pattern = ",", replacement = ".") %>%
      map(as.numeric) %>% map(na.omit)
    
    matriz_datos <- t(matrix(unlist(aux), nrow = 3))
    panel_vacunas[[i]] <-
      data.frame("fechas" = fechas_filtro,
                 "ISO" = as.character(poblacion$ISO[i]),
                 "poblacion" = poblacion$poblacion[i],
                 "porc_pobl_total" = poblacion$porc_pobl_total[i],
                 "poblacion_mayor_16a" =
                   poblacion$poblacion_mayor_16a[i],
                 "porc_pobl_total_mayor_16a" =
                   poblacion$porc_pobl_total_mayor_16a[i],
                 matriz_datos)
    
  }
  names(panel_vacunas[[i]]) <-
    c("fechas", "ISO", "poblacion", "porc_pobl_total",
      "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
      "dosis_entrega_pfizer", "dosis_admin", "porc_admin_sobre_ccaa")
  
  # En estas fechas no había pauta completa y solo había Pfizer
  panel_vacunas[[i]]$dosis_entrega_moderna <-
    panel_vacunas[[i]]$dosis_entrega_astra <-
    panel_vacunas[[i]]$personas_pauta_completa <- 0
  
  # Calculamos dosis entregadas en total
  panel_vacunas[[i]]$dosis_entrega <-
    panel_vacunas[[i]]$dosis_entrega_pfizer +
    panel_vacunas[[i]]$dosis_entrega_moderna +
    panel_vacunas[[i]]$dosis_entrega_astra
  
}
names(panel_vacunas) <- ccaa$ISO

  
  




# ###########################
# Filtramos 14-15 ENERO INCLUIDO y extraemos
# ###########################
fechas_filtro <- fechas[fechas %in%
                         c(as.Date("2021-01-14"), as.Date("2021-01-15"))]
nombres_ccaa <- ccaa$NOMBRES
tabla_valores <- # Matriz de nº ccaa x fechas
  filtrado_extraccion_pdf(pdf_bruto, fechas_filtro, nombres_ccaa = nombres_ccaa)

for (i in 1:length(ccaa$ISO)) {
  
  if (ccaa$ISO[i] == "FFAA") {
    
    aux <- data.frame("fechas" = fechas_filtro,
                       "ISO" = as.character(poblacion$ISO[i]),
                       "poblacion" = poblacion$poblacion[i],
                       "porc_pobl_total" = poblacion$porc_pobl_total[i],
                       "poblacion_mayor_16a" =
                         poblacion$poblacion_mayor_16a[i],
                       "porc_pobl_total_mayor_16a" =
                         poblacion$porc_pobl_total_mayor_16a[i],
                       matrix(0, nrow = length(fechas_filtro),
                              ncol = 5))
    
  } else {
    
    aux <- str_split(str_split(pattern =
                                 as.character(ccaa$NOMBRES[i]),
                               tabla_valores[min(i, 20), ]), pattern = " ")
    aux <- aux %>% map(function(x) { x[x != ""] }) %>%
      map(gsub, pattern = "%", replacement = "") %>%
      map(gsub, pattern = "\\.", replacement = "") %>%
      map(gsub, pattern = ",", replacement = ".") %>%
      map(as.numeric) %>% map(na.omit)
    
    matriz_datos <- t(matrix(unlist(aux), nrow = 5))
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                       "poblacion" = poblacion$poblacion[i],
                       "porc_pobl_total" = poblacion$porc_pobl_total[i],
                       "poblacion_mayor_16a" =
                         poblacion$poblacion_mayor_16a[i],
                       "porc_pobl_total_mayor_16a" =
                         poblacion$porc_pobl_total_mayor_16a[i],
                       matriz_datos)
  }
  names(aux) <-
    c("fechas", "ISO", "poblacion", "porc_pobl_total",
      "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
      "dosis_entrega_pfizer", "dosis_entrega_moderna", "dosis_entrega",
      "dosis_admin", "porc_admin_sobre_ccaa")
  
  # En estas fechas no había pauta completa ni Astra
  aux$dosis_entrega_astra <- aux$personas_pauta_completa <- 0
  
  # Calculamos dosis entregadas en total
  aux$dosis_entrega <- aux$dosis_entrega_pfizer +
    aux$dosis_entrega_moderna + aux$dosis_entrega_astra
  
  # Añadimos
  panel_vacunas[[i]] <- rbind(panel_vacunas[[i]], aux)
}
names(panel_vacunas) <- ccaa$ISO









# ###########################
# Filtramos 16 ENERO - 18 ENERO INCLUIDOS y extraemos
# ###########################
fechas_filtro <-
  fechas[fechas %in%
           seq(as.Date("2021-01-16"), as.Date("2021-01-18"), by = 1)]
nombres_ccaa <- ccaa$NOMBRES
tabla_valores <- # Matriz de nº ccaa x fechas
  filtrado_extraccion_pdf(pdf_bruto, fechas_filtro,
                          nombres_ccaa = nombres_ccaa, idx2 = 0)

for (i in 1:length(ccaa$ISO)) {
  
  if (ccaa$ISO[i] == "FFAA") {
    
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                      "poblacion" = poblacion$poblacion[i],
                      "porc_pobl_total" = poblacion$porc_pobl_total[i],
                      "poblacion_mayor_16a" =
                        poblacion$poblacion_mayor_16a[i],
                      "porc_pobl_total_mayor_16a" =
                        poblacion$porc_pobl_total_mayor_16a[i],
                      matrix(0, nrow = length(fechas_filtro),
                             ncol = 6))
    
  } else {
    
    aux <-
      str_split(
        str_split(pattern =
                    as.character(ccaa$NOMBRES[i]),
                  tabla_valores[min(ifelse(i == 8, i + 1,
                                           ifelse(i >= 9, i + 2, i)), 22), ]),
                pattern = " ")
    aux <- aux %>% map(function(x) { x[x != ""] }) %>%
      map(gsub, pattern = "%", replacement = "") %>%
      map(gsub, pattern = "\\.", replacement = "") %>%
      map(gsub, pattern = ",", replacement = ".") %>%
      map(as.numeric) %>% map(na.omit)
    
    matriz_datos <- t(matrix(unlist(aux), nrow = 6))
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                      "poblacion" = poblacion$poblacion[i],
                      "porc_pobl_total" = poblacion$porc_pobl_total[i],
                      "poblacion_mayor_16a" =
                        poblacion$poblacion_mayor_16a[i],
                      "porc_pobl_total_mayor_16a" =
                        poblacion$porc_pobl_total_mayor_16a[i],
                      matriz_datos)
  }
  names(aux) <-
    c("fechas", "ISO", "poblacion", "porc_pobl_total",
      "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
      "dosis_entrega_pfizer", "dosis_entrega_moderna", "dosis_entrega",
      "dosis_admin", "porc_admin_sobre_ccaa", "personas_pauta_completa")
  
  # En estas fechas ya había pauta completa pero no astra
  aux$dosis_entrega_astra <-  0
  
  # Calculamos dosis entregadas en total
  aux$dosis_entrega <- aux$dosis_entrega_pfizer +
    aux$dosis_entrega_moderna + aux$dosis_entrega_astra
  
  # Añadimos
  panel_vacunas[[i]] <- rbind(panel_vacunas[[i]], aux)
}
names(panel_vacunas) <- ccaa$ISO


# ###########################
# Filtramos 19 ENERO - 8 FEBRERO INCLUIDO y extraemos
# ###########################
fechas_filtro <-
  fechas[fechas %in%
           seq(as.Date("2021-01-19"), as.Date("2021-02-08"), by = 1)]
nombres_ccaa <- ccaa$NOMBRES
tabla_valores <- # Matriz de nº ccaa x fechas
  filtrado_extraccion_pdf(pdf_bruto, fechas_filtro,
                          nombres_ccaa = nombres_ccaa)

for (i in 1:length(ccaa$ISO)) {
  
  if (ccaa$ISO[i] == "FFAA") {
    
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                      "poblacion" = poblacion$poblacion[i],
                      "porc_pobl_total" = poblacion$porc_pobl_total[i],
                      "poblacion_mayor_16a" =
                        poblacion$poblacion_mayor_16a[i],
                      "porc_pobl_total_mayor_16a" =
                        poblacion$porc_pobl_total_mayor_16a[i],
                      matrix(0, nrow = length(fechas_filtro),
                             ncol = 6))
    
  } else {
    
    aux <-
      str_split(str_split(pattern =
                            as.character(ccaa$NOMBRES[i]),
                          tabla_valores[min(i, 20), ]), pattern = " ")
    aux <- aux %>% map(function(x) { x[x != ""] }) %>%
      map(gsub, pattern = "%", replacement = "") %>%
      map(gsub, pattern = "\\.", replacement = "") %>%
      map(gsub, pattern = ",", replacement = ".") %>%
      map(as.numeric) %>% map(na.omit)
    
    matriz_datos <- t(matrix(unlist(aux), nrow = 6))
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                      "poblacion" = poblacion$poblacion[i],
                      "porc_pobl_total" = poblacion$porc_pobl_total[i],
                      "poblacion_mayor_16a" =
                        poblacion$poblacion_mayor_16a[i],
                      "porc_pobl_total_mayor_16a" =
                        poblacion$porc_pobl_total_mayor_16a[i],
                      matriz_datos)
  }
  names(aux) <-
    c("fechas", "ISO", "poblacion", "porc_pobl_total",
      "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
      "dosis_entrega_pfizer", "dosis_entrega_moderna", "dosis_entrega",
      "dosis_admin", "porc_admin_sobre_ccaa", "personas_pauta_completa")
  
  # En estas fechas ya había pauta completa pero no astra
  aux$dosis_entrega_astra <-  0
  
  # Calculamos dosis entregadas en total
  aux$dosis_entrega <- aux$dosis_entrega_pfizer +
    aux$dosis_entrega_moderna + aux$dosis_entrega_astra
  
  # Añadimos
  panel_vacunas[[i]] <- rbind(panel_vacunas[[i]], aux)
}
names(panel_vacunas) <- ccaa$ISO

# ###########################
# Filtramos 9 FEBRERO - 23 MARZO INCLUIDO y extraemos
# ###########################
fechas_filtro <-
  fechas[fechas %in%
           seq(as.Date("2021-02-09"), as.Date("2021-03-23"), by = 1)]
nombres_ccaa <- ccaa$NOMBRES
tabla_valores <- # Matriz de nº ccaa x fechas
  filtrado_extraccion_pdf(pdf_bruto, fechas_filtro, nombres_ccaa = nombres_ccaa)

for (i in 1:length(ccaa$ISO)) {
  
  if (ccaa$ISO[i] == "FFAA") {
    
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                      "poblacion" = poblacion$poblacion[i],
                      "porc_pobl_total" = poblacion$porc_pobl_total[i],
                      "poblacion_mayor_16a" =
                        poblacion$poblacion_mayor_16a[i],
                      "porc_pobl_total_mayor_16a" =
                        poblacion$porc_pobl_total_mayor_16a[i],
                      matrix(0, nrow = length(fechas_filtro),
                             ncol = 7))
    
  } else {
    
    aux <- str_split(str_split(pattern =
                                 as.character(ccaa$NOMBRES[i]),
                               tabla_valores[min(i, 20), ]), pattern = " ")
    aux <- aux %>% map(function(x) { x[x != ""] }) %>%
      map(gsub, pattern = "%", replacement = "") %>%
      map(gsub, pattern = "\\.", replacement = "") %>%
      map(gsub, pattern = ",", replacement = ".") %>%
      map(as.numeric) %>% map(na.omit)
    
    matriz_datos <- t(matrix(unlist(aux), nrow = 7))
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                      "poblacion" = poblacion$poblacion[i],
                      "porc_pobl_total" = poblacion$porc_pobl_total[i],
                      "poblacion_mayor_16a" =
                        poblacion$poblacion_mayor_16a[i],
                      "porc_pobl_total_mayor_16a" =
                        poblacion$porc_pobl_total_mayor_16a[i],
                      matriz_datos)
  }
  names(aux) <-
    c("fechas", "ISO", "poblacion", "porc_pobl_total",
      "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
      "dosis_entrega_pfizer", "dosis_entrega_moderna",
      "dosis_entrega_astra", "dosis_entrega", "dosis_admin",
      "porc_admin_sobre_ccaa", "personas_pauta_completa")
  
  # Calculamos dosis entregadas en total
  aux$dosis_entrega <- aux$dosis_entrega_pfizer +
    aux$dosis_entrega_moderna + aux$dosis_entrega_astra
  
  # Añadimos
  panel_vacunas[[i]] <- rbind(panel_vacunas[[i]], aux)
}
names(panel_vacunas) <- ccaa$ISO
# ###########################
# Filtramos 24 MARZO - 5 ABRIL INCLUIDO y extraemos
# ###########################
fechas_filtro <-
  fechas[fechas %in%
           seq(as.Date("2021-03-24"), as.Date("2021-04-05"), by = 1)]
nombres_ccaa <- ccaa$NOMBRES
tabla_valores <- # Matriz de nº ccaa x fechas
  filtrado_extraccion_pdf(pdf_bruto, fechas_filtro, nombres_ccaa = nombres_ccaa)

for (i in 1:length(ccaa$ISO)) {
  
    aux <- str_split(str_split(pattern =
                                 as.character(ccaa$NOMBRES[i]),
                               tabla_valores[min(i, 21), ]), pattern = " ")
    aux <- aux %>% map(function(x) { x[x != ""] }) %>%
      map(gsub, pattern = "%", replacement = "") %>%
      map(gsub, pattern = "\\.", replacement = "") %>%
      map(gsub, pattern = ",", replacement = ".") %>%
      map(as.numeric) %>% map(na.omit)
    
    matriz_datos <- t(matrix(unlist(aux), nrow = 7))
    aux <- data.frame("fechas" = fechas_filtro,
                      "ISO" = as.character(poblacion$ISO[i]),
                      "poblacion" = poblacion$poblacion[i],
                      "porc_pobl_total" = poblacion$porc_pobl_total[i],
                      "poblacion_mayor_16a" =
                        poblacion$poblacion_mayor_16a[i],
                      "porc_pobl_total_mayor_16a" =
                        poblacion$porc_pobl_total_mayor_16a[i],
                      matriz_datos)
  names(aux) <-
    c("fechas", "ISO", "poblacion", "porc_pobl_total",
      "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
      "dosis_entrega_pfizer", "dosis_entrega_moderna",
      "dosis_entrega_astra", "dosis_entrega", "dosis_admin",
      "porc_admin_sobre_ccaa", "personas_pauta_completa")
  
  # Calculamos dosis entregadas en total
  aux$dosis_entrega <- aux$dosis_entrega_pfizer +
    aux$dosis_entrega_moderna + aux$dosis_entrega_astra
  
  # Añadimos
  panel_vacunas[[i]] <- rbind(panel_vacunas[[i]], aux)
}
names(panel_vacunas) <- ccaa$ISO

# ###########################
# PERSONAS VACUNADAS
# ###########################
for (i in 1:length(ccaa$ISO)) {
  
  # personas con al menos una dosis
  panel_vacunas[[i]]$personas_vacunadas <- 
    panel_vacunas[[i]]$dosis_admin -
    panel_vacunas[[i]]$personas_pauta_completa
  
  # personas solo con una 1 dosis
  panel_vacunas[[i]]$personas_1dosis <- 
    panel_vacunas[[i]]$personas_vacunadas -
    panel_vacunas[[i]]$personas_pauta_completa
}




# ###########################
# Filtramos DESDE 6 ABRIL INCLUIDO y extraemos
# ###########################
fechas_filtro <-
  fechas[fechas %in%
           seq(as.Date("2021-04-06"), as.Date(Sys.time()), by = 1)]
nombres_ccaa <- ccaa$NOMBRES
tabla_valores <- # Matriz de nº ccaa x fechas
  filtrado_extraccion_pdf(pdf_bruto, fechas_filtro, nombres_ccaa = nombres_ccaa)

for (i in 1:length(ccaa$ISO)) {
  
  aux <- str_split(str_split(pattern =
                               as.character(ccaa$NOMBRES[i]),
                             tabla_valores[min(i, 21), ]), pattern = " ")
  aux <- aux %>% map(function(x) { x[x != ""] }) %>%
    map(gsub, pattern = "%", replacement = "") %>%
    map(gsub, pattern = "\\.", replacement = "") %>%
    map(gsub, pattern = ",", replacement = ".") %>%
    map(as.numeric) %>% map(na.omit)
  
  matriz_datos <- t(matrix(unlist(aux), nrow = 8))
  aux <- data.frame("fechas" = fechas_filtro,
                    "ISO" = as.character(poblacion$ISO[i]),
                    "poblacion" = poblacion$poblacion[i],
                    "porc_pobl_total" = poblacion$porc_pobl_total[i],
                    "poblacion_mayor_16a" =
                      poblacion$poblacion_mayor_16a[i],
                    "porc_pobl_total_mayor_16a" =
                      poblacion$porc_pobl_total_mayor_16a[i],
                    matriz_datos)
  names(aux) <-
    c("fechas", "ISO", "poblacion", "porc_pobl_total",
      "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
      "dosis_entrega_pfizer", "dosis_entrega_moderna",
      "dosis_entrega_astra", "dosis_entrega", "dosis_admin",
      "porc_admin_sobre_ccaa", "personas_vacunadas",
      "personas_pauta_completa")
  
  # Calculamos dosis entregadas en total
  aux$dosis_entrega <- aux$dosis_entrega_pfizer +
    aux$dosis_entrega_moderna + aux$dosis_entrega_astra
  
  # Personas solo 1 dosis
  aux$personas_1dosis <- aux$personas_vacunadas -
    aux$personas_pauta_completa
  
  # Añadimos
  panel_vacunas[[i]] <- rbind(panel_vacunas[[i]], aux)
}
names(panel_vacunas) <- ccaa$ISO

# % entregadas respecto total de España
for (i in 1:length(ccaa$ISO)) {
  
  panel_vacunas[[i]]$porc_entregadas_sobre_total <-
    100 * panel_vacunas[[i]]$dosis_entrega / panel_vacunas$ES$dosis_entrega
}




# ##############################################
# DOSIS PAUTA COMPLETA / PRIMERA DOSIS
# ##############################################
for (i in 1:length(panel_vacunas)) {
  
  # Dosis usadas para primera/segunda dosis dosis
  panel_vacunas[[i]]$dosis_pauta_completa <-
    2 * panel_vacunas[[i]]$personas_pauta_completa
  panel_vacunas[[i]]$dosis_primera <-
    panel_vacunas[[i]]$dosis_admin - 
    panel_vacunas[[i]]$dosis_pauta_completa
  
  # % de personas vacunadas (con al menos 1 dosis)
  panel_vacunas[[i]]$porc_personas_vacunadas <-
    round(100 * panel_vacunas[[i]]$personas_vacunadas /
            poblacion$poblacion[i], 3)
  
  # % de personas vacunadas (con al menos 1 dosis) >= 16a
  panel_vacunas[[i]]$porc_personas_vacunadas_16a <-
    round(100 * panel_vacunas[[i]]$personas_vacunadas /
            poblacion$poblacion_mayor_16a[i], 3)
  
  # % de personas pauta completa
  panel_vacunas[[i]]$porc_personas_pauta_completa <-
    round(100 * panel_vacunas[[i]]$personas_pauta_completa /
            poblacion$poblacion[i], 3)
  
  # % de personas pauta completa >= 16a
  panel_vacunas[[i]]$porc_personas_pauta_completa_16a <-
    round(100 * panel_vacunas[[i]]$personas_pauta_completa /
            poblacion$poblacion_mayor_16a[i], 3)
  
  # % del total de entregadas
  panel_vacunas[[i]]$porc_entregadas_sobre_total <-
    round(100 * panel_vacunas[[i]]$dosis_entrega /
            panel_vacunas$ES$dosis_entrega, 3)
  
  panel_vacunas[[i]]$porc_admin_sobre_ccaa <-
    round(100 * panel_vacunas[[i]]$dosis_admin / 
            panel_vacunas[[i]]$dosis_entrega, 3)
}



# ####################
# RELLENAMOS FECHAS
# ####################

seq_fechas <- seq(min(as.Date(panel_vacunas$ES$fechas)),
                  max(as.Date(panel_vacunas$ES$fechas)), by = 1)
fechas_ausentes <- which(!(seq_fechas %in% panel_vacunas$ES$fechas))
aux <- list()
for (i in 1:length(ccaa$ISO)) {
  
  aux[[i]] <- data.frame()
  for (j in 1:length(seq_fechas)) {
    if (j %in% fechas_ausentes) {
      
      datos <- data.frame(seq_fechas[j], panel_vacunas[[i]][1, 2:6],
                          rbind(rep(NA, dim(panel_vacunas[[i]])[2] - 6)))
      names(datos) <- names(panel_vacunas[[i]])
      aux[[i]] <- rbind(aux[[i]], datos)
      
    } else {
      
      aux[[i]] <-
        rbind(aux[[i]],
              panel_vacunas[[i]][panel_vacunas[[i]]$fechas ==
                                   seq_fechas[j], ])
        
    }
  }
  names(aux[[i]]) <- names(panel_vacunas[[i]])
  
  # En entregadas: NA --> anterior
  aux[[i]]$dosis_entrega_pfizer <-
    na_interpolation(aux[[i]]$dosis_entrega_pfizer)
  aux[[i]]$dosis_entrega_moderna <-
    na_interpolation(aux[[i]]$dosis_entrega_moderna)
  aux[[i]]$dosis_entrega_astra <-
    na_interpolation(aux[[i]]$dosis_entrega_astra)
  aux[[i]]$dosis_entrega <- aux[[i]]$dosis_entrega_pfizer +
    aux[[i]]$dosis_entrega_moderna + aux[[i]]$dosis_entrega_astra
  
  # En admin: NA interpolado
  aux[[i]]$dosis_admin <- round(na_interpolation(aux[[i]]$dosis_admin))
  aux[[i]]$dosis_pauta_completa <- 
    round(na_interpolation(aux[[i]]$dosis_pauta_completa))
  aux[[i]]$dosis_primera <- aux[[i]]$dosis_admin - aux[[i]]$dosis_pauta_completa
  
  # En vacunados: NA interpolado
  aux[[i]]$personas_pauta_completa <-
    round(na_interpolation(aux[[i]]$personas_pauta_completa))
  aux[[i]]$personas_vacunadas <-
    round(na_interpolation(aux[[i]]$personas_vacunadas))
  aux[[i]]$personas_1dosis <- aux[[i]]$personas_vacunadas -
    aux[[i]]$personas_pauta_completa
  aux[[i]]$porc_personas_pauta_completa <-
    round(100 * aux[[i]]$personas_pauta_completa /
            aux[[i]]$poblacion, 3)
  aux[[i]]$porc_personas_vacunadas <-
    round(100 * aux[[i]]$personas_vacunadas /
            aux[[i]]$poblacion, 3)

  # Demografía sin NA
  aux[[i]]$ISO<- as.character(unique(aux[[i]]$ISO))
  aux[[i]]$poblacion <- as.numeric(unique(aux[[i]]$poblacion))
  aux[[i]]$poblacion_mayor_16a <-
    as.numeric(unique(aux[[i]]$poblacion_mayor_16a))
  aux[[i]]$porc_pobl_total <-
    as.numeric(unique(aux[[i]]$porc_pobl_total))
  aux[[i]]$porc_pobl_total_mayor_16a <-
    as.numeric(unique(aux[[i]]$porc_pobl_total_mayor_16a))
}
names(aux) <- names(panel_vacunas)

# Guardamos
panel_vacunas <- aux




# ##############################
# DATOS DIARIOS/SEMANALES
# ##############################
for (i in 1:length(panel_vacunas)) {
  
  # Dosis entregadas diarias
  panel_vacunas[[i]]$dosis_diarias_entrega <-
    c(panel_vacunas[[i]]$dosis_entrega[1],
      diff(panel_vacunas[[i]]$dosis_entrega))
  panel_vacunas[[i]]$dosis_diarias_entrega_pfizer <-
    c(panel_vacunas[[i]]$dosis_entrega_pfizer[1],
      diff(panel_vacunas[[i]]$dosis_entrega_pfizer))
  panel_vacunas[[i]]$dosis_diarias_entrega_moderna <-
    c(panel_vacunas[[i]]$dosis_entrega_moderna[1],
      diff(panel_vacunas[[i]]$dosis_entrega_moderna))
  panel_vacunas[[i]]$dosis_diarias_entrega_astra <-
    c(panel_vacunas[[i]]$dosis_entrega_astra[1],
      diff(panel_vacunas[[i]]$dosis_entrega_astra))
  
  # Dosis entregadas acumuladas semanales
  panel_vacunas[[i]]$dosis_7D_entrega <-
    c(panel_vacunas[[i]]$dosis_entrega[1:7],
      diff(panel_vacunas[[i]]$dosis_entrega, 7))
  panel_vacunas[[i]]$dosis_7D_entrega_pfizer <-
    c(panel_vacunas[[i]]$dosis_entrega_pfizer[1:7],
      diff(panel_vacunas[[i]]$dosis_entrega_pfizer, 7))
  panel_vacunas[[i]]$dosis_7D_entrega_moderna <-
    c(panel_vacunas[[i]]$dosis_entrega_moderna[1:7],
      diff(panel_vacunas[[i]]$dosis_entrega_moderna, 7))
  panel_vacunas[[i]]$dosis_7D_entrega_astra <-
    c(panel_vacunas[[i]]$dosis_entrega_astra[1:7],
      diff(panel_vacunas[[i]]$dosis_entrega_astra, 7))

  # Dosis entregadas por 100 hab
  panel_vacunas[[i]]$dosis_entrega_100hab <-
    round(100 * panel_vacunas[[i]]$dosis_entrega /
            panel_vacunas[[i]]$poblacion, 3)
  panel_vacunas[[i]]$dosis_7D_entrega_100hab <-
    round(100 * panel_vacunas[[i]]$dosis_7D_entrega /
            panel_vacunas[[i]]$poblacion, 3)

  # Dosis admin diarias
  panel_vacunas[[i]]$dosis_diarias_admin <-
    c(panel_vacunas[[i]]$dosis_admin[1],
      diff(panel_vacunas[[i]]$dosis_admin))
  
  # Dosis admin acumuladas semanales
  panel_vacunas[[i]]$dosis_7D_admin <-
    c(panel_vacunas[[i]]$dosis_admin[1:7],
      diff(panel_vacunas[[i]]$dosis_admin, 7))
  
  # Dosis admin por 100 hab
  panel_vacunas[[i]]$dosis_admin_100hab <-
    round(100 * panel_vacunas[[i]]$dosis_admin /
            panel_vacunas[[i]]$poblacion, 3)
  panel_vacunas[[i]]$dosis_7D_admin_100hab <-
    round(100 * panel_vacunas[[i]]$dosis_7D_admin /
            panel_vacunas[[i]]$poblacion, 3)
  panel_vacunas[[i]]$dosis_diarias_admin_100hab <-
    round(100 * panel_vacunas[[i]]$dosis_diarias_admin /
            panel_vacunas[[i]]$poblacion, 3)
  
  # Vacunadas
  panel_vacunas[[i]]$personas_vacunadas_diarias <-
    c(panel_vacunas[[i]]$personas_vacunadas[1],
      diff(panel_vacunas[[i]]$personas_vacunadas))
  panel_vacunas[[i]]$porc_personas_vacunadas_diarias <-
    round(100 * panel_vacunas[[i]]$personas_vacunadas_diarias /
            panel_vacunas[[i]]$poblacion, 3)
  panel_vacunas[[i]]$personas_vacunadas_7D <-
    c(panel_vacunas[[i]]$personas_vacunadas[1:7],
      diff(panel_vacunas[[i]]$personas_vacunadas, 7))
  panel_vacunas[[i]]$porc_personas_vacunadas_7D <-
    round(100 * panel_vacunas[[i]]$personas_vacunadas_7D /
            panel_vacunas[[i]]$poblacion, 3)
  panel_vacunas[[i]]$porc_personas_vacunadas_16a_7D <-
    round(100 * panel_vacunas[[i]]$personas_vacunadas_7D /
            panel_vacunas[[i]]$poblacion_mayor_16a, 3)
  
  # Personas con pauta completa
  panel_vacunas[[i]]$personas_pauta_completa_diarias <-
    c(panel_vacunas[[i]]$personas_pauta_completa[1],
      diff(panel_vacunas[[i]]$personas_pauta_completa))
  panel_vacunas[[i]]$porc_personas_pauta_completa_diarias <-
    round(100 * panel_vacunas[[i]]$personas_pauta_completa_diarias /
            panel_vacunas[[i]]$poblacion, 3)
  panel_vacunas[[i]]$personas_pauta_completa_7D <-
    c(panel_vacunas[[i]]$personas_pauta_completa[1:7],
      diff(panel_vacunas[[i]]$personas_pauta_completa, 7))
  panel_vacunas[[i]]$porc_personas_pauta_completa_7D <-
    round(100 * panel_vacunas[[i]]$personas_pauta_completa_7D /
            panel_vacunas[[i]]$poblacion, 3)
  panel_vacunas[[i]]$porc_personas_pauta_completa_16a_7D <-
    round(100 * panel_vacunas[[i]]$personas_pauta_completa_7D /
            panel_vacunas[[i]]$poblacion_mayor_16a, 3)
  
  # Dosis admin diarias primera/segunda dosis
  panel_vacunas[[i]]$dosis_diarias_primera <- 
    panel_vacunas[[i]]$dosis_diarias_admin -
    panel_vacunas[[i]]$personas_pauta_completa_diarias
  panel_vacunas[[i]]$dosis_diarias_segunda <- 
    panel_vacunas[[i]]$personas_pauta_completa_diarias
 
}


# ##############################
# DESVIACIONES Y CRECIMIENTOS
# ##############################

for (i in 1:length(panel_vacunas)) {
  
  # Ritmo crecimiento personas vacunadas
  panel_vacunas[[i]]$crec_diario_personas_vacunadas <- 
    c(0,
      round(100 *
              (panel_vacunas[[i]]$personas_vacunadas[-1] /
                 rev(rev(panel_vacunas[[i]]$personas_vacunadas)[-1]) - 1), 3))
  panel_vacunas[[i]]$crec_7D_personas_vacunadas <- 
    c(0,
      round(100 *
              (panel_vacunas[[i]]$personas_vacunadas_7D[-1] /
                 rev(rev(panel_vacunas[[i]]$personas_vacunadas_7D)[-1]) - 1), 3))
  
  # Ritmo crecimiento personas pauta completa
  panel_vacunas[[i]]$crec_diario_personas_pauta_completa <- 
    c(0,
      round(100 *
              (panel_vacunas[[i]]$personas_pauta_completa[-1] /
                 rev(rev(panel_vacunas[[i]]$personas_pauta_completa)[-1]) - 1), 3))
  panel_vacunas[[i]]$crec_7D_personas_pauta_completa <- 
    c(0,
      round(100 *
              (panel_vacunas[[i]]$personas_pauta_completa_7D[-1] /
                 rev(rev(panel_vacunas[[i]]$personas_pauta_completa_7D)[-1]) - 1), 3))
    
  # Ritmo crecimiento dosis admin
  panel_vacunas[[i]]$crec_diario_dosis_admin <- 
    c(0,
      round(100 *
              (panel_vacunas[[i]]$dosis_admin[-1] /
                 rev(rev(panel_vacunas[[i]]$dosis_admin)[-1]) - 1), 3))
  panel_vacunas[[i]]$crec_7D_dosis_admin <- 
    c(0,
      round(100 *
              (panel_vacunas[[i]]$dosis_7D_admin[-1] /
                 rev(rev(panel_vacunas[[i]]$dosis_7D_admin)[-1]) - 1), 3))
  
  
  # % de dosis admin de cada ccaa vs total españa
  panel_vacunas[[i]]$porc_admin_vs_total <-
    round(100 * panel_vacunas[[i]]$dosis_admin /
            panel_vacunas$ES$dosis_admin, 3)
  panel_vacunas[[i]]$porc_admin_vs_total_7D <-
    round(100 * panel_vacunas[[i]]$dosis_7D_admin /
            panel_vacunas$ES$dosis_7D_admin, 3)
  
  # % de dosis admin (del total de españa) vs % poblacion
  panel_vacunas[[i]]$desv_porc_admin_vs_total <-
    round(100 * (panel_vacunas[[i]]$porc_admin_vs_total /
                   panel_vacunas[[i]]$porc_pobl_total - 1), 3)
    
  # % de dosis entregadas vs % de peso poblacional que representa
  panel_vacunas[[i]]$desv_dosis_entrega <- 
    round(100 * (panel_vacunas[[i]]$porc_entregadas_sobre_total /
                   panel_vacunas[[i]]$porc_pobl_total - 1), 3)
  panel_vacunas[[i]]$desv_dosis_entrega[
    panel_vacunas[[i]]$dosis_entrega == 0] <- NA
  
  # % de vacunados vs % medio
  panel_vacunas[[i]]$desv_porc_personas_vacunadas <-
    round(100 * (panel_vacunas[[i]]$porc_personas_vacunadas /
                   panel_vacunas$ES$porc_personas_vacunadas - 1), 3)
  
  # % de pauta completa vs % medio
  panel_vacunas[[i]]$desv_porc_personas_pauta_completa <-
    round(100 * (panel_vacunas[[i]]$porc_personas_pauta_completa /
                   panel_vacunas$ES$porc_personas_pauta_completa - 1), 3)
  
}

# ##############################
# REORDENAMOS COLUMNAS
# ##############################


for (i in 1:length(panel_vacunas)) {
  
  panel_vacunas[[i]] <- panel_vacunas[[i]] %>%
    select("fechas",
           # Demografía
           "ISO", "poblacion", "porc_pobl_total",
           "poblacion_mayor_16a", "porc_pobl_total_mayor_16a",
           # Dosis entregadas acumuladas
           "dosis_entrega_pfizer", "dosis_entrega_astra",
           "dosis_entrega_moderna", "dosis_entrega",
           "dosis_entrega_100hab", "porc_entregadas_sobre_total",
           # Dosis entregadas diarias
           "dosis_diarias_entrega_pfizer", "dosis_diarias_entrega_astra",
           "dosis_diarias_entrega_moderna", "dosis_diarias_entrega",
           # Dosis entregadas 7D
           "dosis_7D_entrega_pfizer", "dosis_7D_entrega_astra",
           "dosis_7D_entrega_moderna", "dosis_7D_entrega",
           "dosis_7D_entrega_100hab",
           # Dosis admin acumuladas
           "dosis_admin", "dosis_primera", "dosis_pauta_completa",
           "dosis_admin_100hab", "porc_admin_sobre_ccaa",
           "porc_admin_vs_total",
           # Dosis admin diarias
           "dosis_diarias_admin", "dosis_diarias_admin_100hab",
           "crec_diario_dosis_admin", "dosis_diarias_primera",
           "dosis_diarias_segunda",
           # Dosis admin 7D
           "dosis_7D_admin", 
           "dosis_7D_admin_100hab", "crec_7D_dosis_admin",
           "porc_admin_vs_total_7D",
           # Personas vacunadas
           "personas_vacunadas", "personas_pauta_completa", "personas_1dosis",
           "porc_personas_vacunadas", "porc_personas_pauta_completa",
           "porc_personas_vacunadas_16a",  "porc_personas_pauta_completa_16a",
           # Personas vacunadas diarias
           "personas_vacunadas_diarias",
           "personas_pauta_completa_diarias", "porc_personas_vacunadas_diarias",
           "porc_personas_pauta_completa_diarias",
           # Personas vacunadas 7D
           "personas_vacunadas_7D",
           "personas_pauta_completa_7D", "porc_personas_vacunadas_7D",
           "porc_personas_pauta_completa_7D",
           "porc_personas_vacunadas_16a_7D",
           "porc_personas_pauta_completa_16a_7D",
           # Crecimientos y desviaciones
           "crec_diario_personas_vacunadas", "crec_7D_personas_vacunadas",
           "crec_diario_personas_pauta_completa",
           "crec_7D_personas_pauta_completa", "desv_porc_admin_vs_total",
           "desv_dosis_entrega", "desv_porc_personas_vacunadas",
           "desv_porc_personas_pauta_completa")  
}


  
# ################
# EXPORTACIÓN
##################

save(panel_vacunas, file = "./EXPORTADO/panel_vacunas_ccaa.RData")
for (i in 1:length(panel_vacunas)) {

  write.csv(panel_vacunas[[i]],
            file = paste0("./EXPORTADO/POR_CCAA/datos_",
                          names(panel_vacunas)[i], ".csv"),
            row.names = FALSE)
  
}




