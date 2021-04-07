
# Cargamos datos antiguos
load("./DATOS/CANAL_TELEGRAM/pdf_covid_leido.RData")
load("./DATOS/CANAL_TELEGRAM/pdf_vacunas_leido.RData")

# #####################
# INFORMES COVID
# #####################

# Lectura de lo nuevo: intentamos leer 400 informes (aumentar si es ncesario)
idx_pdf <- setdiff(31:400, as.numeric(names(pdf_covid_leido)))
idx_aux <- length(pdf_covid_leido) + 1
for (i in idx_pdf) {
  
  # Con tryCatch, la orden se ejecuta y sigue el proceso aunque
  # devuelva error (que lo marcamos con un -1)
  
  url_covid <- paste0("https://www.mscbs.gob.es/profesionales/",
                      "saludPublica/ccayes/alertasActual/nCov/",
                      "documentos/Actualizacion_", i, "_COVID-19.pdf")
  intento_pdf <-
    tryCatch(pdf_info(url_covid), error = function(e) { -1 })
  
  # Si devuelve error, será un numeric
  # Si lo ha leído bien, lo guarda como una lista la metainfo
  if (typeof(intento_pdf) == "list") {
    
    # Guardamos datos
    pdf_covid_leido[[idx_aux]] <- intento_pdf
    names(pdf_covid_leido) <-
      c(names(pdf_covid_leido)[-length(pdf_covid_leido)], i)
    idx_aux <- idx_aux + 1
    
  }
} 

# Guardamos lo nuevo (que se añade solo a lo antiguo)
save(pdf_covid_leido, file = "./DATOS/CANAL_TELEGRAM/pdf_covid_leido.RData")

# Guardamos páginas, fecha y hora
paginas_covid_pdf <- fechas_covid_pdf <- horas_covid_pdf <- c()
for (i in 1:length(pdf_covid_leido)) {
  
  # Páginas, fecha, hora
  paginas_covid_pdf <- c(paginas_covid_pdf, pdf_covid_leido[[i]]$pages)
  fechas_covid_pdf <- c(fechas_covid_pdf,
                        as.character(as.Date(pdf_covid_leido[[i]]$modified)))
  horas_covid_pdf <-
    c(horas_covid_pdf,
      as.character(format(pdf_covid_leido[[i]]$modified, "%H:%M:%S")))
  
}        
cat(paste0("Se han exportado páginas/fecha/hora de ",
           length(fechas_covid_pdf), " informes covid de Sanidad\n"))
cat(paste0("Media de páginas: ", round(mean(paginas_covid_pdf), 2), "\n"))
cat(paste0("Mediana de páginas: ", round(median(paginas_covid_pdf), 2), "\n"))
cat(paste0("Hora media de última modificación: ",
           format(mean(as.POSIXct(horas_covid_pdf,
                                  format = "%H:%M:%S")), "%H:%M:%S\n")))
cat(paste0("Hora media de última modificación en el último mes: ",
           format(mean(as.POSIXct(horas_covid_pdf[fechas_covid_pdf >=
                                                    as.Date(Sys.time()) - 30],
                                  format = "%H:%M:%S")), "%H:%M:%S\n")))



# #####################
# INFORMES VACUNAS
# #####################

# Fechas posibles
fechas_totales <- format(seq(as.Date("2021-01-01"),
                             as.Date(Sys.time()), by = 1), "%Y%m%d")
fechas <- setdiff(fechas_totales, names(pdf_vacunas_leido))
idx_aux <- length(pdf_vacunas_leido) + 1
for (i in 1:length(fechas)) {
  
  # Con tryCatch, la orden se ejecuta y sigue el proceso aunque
  # devuelva error (que lo marcamos con un -1)
  url_vacunas <- paste0("https://www.mscbs.gob.es/profesionales/",
                        "saludPublica/ccayes/alertasActual/nCov/",
                        "documentos/Informe_GIV_comunicacion_",
                        fechas[i], ".pdf")
  intento_pdf <-
    tryCatch(pdf_info(url_vacunas), error = function(e) { -1 })
  
  # Si devuelve error, será un numeric
  # Si lo ha leído bien, lo guarda como una lista la metainfo
  if (typeof(intento_pdf) == "list") {
    
    # Guardamos datos
    pdf_vacunas_leido[[idx_aux]] <- intento_pdf
    names(pdf_vacunas_leido) <-
      c(names(pdf_vacunas_leido)[-length(pdf_vacunas_leido)], i)
    idx_aux <- idx_aux + 1
    
  }
} 
names(pdf_vacunas_leido) <-
  c(names(pdf_vacunas_leido)[!is.na(as.Date(names(pdf_vacunas_leido),
                                            "%Y%m%d"))],
    fechas[as.numeric(names(pdf_vacunas_leido))][
      !is.na(fechas[as.numeric(names(pdf_vacunas_leido))])])

# Guardamos
save(pdf_vacunas_leido,
     file = "./DATOS/CANAL_TELEGRAM/pdf_vacunas_leido.RData")

# Guardamos páginas, fechas y horas
paginas_pdf_vacunas <- fechas_pdf_vacunas <- horas_pdf_vacunas <- c()
for (i in 1:length(pdf_vacunas_leido)) {
 
  paginas_pdf_vacunas <- c(paginas_pdf_vacunas, pdf_vacunas_leido[[i]]$pages)
  fechas_pdf_vacunas <-
    c(fechas_pdf_vacunas,
      as.character(as.Date(pdf_vacunas_leido[[i]]$modified)))
  horas_pdf_vacunas <-
    c(horas_pdf_vacunas,
      as.character(format(pdf_vacunas_leido[[i]]$modified, "%H:%M:%S")))
}        
cat(paste0("Se han exportado páginas/fecha/hora de ",
           length(fechas_pdf_vacunas), " informes de vacunas de Sanidad\n"))
cat(paste0("Media de páginas: ", round(mean(paginas_pdf_vacunas), 2), "\n"))
cat(paste0("Mediana de páginas: ", round(median(paginas_pdf_vacunas), 2), "\n"))
cat(paste0("Hora media de última modificación: ",
           format(mean(as.POSIXct(horas_pdf_vacunas,
                                  format = "%H:%M:%S")), "%H:%M:%S\n")))
cat(paste0("Hora media de última modificación en el último mes: ",
           format(mean(as.POSIXct(horas_pdf_vacunas[fechas_pdf_vacunas >=
                                                      as.Date(Sys.time()) - 30],
                                  format = "%H:%M:%S")), "%H:%M:%S\n")))

