# ###########################
# POR FECHA
# ###########################
fechas <- panel_vacunas$ES$fechas
panel_vacunas_fecha <- list()
# Me muevo entre fechas
for (i in 1:length(fechas)) {
  
  panel_vacunas_fecha[[i]] <-
    data.frame("ccaa" = names(panel_vacunas))
  
  aux <- data.frame()
  # Para cada fecha fija, me muevo entre ccaa
  for (j in 1:length(names(panel_vacunas))) {
    
    aux <- rbind(aux, panel_vacunas[[j]][i, ])
    
  }
  panel_vacunas_fecha[[i]] <- cbind(panel_vacunas_fecha[[i]], aux)
} 
names(panel_vacunas_fecha) <- as.character(fechas)

# ################
# EXPORTACIÓN
##################

save(panel_vacunas_fecha, file = "./EXPORTADO/panel_vacunas_fecha.RData")
for (i in 1:length(panel_vacunas_fecha)) {
  
  write.csv(panel_vacunas_fecha[[i]],
            file = paste0("./EXPORTADO/POR_FECHAS/datos_",
                          names(panel_vacunas_fecha)[i], ".csv"),
            row.names = FALSE)
  
}




