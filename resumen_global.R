# ########################
# DATOS ESPAÑA
# ########################

panel_vacunas_global <-
  data.frame(matrix(as.numeric(unlist(lapply(panel_vacunas_fecha,
                                             FUN = function(x) { x[21, -c(1:8)] }))),
                               ncol = length(panel_vacunas_fecha)))
names(panel_vacunas_global) <- as.character(names(panel_vacunas_fecha))
row.names(panel_vacunas_global) <- names(panel_vacunas_fecha[[1]])[-c(1:8)]


# ################
# EXPORTACIÓN
##################

save(panel_vacunas_global, file = "./EXPORTADO/panel_vacunas_global.RData")
write.csv(panel_vacunas_global,
          file = paste0("./EXPORTADO/POR_CCAA/resumen_global.csv"))

