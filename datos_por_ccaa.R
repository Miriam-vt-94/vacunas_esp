# ########################
# DATOS POR CCAA
# ########################

panel_vacunas_ccaa <- list()
for (i in 1:(length(ccaa$NOMBRES) + 1)) {

  panel_vacunas_ccaa[[i]] <-
    data.frame(matrix(as.numeric(unlist(lapply(panel_vacunas_fecha,
                                      FUN = function(x, i) { x[i, -c(1:8)] }, i))),
                        ncol = length(panel_vacunas_fecha)))
  names(panel_vacunas_ccaa[[i]]) <- as.character(names(panel_vacunas_fecha))
  row.names(panel_vacunas_ccaa[[i]]) <-
    names(panel_vacunas_fecha[[1]])[-c(1:8)]
}
names(panel_vacunas_ccaa) <- c(as.character(ccaa$ISO), "FFAA")





# ################
# EXPORTACIÓN
##################

save(panel_vacunas_ccaa, file = "./EXPORTADO/panel_vacunas_ccaa.RData")
for (i in 1:length(panel_vacunas_ccaa)) {
  
  write.csv(panel_vacunas_ccaa[[i]],
            file = paste0("./EXPORTADO/POR_CCAA/datos_",
                          names(panel_vacunas_ccaa)[[i]], ".csv"))
  
}