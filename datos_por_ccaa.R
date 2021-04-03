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




# ########################
# RESUMEN ÚLTIMOS 7D/14D
# ########################


for (i in 1:length(panel_vacunas_ccaa)) {
  
  # Pauta completa últimos 7d/14d
  pauta_completa_7d <-
    c(rep(0, 7), diff(as.numeric(panel_vacunas_ccaa[[i]]["PAUTA_COMPLETA", ]), 7))
  pauta_completa_14d <-
    c(rep(0, 14), diff(as.numeric(panel_vacunas_ccaa[[i]]["PAUTA_COMPLETA", ]), 14))
  
  # % con pauta completa últimos 7d/14d
  porc_pauta_completa_7d <- round(100 * pauta_completa_7d /
                                    panel_vacunas_fecha[[1]]$poblacion[i], 3)
  porc_pauta_completa_14d <- round(100 * pauta_completa_14d /
                                    panel_vacunas_fecha[[1]]$poblacion[i], 3)
  
  # Crecimiento % semanal de personas con pauta completa
  crec_pauta_completa_7d <-
    round(100 * (pauta_completa_7d[-(1:7)] /
                   rev(rev(pauta_completa_7d)[-(1:7)]) - 1), 3)
  
  # Desviación (% de exceso/defecto) de personas con pauta completa
  # últimos 7d respecto a media nacional
  desv_pauta_completa_7d <-
    round(100 * (pauta_completa_7d /
                   as.numeric(panel_vacunas_global["PAUTA_COMPLETA_7D", ]) - 1), 3)
    
  # Desviación (% de exceso/defecto) de crecimiento en personas con pauta completa
  # últimos 7d respecto a media nacional
  desv_crec_pauta_completa_7d <-
    round(100 * (crec_pauta_completa_7d /
                   as.numeric(panel_vacunas_global["CREC_PAUTA_COMPLETA_7D", ]) - 1), 3)
  
  panel_vacunas_ccaa[[i]] <-
    rbind(panel_vacunas_ccaa[[i]], pauta_completa_7d, pauta_completa_14d,
          porc_pauta_completa_7d, porc_pauta_completa_14d,
          crec_pauta_completa_7d, desv_pauta_completa_7d)
}
# ################
# EXPORTACIÓN
##################

save(panel_vacunas_ccaa, file = "./EXPORTADO/panel_vacunas_ccaa.RData")
for (i in 1:length(panel_vacunas_ccaa)) {
  
  write.csv(panel_vacunas_ccaa[[i]],
            file = paste0("./EXPORTADO/POR_CCAA/datos_",
                          names(panel_vacunas_ccaa)[[i]], ".csv"))
  
}