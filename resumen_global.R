# ########################
# DATOS ESPAÑA
# ########################

panel_vacunas_global <-
  data.frame(matrix(as.numeric(unlist(lapply(panel_vacunas_fecha,
                                             FUN = function(x) { x[21, -c(1:8)] }))),
                               ncol = length(panel_vacunas_fecha)))
names(panel_vacunas_global) <- as.character(names(panel_vacunas_fecha))
row.names(panel_vacunas_global) <- names(panel_vacunas_fecha[[1]])[-c(1:8)]


# ########################
# RESUMEN ÚLTIMOS 7D/14D
# ########################
pauta_completa_7d <-
  c(rep(0, 7), diff(as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]), 7))
pauta_completa_14d <-
  c(rep(0, 14), diff(as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]), 14))
porc_pauta_completa_7d <-
  round(100 * pauta_completa_7d / rev(panel_vacunas_fecha[[1]]$poblacion)[1], 3)
porc_pauta_completa_14d <-
  round(100 * pauta_completa_14d / rev(panel_vacunas_fecha[[1]]$poblacion)[1], 3)
crec_pauta_completa_7d <-
  round(100 * (pauta_completa_7d[-(1:7)] /
                 rev(rev(pauta_completa_7d)[-(1:7)]) - 1), 3)
desv_pauta_completa_7d <- desv_crec_pauta_completa_7d <-
  rep(0, dim(panel_vacunas_global)[2])

panel_vacunas_global <-
  rbind(panel_vacunas_global, pauta_completa_7d, pauta_completa_14d,
        porc_pauta_completa_7d, porc_pauta_completa_14d,
        crec_pauta_completa_7d, desv_pauta_completa_7d)
row.names(panel_vacunas_global) <-
  c(names(panel_vacunas_fecha[[1]])[-c(1:8)], "PAUTA_COMPLETA_7D",
    "PAUTA_COMPLETA_14D", "PORC_PAUTA_COMPLETA_7D", "PORC_PAUTA_COMPLETA_14D",
    "CREC_PAUTA_COMPLETA_7D", "DESV_PAUTA_COMPLETA_7D")

# ################
# EXPORTACIÓN
##################

save(panel_vacunas_global, file = "./EXPORTADO/panel_vacunas_global.RData")
write.csv(panel_vacunas_global,
          file = paste0("./EXPORTADO/POR_CCAA/resumen_global.csv"))

