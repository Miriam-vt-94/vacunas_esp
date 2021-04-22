
# Panel por variables


panel_variables <- list()

# Dosis admin
panel_variables$dosis_admin <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$dosis_admin <-
    cbind(panel_variables$dosis_admin, panel_vacunas[[i]]$dosis_admin)
}
names(panel_variables$dosis_admin) <-
  c("fechas", names(panel_vacunas))
names(panel_variables$dosis_admin) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$dosis_admin,
          file = "./EXPORTADO/POR_VARIABLES/dosis_admin.csv",
          row.names = FALSE)

# Dosis admin por 100 hab
panel_variables$dosis_admin_100hab <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$dosis_admin_100hab <-
    cbind(panel_variables$dosis_admin_100hab,
          panel_vacunas[[i]]$dosis_admin_100hab)
}
names(panel_variables$dosis_admin_100hab) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$dosis_admin_100hab,
          file = "./EXPORTADO/POR_VARIABLES/dosis_admin_100hab.csv",
          row.names = FALSE)

# Dosis entregadas
panel_variables$dosis_entrega <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$dosis_entrega <-
    cbind(panel_variables$dosis_entrega,
          panel_vacunas[[i]]$dosis_entrega)
}
names(panel_variables$dosis_entrega) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$dosis_entrega,
          file = "./EXPORTADO/POR_VARIABLES/dosis_entrega.csv",
          row.names = FALSE)

# Dosis entregadas por 100 hab
panel_variables$dosis_entrega_100hab <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$dosis_entrega_100hab <-
    cbind(panel_variables$dosis_entrega_100hab,
          panel_vacunas[[i]]$dosis_entrega_100hab)
}
names(panel_variables$dosis_entrega_100hab) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$dosis_entrega_100hab,
          file = "./EXPORTADO/POR_VARIABLES/dosis_entrega_100hab.csv",
          row.names = FALSE)



# Dosis admin semanalmente por 100 hab
panel_variables$dosis_7D_admin_100hab <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$dosis_7D_admin_100hab <-
    cbind(panel_variables$dosis_7D_admin_100hab,
          panel_vacunas[[i]]$dosis_7D_admin_100hab)
}
names(panel_variables$dosis_7D_admin_100hab) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$dosis_7D_admin_100hab,
          file = "./EXPORTADO/POR_VARIABLES/dosis_7D_admin_100hab.csv",
          row.names = FALSE)


# Personas vacunadas con pauta completa
panel_variables$personas_pauta_completa <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$personas_pauta_completa <-
    cbind(panel_variables$personas_pauta_completa,
          panel_vacunas[[i]]$personas_pauta_completa)
}
names(panel_variables$personas_pauta_completa) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$personas_pauta_completa,
          file = "./EXPORTADO/POR_VARIABLES/personas_pauta_completa.csv",
          row.names = FALSE)



# % personas vacunadas con pauta completa
panel_variables$porc_personas_pauta_completa <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$porc_personas_pauta_completa <-
    cbind(panel_variables$porc_personas_pauta_completa,
          panel_vacunas[[i]]$porc_personas_pauta_completa)
}
names(panel_variables$porc_personas_pauta_completa) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$porc_personas_pauta_completa,
          file = "./EXPORTADO/POR_VARIABLES/porc_personas_pauta_completa.csv",
          row.names = FALSE)

# Personas vacunadas
panel_variables$personas_vacunadas <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$personas_vacunadas <-
    cbind(panel_variables$personas_vacunadas,
          panel_vacunas[[i]]$personas_vacunadas)
}
names(panel_variables$personas_vacunadas) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$personas_vacunadas,
          file = "./EXPORTADO/POR_VARIABLES/personas_vacunadas.csv",
          row.names = FALSE)


# % personas vacunadas
panel_variables$porc_personas_vacunadas <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$porc_personas_vacunadas <-
    cbind(panel_variables$porc_personas_vacunadas,
          panel_vacunas[[i]]$porc_personas_vacunadas)
}
names(panel_variables$porc_personas_vacunadas) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$porc_personas_vacunadas,
          file = "./EXPORTADO/POR_VARIABLES/porc_personas_vacunadas.csv",
          row.names = FALSE)

# Personas vacunadas
panel_variables$personas_1dosis <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$personas_1dosis <-
    cbind(panel_variables$personas_1dosis,
          panel_vacunas[[i]]$personas_1dosis)
}
names(panel_variables$personas_1dosis) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$personas_1dosis,
          file = "./EXPORTADO/POR_VARIABLES/personas_1dosis.csv",
          row.names = FALSE)

# % admin del total
panel_variables$porc_admin_sobre_ccaa <-
  data.frame("fechas" = panel_vacunas$ES$fechas)
for (i in 1:length(panel_vacunas)) {
  
  panel_variables$porc_admin_sobre_ccaa <-
    cbind(panel_variables$porc_admin_sobre_ccaa,
          panel_vacunas[[i]]$porc_admin_sobre_ccaa)
}
names(panel_variables$porc_admin_sobre_ccaa) <-
  c("fechas", names(panel_vacunas))
write.csv(panel_variables$porc_admin_sobre_ccaa,
          file = "./EXPORTADO/POR_VARIABLES/porc_admin_sobre_ccaa.csv",
          row.names = FALSE)

save(panel_variables, file = "./EXPORTADO/panel_variables.RData")


