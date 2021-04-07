# ###########################
# GRÁFICAS CREADAS:
#
# 1. Dosis entregadas acumuladas por farma [barras verticales]
#    --> fig_dosis_entregadas_vertical
# 2. Dosis entregadas diarias por farma [barras verticales]
#    --> fig_dosis_entregadas_diarias_vertical
# 3. Dosis entregadas acumuladas por farma [barras horizontales]
#    --> fig_dosis_entregadas_horizontal
# 4. Dosis administradas acumuladas [barras horizontales + forma rellena +
#    + línea tendencia]
#    --> fig_dosis_admin_vertical
# 5. Personas vacunadas acumuladas [barras horizontales + línea tendencia]
#    --> fig_vacunados
# 6. Dosis entregadas acumuladas por farma [diagrama de rosa]
#    --> fig_dosis_entregadas_rosa
# 7. Dosis admin (general vs pauta completa) [diagrama de rosa]
#    --> fig_dosis_admin_rosa
# 8. Personas vacunadas (1 dosis vs 2 dosis) [diagrama de rosa]
#    --> fig_personas_vacunadas_rosa
# ###########################

# #####################################################
# DOSIS ENTREGADAS POR MARCA ACUMULADAS (VERTICALES)
# #####################################################
fig_dosis_entregadas_vertical <- plot_ly()

# Dosis entregadas de Pfizer
fig_dosis_entregadas_vertical <-
  fig_dosis_entregadas_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_PFIZER", ]),
            type = "bar", name = "Pfizer/Biontech",
            marker = list(color = "rgba(96, 178, 232, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis acum. entregadas de Pfizer",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de AstraZeneca
fig_dosis_entregadas_vertical <-
  fig_dosis_entregadas_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_ASTRA_ZENECA", ]),
            type = "bar", name = "AstraZeneca/Oxford",
            marker = list(color = "rgba(233, 105, 128, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis acum. entregadas de AstraZeneca",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Moderna
fig_dosis_entregadas_vertical <- fig_dosis_entregadas_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_MODERNA", ]),
            type = "bar", name = "Moderna",
            marker = list(color = "rgba(78, 210, 172, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis acum. entregadas de Moderna",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis admin
fig_dosis_entregadas_vertical <- fig_dosis_entregadas_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]),
            type = "scatter", mode = "lines",
            name = "Dosis administradas",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 4), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. acum.: %{y}",
                     "</b><extra></extra><br>",
                     "<b>Totales entregadas</b>: ",
                     panel_vacunas_global["DOSIS_ENTREGADAS", ]))

# Ajustes globales de la gráfica
fig_dosis_entregadas_vertical <- fig_dosis_entregadas_vertical %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis entregadas (acumuladas) en España (",
                  as.character(format(max(as.Date(names(panel_vacunas_global))),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a>"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         # Formato eje Y
         yaxis =
           list(title = "Dosis", font = list(size = 15),
                # Números completos, no abreviados tipo 1M
                tickformat = ".0f"),
         # Formato eje X: título y le indicamos que es formato fecha
         xaxis =
           list(title = "Fechas", type = "date", tickformat = "%d-%m-%y",
                font = list(size = 15), nticks = 40),
         # Barras apiladas (separación de 0.1 entre fechas)
         barmode = "stack", bargap = 0.1,
         # Tamaño texto info HTML al pasar el ratón
         hoverlabel = list(font = list(size = 11)),
         # Unificamos info html en el mismo x
         hovermode = "x unified", 
         autosize = TRUE,
         margin = # Margen
           list(l = 100, r = 80, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_dosis_entregadas_vertical,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_dosis_entregadas_acum_vertical.html"),
                        selfcontained = TRUE)

# #####################################################
# DOSIS ENTREGADAS POR MARCA DIARIAS (VERTICALES)
# #####################################################
fig_dosis_entregadas_diarias_vertical <- plot_ly()

# Eliminamos los días sin entregas
idx_sin_entrega <- 
  which(as.numeric(panel_vacunas_global["DOSIS_DIARIAS_ENTREGADAS", ]) == 0) 

# Dosis entregadas de Pfizer
fig_dosis_entregadas_diarias_vertical <-
  fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = names(panel_vacunas_global)[-idx_sin_entrega],
            y =
              as.numeric(panel_vacunas_global["DOSIS_DIARIAS_ENTREGADAS_PFIZER",
                                              -idx_sin_entrega]),
            type = "bar", name = "Pfizer/Biontech",
            marker = list(color = "rgba(96, 178, 232, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Pfizer",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de AstraZeneca
fig_dosis_entregadas_diarias_vertical <-
  fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = names(panel_vacunas_global)[-idx_sin_entrega],
            y =
              as.numeric(panel_vacunas_global["DOSIS_DIARIAS_ENTREGADAS_ASTRA_ZENECA",
                                              -idx_sin_entrega]),
            type = "bar", name = "AstraZeneca/Oxford",
            marker = list(color = "rgba(233, 105, 128, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de AstraZeneca",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Moderna
fig_dosis_entregadas_diarias_vertical <- fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = names(panel_vacunas_global)[-idx_sin_entrega],
            y =
              as.numeric(panel_vacunas_global["DOSIS_DIARIAS_ENTREGADAS_MODERNA",
                                              -idx_sin_entrega]),
            type = "bar", name = "Moderna",
            marker = list(color = "rgba(78, 210, 172, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Moderna",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis admin
fig_dosis_entregadas_diarias_vertical <- fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = names(panel_vacunas_global)[-idx_sin_entrega],
            y =
              as.numeric(panel_vacunas_global["DOSIS_ADMIN_DIARIAS",
                                              -idx_sin_entrega]),
            type = "scatter", mode = "lines",
            name = "Dosis administradas",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 4), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. (diarias): %{y}",
                     "</b><extra></extra><br>",
                     "<b>Entregadas (diarias)</b>: ",
                     panel_vacunas_global["DOSIS_DIARIAS_ENTREGADAS",
                                          -idx_sin_entrega]))

# Ajustes globales de la gráfica
fig_dosis_entregadas_diarias_vertical <- fig_dosis_entregadas_diarias_vertical %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis entregadas (diarias) en España (",
                  as.character(format(max(as.Date(names(panel_vacunas_global))),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a> ",
                  "(solo figuran los dosis con alguna entrega)"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         # Formato eje Y
         yaxis =
           list(title = "Dosis", font = list(size = 15),
                # Números completos, no abreviados tipo 1M
                tickformat = ".0f"),
         # Formato eje X: título y le indicamos que es formato fecha
         xaxis = list(title = "Fechas", font = list(size = 15), nticks = 40),
         # Barras apiladas (separación de 0.1 entre fechas)
         barmode = "stack", bargap = 0.1,
         # Tamaño texto info HTML al pasar el ratón
         hoverlabel = list(font = list(size = 11)),
         # Unificamos info html en el mismo x
         hovermode = "x unified", 
         autosize = TRUE,
         margin = # Margen
           list(l = 100, r = 80, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_dosis_entregadas_diarias_vertical,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_dosis_entregadas_diarias_vertical.html"),
                        selfcontained = TRUE)

# ###########################################
# DOSIS ENTREGADAS POR MARCA (HORIZONTALES)
# ###########################################
fig_dosis_entregadas_horizontal <- plot_ly()

# No pintamos todos, solo unos pocos
idx_plot <- 
  which(names(panel_vacunas_global) %in% names(pdf_bruto))
fechas_plot <- names(panel_vacunas_global)[idx_plot]
idx_quitar <-
  which(fechas_plot %in%
          c("2021-01-07", "2021-01-08", "2021-01-15",
            "2021-01-16", "2021-01-21", "2021-01-22",
            "2021-01-28", "2021-01-29", "2021-02-04",
            "2021-02-05","2021-02-11", "2021-02-12",
            "2021-02-18", "2021-02-19", "2021-02-25",
            "2021-02-26", "2021-03-04", "2021-03-05",
            "2021-03-11", "2021-03-12", "2021-03-18",
            "2021-03-26", "2021-03-29"))
fechas <- 
  as.character(format(as.Date(names(panel_vacunas_global)[idx_plot][-idx_quitar]), "%d-%m-%Y"))

# Dosis entregadas de Pfizer
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  add_trace(y = fechas,
            x =
              as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_PFIZER",
                                              idx_plot][-idx_quitar]),
            type = "bar", name = "Pfizer/Biontech",
            marker = list(color = "rgba(96, 178, 232, 1)"),
            hovertemplate = 
              paste0("<b>Dosis entregadas de Pfizer",
                     "</b><extra></extra><br>",
                     "a fecha de %{y}: %{x}"))

# Dosis entregadas de AstraZeneca
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  add_trace(y = fechas,
            x = as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_ASTRA_ZENECA",
                                                idx_plot][-idx_quitar]),
            type = "bar", name = "AstraZeneca/Oxford",
            marker = list(color = "rgba(233, 105, 128, 1)"),
            hovertemplate =
              paste0("<b>Dosis entregadas de AstraZeneca",
                     "</b><extra></extra><br>",
                     "a fecha de %{y}: %{x}"))

# Dosis entregadas de Moderna
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  add_trace(y = fechas,
            x = as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_MODERNA",
                                                idx_plot][-idx_quitar]),
            type = "bar", name = "Moderna",
            marker = list(color = "rgba(78, 210, 172, 1)"),
            hovertemplate =
              paste0("<b>Dosis entregadas de Moderna",
                     "</b><extra></extra><br>",
                     "a fecha de %{y}: %{x}"))

# Dosis admin
fig_dosis_entregadas_horizontal <- fig_dosis_entregadas_horizontal %>%
  add_trace(y = fechas,
            x = as.numeric(panel_vacunas_global["DOSIS_ADMIN", idx_plot][-idx_quitar]),
            type = "scatter", mode = "lines",
            name = "Dosis administradas",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 4), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis administradas: %{x}",
                     "</b><extra></extra><br>",
                     "<b>Totales entregadas</b>: ",
                     panel_vacunas_global["DOSIS_ENTREGADAS",
                                          idx_plot][-idx_quitar]))

# Ajustes globales de la gráfica
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis entregadas (acumuladas) en España (",
                  as.character(format(max(as.Date(fechas, "%d-%m-%Y")),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a>"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         # Formato eje Y
         xaxis =
           list(title = "Dosis", font = list(size = 15),
                tickformat = ".0f"),
         # Formato eje Y
         yaxis = # Ponemos título y forzamos a que el orden sea el dado (por fecha)
           list(title = "Fechas", categoryarray = fechas, categoryorder = "array",
                font = list(size = 15),
                # Número de «marcas» en el eje
                nticks = 15),
         # Barras apiladas (separación de 0.1 entre fechas)
         barmode = "stack", bargap = 0.1,
         hoverlabel = list(font = list(size = 11)),
         # Unificamos las cajas de info HTML para la misma altura
         hovermode = "y unified",
         autosize = TRUE,
         margin = # Margen
           list(l = 100, r = 80, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_dosis_entregadas_horizontal,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_dosis_entregadas_horizontal.html"),
                        selfcontained = TRUE)



# #####################################################
# DOSIS ADMINISTRADAS ACUMULADAS (VERTICALES)
# #####################################################
fig_dosis_admin_vertical <- plot_ly(type = "bar")


# Dosis entregadas
fig_dosis_admin_vertical <-
  fig_dosis_admin_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS", ]),
            type = "scatter", mode = "lines", name = "Dosis entregadas",
            fill = "tozeroy", fillcolor = "rgba(31, 31, 31, 0.3)",
            line = list(color = "rgba(31, 31, 31, 0.85)",
                        shape = "spline", width = 3),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis entregadas acum.",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}<br>",
                     "<b>% dosis admin</b>: ",
                     as.numeric(panel_vacunas_global["PORC_ADMIN_SOBRE_CCAA", ]),
                     "%"))

# Dosis por 100 hab
fig_dosis_admin_vertical <-
  fig_dosis_admin_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ADMIN_100HAB", ]),
            type = "scatter", mode = "lines",
            name = "Dosis admin. por 100 hab.",
            line = list(color = "rgba(49, 160, 35, 1)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. por 100 hab.",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")

# Dosis administradas para pauta completa
fig_dosis_admin_vertical <-
  fig_dosis_admin_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = 2 * as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]),
            type = "bar", name = "Dosis admin. (pauta completa)",
            marker = list(color = "rgba(170, 81, 217, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. acum. (pauta completa)",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))


# Dosis administradas totales
fig_dosis_admin_vertical <-
  fig_dosis_admin_vertical %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]) -
                 2 * as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]),
            type = "bar", name = "Dosis admin.",
            marker = list(color = "rgba(234, 140, 115, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. acum.",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: ",
                     as.numeric(panel_vacunas_global["DOSIS_ADMIN", ])))


# Ajustes globales de la gráfica
fig_dosis_admin_vertical <- fig_dosis_admin_vertical %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Dosis administradas", orientation = "v",
                xref = "paper", yref = "paper", x = 0.01, y = 1,
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis administradas (acumuladas) en España (",
                  as.character(format(max(as.Date(names(panel_vacunas_global))),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a><br>(se imputa ",
                  "como 0 los días sin informe de Sanidad)"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         # Formato eje Y
         yaxis = list(title = "Dosis", font = list(size = 15),
                      # Números completos, no abreviados tipo 1M
                      tickformat = ".0f"),
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "Dosis por 100 habitantes",
                       font = list(size = 15), ticksuffix = "/100 hab."),
         # Formato eje X: título y le indicamos que es formato fecha
         xaxis =
           list(title = "Fechas", type = "date", tickformat = "%d-%m-%y",
                font = list(size = 15), nticks = 40),
         # Barras apiladas (separación de 0.1 entre fechas)
         barmode = "stack", bargap = 0.1,
         # Tamaño texto info HTML al pasar el ratón
         hoverlabel = list(font = list(size = 11)),
         # Unificamos info html en el mismo x
         hovermode = "x unified", 
         autosize = TRUE,
         margin = # Margen
           list(l = 100, r = 100, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_dosis_admin_vertical,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_dosis_admin_acum_vertical.html"),
                        selfcontained = TRUE)


# #####################################################
# PERSONAS VACUNADAS ACUMULADAS
# #####################################################

# Calculamos la previsión necesaria para cumplir fechas
vacunados <-
  as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]) -
  as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ])
inmunizados <- as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ])

# 5 millones de dosis
fecha_5M <- as.Date("2021-05-03")
idx_desde_prevision <- length(names(panel_vacunas_global)) - 6
fecha_desde_prevision <-
  max(as.Date(names(panel_vacunas_global))[1:idx_desde_prevision])
dias_5M <- fecha_5M - fecha_desde_prevision
inmunizados_5M <- 5e6 - max(inmunizados[1:idx_desde_prevision])
inmunizados_dia_5M <- inmunizados_5M / as.numeric(dias_5M)
fechas_hasta_prevision_5M <- max(as.Date(names(panel_vacunas_global))) + 8
inmunizados_prevision_5M <-
  c(inmunizados[1:idx_desde_prevision],
    inmunizados[idx_desde_prevision + 1] +
      cumsum(rep(inmunizados_dia_5M,
                 as.numeric(fechas_hasta_prevision_5M - fecha_desde_prevision))))
fechas_prevision_5M <-
  as.character(seq(min(as.Date(names(panel_vacunas_global))),
                   fechas_hasta_prevision_5M, by = 1))

fig_vacunados <- plot_ly(type = "bar")

# % población vacunada
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = names(panel_vacunas_global),
            y = round(100 * vacunados / sum(poblacion$poblacion), 3),
            type = "scatter", mode = "lines",
            name = "% población total vacunada",
            line = list(color = "rgba(49, 160, 35, 1)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población vacunada",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")
# % población  > 18 a vacunada
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = names(panel_vacunas_global),
            y = round(100 * vacunados / sum(poblacion$poblacion_mayor_18a), 3),
            type = "scatter", mode = "lines",
            name = "% población vacunada \u2265 18a",
            line = list(color = "rgba(49, 160, 35, 0.5)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población vacunada \u2265 18 años",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")
# % población pauta completa
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = names(panel_vacunas_global),
            y = round(100 * as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]) /
                        sum(poblacion$poblacion), 3),
            type = "scatter", mode = "lines",
            name = "% población pauta completa",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población pauta completa",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")
# % población pauta completa >= 18 años
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = names(panel_vacunas_global),
            y = round(100 * as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]) /
                        sum(poblacion$poblacion_mayor_18a), 3),
            type = "scatter", mode = "lines",
            name = "% población pauta completa \u2265 18a",
            line = list(color = "rgba(31, 31, 31, 0.5)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población pauta completa \u2265 18 años",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")

# Vacunadas con pauta completa
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]),
            type = "bar", name = "Vacunados (pauta completa)",
            marker = list(color = "rgba(170, 81, 217, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<br><b>Vacunados (pauta completa): %{y}",
                     "<extra></extra><br>"))
# Prevision
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = fechas_prevision_5M[idx_desde_prevision:length(fechas_prevision_5M)],
            y =
              inmunizados_prevision_5M[idx_desde_prevision:length(inmunizados_prevision_5M)] -
              c(inmunizados[idx_desde_prevision:length(inmunizados)],
                rep(0, length(fechas_prevision_5M) - length(inmunizados))),
            type = "bar",  name = "Inmunizados necesarios para 5M (3 de mayo)",
            marker = list(color = "rgba(17, 17, 17, 0.3)"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Vacunados (pauta completa) necesarios</b>",
                     "<extra></extra><br>",
                     "a %{x} para 5M inmun. (3 de mayo): ",
                     floor(inmunizados_prevision_5M[-(1:(idx_desde_prevision - 1))])))

# Vacunados totales
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = names(panel_vacunas_global),
            y = as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]) -
              2 * as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]),
            type = "bar", name = "Vacunados (1 dosis)",
            marker = list(color = "rgba(234, 140, 115, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Vacunados (1 dosis): ",
                     as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]) -
                       2 * as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]),
                     "<extra></extra><br>Vacunados totales: ",
                     as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]) -
                       as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ])))


# Ajustes globales de la gráfica
fig_vacunados <- fig_vacunados %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Personas vacunadas acum.", orientation = "v",
                xref = "paper", yref = "paper", x = 0.01, y = 1,
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Personas vacunadas (acumuladas) en España (",
                  as.character(format(max(as.Date(names(panel_vacunas_global))),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a><br>(se imputa ",
                  "como 0 los días sin informe de Sanidad)"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         # Formato eje Y
         yaxis = list(title = "Personas", font = list(size = 15),
                      # Números completos, no abreviados tipo 1M
                      tickformat = ".0f"),
         yaxis2 = list(overlaying = "y", side = "right",
                       title = "% población",
                       font = list(size = 15), ticksuffix = "%"),
         # Formato eje X: título y le indicamos que es formato fecha
         xaxis =
           list(title = "Fechas", type = "date", tickformat = "%d-%m-%y",
                font = list(size = 15), nticks = 40),
         # Barras apiladas (separación de 0.1 entre fechas)
         barmode = "stack", bargap = 0.1,
         # Tamaño texto info HTML al pasar el ratón
         hoverlabel = list(font = list(size = 11)),
         # Unificamos info html en el mismo x
         hovermode = "x unified", 
         autosize = TRUE,
         margin = # Margen
           list(l = 100, r = 100, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_vacunados,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_vacunados_acum.html"),
                        selfcontained = TRUE)


# #############################################
# GRÁFICO DE ROSA: DOSIS ENTREGADAS ACUM.
# #############################################

# Calculamos las dosis entregadas por semana
dosis_semana <-
  data.frame("fechas" = names(panel_vacunas_global),
             "semana" = week(names(panel_vacunas_global)),
             "dosis_pfizer" = 
               as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_PFIZER", ]),
             "dosis_astra" = 
               as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_ASTRA_ZENECA", ]),
             "dosis_moderna" = 
               as.numeric(panel_vacunas_global["DOSIS_ENTREGADAS_MODERNA", ]))
# Agrupamos por semana
aux <- dosis_semana %>% group_by(semana) %>%
  summarise(dosis_pfizer = max(dosis_pfizer))
aux <- cbind(aux, (dosis_semana %>% group_by(semana) %>%
                     summarise(dosis_astra = max(dosis_astra)))$dosis_astra)
aux <- cbind(aux, (dosis_semana %>% group_by(semana) %>%
                     summarise(dosis_moderna =
                                 max(dosis_moderna)))$dosis_moderna)
# Guardamos y renombramos columnas
dosis_semana <- aux
names(dosis_semana) <-
  c("semana", "dosis_pfizer", "dosis_astra", "dosis_moderna")

# Figura como diagrama polar
fig_dosis_entregadas_rosa <-
  plot_ly(type = "scatterpolar", mode = "lines+text")

# Dosis entregadas de AstraZeneca
# (puesto como suma de las 3, como última capa)
fig_dosis_entregadas_rosa <- fig_dosis_entregadas_rosa %>%
  add_trace(t = dosis_semana$semana,
            r = dosis_semana$dosis_astra + dosis_semana$dosis_moderna +
              dosis_semana$dosis_pfizer,
            theta = paste0("Semana ", dosis_semana$semana),
            fill = "toself", fillcolor = "rgba(233, 105, 128, 0.75)",
            line = list(color = "rgba(158, 36, 58, 0.85)"),
            name = "AstraZeneca/Oxford",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ", dosis_semana$semana,
                     "<extra></extra><br>",
                     "<b>Dosis entregadas de AstraZeneca</b>: %{r}"))

# Dosis Moderna
fig_dosis_entregadas_rosa <- fig_dosis_entregadas_rosa %>%
  add_trace(t = dosis_semana$semana,
            r = dosis_semana$dosis_moderna + dosis_semana$dosis_pfizer,
            theta = paste0("Semana ", dosis_semana$semana),
            fill = "toself", fillcolor = "rgba(78, 210, 172, 0.75)",
            line = list(color = "rgba(27, 143, 109, 0.85)"),
            name = "Moderna",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ", dosis_semana$semana,
                     "<extra></extra><br>",
                     "<b>Dosis entregadas de Moderna</b>: %{r}"))

# Pfizer
fig_dosis_entregadas_rosa <- fig_dosis_entregadas_rosa %>%
  add_trace(t = dosis_semana$semana,
            r = dosis_semana$dosis_pfizer,
            theta = paste0("Semana ", dosis_semana$semana),
            fill = "toself", fillcolor = "rgba(96, 178, 232, 0.75)",
            line = list(color = "rgba(32, 105, 154, 0.85)"),
            name = "Pfizer/Biontech",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ", dosis_semana$semana,
                     "<extra></extra><br>",
                     "<b>Dosis entregadas de Pfizer</b>: %{r}"))


fig_dosis_entregadas_rosa <- fig_dosis_entregadas_rosa %>%
  layout(polar = # Radios visibles
           list(radialaxis = list(visible = TRUE, font = list(size = 13),
                                  hoverformat = ".0f")),
         separators = ". ", showlegend = TRUE,
         legend = # Leyenda 
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Gráfico Nightingale Rose ",
                  "de dosis entregadas (acumuladas) en España (",
                  as.character(format(max(as.Date(fechas, "%d-%m-%Y")),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a>"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         hoverlabel = list(font = list(size = 12)), autosize = TRUE,
         margin = # Margen
           list(l = 50, r = 50, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_dosis_entregadas_rosa,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_dosis_entregadas_rosa.html"),
                        selfcontained = TRUE)



# #############################################
# GRÁFICO DE ROSA: DOSIS ADMIN ACUM.
# #############################################

# Calculamos las dosis administradas acum. por semana
dosis_admin_semana <-
  data.frame("fechas" = names(panel_vacunas_global),
             "semana" = week(names(panel_vacunas_global)),
             "admin" = 
               as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]),
             "pauta_completa" = 
               2 * as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]))
# Agrupamos por semana
aux <- dosis_admin_semana %>% group_by(semana) %>%
  summarise(admin = max(admin))
aux <- cbind(aux, (dosis_admin_semana %>% group_by(semana) %>%
                     summarise(pauta_completa =
                                 max(pauta_completa)))$pauta_completa)
# Guardamos y renombramos columnas
dosis_admin_semana <- aux
names(dosis_admin_semana) <-
  c("semana", "admin", "pauta_completa")

# Figura como diagrama polar
fig_dosis_admin_rosa <-
  plot_ly(type = "scatterpolar", mode = "lines+text")

# Dosis admin
fig_dosis_admin_rosa <- fig_dosis_admin_rosa %>%
  add_trace(t = dosis_admin_semana$semana,
            r = dosis_admin_semana$admin,
            theta = paste0("Semana ", dosis_admin_semana$semana),
            fill = "toself", fillcolor = "rgba(31, 31, 31, 0.75)",
            line = list(color = "rgba(31, 31, 31, 1)"),
            name = "Dosis admin. (acum.)",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ", dosis_admin_semana$semana,
                     "<extra></extra><br>",
                     "<b>Dosis admin. (acum.)</b>: %{r}"))

# Dosis pauta completa
fig_dosis_admin_rosa <- fig_dosis_admin_rosa %>%
  add_trace(t = dosis_admin_semana$semana,
            r = dosis_admin_semana$pauta_completa,
            theta = paste0("Semana ", dosis_admin_semana$semana),
            fill = "toself", fillcolor = "rgba(235, 112, 62, 0.75)",
            line = list(color = "rgba(235, 112, 62, 1)"),
            name = "Dosis pauta completa (acum.)",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ", dosis_admin_semana$semana,
                     "<extra></extra><br>",
                     "<b>Dosis a pauta completa (acum.)</b>: %{r}"))


fig_dosis_admin_rosa <- fig_dosis_admin_rosa %>%
  layout(polar = # Radios visibles
           list(radialaxis = list(visible = TRUE, font = list(size = 13),
                                  hoverformat = ".0f")),
         separators = ". ", showlegend = TRUE,
         legend = # Leyenda 
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Gráfico Nightingale Rose ",
                  "de dosis administradas (acumuladas) en España (",
                  as.character(format(max(as.Date(fechas, "%d-%m-%Y")),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a>"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         hoverlabel = list(font = list(size = 12)), autosize = TRUE,
         margin = # Margen
           list(l = 50, r = 50, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_dosis_admin_rosa,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_dosis_admin_rosa.html"),
                        selfcontained = TRUE)

# #############################################
# GRÁFICO DE ROSA: PERSONAS VACUNADAS ACUM.
# #############################################

# Calculamos vacunados (acumulados) por semana
personas_vacunadas_semana <-
  data.frame("fechas" = names(panel_vacunas_global),
             "semana" = week(names(panel_vacunas_global)),
             "personas_vacunadas" = 
               as.numeric(panel_vacunas_global["DOSIS_ADMIN", ]) -
               as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]),
             "pauta_completa" = 
               as.numeric(panel_vacunas_global["PAUTA_COMPLETA", ]))
# Agrupamos por semana
aux <- personas_vacunadas_semana %>% group_by(semana) %>%
  summarise(personas_vacunadas = max(personas_vacunadas))
aux <- cbind(aux, (personas_vacunadas_semana %>% group_by(semana) %>%
                     summarise(pauta_completa =
                                 max(pauta_completa)))$pauta_completa)

# Guardamos y renombramos columnas
personas_vacunadas_semana <- aux
names(personas_vacunadas_semana) <-
  c("semana", "personas_vacunadas", "pauta_completa")

# Figura como diagrama polar
fig_personas_vacunadas_rosa <-
  plot_ly(type = "scatterpolar", mode = "lines+text")

# Personas con al menos una dosis
fig_personas_vacunadas_rosa <- fig_personas_vacunadas_rosa %>%
  add_trace(t = personas_vacunadas_semana$semana,
            r = personas_vacunadas_semana$personas_vacunadas,
            theta = paste0("Semana ", personas_vacunadas_semana$semana),
            fill = "toself", fillcolor = "rgba(31, 31, 31, 0.75)",
            line = list(color = "rgba(31, 31, 31, 1)"),
            name = "Personas vacunadas",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ",
                     personas_vacunadas_semana$semana,
                     "<extra></extra><br>",
                     "<b>Personas vacunadas acum. </b> (alguna dosis): %{r}"))

# Personas con pauta completa
fig_personas_vacunadas_rosa <- fig_personas_vacunadas_rosa %>%
  add_trace(t = personas_vacunadas_semana$semana,
            r = personas_vacunadas_semana$pauta_completa,
            theta = paste0("Semana ", personas_vacunadas_semana$semana),
            fill = "toself", fillcolor = "rgba(235, 112, 62, 0.75)",
            line = list(color = "rgba(235, 112, 62, 1)"),
            name = "Personas con pauta completa",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ",
                     personas_vacunadas_semana$semana,
                     "<extra></extra><br>",
                     "<b>Personas con pauta completa (acum.)</b>: %{r}"))

# Ajustes generales
fig_personas_vacunadas_rosa <- fig_personas_vacunadas_rosa %>%
  layout(polar = # Radios visibles
           list(radialaxis = list(visible = TRUE, font = list(size = 13),
                                  hoverformat = ".0f")),
         separators = ". ", showlegend = TRUE,
         legend = # Leyenda 
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Gráfico Nightingale Rose ",
                  "de personas vacunadas (acumuladas) en España (",
                  as.character(format(max(as.Date(fechas, "%d-%m-%Y")),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a>"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         hoverlabel = list(font = list(size = 12)), autosize = TRUE,
         margin = # Margen
           list(l = 50, r = 50, b = 50, t = 80, pad = 3)) %>%
  config(locale = "es", showLink = TRUE, displayModeBar = TRUE)

# Guardamos
htmlwidgets::saveWidget(fig_personas_vacunadas_rosa,
                        file = paste0("./GRAFICAS_HTML/",
                                      "fig_personas_vacunadas_rosa.html"),
                        selfcontained = TRUE)

