# ######################################################
# GRÁFICAS CREADAS:
#
# 1. Dosis entregadas acumuladas por farma [barras verticales]
#    --> fig_dosis_entregadas_vertical
# 2. Dosis entregadas diarias por farma [barras verticales]
#    --> fig_dosis_entregadas_diarias_vertical
# 3. Dosis entregadas diarias por farma [barras verticales] sin huecos
#    --> fig_dosis_entregadas_diarias_vertical_sin_huecos
# 4. Dosis entregadas acumuladas por farma [barras horizontales]
#    --> fig_dosis_entregadas_horizontal
#
# 5. Dosis administradas acumuladas [barras verticales + forma rellena +
#    + línea tendencia]
#    --> fig_dosis_admin_acum
# 6. Dosis administradas diarias [barras verticales + forma rellena +
#    + línea tendencia]
#    --> fig_dosis_admin_diarias
#
# 7. Personas vacunadas acumuladas [barras verticales + línea tendencia]
#    --> fig_vacunados
#
# 8. Dosis entregadas acumuladas por farma [diagrama de rosa]
#    --> fig_dosis_entregadas_rosa
# 9. Dosis admin (general vs pauta completa) [diagrama de rosa]
#    --> fig_dosis_admin_rosa
# 10. Personas vacunadas (1 dosis vs 2 dosis) [diagrama de rosa]
#    --> fig_personas_vacunadas_rosa
#
# 11. Dosis entregadas acum. por farma [gráfico de gofre + animación]
#    --> fig_waffle_dosis_entregadas[[i]] para la fecha i
# 12. Dosis administradas acum. por hab. [gráfico de gofre + animación]
#    --> fig_waffle_dosis_admin[[i]] para la fecha i
# 13. Personas vacunadas acum. [gráfico de gofre + animación]
#    --> fig_waffle_personas_vacunadas[[i]] para la fecha i
#
# 14. Mapa relleno [mapa ggplot2]
# ######################################################

# #####################################################
# 1. DOSIS ENTREGADAS POR MARCA ACUMULADAS (VERTICALES)
# #####################################################
fig_dosis_entregadas_vertical <- plot_ly()

# Dosis entregadas de Pfizer
fig_dosis_entregadas_vertical <-
  fig_dosis_entregadas_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_entrega_pfizer,
            type = "bar", name = "Pfizer/Biontech",
            marker = list(color = "rgba(96, 178, 232, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis acum. entregadas de Pfizer",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de AstraZeneca
fig_dosis_entregadas_vertical <-
  fig_dosis_entregadas_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_entrega_astra,
            type = "bar", name = "AstraZeneca/Oxford",
            marker = list(color = "rgba(233, 105, 128, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis acum. entregadas de AstraZeneca",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Moderna
fig_dosis_entregadas_vertical <- fig_dosis_entregadas_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_entrega_moderna,
            type = "bar", name = "Moderna",
            marker = list(color = "rgba(78, 210, 172, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis acum. entregadas de Moderna",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Janssen
fig_dosis_entregadas_vertical <- fig_dosis_entregadas_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_entrega_janssen,
            type = "bar", name = "Janssen",
            marker = list(color = "rgba(245, 228, 57, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis acum. entregadas de Janssen",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis admin
fig_dosis_entregadas_vertical <- fig_dosis_entregadas_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_admin,
            type = "scatter", mode = "lines",
            name = "Dosis administradas",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 4), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. acum.: %{y}",
                     "</b><extra></extra><br>",
                     "<b>Totales entregadas</b>: ",
                     panel_vacunas$ES$dosis_entrega))

# Ajustes globales de la gráfica
fig_dosis_entregadas_vertical <- fig_dosis_entregadas_vertical %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis entregadas (acumuladas) en España (",
                  as.character(format(max(as.Date(panel_vacunas$ES$fechas)),
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
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_entregadas_acum_vertical.html"),
                        selfcontained = TRUE)

# #####################################################
# 2. DOSIS ENTREGADAS POR MARCA DIARIAS (VERTICALES)
# #####################################################
fig_dosis_entregadas_diarias_vertical <- plot_ly()

# # Eliminamos los días sin entregas
# idx_sin_entrega <- which()
#   which(as.numeric(panel_vacunas_global["DOSIS_DIARIAS_ENTREGADAS", ]) == 0) 

# Dosis entregadas de Pfizer
fig_dosis_entregadas_diarias_vertical <-
  fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_diarias_entrega_pfizer,
            type = "bar", name = "Pfizer/Biontech",
            marker = list(color = "rgba(96, 178, 232, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Pfizer",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de AstraZeneca
fig_dosis_entregadas_diarias_vertical <-
  fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_diarias_entrega_astra,
            type = "bar", name = "AstraZeneca/Oxford",
            marker = list(color = "rgba(233, 105, 128, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de AstraZeneca",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Moderna
fig_dosis_entregadas_diarias_vertical <- fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_diarias_entrega_moderna,
            type = "bar", name = "Moderna",
            marker = list(color = "rgba(78, 210, 172, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Moderna",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Janssen
fig_dosis_entregadas_diarias_vertical <- fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_diarias_entrega_janssen,
            type = "bar", name = "Janssen",
            marker = list(color = "rgba(245, 228, 57, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Janssen",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis admin
fig_dosis_entregadas_diarias_vertical <- fig_dosis_entregadas_diarias_vertical %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_diarias_admin,
            type = "scatter", mode = "lines",
            name = "Dosis administradas",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 4), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. (diarias): %{y}",
                     "</b><extra></extra><br>",
                     "<b>Entregadas (diarias)</b>: ",
                     panel_vacunas$ES$dosis_diarias_entrega))

# Ajustes globales de la gráfica
fig_dosis_entregadas_diarias_vertical <- fig_dosis_entregadas_diarias_vertical %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis entregadas (diarias) en España (",
                  as.character(format(max(as.Date(panel_vacunas$ES$fechas)),
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
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_entregadas_diarias_vertical.html"),
                        selfcontained = TRUE)


# #####################################################
# 3. DOSIS ENTREGADAS POR MARCA DIARIAS (VERTICALES) SIN HUECOS
# #####################################################
fig_dosis_entregadas_diarias_vertical_sin_huecos <- plot_ly()

# # Eliminamos los días sin entregas
idx_sin_entrega <- 
  which(as.numeric(panel_vacunas$ES$dosis_diarias_entrega) == 0) 

# Dosis entregadas de Pfizer
fig_dosis_entregadas_diarias_vertical_sin_huecos <-
  fig_dosis_entregadas_diarias_vertical_sin_huecos %>%
  add_trace(x = as.character(panel_vacunas$ES$fechas[-idx_sin_entrega]),
            y = panel_vacunas$ES$dosis_diarias_entrega_pfizer[-idx_sin_entrega],
            type = "bar", name = "Pfizer/Biontech",
            marker = list(color = "rgba(96, 178, 232, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Pfizer",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de AstraZeneca
fig_dosis_entregadas_diarias_vertical_sin_huecos <-
  fig_dosis_entregadas_diarias_vertical_sin_huecos %>%
  add_trace(x = as.character(panel_vacunas$ES$fechas[-idx_sin_entrega]),
            y = panel_vacunas$ES$dosis_diarias_entrega_astra[-idx_sin_entrega],
            type = "bar", name = "AstraZeneca/Oxford",
            marker = list(color = "rgba(233, 105, 128, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de AstraZeneca",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Moderna
fig_dosis_entregadas_diarias_vertical_sin_huecos <-
  fig_dosis_entregadas_diarias_vertical_sin_huecos %>%
  add_trace(x = as.character(panel_vacunas$ES$fechas[-idx_sin_entrega]),
            y = panel_vacunas$ES$dosis_diarias_entrega_moderna[-idx_sin_entrega],
            type = "bar", name = "Moderna",
            marker = list(color = "rgba(78, 210, 172, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Moderna",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis entregadas de Janssen
fig_dosis_entregadas_diarias_vertical_sin_huecos <-
  fig_dosis_entregadas_diarias_vertical_sin_huecos %>%
  add_trace(x = as.character(panel_vacunas$ES$fechas[-idx_sin_entrega]),
            y = panel_vacunas$ES$dosis_diarias_entrega_janssen[-idx_sin_entrega],
            type = "bar", name = "Janssen",
            marker = list(color = "rgba(245, 228, 57, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis diarias entregadas de Janssen",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))

# Dosis admin
fig_dosis_entregadas_diarias_vertical_sin_huecos <-
  fig_dosis_entregadas_diarias_vertical_sin_huecos %>%
  add_trace(x = as.character(panel_vacunas$ES$fechas[-idx_sin_entrega]),
            y = panel_vacunas$ES$dosis_diarias_admin[-idx_sin_entrega],
            type = "scatter", mode = "lines",
            name = "Dosis administradas",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 4), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. (diarias): %{y}",
                     "</b><extra></extra><br>",
                     "<b>Entregadas (diarias)</b>: ",
                     panel_vacunas$ES$dosis_diarias_entrega[-idx_sin_entrega]))

# Ajustes globales de la gráfica
fig_dosis_entregadas_diarias_vertical_sin_huecos <-
  fig_dosis_entregadas_diarias_vertical_sin_huecos %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Farmacéutica", orientation = "v",
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis entregadas (diarias) en España (",
                  as.character(format(max(as.Date(panel_vacunas$ES$fechas[-idx_sin_entrega])),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a> ",
                  "(solo figuran los días con entrega)"),
         # Fuente
         font = list(family = "poppins"), 
         titlefont = list(face = "bold", size = 17),
         # Formato eje Y
         yaxis =
           list(title = "Dosis", font = list(size = 15),
                # Números completos, no abreviados tipo 1M
                tickformat = ".0f"),
         # Formato eje X: título y le indicamos que es formato fecha
         xaxis = list(title = "Fechas", font = list(size = 15), nticks = 40,
                      range = as.character(panel_vacunas$ES$fechas[-idx_sin_entrega])),
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
htmlwidgets::saveWidget(fig_dosis_entregadas_diarias_vertical_sin_huecos,
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_entregadas_diarias_vertical_sin_huecos.html"),
                        selfcontained = TRUE)

# ###########################################
# 4. DOSIS ENTREGADAS POR MARCA ACUMULADAS (HORIZONTALES)
# ###########################################
fig_dosis_entregadas_horizontal <- plot_ly()

# Dosis entregadas de Pfizer
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  add_trace(y = as.character(panel_vacunas$ES$fechas),
            x = panel_vacunas$ES$dosis_entrega_pfizer,
            type = "bar", name = "Pfizer/Biontech",
            marker = list(color = "rgba(96, 178, 232, 1)"),
            hovertemplate = 
              paste0("<b>Dosis entregadas de Pfizer",
                     "</b><extra></extra><br>",
                     "a fecha de %{y}: %{x}"))

# Dosis entregadas de AstraZeneca
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  add_trace(y = as.character(panel_vacunas$ES$fechas),
            x = panel_vacunas$ES$dosis_entrega_astra,
            type = "bar", name = "AstraZeneca/Oxford",
            marker = list(color = "rgba(233, 105, 128, 1)"),
            hovertemplate =
              paste0("<b>Dosis entregadas de AstraZeneca",
                     "</b><extra></extra><br>",
                     "a fecha de %{y}: %{x}"))

# Dosis entregadas de Moderna
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  add_trace(y = as.character(panel_vacunas$ES$fechas),
            x = panel_vacunas$ES$dosis_entrega_moderna,
            type = "bar", name = "Moderna",
            marker = list(color = "rgba(78, 210, 172, 1)"),
            hovertemplate =
              paste0("<b>Dosis entregadas de Moderna",
                     "</b><extra></extra><br>",
                     "a fecha de %{y}: %{x}"))

# Dosis entregadas de Janssen
fig_dosis_entregadas_horizontal <-
  fig_dosis_entregadas_horizontal %>%
  add_trace(y = as.character(panel_vacunas$ES$fechas),
            x = panel_vacunas$ES$dosis_entrega_janssen,
            type = "bar", name = "Janssen",
            marker = list(color = "rgba(245, 228, 57, 1)"),
            hovertemplate =
              paste0("<b>Dosis entregadas de Janssen",
                     "</b><extra></extra><br>",
                     "a fecha de %{y}: %{x}"))

# Dosis admin
fig_dosis_entregadas_horizontal <- fig_dosis_entregadas_horizontal %>%
  add_trace(y = as.character(panel_vacunas$ES$fechas),
            x = panel_vacunas$ES$dosis_admin,
            type = "scatter", mode = "lines",
            name = "Dosis administradas",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 4), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis administradas: %{x}",
                     "</b><extra></extra><br>",
                     "<b>Totales entregadas</b>: ",
                     panel_vacunas$ES$dosis_entrega))

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
                  as.character(format(max(as.Date(panel_vacunas$ES$fechas, "%d-%m-%Y")),
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
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_entregadas_acum_horizontal.html"),
                        selfcontained = TRUE)



# #####################################################
# 5. DOSIS ADMINISTRADAS ACUMULADAS (VERTICALES)
# #####################################################
fig_dosis_admin_acum <- plot_ly(type = "bar")


# Dosis entregadas
fig_dosis_admin_acum <-
  fig_dosis_admin_acum %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_entrega,
            type = "scatter", mode = "lines", name = "Dosis entregadas",
            fill = "tozeroy", fillcolor = "rgba(31, 31, 31, 0.3)",
            line = list(color = "rgba(31, 31, 31, 0.85)",
                        shape = "spline", width = 3),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis entregadas acum.",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}<br>",
                     "<b>% dosis admin</b>: ",
                     panel_vacunas$ES$porc_admin_sobre_ccaa,
                     "%"))

# Dosis por 100 hab
fig_dosis_admin_acum <-
  fig_dosis_admin_acum %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_admin_100hab,
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
fig_dosis_admin_acum <-
  fig_dosis_admin_acum %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_pauta_completa,
            type = "bar", name = "Dosis admin. (pauta completa)",
            marker = list(color = "rgba(170, 81, 217, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. acum. (pauta completa)",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"))


# Dosis administradas totales
fig_dosis_admin_acum <-
  fig_dosis_admin_acum %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_primera,
            type = "bar", name = "Dosis admin.",
            marker = list(color = "rgba(234, 140, 115, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. acum.",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: ",
                     panel_vacunas$ES$dosis_admin, "<br>",
                     "<b>Dosis admin. acum.</b> (1ª dosis): ",
                     panel_vacunas$ES$dosis_primera))


# Ajustes globales de la gráfica
fig_dosis_admin_acum <-
  fig_dosis_admin_acum %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Dosis administradas", orientation = "v",
                xref = "paper", yref = "paper", x = 0.01, y = 1,
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis administradas (acumuladas) en España (",
                  as.character(format(max(as.Date(panel_vacunas$ES$fechas)),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
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
htmlwidgets::saveWidget(fig_dosis_admin_acum,
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_admin_acum.html"),
                        selfcontained = TRUE)


# #####################################################
# 6. DOSIS ADMIN DIARIAS
# #####################################################

fig_dosis_admin_diarias <- plot_ly(type = "bar")

# Dosis admin (2ª dosis)
fig_dosis_admin_diarias <-
  fig_dosis_admin_diarias %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_diarias_segunda,
            type = "bar", name = "Dosis diarias (2ª dosis)",
            marker = list(color = "rgba(170, 81, 217, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<br><b>Dosis diarias (2ª dosis): %{y}",
                     "<extra></extra><br>"))

# Dosis admin (1ª dosis)
fig_dosis_admin_diarias <-
  fig_dosis_admin_diarias %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$dosis_diarias_primera,
            type = "bar", name = "Dosis admin. diarias (1ª dosis)",
            marker = list(color = "rgba(234, 140, 115, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Dosis admin. diarias (1ª dosis): %{y}",
                     "</b><extra></extra><br>",
                     "Dosis admin. diarias: ",
                     panel_vacunas$ES$dosis_diarias_admin, "<br>",
                     "Dosis admin. diarias / 100 hab: ",
                     panel_vacunas$ES$dosis_diarias_admin_100hab))

# Ajustes globales de la gráfica
fig_dosis_admin_diarias <- fig_dosis_admin_diarias %>%
  layout(separators = ". ", showlegend = TRUE,
         legend = # Leyenda
           list(title = "Personas vacunadas acum.", orientation = "v",
                xref = "paper", yref = "paper", x = 0.01, y = 1,
                font = list(size = 13),
                bgcolor = "rgba(256, 256, 256, 0.7)"),
         title = # Título gráfica
           paste0("<b>Dosis administradas (diarias) en España (",
                  as.character(format(max(as.Date(panel_vacunas$ES$fechas)),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a><br>(se ",
                  "interpola linealmente los días sin informe)"),
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
htmlwidgets::saveWidget(fig_dosis_admin_diarias,
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_admin_diarias.html"),
                        selfcontained = TRUE)


# #####################################################
# 7. PERSONAS VACUNADAS ACUMULADAS
# #####################################################

# Previsión de los 10 millones de inmunizados con pauta completa
fecha_5M <- as.Date("2021-06-07")
idx_desde_prevision <- length(panel_vacunas$ES$fechas) - 6
fecha_desde_prevision <-
  max(as.Date(panel_vacunas$ES$fechas)[1:idx_desde_prevision])
dias_5M <- fecha_5M - fecha_desde_prevision
inmunizados_5M <- 1e7 -
  max(panel_vacunas$ES$personas_pauta_completa[1:idx_desde_prevision])
inmunizados_dia_5M <- inmunizados_5M / as.numeric(dias_5M)
fechas_hasta_prevision_5M <- min(fecha_5M,
                                 max(as.Date(panel_vacunas$ES$fechas)) + 8)
inmunizados_prevision_5M <-
  c(panel_vacunas$ES$personas_pauta_completa[1:idx_desde_prevision],
    panel_vacunas$ES$personas_pauta_completa[idx_desde_prevision + 1] +
      cumsum(rep(inmunizados_dia_5M,
                 as.numeric(fechas_hasta_prevision_5M - fecha_desde_prevision))))
fechas_prevision_5M <-
  as.character(seq(min(as.Date(panel_vacunas$ES$fechas)),
                   fechas_hasta_prevision_5M, by = 1))

fig_vacunados <- plot_ly(type = "bar")

# % población vacunada
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$porc_personas_vacunadas,
            type = "scatter", mode = "lines",
            name = "% población total vacunada",
            line = list(color = "rgba(49, 160, 35, 1)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población vacunada",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")
# % población  > 16 a vacunada
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$porc_personas_vacunadas_16a,
            type = "scatter", mode = "lines",
            name = "% población vacunada \u2265 16a",
            line = list(color = "rgba(49, 160, 35, 0.5)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población vacunada \u2265 16 años",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")
# % población pauta completa
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$porc_personas_pauta_completa,
            type = "scatter", mode = "lines",
            name = "% población pauta completa",
            line = list(color = "rgba(31, 31, 31, 1)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población pauta completa",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")
# % población pauta completa >= 16 años
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$porc_personas_pauta_completa_16a,
            type = "scatter", mode = "lines",
            name = "% población pauta completa \u2265 16a",
            line = list(color = "rgba(31, 31, 31, 0.5)",
                        shape = "spline", width = 3.5, dash = "dash"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>% población pauta completa \u2265 16 años",
                     "</b><extra></extra><br>",
                     "a fecha de %{x}: %{y}"),
            yaxis = "y2")

# Vacunadas con pauta completa
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$personas_pauta_completa,
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
              c(panel_vacunas$ES$personas_pauta_completa[idx_desde_prevision:length(panel_vacunas$ES$personas_pauta_completa)],
                rep(0, length(fechas_prevision_5M) -
                      length(panel_vacunas$ES$personas_pauta_completa))),
            type = "bar",  name = "Inmunizados necesarios para 10M (7 de junio)",
            marker = list(color = "rgba(17, 17, 17, 0.3)"),
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Vacunados (pauta completa) necesarios</b>",
                     "<extra></extra><br>",
                     "a %{x} para 10M inmun. (7 de junio): ",
                     floor(inmunizados_prevision_5M[-(1:(idx_desde_prevision - 1))])))

# Vacunados totales
fig_vacunados <-
  fig_vacunados %>%
  add_trace(x = panel_vacunas$ES$fechas,
            y = panel_vacunas$ES$personas_1dosis,
            type = "bar", name = "Vacunados (1 dosis)",
            marker = list(color = "rgba(234, 140, 115, 1)"), # Color
            hovertemplate = # Info HTML al pasar el ratón
              paste0("<b>Vacunados (1 dosis): %{y}",
                     "<extra></extra><br>",
                     "Vacunados totales: ",
                     panel_vacunas$ES$personas_vacunadas))


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
                  as.character(format(max(as.Date(panel_vacunas$ES$fechas)),
                                      "%d-%m-%Y")), ")</b><br>",
                  "<sup>Gráficos elaborados por Javier Álvarez Liébana ",
                  "(@DadosDeLaplace). Datos: INE y ",
                  "<a href = 'https://www.mscbs.gob.es/profesionales/",
                  "saludPublica/ccayes/alertasActual/nCov/",
                  "situacionActual.htm'>Ministerio Sanidad</a><br>(se ",
                  "inteporla linealmente los días sin informe)"),
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
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "vacunados_acum.html"),
                        selfcontained = TRUE)



# #############################################
# 8. GRÁFICO DE ROSA: DOSIS ENTREGADAS ACUM.
# #############################################

# Preparamos los datos semanlmente
dosis_semana <-
  data.frame("fechas" = panel_vacunas$ES$fechas,
             "semana" = week(as.Date(panel_vacunas$ES$fechas)),
             "dosis_pfizer" = panel_vacunas$ES$dosis_entrega_pfizer,
             "dosis_astra" = panel_vacunas$ES$dosis_entrega_astra,
             "dosis_moderna" = panel_vacunas$ES$dosis_entrega_moderna,
             "dosis_janssen" = panel_vacunas$ES$dosis_entrega_janssen)


# Agrupamos por semana
aux <- dosis_semana %>% group_by(semana) %>%
  summarise(dosis_pfizer = max(dosis_pfizer))
aux <- cbind(aux, (dosis_semana %>% group_by(semana) %>%
                     summarise(dosis_astra = max(dosis_astra)))$dosis_astra)
aux <- cbind(aux, (dosis_semana %>% group_by(semana) %>%
                     summarise(dosis_moderna =
                                 max(dosis_moderna)))$dosis_moderna)
aux <- cbind(aux, (dosis_semana %>% group_by(semana) %>%
                     summarise(dosis_janssen =
                                 max(dosis_janssen)))$dosis_janssen)

# Guardamos y renombramos columnas
dosis_semana <- aux
names(dosis_semana) <-
  c("semana", "dosis_pfizer", "dosis_astra",
    "dosis_moderna", "dosis_janssen")

# Figura como diagrama polar
fig_dosis_entregadas_rosa <-
  plot_ly(type = "scatterpolar", mode = "lines+text")

# Dosis entregadas de AstraZeneca
# (puesto como suma de las 3, como última capa)
fig_dosis_entregadas_rosa <- fig_dosis_entregadas_rosa %>%
  add_trace(t = dosis_semana$semana,
            r = dosis_semana$dosis_astra + dosis_semana$dosis_moderna +
              dosis_semana$dosis_pfizer + dosis_semana$dosis_janssen,
            theta = paste0("Semana ", dosis_semana$semana),
            fill = "toself", fillcolor = "rgba(245, 228, 57, 0.75)",
            line = list(color = "rgba(183, 170, 39, 0.85)"),
            name = "Janssen",
            hovertemplate = # Info HTML al pasar el ratón
              paste0("Semana ", dosis_semana$semana,
                     "<extra></extra><br>",
                     "<b>Dosis entregadas de Janssen</b>: %{r}"))

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
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_entregadas_rosa.html"),
                        selfcontained = TRUE)



# #############################################
# 9. GRÁFICO DE ROSA: DOSIS ADMIN ACUM.
# #############################################

# Calculamos las dosis administradas acum. por semana
dosis_admin_semana <-
  data.frame("fechas" = panel_vacunas$ES$fechas,
             "semana" = week(as.Date(panel_vacunas$ES$fechas)),
             "admin" = panel_vacunas$ES$dosis_admin,
             "pauta_completa" =
               2 * panel_vacunas$ES$personas_pauta_completa)
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
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "dosis_admin_rosa.html"),
                        selfcontained = TRUE)

# #############################################
# 10. GRÁFICO DE ROSA: PERSONAS VACUNADAS ACUM.
# #############################################

# Calculamos vacunados (acumulados) por semana
personas_vacunadas_semana <-
  data.frame("fechas" = panel_vacunas$ES$fechas,
             "semana" = week(as.Date(panel_vacunas$ES$fechas)),
             "personas_vacunadas" =
               panel_vacunas$ES$personas_vacunadas,
             "pauta_completa" = 
               panel_vacunas$ES$personas_pauta_completa)
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
                        file = paste0("./GRAFICAS/NACIONALES/",
                                      "personas_vacunadas_rosa.html"),
                        selfcontained = TRUE)



# #############################################
# 11. WAFFLES
# #############################################

if (gofres) {
  
  cat("\n Generando gofres...\n")
  # Número de cuadrados
  n_cuadrados <- 200
  
  # Ya generados
  waffle_generados <-
    gsub("waffle_dosis_entregadas_", "",
         gsub(".png", "", dir("./GRAFICAS/GOFRES/")))
  # Un gráfico por fecha
  fig_waffle_dosis_entregadas <- list()
  for (i in 1:length(panel_vacunas$ES$fechas)) {
    
    proporciones <- c()
    proporciones[1] <- panel_vacunas$ES$dosis_entrega_pfizer[i] /
      panel_vacunas$ES$dosis_entrega[i]
    proporciones[2] <- panel_vacunas$ES$dosis_entrega_moderna[i] /
      panel_vacunas$ES$dosis_entrega[i]
    proporciones[3] <- panel_vacunas$ES$dosis_entrega_astra[i] /
      panel_vacunas$ES$dosis_entrega[i]
    proporciones[4] <- panel_vacunas$ES$dosis_entrega_janssen[i] /
      panel_vacunas$ES$dosis_entrega[i]
    names(proporciones) <- c("Pfizer", "Moderna", "AstraZeneca", "Janssen")
    
    # Los waffle son ggplot (no plotly) así que deben ser editados como tal
    fig_waffle_dosis_entregadas[[i]] <-
      waffle(parts =
               c(round(proporciones[1:3] * n_cuadrados),
                 "Janssen" =
                   max(0, n_cuadrados - sum(round(proporciones[1:3] * n_cuadrados)))),
             rows = 10, colors = c("#60B2FF", "#4ED2AC", "#E96980", "#F5E439")) +
      ggtitle(paste0("Dosis entregadas (acumuladas) en España (",
                     format(as.Date(panel_vacunas$ES$fechas[i]), "%d-%m-%Y"),
                     ")"),
              subtitle =
                paste0("Gráficos elaborados por ",
                       "Javier Álvarez Liébana (@DadosDeLaplace). ",
                       "Datos: INE y Ministerio de Sanidad")) +
      xlab(paste0("Dosis entregadas (cuadrado = 0.5% = ",
                  round(0.005 * panel_vacunas$ES$dosis_entrega[i]),
                  " dosis, columna = 5%)")) +
      theme(plot.title = element_text(size = 19, face = "bold",
                                      family = "poppins"),
            plot.subtitle = element_text(size = 13,
                                         family = "poppins"),
            axis.title.x = element_text(size = 15, family = "poppins"),
            legend.text = element_text(size = 15, family = "poppins"),
            plot.margin = margin(l = 60))
  
    # Guardamos
    if (!(panel_vacunas$ES$fechas[i] %in% waffle_generados)) {
      
      ggsave(paste0("./GRAFICAS/GOFRES/waffle_dosis_entregadas_",
                    panel_vacunas$ES$fechas[i], ".png"),
             plot = fig_waffle_dosis_entregadas[[i]],
             width = 10, height = 10)
      
    }
  }
  names(fig_waffle_dosis_entregadas) <- panel_vacunas$ES$fechas
    
  
  # #############################################
  # WAFFLE: ANIMACIÓN DOSIS ENTREGADAS
  # #############################################
  
  if (animaciones) {
    ani.options("interval" = 0.2) # 0.2s entre frames
    saveGIF({
      for (i in 1:length(fig_waffle_dosis_entregadas)) {
        
        print(fig_waffle_dosis_entregadas[[i]])
        
      }},
      movie.name = "./gif_waffle_dosis_entregadas.gif",
      ani.height = 700, ani.width = 700)
  
  }
  
  # #############################################
  # 12. WAFFLE: DOSIS ADMIN
  # #############################################
  
  # Número de cuadrados
  n_cuadrados <- 200
  
  # Ya generados
  waffle_generados <-
    gsub("waffle_dosis_admin_", "",
         gsub(".png", "", dir("./GRAFICAS/GOFRES/")))
  # Un gráfico por fecha
  fig_waffle_dosis_admin <- list()
  for (i in 1:length(panel_vacunas$ES$fechas)) {
  
    proporciones <- c()
    proporciones[1] <-
      round(panel_vacunas$ES$dosis_admin_100hab[i] * (n_cuadrados / 100))
    proporciones[2] <-
      round(n_cuadrados * panel_vacunas$ES$dosis_entrega[i] /
              panel_vacunas$ES$poblacion[1])
    proporciones[3] <- n_cuadrados - sum(proporciones)
    names(proporciones) <- c("Admin. por 100 hab.", "Entregadas por 100 hab.",
                             "restantes")
  
    # Los waffle son ggplot (no plotly) así que deben ser editados como tal
    fig_waffle_dosis_admin[[i]] <-
      waffle(parts = proporciones, rows = 10,
             colors = c("#41B033", "#B29393", "#494949")) +
      ggtitle(paste0("Dosis entregadas (acumuladas) en España (",
                     format(as.Date(panel_vacunas$ES$fechas[i]), "%d-%m-%Y"),
                     ")"),
              subtitle =
                paste0("Gráficos elaborados por ",
                       "Javier Álvarez Liébana (@DadosDeLaplace). ",
                       "Datos: INE y Ministerio de Sanidad")) +
      xlab(paste0("Dosis entregadas (cuadrado = 0.5 dosis/100 hab, ",
                  "columna = 5 dosis/100 hab)")) +
      theme(plot.title = element_text(size = 19, face = "bold",
                                      family = "poppins"),
            plot.subtitle = element_text(size = 13,
                                         family = "poppins"),
            axis.title.x = element_text(size = 15, family = "poppins"),
            legend.text = element_text(size = 15, family = "poppins"),
            plot.margin = margin(l = 60))
  
    # Guardamos
    if (!(panel_vacunas$ES$fechas[i] %in% waffle_generados)) {
  
      ggsave(paste0("./GRAFICAS/GOFRES/waffle_dosis_admin_",
                    panel_vacunas$ES$fechas[i], ".png"),
             plot = fig_waffle_dosis_admin[[i]],
             width = 10, height = 10)
  
    }
  }
  names(fig_waffle_dosis_admin) <- panel_vacunas$ES$fechas
  
  
  
  
  
  
    
  
  # #############################################
  # WAFFLE: ANIMACIÓN DOSIS ADMIN
  # #############################################
  
  if (animaciones) {
    ani.options("interval" = 0.2) # 0.2s entre frames
    saveGIF({
      for (i in 1:length(fig_waffle_dosis_admin)) {
        
        print(fig_waffle_dosis_admin[[i]])
        
      }},
      movie.name = "./gif_waffle_dosis_admin.gif",
      ani.height = 700, ani.width = 700)
  }
  
  
  
  # #############################################
  # 13. WAFFLE: PERSONAS VACUNADAS
  # #############################################
  
  # Número de cuadrados
  n_cuadrados <- 200
  
  # Ya generados
  waffle_generados <- gsub("waffle_personas_vacunadas", "",
                           gsub(".png", "", dir("./GRAFICAS/GOFRES/")))
  
  # Un gráfico por fecha
  fig_waffle_personas_vacunadas <- list()
  for (i in 1:length(panel_vacunas$ES$fechas)) {
    
    proporciones <- c()
    proporciones[1] <-
      round(panel_vacunas$ES$porc_personas_pauta_completa[i] *
              (n_cuadrados / 100))
    proporciones[2] <-
      round((panel_vacunas$ES$porc_personas_vacunadas[i] -
               panel_vacunas$ES$porc_personas_pauta_completa[i]) *
              (n_cuadrados / 100))
    proporciones[3] <-
      round(n_cuadrados * (1.5e7 / panel_vacunas$ES$poblacion[1]) -
              sum(proporciones))
    proporciones[4] <-
      round(n_cuadrados * (2.5e7 / panel_vacunas$ES$poblacion[1]) -
              sum(proporciones))
    proporciones[5] <- round(n_cuadrados * 0.7 - sum(proporciones))
    proporciones[6] <- n_cuadrados - sum(proporciones)
    names(proporciones) <-
      c("Con pauta completa", "Esperando 2ª dosis",
        "15M con pauta completa (14 de junio)",
        "25M con pauta completa (19 de julio)",
        "70% con pauta completa", "Resto de población")
    
    # Los waffle son ggplot (no plotly) así que deben ser editados como tal
    fig_waffle_personas_vacunadas[[i]] <-
      waffle(parts = proporciones, rows = 10,
             colors = c("#AA51D9", "#EA8C73", "#66AADF",
                        "#A3C4B7", "#B4B8BB", "#515151")) +
      ggtitle(paste0("Personas vacunadas (acumuladas) en España (",
                     format(as.Date(panel_vacunas$ES$fechas[i]), "%d-%m-%Y"),
                     ")"),
              subtitle =
                paste0("Gráficos elaborados por ",
                       "Javier Álvarez Liébana (@DadosDeLaplace). ",
                       "Datos: INE y Ministerio de Sanidad")) +
      xlab(paste0("Personas vacunadas (1 cuadrado = 0.5% población, ",
                  "columna = 5%, 5M = 10%)")) +
      theme(plot.title = element_text(size = 19, face = "bold",
                                      family = "poppins"),
            plot.subtitle = element_text(size = 13,
                                         family = "poppins"),
            axis.title.x = element_text(size = 15, family = "poppins"),
            legend.text = element_text(size = 15, family = "poppins"),
            plot.margin = margin(l = 60))
    
    # Guardamos
    if (!(panel_vacunas$ES$fechas[i] %in% waffle_generados)) {
      
      ggsave(paste0("./GRAFICAS/GOFRES/waffle_personas_vacunadas_",
                    panel_vacunas$ES$fechas[i], ".png"),
             plot = fig_waffle_personas_vacunadas[[i]],
             width = 10, height = 10)
      
    }
  }
  names(fig_waffle_personas_vacunadas) <- panel_vacunas$ES$fechas
  
  
  
  # #############################################
  # WAFFLE: ANIMACIÓN VACUNADOS
  # #############################################
  
  if (animaciones) {
    ani.options("interval" = 0.2) # 0.2s entre frames
    saveGIF({
      for (i in 1:length(fig_waffle_personas_vacunadas)) {
        
        print(fig_waffle_personas_vacunadas[[i]])
        
      }},
      movie.name = "./gif_waffle_personas_vacunadas.gif",
      ani.height = 700, ani.width = 700)
  }
}
  
  







# #############################################
# 14. MAPA RELLENO
# #############################################

if (mapas) { source("./mapa_relleno.R") }


# #########################
# 15. GRÁFICOS VIOLÍN
# #########################


# 
# 
# datos <-
#   c(panel_vacunas$ES$dosis_admin_100hab[107],
#     panel_vacunas$AN$dosis_admin_100hab[107],
#     panel_vacunas$AR$dosis_admin_100hab[107],
#     panel_vacunas$AS$dosis_admin_100hab[107],
#     panel_vacunas$IB$dosis_admin_100hab[107],
#     panel_vacunas$CN$dosis_admin_100hab[107],
#     panel_vacunas$CB$dosis_admin_100hab[107],
#     panel_vacunas$CL$dosis_admin_100hab[107],
#     panel_vacunas$CM$dosis_admin_100hab[107],
#     panel_vacunas$CT$dosis_admin_100hab[107],
#     panel_vacunas$VC$dosis_admin_100hab[107],
#     panel_vacunas$EX$dosis_admin_100hab[107],
#     panel_vacunas$GA$dosis_admin_100hab[107],
#     panel_vacunas$RI$dosis_admin_100hab[107])
# fig <- plot_ly(type = "violin")
# fig <- fig %>%
#   add_trace(y = datos,
#             type = "violin", box = list(visible = TRUE),
#             meanline = list(visible = TRUE), hoveron = "points+kde",
#             points = "all", pointpos = 1, jitter = 0,
#             scalemode = "count",
#             color = I("#8dd3c7"),
#             marker = list(line = list(width = 2, color = "#8dd3c7"),
#               symbol = 'line-ns')) 
# 
# # 
# # fig <- fig %>%
# #   layout(
# #     xaxis = list(
# #       title = "Day"
# #     ),
# #     yaxis = list(
# #       title = "Total Bill",
# #       zeroline = F
# #     )
# #   )
# # 
# # fig
# # 
# # df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/violin_data.csv")
# # 
# # pointposMale <- c(-0.9,-1.1,-0.6,-0.3)
# # pointposFemale <- c(0.45,0.55,1,0.4)
# # showLegend <- c(T,F,F,F)
# # 
# # fig <- plot_ly(type = 'violin')
# # 
# # i = 0
# # for (i in 1:length(unique(df$day))) {
# #   fig <- add_trace(
# #     fig,
# #     x = df$day[df$sex == 'Male' & df$day == unique(df$day)[i]],
# #     y = df$total_bill[df$sex == 'Male' & df$day == unique(df$day)[i]],
# #     hoveron = "points+kde",
# #     legendgroup = 'M',
# #     scalegroup = 'M',
# #     name = 'M',
# #     side = 'negative',
# #     box = list(
# #       visible = T
# #     ),
# #     points = 'all',
# #     pointpos = pointposMale[i],
# #     jitter = 0,
# #     scalemode = 'count',
# #     meanline = list(
# #       visible = T
# #     ),
# #     color = I("#8dd3c7"),
# #     marker = list(
# #       line = list(
# #         width = 2,
# #         color = "#8dd3c7"
# #       ),
# #       symbol = 'line-ns'
# #     ),
# #     showlegend = showLegend[i]
# #   ) 
# #   
# #   fig <- fig %>%
# #     add_trace(
# #       x = df$day[df$sex == 'Female' & df$day == unique(df$day)[i]],
# #       y = df$total_bill[df$sex == 'Female' & df$day == unique(df$day)[i]],
# #       hoveron = "points+kde",
# #       legendgroup = 'F',
# #       scalegroup = 'F',
# #       name = 'F',
# #       side = 'positive',
# #       box = list(
# #         visible = T
# #       ),
# #       points = 'all',
# #       pointpos = pointposFemale[i],
# #       jitter = 0,
# #       scalemode = 'count',
# #       meanline = list(
# #         visible = T
# #       ),
# #       color = I("#bebada"),
# #       marker = list(
# #         line = list(
# #           width = 2,
# #           color = "#bebada"
# #         ),
# #         symbol = 'line-ns'
# #       ),
# #       showlegend = showLegend[i]
# #     )
# # }
# # 
# # fig <- layout(
# #   fig,
# #   title = "Total bill distribution<br><i>scaled by number of bills per gender",
# #   yaxis = list(
# #     zeroline = F
# #   ),
# #   violingap = 0,
# #   violingroupgap = 0,
# #   violinmode = 'overlay',
# #   legend = list(
# #     tracegroupgap = 0
# #   )
# # )
# # 
# # fig
