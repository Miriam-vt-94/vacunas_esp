cat("\nCreando mapas rellenos...\n")
# ######################
# CARGA DE DATOS
# ######################
# Cargamos datos de vacunación de España
datos_globales <- read.csv("./EXPORTADO/POR_CCAA/datos_ES.csv",
                      stringsAsFactors = FALSE)

# ######################################
# MAPA POR FECHA DESDE 20 DE ENERO
# ######################################
mapas_generados <- # Si ya los tengo, no los genero
  dir("./GRAFICAS/MAPAS_RELLENOS/")[grepl("mapa_vacunacion", dir("./GRAFICAS/MAPAS_RELLENOS/"))]
fechas_generadas <- # Fechas ya generadas
  as.Date(gsub(".png", "", gsub("mapa_vacunacion_esp_", "",
                                mapas_generados)))
idx <- which(!(as.Date(datos_globales$fechas) %in% fechas_generadas) &
               as.Date(datos_globales$fechas) >= "2021-01-20")
load("./GRAFICAS/MAPAS_RELLENOS/mapas_guardados.RData") # cargamos mapas ya generados
for (i in idx) {
  
  datos_esp <- datos_globales[i, ]
  
  # Cargamos mapa de españa
  sf_spain <- esp_get_ccaa() %>% st_transform(crs = 25830) # %>%
    # Si queremos excluir islas:
    # filter(ine.ccaa.name != "Canarias")
  
  # ######################
  # CREAMOS GRID
  # ######################
  # Creamos grid (generamos n_grid cuadrados aleatorios)
  n_grid <- 160000 #200000
  set.seed(1234)
  grid_generado <-
    st_sample(sf_spain, size = n_grid, # Nº de celdas (definición)
              type = "regular", exact = TRUE)
  
  # Convertimos grid en data.frame
  esp_tiles <- grid_generado %>% st_coordinates() %>%
    as.data.frame()
  colnames(esp_tiles) <- c("x", "y")
  
  esp_tiles <- esp_tiles %>%
    arrange(y) %>%
    # Here we split the x-y coordinates according to % of each (non/)vaccinated group
    mutate(id = 1:nrow(.),
           group_name = as.factor(
             case_when(
               id <= nrow(.) * datos_esp$porc_personas_pauta_completa/100 ~ "second",
               id >= nrow(.) * datos_esp$porc_personas_pauta_completa/100 &
                 id <= nrow(.) * datos_esp$porc_personas_vacunadas/100 ~ "first",
               TRUE ~ "nonvac")))
  
  # ######################
  # CREAMOS ETIQUETAS EN EL EJE Y
  # ######################
  # Identify percent labels as y-axis
  id_to_filter <- data.frame(
    id = 1:nrow(esp_tiles),
    point_to_label = as.character(
      cut(
        esp_tiles$id,
        breaks = quantile(esp_tiles$id, probs = 0:10 / 10),
        labels = paste0(seq(0, 90, 10), "%"),
        include.lowest = TRUE
      )
    ) %>%
      ifelse(duplicated(.), NA, .)
  )
  
  
  pct_labels <- inner_join(esp_tiles, id_to_filter) %>%
    filter(!is.na(point_to_label)) %>%
    mutate(
      start_line = min(esp_tiles$x) - n_grid,
      # We want the line to end a little before the shape of the map,
      # which means where X is at minimum.
      # The 0 is a place holder for the first value
      end_line = with(esp_tiles[esp_tiles$y %in% .$y, ],
                      tapply(x, y, min)) + 5000,
      end_line2 = with(esp_tiles[esp_tiles$y %in% .$y, ],
                       tapply(x, y, max)) + 5000
    ) %>%
    filter(id != 1)
  
  # ######################
  # BARRAS VERTICALES/HORIZONTALES
  # ######################
  
  # Vertical bars
  ver_bars <- esp_tiles %>% group_by(group_name) %>% summarise(yend = max(y))
  ver_bars$y <- min(esp_tiles$y)
  
  # Add some white space on the side of the map
  ver_bars$x <- max(esp_tiles$x) + 70000
  ver_bars$xend <- ifelse(ver_bars$group_name == "first",
                          ver_bars$x,
                          ver_bars$x + 30e4)
  
  # Same idea but for easier reading in the plot itself
  hor_bars <- ver_bars
  
  # ######################
  # COLORES/ANOTACIONES
  # ######################
  
  # Colors info
  aes_details <- data.frame(
    group_name = c("nonvac", "first", "second"),
    group_color = c("gray70", "#1ab0d4", "#1a698a")
  )
  
  
  # Position of annotations
  text_pos <- data.frame(
    # Find the middle location between where the segments starts and ends
    transmute(ver_bars, y = (yend - y) / 2 + y),
    group_name = ver_bars$group_name,
    x = ver_bars$xend + 4e3,
    group_color = aes_details$group_color,
    # Create label using {glue} and the downloaded df so it's easy to update when necessary
    label = c(
      glue(
        paste0("{round(datos_esp$personas_vacunadas / 1e6, 1)}M<br>",
               "personas<br><span style='color:{aes_details$group_color[2]}'>",
               "<b>vacunadas<br>",
               "({round(datos_esp$porc_personas_vacunadas, 1)}%)</b></span>"),
      ),
      glue(
        paste0("{round((datos_esp$poblacion - datos_esp$personas_vacunadas)/1e6, 1)}",
               "M pendientes<br><span style='color:gray50'><b>de vacunar ",
               "({round(100 - datos_esp$porc_personas_vacunadas, 1)}%)",
               "</b></span>"),
      ),
      glue(
        paste0("{round(datos_esp$personas_pauta_completa/1e6, 1)}M<br>",
               "inmunizadas<br><span style='color:{aes_details$group_color[3]}'>",
               "<b>con doble pauta<br>({round(datos_esp$porc_personas_pauta_completa, 1)}%)",
               "</b></span>"),
      )
    )
  )
  
  # ######################
  # CONSTRUCCIÓN MAPA
  # ######################
  
  ver_bars$y[ver_bars$group_name == "nonvac"] <-
    ver_bars$yend[ver_bars$group_name == "first"]
  hor_bars$y[hor_bars$group_name == "nonvac"] <-
    hor_bars$yend[hor_bars$group_name == "first"]
  mapa_relleno <- ggplot(esp_tiles) +
    # The tiles that fill the map
    geom_tile(aes(x = x, y = y, fill = group_name), size = .3,
              show.legend = FALSE) +
    coord_equal(clip = "off") +
    theme_void() +
    theme(text = element_text(family = "Arial"),
          plot.title = element_text(size = 35, face = "bold",
                                    family = "Arial", hjust = 0),
      # adjust subtitle space between lines
      plot.subtitle =
        element_markdown(size = 14, color = "gray25", lineheight = 1.2),
      plot.caption =
        element_text(color = "gray50", hjust = 0, size = 12),
      plot.margin = margin(8, 6, 6, 8, "mm"),
      plot.background = element_rect(fill = "white", color = NA)) +
    scale_x_continuous(limits = c(min(pct_labels$start_line) - 1e4,
                                  max(text_pos$x) + 50e4)) +
    scale_fill_manual(values = c("nonvac" = "gray65",
                                 "first" = "#1ab0d4", "second" = "#1a698a")) +
    geom_segment(
      data = pct_labels[!(pct_labels$point_to_label %in% c("20%", "70%")), ],
      aes( x = start_line, xend = end_line, y = y, yend = y),
      color = "gray80", linetype = "dashed") +
    geom_segment(
      data = pct_labels[pct_labels$point_to_label == "20%", ],
      aes(x = start_line, xend = end_line, y = y, yend = y),
      color = "#17CF9A", linetype = "dashed") +
    geom_segment(
      data = pct_labels[pct_labels$point_to_label == "70%", ],
      aes(x = start_line, xend = end_line2, y = y, yend = y),
      color = "#EB4444", linetype = "dashed") +
    # Percent labels
    geom_text(
      data = pct_labels[!(pct_labels$point_to_label %in% c("20%", "70%")), ],
      aes(x = start_line - 10000, y = y, label = point_to_label),
      size = 5, color = "gray60", family = "Arial") +
    geom_text(
      data = pct_labels[pct_labels$point_to_label == "20%", ],
      aes(x = start_line - 5000, y = y, label = "20% (7 de junio)"),
      size = 7, color = "#129E75", family = "Arial") +
    geom_text(
      data = pct_labels[pct_labels$point_to_label == "70%", ],
      aes(x = start_line - 10000, y = y, label = point_to_label),
      size = 7, color = "#D92828", family = "Arial") +
    # vertical bars on the side for each group
    geom_segment(data = ver_bars,
                 aes(x = xend, xend = xend, y = y, yend = yend),
                 color = "gray70") +
    # horizontal bars with the *minimum* y value (y at minimum for each group)
    geom_segment(data = hor_bars, #[hor_bars$group_name != "nonvac", ],
                 aes(x = xend - 10000, xend = xend, y = y, yend = y),
                 color = "gray70") +
    # horizontal bars with the *maximum* y values (y at max for each group)
    geom_segment(data = hor_bars,
                 aes(x = xend - 10000, xend = xend,  y = yend, yend = yend),
                 color = "gray70") +
    # Annotation text
    geom_richtext(
      data = text_pos,
      aes(x = x, y = y, label = label),
      fill = NA, label.color = NA, hjust = 0,
      size = 6, color = "gray55", family = "Arial" ) +
    labs(title =
           paste0("Vacunación en España ", "(fecha: ",
                  format(as.Date(datos_esp$fechas), "%d-%m-%Y"), ")"),
      subtitle = glue(
        paste0("Gráfico elaborado por Javier Álvarez Liébana (@DadosDeLaplace). ",
               "Se ha prometido llegar a 10M (21.12%) de inmunizados el 3 de mayo.")
      ),
      # Take the most up to date from the esp_data df we created above
      caption = glue(paste0("Datos: Ministerio de Sanidad e INE | ",
                            "Código: @dieghernan | ",
                            "Inspirado en: @Amit_Levinson"))
    )
  
  
  # ######################
  # GUARDAMOS
  # ######################
  ggsave(paste0("./GRAFICAS/MAPAS_RELLENOS/mapa_vacunacion_esp_",
                as.character(datos_esp$fechas), ".png"),
         mapa_relleno, height = 10, width = 15, device = ragg::agg_png())
  mapas_guardados[[i]] <- mapa_relleno
}
save(mapas_guardados, file = "./GRAFICAS/MAPAS_RELLENOS/mapas_guardados.RData")

if (animaciones) {
  
  load("./GRAFICAS/MAPAS_RELLENOS/mapas_guardados.RData")
  ani.options("interval" = 0.2) # 0.2s entre frames
  saveGIF({
    for (i in 1:length(mapas_guardados)) {
      
      print(mapas_guardados[[i]])
      
    }},
    movie.name = "./gif_mapa_relleno.gif",
    ani.height = 850, ani.width = 850)
  
}