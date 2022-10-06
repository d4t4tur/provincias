#Llamo a las librerias.
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(plotly)
library(glue)
library(d4t4tur)
library(DT)
library(crosstalk)

# Data por por clasificacion
cabotaje <- read_rds("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada.rds") %>% 
  filter(ClasificaciónVuelo == "Cabotaje",
         reg_no_reg == "Empresa de vuelos regulares") %>% 
  select(Año_Local, TipodeMovimiento,
         origen_aeropuerto_etiqueta, origen_localidad_etiqueta, origen_provincia_etiqueta,
         destino_aeropuerto_etiqueta, destino_localidad_etiqueta, destino_provincia_etiqueta,
         pasajeros, pax) %>% 
  mutate(aero_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_aeropuerto_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_aeropuerto_etiqueta),
         prov_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_provincia_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_provincia_etiqueta),
         loc_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_localidad_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_localidad_etiqueta))


internacional <- read_rds("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada.rds") %>% 
  filter(ClasificaciónVuelo == "Internacional",
         reg_no_reg == "Empresa de vuelos regulares") %>% 
  select(Año_Local, TipodeMovimiento,
         origen_aeropuerto_etiqueta, origen_localidad_etiqueta, origen_provincia_etiqueta,
         destino_aeropuerto_etiqueta, destino_localidad_etiqueta, destino_provincia_etiqueta,
         pasajeros) %>% 
  mutate(aero_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_aeropuerto_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_aeropuerto_etiqueta),
         prov_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_provincia_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_provincia_etiqueta),
         loc_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_localidad_etiqueta,
                            TipodeMovimiento == "Despegue" ~ origen_localidad_etiqueta))

# DT idioma
options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))


## TABLAS

tabla_cabotaje <- cabotaje %>% 
  filter(Año_Local >= 2017 & Año_Local < 2022) %>% 
  group_by(Año_Local, prov_ad, loc_ad) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(Año_Local), prov_ad) %>% 
  rename(Año = Año_Local,
         Provincia = prov_ad,
         Localidad = loc_ad,
         Pasajeros = pasajeros) %>% 
  drop_na()


tabla_internacional <- internacional %>% 
  filter(Año_Local >= 2017 & Año_Local < 2022) %>% 
  group_by(Año_Local, prov_ad, loc_ad) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(Año_Local), prov_ad) %>% 
  rename(Año = Año_Local,
         Provincia = prov_ad,
         Localidad = loc_ad,
         Pasajeros = pasajeros) %>% 
  drop_na()


## PLOTS + DT interactivos

plot_cabotaje <- highlight_key(tabla_cabotaje %>% mutate(Año = as.character(Año)), ~ Provincia)

dt_cabotaje <- datatable(plot_cabotaje,
                              options = list(lengthMenu = c(10, 25), 
                                             pageLength = 10, 
                                             dom = 'lrtipB'),
                              rownames= FALSE)

gg <- ggplot(plot_cabotaje) + 
  geom_line(aes(Año, Pasajeros, group = Localidad, color = Localidad)) +
  geom_point(aes(Año, Pasajeros, group = Localidad, color = Localidad, text = paste0(Localidad, "<br>", format(Pasajeros, big.mark = "."), " Pasajeros"))) +
  theme_minimal() +
  theme(legend.position = "none")

graph_cabotaje <- bscols(
  filter_select("id", "Elegir una provincia", plot_cabotaje, ~ Provincia,
                multiple = FALSE),
  ggplotly(gg, dynamicTicks = TRUE, tooltip = "text"),
  widths = c(12, 12)
)


cabotaje <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12), 
                    graph_cabotaje,
                    dt_cabotaje)
)

write_rds(cabotaje, "outputs/graph_cabotaje.rds")


plot_internacional <- highlight_key(tabla_internacional %>% mutate(Año = as.character(Año)), ~ Provincia)

dt_internacional <- datatable(plot_internacional,
                              options = list(lengthMenu = c(10, 25, 50), 
                                             pageLength = 10, 
                                             dom = 'lrtipB'),
                              rownames= FALSE)

gg <- ggplot(plot_internacional) + 
  geom_line(aes(Año, Pasajeros, group = Localidad, color = Localidad)) +
  geom_point(aes(Año, Pasajeros, group = Localidad, color = Localidad, text = paste0(Localidad, "<br>", format(Pasajeros, big.mark = "."), " Pasajeros"))) +
  theme_minimal() +
  theme(legend.position = "none")

graph_internacional <- bscols(
  filter_select("id", "Elegir una provincia", plot_internacional, ~ Provincia,
                multiple = FALSE),
  ggplotly(gg, dynamicTicks = TRUE, tooltip = "text"),
  widths = c(12, 12)
)


internacional <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12), 
                    graph_internacional,
                    dt_internacional)
)

write_rds(internacional, "outputs/graph_internacional.rds")

# grafico_serietiempo_aero <- function(x) {
#   
#   plot <- list()
#   
#   df <-   x %>%
#     # filter(TipodeMovimiento == "Aterrizaje") %>% 
#     mutate(fecha = lubridate::ym(paste(Año_Local, Mes_Local, sep = "-"), locale = "es_AR.utf8")) %>% 
#     group_by(ad_aeropuerto_etiqueta, fecha) %>%
#     summarise(pasajeros = sum(pax_ad, na.rm = T),
#               asientos = sum(asientos_pax, na.rm = T),
#     ) %>% 
#     mutate(across(c(pasajeros, asientos), ~ ifelse(.x == .5, 1, round(.x, 0)))) %>% 
#     ungroup()
#   
#   
#   plot$pasajeros <-  df %>% 
#     ggplot(aes(group = ad_aeropuerto_etiqueta, x = fecha,
#                y = pasajeros, color = ad_aeropuerto_etiqueta,
#                text = glue('{format(pasajeros, big.mark = ".")} pasajeros\n{fecha}\n{ad_aeropuerto_etiqueta}'))) +
#     geom_line() +
#     geom_point() +
#     scale_x_date() + 
#     labs(title = glue("Pasajeros en Vuelos Comerciales {unique(df$ad_provincia_etiqueta)}")) +
#     xlab("") +
#     guides(fill = guide_legend(title="Aeropuertos")) +
#     theme(legend.position = "bottom") +
#     theme_minimal() 
#   
#   plot$pasajeros <- plot$pasajeros %>% 
#     ggplotly(tooltip = "text") %>% 
#     layout(legend = list(orientation = "h", y = 0, x = 0))
#   
#   
#   plot$asientos <-  df %>% 
#     ggplot(aes(group = ad_aeropuerto_etiqueta, x = fecha,
#                y = asientos, color = ad_aeropuerto_etiqueta,
#                text = glue('{format(asientos, big.mark = ".")} asientos\n{fecha}\n{ad_aeropuerto_etiqueta}'))) +
#     geom_line() +
#     geom_point() +
#     scale_x_date() + 
#     labs(title = glue("Asientos en Vuelos Comerciales {unique(x$ad_provincia_etiqueta)}")) +
#     xlab("") +
#     guides(fill = guide_legend(title="Aeropuertos")) +
#     theme(legend.position = "bottom") +
#     theme_minimal()
#   
#   plot$asientos <- plot$asientos %>% 
#     ggplotly(tooltip = "text") %>% 
#     layout(legend = list(orientation = "h", y = 0, x = 0))
#   
#   plot
# }
# 
# aero_cabotaje_nest_data <- aero_cabotaje_nest_data %>% 
#   mutate(graficos = map(nested_column_provs, ~ grafico_serietiempo_aero(.x))) %>% 
#   mutate(.plot_timeseries_pasajeros = map(graficos, ~ .x$pasajeros),
#          .plot_timeseries_asientos = map(graficos, ~ .x$asientos)) %>% 
#   select(-graficos)
# 
# aero_internacional_nest_data <- aero_internacional_nest_data %>% 
#   mutate(graficos = map(nested_column_provs, ~ grafico_serietiempo_aero(.x))) %>% 
#   mutate(.plot_timeseries_pasajeros = map(graficos, ~ .x$pasajeros),
#          .plot_timeseries_asientos = map(graficos, ~ .x$asientos),
#   ) %>% 
#   select(-graficos)
# 
# ##
# ##
# 
# 
# tabla_aero_cabotaje <-  function(x) {
#   x %>% 
#     filter(Año_Local == 2022) %>% 
#     group_by(ad_aeropuerto_etiqueta) %>%
#     summarise(pasajeros = round(sum(pax_ad, na.rm = T),0)) %>% 
#     arrange(-pasajeros) %>% 
#     janitor::adorn_totals() %>% 
#     mutate(across(-ad_aeropuerto_etiqueta, ~ifelse(is.na(.x), 0, .x))) %>% 
#     gt()  %>% 
#     cols_label(
#       ad_aeropuerto_etiqueta  = md(""),
#       pasajeros = md("**Pasajeros en Vuelos Regulares**")
#     )  %>%
#     fmt_number(
#       columns = 2,
#       decimals = 0,
#       sep_mark = ".",
#       dec_mark = ","
#     ) %>% 
#     cols_align(align = "center",
#                columns = c(pasajeros))  %>%
#     opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
#     tab_options(table.align = "center", 
#                 row_group.font.weight = "bold",
#                 #table.font.size = 11,
#                 #table.width = 580,
#                 #data_row.padding = px(7)
#     ) %>% 
#     tab_style(
#       style = list(
#         cell_text(weight = "bold")
#       ),
#       locations = cells_body(
#         rows =  ad_aeropuerto_etiqueta == "Total"
#       )) %>% 
#     tab_header(title = md(glue("**Vuelos de Cabotaje**")),
#                subtitle =  md(glue("{unique(x$ad_provincia_etiqueta)}. Año 2019"))) %>%
#     tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos de la ANAC"   )
#     )
# }  
# 
# 
# 
# tabla_aero_internacional <-  function(x) {
#   x %>% 
#     filter(Año_Local == 2022) %>% 
#     group_by(ad_aeropuerto_etiqueta) %>%
#     summarise(pasajeros = round(sum(pax_ad, na.rm = T),0)) %>% 
#     arrange(-pasajeros) %>% 
#     janitor::adorn_totals() %>% 
#     mutate(across(-ad_aeropuerto_etiqueta, ~ifelse(is.na(.x), 0, .x))) %>% 
#     gt()  %>% 
#     cols_label(
#       ad_aeropuerto_etiqueta  = md(""),
#       pasajeros = md("**Pasajeros en Vuelos Regulares**")
#     )  %>%
#     fmt_number(
#       columns = 2,
#       decimals = 0,
#       sep_mark = ".",
#       dec_mark = ","
#     ) %>% 
#     cols_align(align = "center",
#                columns = c(pasajeros))  %>%
#     opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
#     tab_options(table.align = "center", 
#                 row_group.font.weight = "bold",
#                 #table.font.size = 11,
#                 #table.width = 580,
#                 #data_row.padding = px(7)
#     ) %>% 
#     tab_style(
#       style = list(
#         cell_text(weight = "bold")
#       ),
#       locations = cells_body(
#         rows =  ad_aeropuerto_etiqueta == "Total"
#       )) %>% 
#     tab_header(title = md(glue("**Vuelos Internacionales**")),
#                subtitle = md(glue("{unique(x$ad_provincia_nombre)}. Año 2019"))) %>%
#     tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos de la ANAC"   )
#     )
# }  
# 
# 
# aero_internacional_nest_data <- aero_internacional_nest_data %>% 
#   mutate(.tablas_prov = map(nested_column_provs,  tabla_aero_internacional)) %>% 
#   arrange(nombre_prov) %>% 
#   select(-nested_column_provs)
# 
# aero_cabotaje_nest_data <- aero_cabotaje_nest_data %>% 
#   mutate(.tablas_prov = map(nested_column_provs,  tabla_aero_cabotaje)) %>% 
#   arrange(nombre_prov) %>% 
#   select(-nested_column_provs)
# 
# write_rds(aero_internacional_nest_data, file= "outputs/aero_internacional_nest_data.RDS")
# write_rds(aero_cabotaje_nest_data, file= "outputs/aero_cabotaje_nest_data.RDS")