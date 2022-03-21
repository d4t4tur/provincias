library(tidyverse)
library(janitor)
library(DT)
library(gt)
library(geoAr)
library(highcharter)
library(glue)

source("scripts/aux_function.R")

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Arial'});",
                            "}")))


serie_puna <- read_csv("/srv/DataDNMYE/puna/serie_puna.zip") %>% 
  mutate(across(2:6, ~ case_when(is.na(.) ~ "Sin dato",
                                 TRUE ~ .)))
serie_puna_nac <- serie_puna %>% 
  group_by(anio) %>% 
  summarise(total_nac_establecimientos = sum(establecimientos),
         total_nac_plazas  = sum(plazas)) 

serie_puna_nest_data <- serie_puna %>% 
  mutate(nombre_prov = as_factor(provincia)) %>% 
  ungroup() %>% 
  nest(nested_column_prov = -nombre_prov)


tabla_gt_resumen <- function(x) {
  
  x %>% 
    filter(anio == 2019) %>% 
    group_by(tipo) %>% 
    summarise(total_establecimientos = sum(establecimientos),
              total_plazas = sum(plazas)) %>% 
    mutate(part_establecimientos = total_establecimientos/sum(total_establecimientos),
           part_plazas = total_plazas/sum(total_plazas)) %>% 
    adorn_totals() %>% 
    arrange(tipo) %>% 
    gt() %>% 
    cols_label(
      tipo = "",
      total_establecimientos = md("**Cantidad de Establecimientos**"),
      total_plazas = md("**Cantidad de Plazas**"),
      part_establecimientos = md("**% de establecimientos<br>sobre el total provincial**"),
      part_plazas = md("**% de plazas<br>sobre el total provincial**")
    )  %>% 
    fmt_number(
      columns = 2:3,
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>% 
    fmt_percent(
      columns = 4:5,
      decimals = 2,
      sep_mark = ".",
      dec_mark = ","
    ) %>% 
    cols_align(align = "center",
               columns = -tipo)  %>%
    opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
    tab_options(table.align = "center", 
                row_group.font.weight = "bold",
                #table.font.size = 11,
                #table.width = 580,
                #data_row.padding = px(7)
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows =  tipo == "Total"
      )) %>% 
    tab_header(title = md(glue("**Establecimientos de alojamiento turístico según categoría**")),
               subtitle =  md(glue("{unique(x$provincia)}. Año 2019"))) %>%
    tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos del Padrón Único Nacional de Alojamientos"   )
    ) 
  
}

graf_ppales_loc <- function(x) {
  
  ppales_localidades <- x %>% 
    filter(anio == 2019) %>% 
    mutate(localidad = as_factor(localidad)) %>% 
    group_by(localidad) %>% 
    summarise(total_plazas = sum(plazas)) %>% 
    mutate(localidad = fct_lump(localidad, w = total_plazas, n = 20, other_level = "Otras localidades"))
  
  ppales_localidades <- ppales_localidades %>% 
    filter(localidad != "Otras localidades") %>% 
    mutate(localidad = ifelse(localidad == "Otras localidades", glue("Otras localidades ({sum(ppales_localidades$localidad == 'Otras localidades')})"), as.character(localidad))) %>% 
    group_by(localidad) %>% 
    summarise(total_plazas = sum(total_plazas)) %>%
    mutate(participacion_plazas = total_plazas/sum(total_plazas))
  # arrange(-participacion_plazas) %>% 
  # mutate(orden = ifelse(localidad == "Otras localidades", 16,1:n())) %>% 
  # arrange(orden) %>% 
  # select(-orden) %>% 
  # adorn_totals() 
  
  # ppales_localidades %>% 
  #   hchart(type = "treemap", 
  #          hcaes(x = paste0(localidad,"<br>",
  #                                             format(round(participacion_plazas*100, 1), big.mark = ".", decimal.mark = ","),"%"),
  #                                  value = total_plazas)) %>% 
  #   hc_title(
  #     text = "% de plazas por localidad",
  #     margin = 20,
  #     align = "left",
  #     style = list(color = "#22A884", useHTML = TRUE)
  #   )
  
  x_ppales_localidades <- c("Cantidad de Plazas", "% sobre el total")
  y_ppales_localidades <- c("{point.total_plazas:.,0f}", "{point.participacion_plazas:.,2f}%")
  
  tt_ppales_localidades <- tooltip_table(x_ppales_localidades, y_ppales_localidades)
  
  ppales_localidades %>% 
    mutate(participacion_plazas = round(participacion_plazas*100, 2)) %>% 
    arrange(-total_plazas) %>% 
    hchart(type = "column", 
           hcaes(x = localidad ,
                 y = participacion_plazas)) %>% 
    hc_title(
      text = glue("Principales localidades en {unique(x$provincia)}"),
      margin = 10,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    ) %>% 
    hc_subtitle(
      text = "Principales 20 localidades por cantidad de plazas. Año 2019",
      margin = 10,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    ) %>% 
    hc_tooltip(
      pointFormat = tt_ppales_localidades, # "{point.name} {point.pop}"
      useHTML = TRUE, 
      valueDecimals = 2) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(title = list(text = "% del total de plazas")) 
}


graf_ppales_cat <- function(x) {
  
  ppales_categorias <- x %>% 
    filter(anio == 2019) %>% 
    mutate(clasificacion_mintur = as_factor(clasificacion_mintur)) %>% 
    group_by(clasificacion_mintur) %>% 
    summarise(total_plazas = sum(plazas)) %>% 
    mutate(clasificacion_mintur = fct_lump(clasificacion_mintur, w = total_plazas, n = 15, other_level = "Otros"))
  
  
  
  ppales_categorias <- ppales_categorias %>% 
    filter(clasificacion_mintur != "Otros") %>% 
    mutate(clasificacion_mintur = ifelse(clasificacion_mintur == "Otros", glue("Otros ({sum(ppales_categorias$clasificacion_mintur == 'Otros')})"), as.character(clasificacion_mintur))) %>% 
    group_by(clasificacion_mintur) %>% 
    summarise(total_plazas = sum(total_plazas)) %>%
    mutate(participacion_plazas = total_plazas/sum(total_plazas)) 
  # arrange(-participacion_plazas) %>% 
  # mutate(orden = ifelse(clasificacion_mintur == "Otros", 16,1:n())) %>% 
  # arrange(orden) %>% 
  # select(-orden) %>% 
  # adorn_totals() 
  
  ppales_categorias
  
  x_ppales_categorias <- c("Cantidad de Plazas", "% sobre el total")
  y_ppales_categorias <- c("{point.total_plazas:.,0f}", "{point.participacion_plazas:.,2f}%")
  
  tt_ppales_categorias <- tooltip_table(x_ppales_categorias, y_ppales_categorias)
  
  
  ppales_categorias %>% 
    mutate(participacion_plazas = round(participacion_plazas*100, 2)) %>% 
    arrange(-total_plazas) %>% 
    hchart(type = "column", 
           hcaes(x = clasificacion_mintur ,
                 y = participacion_plazas)) %>% 
    hc_title(
      text = glue("Principales Categorías de Alojamiento en {unique(x$provincia)}"),
      margin = 10,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    ) %>% 
    hc_subtitle(
      text = "Principales 20 categorías por cantidad de plazas. Año 2019",
      margin = 10,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    ) %>% 
    hc_tooltip(
      pointFormat = tt_ppales_categorias, # "{point.name} {point.pop}"
      useHTML = TRUE, 
      valueDecimals = 2) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(title = list(text = "% del total de plazas"))
}



graf_time_ppales_cat <- function(x) {
  
  ppales_categorias <- x %>% 
    mutate(clasificacion_mintur = as_factor(clasificacion_mintur)) %>% 
    group_by(clasificacion_mintur, anio) %>% 
    summarise(total_plazas = sum(plazas)) %>% 
    mutate(clasificacion_mintur = fct_lump(clasificacion_mintur, w = total_plazas, n = 10, other_level = "Otros"))
  
  
  
  ppales_categorias <- ppales_categorias %>% 
    filter(clasificacion_mintur != "Otros") %>% 
    mutate(clasificacion_mintur = ifelse(clasificacion_mintur == "Otros", glue("Otros ({sum(ppales_categorias$clasificacion_mintur == 'Otros')})"), as.character(clasificacion_mintur))) %>% 
    group_by(clasificacion_mintur, anio) %>% 
    summarise(total_plazas = sum(total_plazas))
  # arrange(-participacion_plazas) %>% 
  # mutate(orden = ifelse(clasificacion_mintur == "Otros", 16,1:n())) %>% 
  # arrange(orden) %>% 
  # select(-orden) %>% 
  # adorn_totals() 
  
  x_ppales_categorias <- c("Cantidad de Plazas")
  y_ppales_categorias <- c("{point.total_plazas:.,0f}")
  
  tt_ppales_categorias <- tooltip_table(x_ppales_categorias, y_ppales_categorias)
  
  
  ppales_categorias %>% 
    hchart(type = "line", 
           hcaes(x = anio ,
                 y = total_plazas, group = clasificacion_mintur)) %>% 
    hc_title(
      text = glue("Principales Categorías de Alojamiento en {unique(x$provincia)}"),
      margin = 10,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    ) %>% 
    hc_subtitle(
      text = "Principales 20 categorías por cantidad de plazas",
      margin = 10,
      align = "left",
      style = list(color = "#22A884", useHTML = TRUE)
    ) %>% 
    hc_tooltip(
      pointFormat = tt_ppales_categorias, # "{point.name} {point.pop}"
      useHTML = TRUE, 
      valueDecimals = 2) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(title = list(text = "total de plazas"))
}


## en vez de la tabla sig. mejor poner referencia tablero puna pero ver aca abajo
## opciones de estilo mejoradas
# 
# 
# serie_puna %>%
#   filter(provincia == "Buenos Aires") %>%
#   group_by(localidad, clasificacion_mintur) %>%
#   summarise(total_establecimientos = sum(establecimientos),
#             total_plazas = sum(plazas)) %>%
#   mutate(participacion_establecimientos = total_establecimientos/sum(total_establecimientos),
#          participacion_plazas = total_plazas/sum(total_plazas)) %>%
#   arrange(-total_plazas) %>%
#   DT::datatable(filter = "top", colnames = c("Clasificación", "Establecimientos", "Plazas", "% Establecimientos", "% Plazas"),
#                 options = list(
#     lengthMenu = c(10, 15, 30),  dom = 'lfrtipB', buttons = list('copy',
#                                                list(
#                                                  extend = 'collection',
#                                                  buttons = list(list(extend = 'csv', filename = "puna"),
#                                                                 list(extend = 'excel', filename = "puna")),
#                                                  text = 'Download'
#                                                ))), rownames = F, extensions = 'Buttons') %>%
#   formatPercentage(c("participacion_establecimientos", "participacion_plazas"),
#                    digits = 1, dec.mark = ",") %>%
#   formatRound(c("total_establecimientos","total_plazas"), digits = 0, dec.mark = ",", mark = ".")
# 

#### salidas ####


serie_puna_nest_data <- serie_puna_nest_data %>% 
  mutate(.tablas_prov = map(nested_column_prov, tabla_gt_resumen),
         .plot_ppales_loc = map(nested_column_prov, graf_ppales_loc),
         .plot_ppales_cat = map(nested_column_prov, graf_ppales_cat),
         .plot_timeseries = map(nested_column_prov, graf_time_ppales_cat))

serie_puna_nest_data <- serie_puna_nest_data %>% 
  arrange(nombre_prov)

write_rds(serie_puna_nest_data, "outputs/serie_puna_nest_data.RDS")