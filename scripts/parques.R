#parques
library(dnmye)
library(readxl)
library(tidyverse)
library(glue)
library(gt)
library(highcharter)

source("scripts/aux_function.R")

base_pn <- read_excel("/srv/DataDNMYE/parques_nacionales/pivot_pn.xlsx", sheet = 2) %>% 
  mutate(parque_nacional = limpiar_texto(parque_nacional))

provincias <- read_excel("/srv/DataDNMYE/parques_nacionales/provincias.xlsx") %>% 
  add_row(parque_nacional = "Pizarro", provincia = "Salta") %>% 
  mutate(parque_nacional = ifelse(parque_nacional == "Nogalar de los Toldos", "El Nogalar de Los Toldos", parque_nacional),
         etiq_parque = parque_nacional,
         parque_nacional = limpiar_texto(parque_nacional))

base_pn <- left_join(base_pn, provincias) %>% 
  filter(anio >= 2012)

parques_nest_data <- base_pn %>% 
  mutate(nombre_prov = as_factor(provincia)) %>% 
  nest(nested_column_indicadores = -nombre_prov)

tabla_provincial <- function(x) {
  x %>% 
    filter(anio == 2019) %>% 
    group_by(etiq_parque,residencia) %>% 
    summarise(visitantes = sum(visitantes, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = etiq_parque, names_from = residencia, values_from = visitantes) %>% 
    janitor::clean_names() %>% 
    mutate( total = no_residentes + residentes) %>% 
    arrange(-total) %>% 
    janitor::adorn_totals() %>% 
    gt() %>% 
    cols_label(
      etiq_parque = md(""),
      no_residentes = md("**No Residentes**"),
      residentes = md("**Residentes**"),
      total = md("**Total**")
    )  %>% 
    fmt_number(
      columns = 2:4,
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>% 
    cols_align(align = "center",
               columns = c(residentes, no_residentes, total))  %>%
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
        rows =  etiq_parque == "Total"
      )) %>% 
    tab_header(title = md(glue("**Visitantes a Áreas Protegidas Naturales Nacionales<br>según residencia**")),
               subtitle =  md(glue("{unique(x$provincia)}. Año 2019"))) %>%
    tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos de la APN"   )
    )
}


grafico_serietiempo <- function(x) {
  
   x %>%
    mutate(fecha = lubridate::ym(paste0(anio, mes), locale = "es_AR.utf8"), grupo = paste(etiq_parque, str_to_title(residencia), sep =": ")) %>% 
    group_by(grupo, fecha) %>%
    summarise(visitantes = round(sum(visitantes, na.rm = T), 0)) %>% 
    # se puede definir un filtro para no graficar los pasos con menos
    # de un % del total de visitantes para la provincia
    hchart("line", hcaes(x = fecha, y = visitantes, group = grupo)) %>%  
    hc_title(
      text = glue("Visitantes a Áreas Protegidas Naturales Nacionales - {unique(x$provincia)}"))
}


parques_nest_data <- parques_nest_data %>% 
  mutate(.tablas_prov = map(nested_column_indicadores, tabla_provincial),
         .plot_timeseries = map(nested_column_indicadores, grafico_serietiempo))

parques_nest_data <- parques_nest_data %>% 
  arrange(nombre_prov)

write_rds(parques_nest_data, "outputs/parques_nest_data.RDS")
