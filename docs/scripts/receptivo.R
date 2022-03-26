library(highcharter)
library(tidyverse)
library(haven)
library(lubridate)
library(gt)
library(glue)
source(file = "scripts/aux_function.R")

visitantes_dnm <-  read_csv2("/srv/DataDNMYE/turismo_internacional/turismo_internacional_visitantes.zip",
                             locale = locale(encoding = "ISO-8859-1"))

# visitantes_dnm %>% 
#   filter(prov == "CABA-GBA") %>% 
#   select(paso_publ) %>% 
#   unique()

# turismo receptivo (visitantes: turistas y excursionistas)
receptivo <-  visitantes_dnm %>%
  filter(turismo_internac == "Receptivo") %>%
  group_by(anio, prov, paso_publ) %>%
  summarise(visitantes = round(sum(casos_ponderados))) %>%
  ungroup() %>%
  mutate(
    prov = case_when(
      prov == "CABA-GBA" ~ "Buenos Aires",
      prov == "Resto prov. Bs. As." ~ "Buenos Aires",
      prov == "Tierra del Fuego" ~ "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
      TRUE ~ prov),
    paso_publ = str_replace_all(paso_publ, "Aero ", "Aeropuerto "),
    concatenado = paste(paso_publ, prov)) %>%
  group_by(anio) %>% 
  mutate(participacion_pais = visitantes/sum(visitantes)) %>% 
  ungroup()


#### salidas

# TURTISAS POR PASOS
receptivo_nest_data <- receptivo %>% 
  mutate(nombre_prov = prov) %>% 
  nest(nested_column_provs = -prov)

tabla_pasos <-  function(x) {
  x %>% 
    filter(anio <= 2019) %>% 
    filter(anio == last(anio)) %>% 
    arrange(-visitantes) %>% 
    mutate(rank = 1:n(),
           paso_publ = ifelse(rank <= 5,paso_publ,"Otros" )) %>% 
    group_by(paso_publ) %>% 
    summarise(visitantes = sum(visitantes),
              participacion_pais = sum(participacion_pais)) %>% 
    mutate(orden= ifelse(paso_publ == "Otros",2,1)) %>% 
    arrange(orden,-visitantes) %>% 
    select(-orden) %>% 
    janitor::adorn_totals() %>% 
    gt()  %>%
    cols_label(
      paso_publ  = md(""),
      visitantes = md("**Visitantes**"),
      participacion_pais = md("**% del país**"),
    )  %>%
    fmt_number(
      columns = 2,
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    fmt_percent(columns = 3, 
                decimals = 1, 
                sep_mark = ".", 
                dec_mark = ",") %>%
    cols_align(align = "center",
               columns = c(visitantes, participacion_pais))  %>%
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
        rows =  paso_publ == "Total"
      )) %>% 
    tab_header(title = md(glue("**Pasos Internacionales {unique(x$nombre_prov)}**")),
               subtitle = "Visitantes no residentes por paso internacional. Año 2019") %>%
    tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos de DNM"
                                     #"**Fuente**: DNMyE ({unique(indicadores_tot$fuente)[1]}, {unique(indicadores_tot$fuente)[2]}, {unique(indicadores_tot$fuente)[3]})"
    )
    )
}  


receptivo_nest_data <- receptivo_nest_data %>% 
  mutate(.tablas_prov = map(nested_column_provs,  tabla_pasos))



#series temporales por provincia


plot_timeseries_pasos <- function(x) {
  
  x  %>% 
    # se puede definir un filtro para no graficar los pasos con menos
    # de un % del total de visitantes para la provincia
    hchart("line", hcaes(x = anio, y = visitantes, group = paso_publ)) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_title(
      text = glue("{unique(x$nombre_prov)}")
    )
   
}

receptivo_nest_data <- receptivo_nest_data %>% 
  mutate(.plot_timeseries = map(nested_column_provs, 
                                plot_timeseries_pasos)
  ) 


receptivo_nest_data <- receptivo_nest_data %>% 
  ungroup() %>% 
  select(-nested_column_provs)

write_rds(receptivo_nest_data , file = "outputs/receptivo_nest_data.RDS")