#Llamo a las librerias.
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(plotly)
library(glue)
library(gt)
library(dnmye)
library(highcharter)
library(googlesheets4)

# falta terminar las tablas con el resumen de variable vuelos y asientos y el % de prov /nacion


source(file = "scripts/aux_function.R")
gs4_deauth()

cabotaje <- read_csv("/srv/DataDNMYE/aerocomercial/anac/tablas planas/cabotaje_2017_ene_2022_aeropuertos_completa.csv", 
                     locale = locale(encoding = "ISO-8859-1"))

# cabotaje <- read.csv("/srv/DataDNMYE/aerocomercial/anac/tablas planas/cabotaje_2017_ene_2022_aeropuertos_completa.csv", sep = ",", quote = "")
                  
# 
internacional <- read_csv("/srv/DataDNMYE/aerocomercial/anac/tablas planas/internacional_2017_ene_2022_aeropuertos_completa_vf.csv",
                          locale = locale(encoding = "ISO-8859-1"))

# 
aeropuertos <- read_xlsx("entradas/Tabla de aeropuertos.xlsx")

# cabotaje <- read.csv("entradas/cabotaje_rn_agrup.csv", fileEncoding="latin1") %>% 
#   select(-ad_ruta_natural) %>% 
#   mutate(mes = lubridate::month(Mes_Local, label = T))

aero_cabotaje_nest_data <- cabotaje %>% 
  filter(regular_noregular == 1) %>% 
  left_join(aeropuertos, by = c("ad_aeropuerto" = "aeropuerto")) %>% 
  mutate(nombre_prov = provincia) %>% 
  nest(nested_column_provs = -nombre_prov)


# internacional <- read.csv("entradas/internacional_rn_agrup.csv", fileEncoding="latin1") %>% 
#   select(-ad_ruta_natural) %>% 
#   mutate(mes = lubridate::month(Mes_Local, label = T))

aero_internacional_nest_data <- internacional %>% 
  filter(regular_noregular == 1) %>% 
  left_join(aeropuertos, by = c("ad_aeropuerto" = "aeropuerto")) %>% 
  mutate(nombre_prov = provincia) %>% 
  nest(nested_column_provs = -nombre_prov) 

#### salidas ####

# En internacional tendrías que utilizar las variables "promedio_pasajeros",
# "promedio_asientos" y "promedio_vuelos", ya que promedia los despegues y
# aterrizajes de cada aeropuerto Y en cabotaje tendrías que usar "pax_ad", "asientos_Pax" y "vuelos"

grafico_serietiempo_aero <- function(x,y) {
  
  plot <- list()
  
  if (y == "internacional") {
  df <-   x %>%
      # filter(TipodeMovimiento == "Aterrizaje") %>% 
    mutate(fecha = lubridate::ym(paste0(Año_Local, Mes_Local), locale = "es_AR.utf8")) %>% 
    group_by(ad_aeropuerto, fecha) %>%
      summarise(pasajeros = sum(promedio_pasajeros, na.rm = T),
                asientos = sum(promedio_asientos, na.rm = T),
                ) %>% 
    mutate(across(c(pasajeros, asientos), ~ ifelse(.x == .5, 1, round(.x, 0))))
  } 
  
  if (y == "cabotaje") {
    df <-   x %>%
      # filter(TipodeMovimiento == "Aterrizaje") %>% 
      mutate(fecha = lubridate::ym(paste0(Año_Local, Mes_Local), locale = "es_AR.utf8")) %>% 
      group_by(ad_aeropuerto, fecha) %>%
      summarise(pasajeros = sum(pax_ad, na.rm = T),
                asientos = sum(asientos_Pax, na.rm = T),
                ) %>% 
      mutate(across(c(pasajeros, asientos), ~ ifelse(.x == .5, 1, round(.x, 0))))
  }
  
 plot$pasajeros <-  df %>% 
    hchart("line", hcaes(x = fecha, y = pasajeros, group = ad_aeropuerto)) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_title(
      text = glue("Pasajeros en Vuelos Comerciales {unique(x$provincia)}")) 
  
 plot$asientos <-  df %>% 
    hchart("line", hcaes(x = fecha, y = asientos, group = ad_aeropuerto)) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_title(
      text = glue("Asientos en Vuelos Comerciales {unique(x$provincia)}"))
 
 plot
}

aero_cabotaje_nest_data <- aero_cabotaje_nest_data %>% 
  mutate(graficos = map(nested_column_provs, ~ grafico_serietiempo_aero(.x, y = "cabotaje"))) %>% 
  mutate(.plot_timeseries_pasajeros = map(graficos, ~ .x$pasajeros),
         .plot_timeseries_asientos = map(graficos, ~ .x$asientos)) %>% 
  select(-graficos)

aero_internacional_nest_data <- aero_internacional_nest_data %>% 
  mutate(graficos = map(nested_column_provs, ~ grafico_serietiempo_aero(.x, y = "internacional"))) %>% 
  mutate(.plot_timeseries_pasajeros = map(graficos, ~ .x$pasajeros),
         .plot_timeseries_asientos = map(graficos, ~ .x$asientos),
         ) %>% 
  select(-graficos)

##
##


tabla_aero_cabotaje <-  function(x) {
  x %>% 
    filter(Año_Local == 2019) %>% 
    group_by(ad_aeropuerto) %>%
    summarise(pasajeros = round(sum(pax_ad, na.rm = T),0)) %>% 
    arrange(-pasajeros) %>% 
    janitor::adorn_totals() %>% 
    mutate(across(-ad_aeropuerto, ~ifelse(is.na(.x), 0, .x))) %>% 
    gt()  %>% 
    cols_label(
      ad_aeropuerto  = md(""),
      pasajeros = md("**Pasajeros en Vuelos Regulares**")
    )  %>%
    fmt_number(
      columns = 2,
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>% 
    cols_align(align = "center",
               columns = c(pasajeros))  %>%
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
        rows =  ad_aeropuerto == "Total"
      )) %>% 
    tab_header(title = md(glue("**Vuelos de Cabotaje**")),
               subtitle =  md(glue("{unique(x$provincia)}. Año 2019"))) %>%
    tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos de la ANAC"   )
    )
}  



tabla_aero_internacional <-  function(x) {
  x %>% 
    filter(Año_Local == 2019) %>% 
    group_by(ad_aeropuerto) %>%
    summarise(pasajeros = round(sum(promedio_pasajeros, na.rm = T),0)) %>% 
    arrange(-pasajeros) %>% 
    janitor::adorn_totals() %>% 
    mutate(across(-ad_aeropuerto, ~ifelse(is.na(.x), 0, .x))) %>% 
    gt()  %>% 
    cols_label(
      ad_aeropuerto  = md(""),
      pasajeros = md("**Pasajeros en Vuelos Regulares**")
    )  %>%
    fmt_number(
      columns = 2,
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>% 
    cols_align(align = "center",
               columns = c(pasajeros))  %>%
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
        rows =  ad_aeropuerto == "Total"
      )) %>% 
    tab_header(title = md(glue("**Vuelos Internacionales**")),
               subtitle = md(glue("{unique(x$ad_provincia_nombre)}. Año 2019"))) %>%
    tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos de la ANAC"   )
    )
}  


aero_internacional_nest_data <- aero_internacional_nest_data %>% 
  mutate(.tablas_prov = map(nested_column_provs,  tabla_aero_internacional)) %>% 
  arrange(nombre_prov)

aero_cabotaje_nest_data <- aero_cabotaje_nest_data %>% 
  mutate(.tablas_prov = map(nested_column_provs,  tabla_aero_cabotaje)) %>% 
  arrange(nombre_prov)

write_rds(aero_internacional_nest_data, file= "outputs/aero_internacional_nest_data.RDS")
write_rds(aero_cabotaje_nest_data, file= "outputs/aero_cabotaje_nest_data.RDS")
