library(highcharter)
library(tidyverse)
library(haven)
library(lubridate)
library(gt)
library(glue)
source(file = "scripts/aux_function.R")
#### EVYTH ####
# revisar si esto sirve o habría que generar algo nuevo más limpio
# agregar N turistas!!
#BASES EVYTH
evyth <- read_sav("/srv/DataDNMYE/evyth/mensual/EVyTH_Ondas22012a202111_USU4_VIAJES-PERSONAS_(VISITANTES-COMPLETA).zip") %>%
  mutate(mes3  = as.numeric(substr(Mes,5,6))) %>%
  filter(anio >= 2012)

#sjPlot::view_df(evyth_2019)
#TIPO DE CAMBIO PARA TENER GASTO EN USD
tipos_de_cambio <- read.csv("/srv/DataDNMYE/mulc/base_tipos_de_cambio.csv",fileEncoding="latin1") %>%
  mutate(Fecha = mdy(Fecha)) %>%
  select(anio = Año, mes = Mes, fecha = Fecha,tc_oficial = EE.UU..Oficial) %>%
  mutate(tc_oficial = as.numeric(tc_oficial))

#CALCULO tc mensual:
tc_mensual <- tipos_de_cambio %>%
  group_by(anio,mes) %>%
  summarise(tc_oficial = mean(tc_oficial,na.rm=T))

evyth <- left_join(evyth, tc_mensual,by=c("anio"="anio","mes3"="mes"))
evyth$Provincia <- as_factor(evyth$provincia_destino)

# para unir Prov Bs As en una sola categoria
evyth <- evyth %>%
  mutate(Provincia = fct_collapse(Provincia,
                                  "Buenos Aires" = c("Partidos del GBA (Pcia. Bs. As.)",
                                                     "Buenos Aires (Resto)")
  )
  ) %>%
  filter(anio < 2020)

levels(evyth$Provincia)[levels(evyth$Provincia) == "Tierra Del Fuego'"] <- "Tierra del Fuego, Antártida e Islas del Atlántico Sur"

evyth_tabla_pais <- evyth %>%
  filter(arg_o_ext == 1) %>%
  group_by(anio) %>%
  summarise(visitantes_gau = sum(pondera),
            turistas = sum(pondera[tipo_visitante == 1]),
            gasto_visitantes_pesos_real = sum(pondera*gasto_pc),
            gasto_visitantes_usd = sum(pondera*gasto_pc / tc_oficial),
            estadia_media_turistas = sum(pondera[tipo_visitante == 1] *px07[tipo_visitante == 1])/sum(pondera[tipo_visitante == 1]),
            gasto_promedio_usd_turistas = sum(pondera[tipo_visitante == 1]*gasto_pc[tipo_visitante == 1] / tc_oficial[tipo_visitante == 1])/sum(pondera[tipo_visitante == 1]))

evyth_tabla_pais_2019 <- evyth_tabla_pais %>%
  filter(anio == 2019) %>%
  pivot_longer(everything(),names_to="indicador",values_to = "total_pais")

evyth_tabla_provincias <- evyth %>%
  filter(arg_o_ext == 1) %>%
  group_by(Provincia, anio, trimestre) %>%
  summarise(visitantes_gau = sum(pondera),
            turistas = sum(pondera[tipo_visitante == 1]),
            gasto_visitantes_pesos_real = sum(pondera*gasto_pc),
            gasto_visitantes_usd = sum(pondera*gasto_pc / tc_oficial),
            estadia_media_turistas = sum(pondera[tipo_visitante == 1] *px07[tipo_visitante == 1])/sum(pondera[tipo_visitante == 1]),
            gasto_promedio_usd_turistas = sum(pondera[tipo_visitante == 1]*gasto_pc[tipo_visitante == 1] / tc_oficial[tipo_visitante == 1])/sum(pondera[tipo_visitante == 1]))

evyth_tabla_provincias_2019 <- evyth %>%
  filter(arg_o_ext == 1 & anio == 2019) %>% 
  group_by(Provincia) %>%
  summarise(visitantes_gau = sum(pondera),
            turistas = sum(pondera[tipo_visitante == 1]),
            gasto_visitantes_pesos_real = sum(pondera*gasto_pc),
            gasto_visitantes_usd = sum(pondera*gasto_pc / tc_oficial),
            estadia_media_turistas = sum(pondera[tipo_visitante == 1] *px07[tipo_visitante == 1])/sum(pondera[tipo_visitante == 1]),
            gasto_promedio_usd_turistas = sum(pondera[tipo_visitante == 1]*gasto_pc[tipo_visitante == 1] / tc_oficial[tipo_visitante == 1])/sum(pondera[tipo_visitante == 1])) %>%
  pivot_longer(-c(Provincia),names_to="indicador",values_to = "valor_provincia")

evyth_indicadores_2019 <- left_join(evyth_tabla_provincias_2019 , evyth_tabla_pais_2019) %>%
  filter(indicador != "gasto_visitantes_pesos_real" & !is.na(Provincia)) %>%
  mutate(unidad = case_when(indicador == "visitantes_gau" ~ "personas",
                            indicador == "turistas" ~ "personas",
                            indicador == "gasto_visitantes_pesos_real" ~ "pesos",
                            indicador == "gasto_visitantes_usd" ~ "dólares",
                            indicador == "estadia_media_turistas" ~ "noches",
                            indicador == "gasto_promedio_usd_turistas" ~ "dólares")) %>%
  mutate(indicador = case_when(indicador == "visitantes_gau" ~ "Visitantes",
                               indicador == "turistas" ~ "Turistas",
                               indicador == "gasto_visitantes_usd" ~ "Gasto Total USD (visitantes)",
                               indicador == "estadia_media_turistas" ~ "Estadía media (turistas)",
                               indicador == "gasto_promedio_usd_turistas" ~ "Gasto promedio USD (turistas)")) %>%
  mutate(fuente = "Encuesta de Viajes y Turismo de los Hogares") %>%
  mutate(unidad = str_to_sentence(unidad),
         indicador = str_to_sentence(indicador),
         part_pais = valor_provincia / total_pais) %>%
  mutate(part_pais = case_when(
    indicador %in% c("Estadía media (turistas)",
                     "Gasto promedio usd (turistas)") ~ as.character(round(part_pais,2)),
    indicador == "Part. de los residentes (%)" ~ "-",
    TRUE ~ paste0(round(part_pais * 100, 1), "%"))
  ) %>%
  select(Provincia, indicador, valor_provincia, total_pais, part_pais, unidad, fuente)

perfil_transporte <- evyth %>%
  filter(arg_o_ext == 1 & anio == 2019 & tipo_visitante == 1) %>%
  mutate(categorias = as_factor(px09_t)%>%
           fct_recode("Otros" = "Resto",
                      "Automóvil" = "Automovil",
                      "Ómnibus" = "Omnibus") %>%
           fct_relevel("Otros", after = Inf)) %>%
  group_by(Provincia, categorias) %>% #tipo de transporte
  summarise(n = sum(pondera)) %>%
  mutate(valor =  n/sum(n),
         indicador = "Tipo de transporte") %>%
  select(-n) %>% 
  mutate(nombre_prov = Provincia) %>%
  nest(nested_perfil_transporte = -Provincia)

perfil_motivo <- evyth %>%
  filter(arg_o_ext == 1 & anio == 2019 & tipo_visitante == 1) %>%
  mutate(categorias = as_factor(px10_1_t) %>%
           fct_recode("Otros" = "Resto") %>%
           fct_relevel("Otros", after = Inf)) %>%
  group_by(Provincia, categorias) %>% #tipo de transporte
  summarise(n = sum(pondera)) %>%
  mutate(valor =  n/sum(n),
         indicador = "Motivo de viaje") %>%
  select(-n) %>% 
  mutate(nombre_prov = Provincia) %>%
  nest(nested_perfil_motivo = -Provincia)

perfil_edad <- evyth %>%
  filter(arg_o_ext == 1 & anio == 2019 & tipo_visitante == 1) %>%
  mutate(categorias = as_factor(p006_agrup)) %>%
  group_by(Provincia, categorias) %>% #tipo de transporte
  summarise(n = sum(pondera),
            indicador = "Edad") %>%
  mutate(valor =  n/sum(n)) %>%
  select(-n) %>% 
  mutate(nombre_prov = Provincia) %>%
  nest(nested_perfil_edad = -Provincia)

perfil_alojamiento <- evyth %>%
  filter(arg_o_ext == 1 & anio == 2019 & tipo_visitante == 1) %>%
  mutate(categorias = as_factor(px08_agrup)) %>%
  mutate(categorias = fct_recode(categorias,
                                 "Hotel" = "Hotel o similar hasta 4 estrellas",
                                 "Hotel" = "Hotel similar 4 o 5 estrellas",
                                 "Otros" = "Camping",
                                 "Otros" = "Resto") %>%
           fct_relevel("Otros", after = Inf)
  ) %>%
  group_by(Provincia, categorias) %>% #tipo de transporte
  summarise(n = sum(pondera),
            indicador = "Tipo de alojamiento") %>%
  mutate(valor =  n/sum(n)) %>%
  select(-n) %>% 
  mutate(nombre_prov = Provincia) %>%
  nest(nested_perfil_alojamiento = -Provincia)

perfil_evyth <- perfil_motivo %>% 
  left_join(perfil_transporte, by = "Provincia") %>% 
  left_join(perfil_alojamiento, by = "Provincia") %>% 
  left_join(perfil_edad, by = "Provincia")

# actividades
actividades <- evyth %>%
  filter(arg_o_ext == 1 & anio == 2019 & tipo_visitante == 1) %>%
  select(68:80, pondera, Provincia) %>%
  pivot_longer(-c(pondera, Provincia), names_to = "actividad") %>%
  mutate(value = ifelse(value %in% c(2,9) | is.na(value), 0, value)) %>%
  mutate(
    actividad = case_when(
      actividad == "px17_2_1" ~ "Actividades rurales (Estancias, granjas, etc.)",
      actividad =="px17_2_2"~ "Spa, termas, etc.",
      actividad =="px17_2_3"~ "Playas (mar, río, lago)",
      actividad =="px17_2_4"~ "Esqui, snowboard, u otro deporte de nieve",
      actividad =="px17_2_5"~"Deportes de aventura (montanismo, rafting, etc.)",
      actividad =="px17_2_6"~"Caza o  pesca",
      actividad =="px17_2_7"~"Espectáculos deportivos",
      actividad =="px17_2_8"~"Actos o festividades religiosas",
      actividad =="px17_2_9"~"Teatro, cine, o conciertos",
      actividad =="px17_2_10"~"Museos, monumentos, parques temáticos, etc.",
      actividad =="px17_2_11"~ "Parques nacionales o provinciales, reservas, etc.",
      actividad =="px17_2_12"~"Casinos o bingos",
      actividad =="px17_2_13" ~ "Salidas nocturnas (discotecas, bares, etc.)"
    ) %>% as_factor()
  ) %>%
  group_by(Provincia, actividad) %>%
  summarise(part = sum(pondera[value == 1])/sum(pondera)) %>%
  group_by(Provincia) %>%
  mutate(actividad = fct_lump(f = actividad, n =9, other_level = "Otras", w = part)) %>%
  group_by(Provincia, actividad) %>%
  summarise(valor = sum(part)) %>%
  mutate(nombre_prov = Provincia) %>%
  nest(nested_column_actividades = -Provincia)

#localidades

evyth_tabla_localidades_2019 <- evyth %>%
  filter(arg_o_ext == 1 & anio == 2019) %>% 
  group_by(Provincia, localidad_destino) %>%
  summarise(visitantes_gau = sum(pondera)) %>%
  mutate(localidad_destino = as_factor(localidad_destino)) %>% 
  mutate(localidad_destino = fct_lump(localidad_destino, n = 14, w = visitantes_gau, other_level = "Otras"))%>% 
  mutate(localidad_destino = fct_recode(localidad_destino, "Ns.Nc." = "999", "Otras" = "998"))%>% 
  group_by(Provincia, localidad_destino) %>%
  summarise(visitantes = sum(visitantes_gau)) %>%
  group_by(Provincia) %>% 
  mutate(visitantes = visitantes/sum(visitantes)) %>% 
  pivot_longer(-c(Provincia, localidad_destino),names_to="indicador",values_to = "valor") %>% 
  mutate(nombre_prov = Provincia) %>%
  nest(nested_column_localidades = -Provincia)

#### salidas ####
gt_evyth_tabla_pais_2019 <- evyth_tabla_pais_2019 %>%
  filter(!indicador %in% c("gasto_visitantes_pesos_real", "anio")) %>%
  mutate(unidad = case_when(indicador == "visitantes_gau" ~ "personas",
                            indicador == "turistas" ~ "personas",
                            indicador == "gasto_visitantes_pesos_real" ~ "pesos",
                            indicador == "gasto_visitantes_usd" ~ "dólares",
                            indicador == "estadia_media_turistas" ~ "noches",
                            indicador == "gasto_promedio_usd_turistas" ~ "dólares")) %>%
  mutate(indicador = case_when(indicador == "visitantes_gau" ~ "Visitantes",
                               indicador == "turistas" ~ "Turistas",
                               indicador == "gasto_visitantes_usd" ~ "Gasto Total USD (visitantes)",
                               indicador == "estadia_media_turistas" ~ "Estadía media (turistas)",
                               indicador == "gasto_promedio_usd_turistas" ~ "Gasto promedio USD (turistas)")) %>%
  mutate(unidad = str_to_sentence(unidad),
         indicador = str_to_sentence(indicador)) %>%
  select(indicador, total_pais, unidad) %>%
  gt() %>%
  cols_label(
    indicador  = md("**Indicador**"),
    total_pais = md("**Total País**"),
    unidad     = md("**Unidad de medida**"),
  )  %>%
  fmt_number(
    columns = c("total_pais"),
    rows = c(1:3),
    decimals = 0,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  fmt_number(
    columns = c("total_pais"),
    rows = c(4,5),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  cols_align(align = "center",
             columns = c(total_pais, unidad))  %>%
  opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
  tab_options(table.align = "center",
              row_group.font.weight = "bold",
  ) %>%
  tab_header(title = md(glue("**Turismo Interno**")),
             subtitle = "Total Nacional Año 2019") %>%
  tab_source_note(source_note = md(
    glue(
      "**Fuente:** Encuesta de Viajes y Turismo de los Hogares"
      #"**Fuente**: DNMyE ({unique(indicadores_oferta$fuente)[1]}, {unique(indicadores_oferta$fuente)[2]}, {unique(indicadores_oferta$fuente)[3]})"
    )
  ))

#treemap visitantes por provincia
evyth_treemap_vis_nac <- evyth_indicadores_2019 %>%
  filter(indicador == "Visitantes") %>%
  hchart(type = "treemap", hcaes(x = paste0(Provincia,"<br>",
                                            format(round(valor_provincia, 0),
                                                   big.mark = ".", decimal.mark = ",")),
                                 value = valor_provincia)) %>%
  hc_title(
    text = "Visitantes por Provincia. 2019",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  )

#treemap de gasto por provincias
evyth_treemap_gasto_nac <- evyth_indicadores_2019 %>%
  filter(indicador == "Gasto total usd (visitantes)") %>%
  hchart(type = "treemap", hcaes(x = paste0(Provincia,"<br>",
                                            format(round(valor_provincia/1000, 0), big.mark = ".", decimal.mark = ",")),
                                 value = valor_provincia)) %>%
  hc_title(
    text = "Gasto total de visitantes por Provincia (Miles de usd.). 2019",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  )

#### provinciales ####
#tabla por provincias
evyth_nest_data <- evyth_indicadores_2019 %>%
  mutate(nombre_prov = Provincia) %>%
  nest(nested_column_indicadores = -Provincia)

evyth_nest_data <- left_join(evyth_nest_data, perfil_evyth, by = "Provincia")
evyth_nest_data <- left_join(evyth_nest_data, actividades, by = "Provincia")
evyth_nest_data <- left_join(evyth_nest_data, evyth_tabla_localidades_2019, by = "Provincia")


evyth_gt_tables_prov <- function(x) {
  x %>%
    mutate(
           valor_provincia = ifelse(indicador== "Gasto total usd (visitantes)", valor_provincia/10^6, valor_provincia),
           total_pais = ifelse(indicador== "Gasto total usd (visitantes)",total_pais/10^6, total_pais),
           indicador = ifelse(indicador== "Gasto total usd (visitantes)", "Gasto total millones usd (visitantes)", indicador))%>% 
    select(-c(fuente, nombre_prov)) %>%
    #relocate(indicador, valor_ruta, total_pais, unidad, fuente) %>%
    gt() %>%
    cols_label(
      indicador  = md("**Indicador**"),
      valor_provincia = md("**Provincia**"),
      total_pais = md("**Total País**"),
      part_pais  = md("**Provincia / País**"),
      unidad     = md("**Unidad de medida**"),
    )  %>%
    fmt_number(
      columns = c("valor_provincia", "total_pais"),
      rows = c(1:3),
      decimals = 0,
      sep_mark = ".",
      dec_mark = ",",
    ) %>%
    fmt_number(
      columns = c("valor_provincia", "total_pais"),
      rows = c(4,5),
      decimals = 2,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    cols_align(align = "center",
               columns = c(valor_provincia, total_pais, unidad))  %>%
    opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
    tab_options(table.align = "center",
                row_group.font.weight = "bold",
    ) %>%
    tab_header(title = md(glue("**Turismo Interno {unique(x$nombre_prov)}**")),
               subtitle = "Estadísticas de Turismo Año 2019") %>%
    tab_source_note(source_note = md(
      glue(
        "**Fuente:** Encuesta de Viajes y Turismo de los Hogares"
        #"**Fuente**: DNMyE ({unique(indicadores_oferta$fuente)[1]}, {unique(indicadores_oferta$fuente)[2]}, {unique(indicadores_oferta$fuente)[3]})"
      )
    ))
}

perfiles_evyth_tables <- function(x) {
  titulo <- unique(x$indicador)
  if (!"Edad" %in% x$indicador) {
    x <- x %>%
      select(-c(nombre_prov, indicador)) %>%
      # group_by(indicador) %>%
      #relocate(indicador, valor_ruta, total_pais, unidad, fuente) %>%
      filter(valor != 0) %>%
      arrange(desc(valor)) %>%
      mutate(orden = ifelse(categorias == "Otros", Inf,row_number())) %>%
      arrange(orden) %>%
      select(-orden)
  } else {
    x <-  x %>%
      select(-c(nombre_prov, indicador))
      # group_by(indicador) %>%
      #relocate(indicador, valor_ruta, total_pais, unidad, fuente) %>%
  }
  
  x %>% 
    gt() %>%
    cols_label(
      categorias = md(""),
      valor = md("**% del total de turistas**")
    )  %>%
    fmt_percent(
      columns = "valor",
      # rows = c(1:3),
      decimals = 1,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    cols_align(align = "center",
               columns = "valor")  %>%
    cols_align(align = "left",
               columns = "categorias")  %>%
    opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
    tab_options(table.align = "center",
                row_group.font.weight = "bold",
    ) %>%
    tab_header(title = md(glue("**{titulo}**")),
               subtitle = "Estadísticas de Turismo Año 2019") %>%
    tab_source_note(source_note = md(
      glue(
        "**Fuente:** Encuesta de Viajes y Turismo de los Hogares"
      )
    )) %>%
    tab_style(
      style = cell_text(weight =  "bold"),
      locations = cells_row_groups()
    )
}

actividades_evyth_tables <- function(x) {
  x %>%
    select(-nombre_prov) %>%
    filter(valor != 0) %>%
    arrange(desc(valor)) %>%
    mutate(orden = ifelse(actividad == "Otras", Inf,row_number())) %>%
    arrange(orden) %>%
    select(-orden) %>%
    #relocate(indicador, valor_ruta, total_pais, unidad, fuente) %>%
    gt() %>%
    cols_label(
      actividad = md(""),
      valor = md("**% del total de turistas**")
    )  %>%
    fmt_percent(
      columns = "valor",
      # rows = c(1:3),
      decimals = 1,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    cols_align(align = "center",
               columns = "valor")  %>%
    cols_align(align = "left",
               columns = "actividad")  %>%
    opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
    tab_options(table.align = "center",
                row_group.font.weight = "bold",
    ) %>%
    tab_header(title = md(glue("**Actividades realizadas en {unique(x$nombre_prov)}**")),
               subtitle = "Participación sobre el total de turistas. Año 2019") %>%
    tab_source_note(source_note = md(
      glue(
        "**Fuente:** Encuesta de Viajes y Turismo de los Hogares"
      )
    ))
}


localidad_evyth_tables <- function(x) {
  x %>%
    select(-nombre_prov) %>%
    filter(valor != 0) %>%
    arrange(desc(valor)) %>%
    mutate(orden = ifelse(localidad_destino == "Otras", Inf,row_number())) %>%
    arrange(orden) %>%
    select(-c(orden, indicador)) %>%
    #relocate(indicador, valor_ruta, total_pais, unidad, fuente) %>%
    gt() %>%
    cols_label(
      localidad_destino = md(""),
      valor = md("**% del total de visitantes**")
    )  %>%
    fmt_percent(
      columns = "valor",
      # rows = c(1:3),
      decimals = 1,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    cols_align(align = "center",
               columns = "valor")  %>%
    cols_align(align = "left",
               columns = "localidad_destino")  %>%
    opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
    tab_options(table.align = "center",
                row_group.font.weight = "bold",
    ) %>%
    tab_header(title = md(glue("**Principales localidades de destino en {unique(x$nombre_prov)}**")),
               subtitle = "Participación sobre el total de visitantes. Año 2019") %>%
    tab_source_note(source_note = md(
      glue(
        "**Fuente:** Encuesta de Viajes y Turismo de los Hogares"
      )
    ))
}

evyth_nest_data <- evyth_nest_data %>%
  mutate(.tablas_prov = map(nested_column_indicadores,  evyth_gt_tables_prov),
         .actividades = map(nested_column_actividades, actividades_evyth_tables),
         .localidades = map(nested_column_localidades, localidad_evyth_tables))

perfil_evyth <- perfil_evyth %>% 
    mutate(.motivo = map(nested_perfil_motivo, perfiles_evyth_tables),
           .alojamiento = map(nested_perfil_alojamiento, perfiles_evyth_tables),
           .transporte = map(nested_perfil_transporte, perfiles_evyth_tables),
           .edad = map(nested_perfil_edad, perfiles_evyth_tables))

#series temporales por provincias
evyth_nest_data$nested_column_timeseries <- evyth_tabla_provincias %>%
  select(-gasto_visitantes_pesos_real) %>%
  pivot_longer(-c(Provincia, anio, trimestre),names_to="indicador",values_to = "valor_provincia") %>%
  mutate(unidad = case_when(indicador == "visitantes_gau" ~ "personas",
                            indicador == "turistas" ~ "personas",
                            indicador == "gasto_visitantes_pesos_real" ~ "pesos",
                            indicador == "gasto_visitantes_usd" ~ "dólares",
                            indicador == "estadia_media_turistas" ~ "noches",
                            indicador == "gasto_promedio_usd_turistas" ~ "dólares")) %>%
  mutate(indicador = case_when(indicador == "visitantes_gau" ~ "Visitantes",
                               indicador == "turistas" ~ "Turistas",
                               indicador == "gasto_visitantes_usd" ~ "Gasto Total USD (visitantes)",
                               indicador == "estadia_media_turistas" ~ "Estadía media (turistas)",
                               indicador == "gasto_promedio_usd_turistas" ~ "Gasto promedio USD (turistas)")) %>%
  mutate(fuente = "Encuesta de Viajes y Turismo de los Hogares") %>%
  mutate(unidad = str_to_sentence(unidad),
         indicador = str_to_sentence(indicador),
         periodo = paste0(trimestre, "° ", anio)) %>%
  mutate(nombre_prov = Provincia) %>%
  nest(nested_column = -Provincia) %>%
  .$nested_column

# evyth_nest_data$nested_column_timeseries <- evyth_nest_data_timeseries$nested_column
# rm(evyth_nest_data_timeseries)

plot_timeseries_evyth <- function(x) {

  highchart() %>%
    hc_xAxis(categories = unique(x$periodo)) %>%
    hc_yAxis_multiples(
      list(offset = 0, min = 0, title = list(text = "Visitantes"), opposite = FALSE, top = "0%", height = "25%"),
      list(offset = 0, min = 0, showLastLabel = FALSE, opposite = F, title = list(text = "Dólares (visitantes)"),  height = "25%", top = "25%"),
      list(offset = 0, min = 0, showLastLabel = FALSE, opposite = F, title = list(text = "Cantidad de Días"), height = "25%", top = "50%"),
      list(offset = 0, min = 0, showLastLabel = FALSE, opposite = F, title = list(text = "Dólares (turistas)"), height = "25%", top = "75%")) %>%
    # hc_yAxis_multiples(create_yaxis(naxis = 4, title = list(text = c("Visitantes", "Visitantes", "Visitantes", "Visitantes")))) %>%
    hc_add_series(data = round(x$valor_provincia[x$indicador == "Visitantes"], 0),
                  yAxis = 0, name = "Visitantes") %>%
    hc_add_series(data= round(x$valor_provincia[x$indicador == "Gasto total usd (visitantes)"], 0),
                  yAxis = 1, name = "Gasto total usd (visitantes)") %>%
    hc_add_series(data = round(x$valor_provincia[x$indicador == "Estadía media (turistas)"], 1),
                  yAxis = 2, name = "Estadía media (turistas)") %>%
    hc_add_series(data= round(x$valor_provincia[x$indicador == "Gasto promedio usd (turistas)"],0),
                  name = "Gasto promedio usd (turistas)",
                  yAxis = 3) %>%
    hc_title(
      text = glue("{unique(x$nombre_prov)}")
    )
}

evyth_nest_data <- evyth_nest_data %>%
  mutate(.plot_timeseries = map(nested_column_timeseries,
                                plot_timeseries_evyth)
  )

evyth_nest_data <- evyth_nest_data %>% 
  left_join(perfil_evyth, by = "Provincia")

evyth_nest_data <- evyth_nest_data[,c(1,grep("^\\.", colnames(evyth_nest_data)))]

evyth_nest_data <- evyth_nest_data %>%
  ungroup()


evyth_nest_data %>% 
  write_rds(file = "outputs/evyth_nest_data.RDS")
