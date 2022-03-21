# demanda


# datos eoh
eoh <- read_rds("/srv/DataDNMYE/eoh/bases/eoh.rds")

eoh %>% 
  select(ANIO, PROVINCIA, LOCALIDAD, P5_TR1, P5_TN1, w)

# library(tidyverse)
library(gt)
library(glue)
source("scripts/auxiliar/funciones.R")

#Levanto la base de la carpeta "data"
eoh_rn <- read.csv(file = "data/eoh_rn.csv",encoding = "UTF-8") %>% 
  rename(ruta_natural = ruta_nutural)

#Recodificación del nuevo agrupamiento

eoh_rn <- eoh_rn %>% 
  mutate(ruta_natural = case_when(
    ruta_natural %in% c("Valles, Quebradas y Yungas") ~ "Puna - Valles, Quebradas y Yungas",
    ruta_natural %in% c("Altos Andes", "Desiertos y Volcanes") ~ "Altos Andes - Desiertos y Volcanes",
    ruta_natural %in% c("Litoral y Grandes Ríos", "Delta") ~ "Litoral y Los Grandes Ríos - Delta - Iberá",
    ruta_natural %in% c("Patagonia Andina", "Ruta de la Estepa", "Mar Patagónico", "Patagonia Austral") ~ "Patagonia Andina - Estepa - Mar Patagónico - Patagonia Austral",
    ruta_natural %in% c("Fin del Mundo") ~ "Fin del Mundo - Continente Blanco",
    TRUE ~ ruta_natural),
    ruta_natural = limpio_texto(ruta_natural))

#Limpieza de nombres

# ruta_natural=="Altos Andes - Desiertos y Volcanes"~"altos_andes__desiertos_y_volcanes",
# ruta_natural=="Fin del Mundo - Continente Blanco"~"fin_del_mundo__continente_blanco",
# ruta_natural=="Gran Chaco"~"gran_chaco",
# ruta_natural=="Litoral y Los Grandes Ríos - Delta - Iberá"~"litoral_y_los_grandes_rios__delta_ibera",
# ruta_natural=="Llanuras y Costa Atlántica"~"llanuras_y_costa_atlantica",
# ruta_natural=="Patagonia Andina - Estepa - Mar Patagónico - Patagonia Austral"~"patagonia_andina__estepa__mar_patagonico__patagonia_austral",
# ruta_natural=="Puna - Valles, Quebradas y Yungas"~"puna__valles_quebradas_y_yungas",
# ruta_natural=="Selva Misionera"~"selva_misionera",
# ruta_natural=="Sierras Centrales"~"sierras_centrales")))


# Se genera la tabla con resultados para el año 2019
indicadores_rn_eoh <- eoh_rn %>%
  filter(anio == 2019) %>%
  group_by(anio, localidad, ruta_natural) %>%
  summarise(
    residentes = sum(round(p5_tr1, 0) * wl, na.rm = TRUE),
    no_residentes = sum(round(p5_tn1, 0) * wl, na.rm = TRUE)
  ) %>%
  rename(indice_tiempo = anio) %>% 
  group_by(ruta_natural) %>% 
  summarise(residentes = sum(residentes),
            no_residentes = sum(no_residentes)) %>% 
  ungroup() %>% 
  mutate(relacion_res_no_res = residentes/(residentes + no_residentes)) %>% 
  pivot_longer(cols = 2:4, names_to = "indicador", values_to = "valor_ruta") %>% 
  mutate(unidad = "personas")%>%
  group_by(indicador) %>%
  mutate(total_pais=sum(valor_ruta)) %>% 
  ungroup() %>% 
  mutate(total_pais=case_when(indicador=="relacion_res_no_res"~lag(total_pais, n = 2)/(lag(total_pais, n = 1)+lag(total_pais, n = 2)),
                              !indicador=="relacion_res_no_res"~total_pais),
         across(where(is.numeric), round, 3))
# tabla gral

indicadores_demanda <- bind_rows(indicadores_evyth, indicadores_eoh, 
                                 indicadores_aerocomercial_demanda, indicadores_parques) %>% 
  mutate(unidad = str_to_sentence(unidad),
         indicador = str_to_sentence(indicador),
         part_pais = valor_ruta / total_pais) %>% 
  filter(!is.na(ruta_natural_etiq)) %>% 
  select(ruta_natural, ruta_natural_agrupada_etiq, indicador, valor_ruta, total_pais, part_pais, unidad, fuente) %>%
  distinct(., across())


#### Ruta para probar el for
#unique(indicadores_demanda$ruta_natural)
#i <- "litoral_y_los_grandes_rios__delta"
#i <- unique(indicadores_demanda$ruta_natural)[4]

### Armo tabulado

for (i in unique(indicadores_demanda$ruta_natural)) {
  
  ruta_etiqueta <-
    unique(indicadores_demanda$ruta_natural_agrupada_etiq[indicadores_demanda$ruta_natural == i])
  
  tabla_resumen <-
    indicadores_demanda %>%
    filter(ruta_natural == i) %>%
    select(-c(ruta_natural, ruta_natural_agrupada_etiq)) %>%
    mutate(part_pais = case_when(indicador %in% c("Estadía media (turistas)", 
                                                  "Gasto promedio usd (turistas)") ~ as.character(round(part_pais,2)),
                                 indicador == "Part. de los residentes (%)" ~ "-",
                                 TRUE ~ paste0(round(part_pais * 100, 1), "%"))) %>% 
    #relocate(indicador, valor_ruta, total_pais, unidad, fuente) %>%
    group_by(fuente) %>%
    gt() %>%
    cols_label(
      indicador  = md("**Indicador**"),
      valor_ruta = md("**Ruta**"),
      total_pais = md("**Total País**"),
      part_pais  = md("**Part. RN / País**"),
      unidad     = md("**Unidad de medida**"),
      fuente     = md("**Fuente de datos**")
    )  %>%
    fmt_number(
      columns = c(2, 3),
      #rows = if(indicador == "Cantidad de asientos de cabotaje en aeropuertos de la ruta natural", FALSE),
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    fmt_percent(columns = c(2,3),
                rows = ifelse(
                  test = i %in% c("ibera"),
                  yes  = FALSE,
                  no   = 7),
                decimals = 1,
                sep_mark = ".",
                dec_mark = ",") %>% 
    cols_align(align = "center",
               columns = c(valor_ruta, total_pais, unidad, fuente))  %>%
    opt_table_font(font = list(google_font(name = "Encode Sans"))) %>%
    tab_options(table.align = "center", 
                row_group.font.weight = "bold",
    ) %>%  
    tab_header(title = md(glue("**DEMANDA**")),
               subtitle = "Año 2019") %>%
    tab_source_note(source_note = md(
      glue(
        "**Fuente:** Dirección Nacional de Mercados y Estadísticas"
        #"**Fuente**: DNMyE ({unique(indicadores_oferta$fuente)[1]}, {unique(indicadores_oferta$fuente)[2]}, {unique(indicadores_oferta$fuente)[3]})"
      )
    ))
  
  gtsave(tabla_resumen,
         glue("salidas/indicadores_x_ruta/rutas_10/{i}/{i}_tabla_resumem_demanda.png"))