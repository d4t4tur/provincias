library(geoAr)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(herramientas)
library(comunicacion)
library(leaflet.minicharts)
library(crosstalk)
source(file = "scripts/aux_function.R")

ramas_turismo <- readxl::read_excel("/srv/DataDNMYE/padron_afip/ramas turismo 6 D.xlsx")
diccionario_claes <- read.csv("/srv/DataDNMYE/padron_afip/clae_diccionario.csv") %>% 
  select(clae6,clae6_desc)

#..............Armo base final con join de lat/long..............

ubicacion_claes_turismo <- read_delim("/srv/DataDNMYE/padron_afip/ubicacion_claes_turismo_empleo.csv",
                                               delim = ",", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(empleadora_jun21_actualizado == 1) %>%
  select(-empleadora_jun21_actualizado)

#
# coordenadas_afip <- read_rds("/srv/DataDNMYE/rutas_nautrales/geometrias/prestadores_afip_geo.RDS") %>% 
#   select(address, lat, long)
#
# ubicacion_claes_turismo <- bind_rows(ubicacion_claes_turismo_empleo19)

ubicacion_claes_turismo <- ubicacion_claes_turismo %>% 
  filter(!clae6 %in% c(c(473000,681098,681099,780000,562091))) %>% 
  left_join(diccionario_claes) %>% 
  mutate(rct = ifelse(clae6 %in% ramas_turismo$`6D`, "RCT","NO RCT"),
         cat_rct = case_when(substr(clae6,1,3) %in% c(473,491,492,501,502,511,524,771) & rct == "RCT"~ "Transporte",
                             substr(clae6,1,3) %in% c(551,552) & rct == "RCT"~ "Alojamiento",
                             substr(clae6,1,3) %in% c(561,562) & rct == "RCT" ~ "Gastronomía",
                             substr(clae6,1,3) == 791 & rct == "RCT"~ "Agencias de Viaje",
                             substr(clae6,1,3) %in% c(591,592,681,780,854,900,910,920,931,939) & rct == "RCT"~ "Otros Servicios Turísticos")) 

empresas_afip_loc <- ubicacion_claes_turismo %>% 
  group_by(provincia, departamento_arcgis, cat_rct,cat_empresa) %>% 
  summarise(cantidad = n()) %>% 
  filter(!is.na(provincia))


env_empresas_categoria_tamaño <- SharedData$new(empresas_afip_loc %>% 
                                                  group_by(provincia, cat_rct,cat_empresa) %>% 
                                                  summarise(cantidad = sum(cantidad)) %>% 
                                                  pivot_wider(names_from = cat_empresa, values_from = cantidad) %>% 
                                                  select(provincia, cat_rct, micro, pequeña, mediana, grande),
                                   key = ~ provincia,
                                   group = "provincia")

env_empresas_dpto_cat <- SharedData$new(empresas_afip_loc %>% 
                                                  group_by(provincia,  departamento_arcgis, cat_rct) %>% 
                                                  summarise(cantidad = sum(cantidad)) %>%
                                                   pivot_wider(names_from = cat_rct, values_from = cantidad, values_fill = 0) %>% 
                                                    relocate(.after = everything(), `Otros Servicios Turísticos`) %>% 
                                                    mutate(mutate(across(where(is.numeric), ~ifelse(.x < 3, NA, .x)))) %>% 
                                             filter(!(if_all(where(is.numeric), ~ is.na(.x)))),
                                                key = ~ provincia,
                                                group = "provincia")

select_prov <- filter_select(id = "provs", sharedData = env_empresas_categoria_tamaño, group = ~ provincia,
                      label = "Provincia")

dt_empresas_categoria_tamaño <- env_empresas_categoria_tamaño %>% 
  DT::datatable( extensions = 'Buttons',
                 options = list(lengthMenu = c(10, 25), 
                                pageLength = 10, 
                                dom = 'lrtipB',buttons = list(list(
                                  extend = "copy",
                                  text = "Copiar"
                                ),
                                list(extend = 'collection',
                                     buttons = list(list(extend = 'csv', filename = "empresas"),
                                                    list(extend = 'excel', filename = "empresas")),
                                     text = 'Descargar'
                                ))),
                 rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                 colnames = c('Provincia', 'Actividad', 'Micro', 'Pequeña', 'Mediana', 'Grande')
  )


dt_empresas_dpto_cat <- env_empresas_dpto_cat %>% 
  DT::datatable( extensions = 'Buttons',
                 options = list(lengthMenu = c(10, 25), 
                                pageLength = 10, 
                                dom = 'lrtipB',buttons = list(list(
                                  extend = "copy",
                                  text = "Copiar"
                                ),
                                list(extend = 'collection',
                                     buttons = list(list(extend = 'csv', filename = "empresas"),
                                                    list(extend = 'excel', filename = "empresas")),
                                     text = 'Descargar'
                                ))),
                 rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                 colnames = c('Provincia', 'Departamento', 'Alojamiento', 'Gastronomía', 'Transporte', 'Agencias de Viaje', 'Otros Servicios Turísticos')
  )




mapa_base <- geoAr::get_geo("ARGENTINA") %>% 
  geoAr::add_geo_codes()

mapa_base <- mapa_base %>% 
  mutate(name_prov = limpiar_texto(name_prov, enie = F),
         nomdepto_censo = limpiar_texto(nomdepto_censo, enie = F)) %>% 
  mutate(nomdepto_censo = case_when(
    nomdepto_censo == "comuna 01" ~ "comuna 1",
    nomdepto_censo == "comuna 02" ~ "comuna 2",
    nomdepto_censo == "comuna 03" ~ "comuna 3",
    nomdepto_censo == "comuna 04" ~ "comuna 4",
    nomdepto_censo == "comuna 05" ~ "comuna 5",
    nomdepto_censo == "comuna 06" ~ "comuna 6",
    nomdepto_censo == "comuna 07" ~ "comuna 7",
    nomdepto_censo == "comuna 08" ~ "comuna 8",
    nomdepto_censo == "comuna 09" ~ "comuna 9", 
    T ~ nomdepto_censo),
    key = paste(name_prov, nomdepto_censo))


empresas_afip_loc_mapa <- empresas_afip_loc %>% 
  group_by(provincia, departamento_arcgis, cat_rct) %>% 
  summarise(cantidad = sum(cantidad)) %>%
  filter(cantidad >= 3) %>% 
  mutate(key = paste( limpiar_texto(provincia, enie = F),limpiar_texto(departamento_arcgis,enie =F))) %>% 
  filter(!is.na(provincia) & !is.na(departamento_arcgis))

sum(empresas_afip_loc_mapa$cantidad[(empresas_afip_loc_mapa$key) %in% mapa_base$key])/sum(empresas_afip_loc_mapa$cantidad)


empresas_afip_dpto_geo <- left_join(mapa_base, empresas_afip_loc_mapa, by = "key")

empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>% 
  group_by(provincia, departamento_arcgis, cat_rct) %>% 
  summarise(empresas = sum(cantidad, na.rm = T))

empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>%
  st_cast("MULTIPOLYGON") %>% 
  st_centroid() 

empresas_afip_dpto_geo <- bind_cols(empresas_afip_dpto_geo, st_coordinates(empresas_afip_dpto_geo)) %>% 
  st_drop_geometry() %>% 
  rename(long  = X, lat = Y)

# empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>% 
#   st_centroid()
# 
# empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>%  
#   mutate(long = unlist(map(geometry,1)),
#          lat = unlist(map(geometry,2)))

# empresas_afip_dpto_geo_wider <- empresas_afip_dpto_geo %>% 
#   pivot_wider(names_from = cat_rct, values_from = empresas) %>% 
#   select(-c("NA"))
# 
# datachart <- empresas_afip_dpto_geo_wider %>% ungroup() %>% select(Gastronomía, Transporte, Alojamiento, `Agencias de Viaje`, `Otros Servicios Turísticos`) %>% 
#   ungroup()
# 
# leaflet() %>% 
#   addArgTiles() %>% 
#   addMinicharts(
#     empresas_afip_dpto_geo_wider$long, empresas_afip_dpto_geo_wider$lat,
#     chartdata = datachart,
#     # colorPalette =,
#     width = 45, height = 45
#   )

paleta1 <- RColorBrewer::brewer.pal(n = 5, "Reds")

empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>% 
  group_by(provincia, departamento_arcgis, cat_rct = "Total",long, lat) %>% 
  summarise(empresas = sum(empresas)) %>% 
  bind_rows(empresas_afip_dpto_geo)

empresas_map_data <- empresas_afip_dpto_geo %>% 
  filter(!is.na(provincia)) %>% 
  ungroup() %>% 
  mutate(
         etiqueta = paste0(departamento_arcgis, "<br>", cat_rct,"<br>Empresas Registradas:", empresas))

empresas_map_data <- empresas_map_data %>% 
  mutate(key = paste(provincia, departamento_arcgis, cat_rct, sep = "-"))

master_mapa <- SharedData$new(empresas_map_data,
                              key = ~ key, group = "mapa")


env_total_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Total") %>% 
                                              mutate(hexcolor = colorNumeric(domain = log(empresas),
                                                                              palette =paleta1 )(log(empresas))),
                                     key = ~ key, group = "mapa")
  
env_alojamientos_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                      cat_rct == "Alojamiento") %>% 
                                              mutate(hexcolor = colorNumeric(domain = log(empresas),
                                                                              palette =paleta1 )(log(empresas))),
                                            key = ~ key, group = "mapa")

env_gastro_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Gastronomía") %>% 
                                              mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                      key = ~ key, group = "mapa")

env_transporte_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Transporte") %>% 
                                              mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                          key = ~ key, group = "mapa")

env_agencias_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Agencias de Viaje") %>% 
                                              mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                        key = ~ key, group = "mapa")

env_otros_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Otros Servicios Turísticos") %>% 
                                              mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                     key = ~ key, group = "mapa")


empresas <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12, 
         filter_select("empresas", "Elegir una provincia", env_empresas_categoria_tamaño, ~ provincia,
                       multiple = F),
         htmltools::h2('Cantidad de empresas registradas por provincia y por actividad según tamaño de la empresa'),
         dt_empresas_categoria_tamaño,
         htmltools::br(),
         htmltools::h2('Cantidad de empresas registradas por provincia, departamento y actividad'),
         dt_empresas_dpto_cat,
         htmltools::br(),
         htmltools::h2('Mapa de la distribución de empresas registradas por departamento y actividad'),
         filter_select("mapa", "Elegir una provincia", master_mapa, ~ provincia,
                       multiple = F),
         htmltools::br(),
         leaflet() %>%
           addArgTiles() %>%
           addCircleMarkers(data = env_total_map_data, radius = 10,
                      fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8, color = "white",
                      group = "Total",  
                      label = ~ lapply(etiqueta, htmltools::HTML)) %>%
           addCircleMarkers(data = env_alojamientos_map_data, radius = 10,
                       fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
                       group = "Alojamiento",
                       label = ~ lapply(etiqueta, htmltools::HTML)) %>%
           addCircleMarkers(data = env_transporte_map_data, radius = 10,
                      fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
                      group = "Transporte", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
           addCircleMarkers(data = env_gastro_map_data, radius = 10,
                      fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
                      group = "Gastronomía", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
           addCircleMarkers(data = env_agencias_map_data, radius = 12,
                      fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
                      group = "Agencias de Viaje", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
           addCircleMarkers(data = env_otros_map_data, radius = 10,
                      fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
                      group = "Otros rubros", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
           addLayersControl(
             baseGroups = c("Total","Alojamiento", "Transporte", "Gastronomía", "Agencias de Viaje", "Otros rubros"),
             options = layersControlOptions(collapsed = FALSE)
           ),
         htmltools::br()
   )
  )

write_rds(empresas, "outputs/empresas.rds")
