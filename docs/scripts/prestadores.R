library(geoAr)
#library(ARTofR)
library(tidyverse)
#install.packages("opencage")
library(georefar)
#vignette("opencage")
library(sf)
library(s2)
library(dnmye)
library(plotly)
library(glue)
library(gt)
library(leaflet)
library(herramientas)
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
                                                  group_by(provincia, departamento_arcgis, cat_rct) %>% 
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
                                     buttons = list(list(extend = 'csv', filename = "alojamientos"),
                                                    list(extend = 'excel', filename = "alojamientos")),
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
                                     buttons = list(list(extend = 'csv', filename = "alojamientos"),
                                                    list(extend = 'excel', filename = "alojamientos")),
                                     text = 'Descargar'
                                ))),
                 rownames= FALSE,  filter = list(position = 'top', clear = FALSE)
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
  st_cast("MULTIPOLYGON")

paleta1 <- MetBrewer::MetPalettes$Hokusai3[[1]]

empresas_map_data <- empresas_afip_dpto_geo %>% 
  filter(!is.na(provincia)) %>% 
  ungroup() %>% 
  mutate(
         Dpto = paste0(departamento_arcgis, "<br>", cat_rct,"<br>Empresas Registradas:", empresas))

  
env_alojamientos_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                      cat_rct == "Alojamiento") %>% 
                                              mutate(hexcolor = colorQuantile(domain = empresas, n = 4, palette =paleta1 )(empresas)),
                                        key = ~ provincia, "provincia")
env_gastro_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Gastronomía") %>% 
                                              mutate(hexcolor = colorQuantile(domain = empresas, n = 4, palette =paleta1 )(empresas)),
                                            key = ~ provincia, "provincia")
env_transporte_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Transporte") %>% 
                                              mutate(hexcolor = colorQuantile(domain = empresas, n = 4, palette =paleta1 )(empresas)),
                                            key = ~ provincia, "provincia")
env_agencias_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Agencias de Viaje") %>% 
                                              mutate(hexcolor = colorQuantile(domain = empresas, n = 4, palette =paleta1 )(empresas)),
                                            key = ~ provincia, "provincia")
env_otros_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Otros Servicios Turísticos") %>% 
                                              mutate(hexcolor = colorQuantile(domain = empresas, n = 4, palette =paleta1 )(empresas)),
                                            key = ~ provincia, "provincia")





empresas <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12, 12, 12), 
         filter_select("empresas", "Elegir una provincia", env_empresas_categoria_tamaño, ~ provincia,
                       multiple = F),
         dt_empresas_categoria_tamaño,
         dt_empresas_dpto_cat,
         leaflet(env_alojamientos_map_data) %>% 
           addArgTiles() %>% 
           addPolygons(data = env_alojamientos_map_data,
                       fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
                       group = "Alojamiento",
                       label = ~ lapply(Dpto, htmltools::HTML)) %>% 
  addPolygons(data = env_transporte_map_data,
              fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
              group = "Transporte", label = ~ lapply(Dpto, htmltools::HTML)) %>%
  addPolygons(data = env_gastro_map_data,
              fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
              group = "Gastronomía", label = ~ lapply(Dpto, htmltools::HTML)) %>%
  addPolygons(data = env_agencias_map_data,
              fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
              group = "Agencias de Viaje", label = ~ lapply(Dpto, htmltools::HTML)) %>%
  addPolygons(data = env_otros_map_data,
              fillColor = ~ hexcolor, stroke = F, fillOpacity =  .8,
              group = "Otros rubros", label = ~ lapply(Dpto, htmltools::HTML)) %>%
           addLayersControl(
             overlayGroups = c("Alojamiento", "Transporte", "Gastronomía", "Agencias de Viaje", "Otros rubros"),
             options = layersControlOptions(collapsed = FALSE)
           )
   )
  )

write_rds(empresas, "outputs/empresas.rds")
