library(geoAr)
#library(ARTofR)
library(tidyverse)
#vignette("opencage")
library(sf)
library(s2)
library(dnmye)
library(plotly)
library(leaflet)
library(glue)
source(file = "scripts/aux_function.R")

ramas_turismo <- readxl::read_excel("/srv/DataDNMYE/padron_afip/ramas turismo 6 D.xlsx")
diccionario_claes <- read.csv("/srv/DataDNMYE/padron_afip/clae_diccionario.csv") %>% 
  select(clae6,clae6_desc)

#..............Armo base final con join de lat/long..............
ubicacion_claes_turismo_empleo19 <- read_delim("/srv/DataDNMYE/padron_afip/ubicacion_claes_turismo_empleo_dic19.csv",
                                               delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  filter(empleadora_dic19 == 1) %>% 
  mutate(nombre_ubicacion = paste(localidad_arcgis, departamento_arcgis, provincia, sep = ", ")) %>% 
  select(-empleadora_dic19) %>% 
  mutate(empleadora = 2019)
# 
# ubicacion_claes_turismo_empleo21 <- read_delim("/srv/DataDNMYE/padron_afip/ubicacion_claes_turismo_empleo.csv",
#                                                delim = ",", escape_double = FALSE, trim_ws = TRUE) %>% 
#   filter(empleadora_jun21_actualizado == 1) %>% 
#   mutate(nombre_ubicacion = paste(localidad_arcgis, departamento_arcgis, provincia, sep = ", ")) %>% 
#   select(-empleadora_jun21_actualizado) %>% 
#   mutate(empleadora = 2021)

#
# coordenadas_afip <- read_rds("/srv/DataDNMYE/rutas_nautrales/geometrias/prestadores_afip_geo.RDS") %>% 
#   select(address, lat, long)
#
ubicacion_claes_turismo <- bind_rows(ubicacion_claes_turismo_empleo19) %>% 
  distinct(cuit, .keep_all = T)

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
  group_by(provincia, departamento_arcgis, localidad_arcgis, cat_rct, nombre_ubicacion) %>% 
  summarise(empresas_cat = n()) %>% 
  filter(!is.na(provincia) | !is.na(departamento_arcgis))

mapa_base <- geoAr::get_geo("ARGENTINA") %>% 
  left_join(geoAr::geo_metadata) 

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


empresas_afip_loc <- empresas_afip_loc %>% 
  mutate(provincia = limpiar_texto(provincia, enie = F),
         departamento_arcgis = limpiar_texto(departamento_arcgis,enie =F),
         key = paste(provincia,departamento_arcgis ))

sum(empresas_afip_loc$prestadores_cat[!(empresas_afip_loc$key) %in% mapa_base$key])


empresas_afip_dpto_geo <- left_join(mapa_base, empresas_afip_loc, by = "key")

empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>% 
  group_by(name_iso, nomdepto_censo, cat_rct) %>% 
  summarise(empresas = sum(empresas_cat, na.rm = T))

empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>% 
  bind_rows(empresas_afip_dpto_geo %>% 
              group_by(name_iso, nomdepto_censo) %>% 
              summarise(cat_rct = "Total",
                        empresas = sum(empresas,  na.rm = T)))

empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>%
  st_cast("MULTIPOLYGON")

empresas_map_nest_data <- empresas_afip_dpto_geo %>% 
  filter(!is.na(name_iso)) %>% 
  mutate(nombre_prov = name_iso,
         Dpto = paste0(nomdepto_censo,"\nEmpresas Registradas:", empresas)) %>% 
  ungroup() %>% 
  nest(nested_column_provs = -name_iso)

mapas_prestadores2 <- function(data, categoria, color) {
  mapear <- function(x, categoria, color) {
    graf <- mapa_base %>% 
      filter(name_iso == unique(x$nombre_prov)) %>% 
      ggplot() +
      theme_void()+
      geom_sf(size = .2) +
      geom_sf(data = filter(x, cat_rct == categoria ),
              aes(alpha = as_factor(empresas), 
                  label = Dpto), fill = color[[1]]) +
      
      theme(legend.position = "none") +
      scale_fill_dnmye() +
      labs(title = categoria)
    
    graf %>% 
      ggplotly(tooltip = "label") %>% 
      add_annotations(
        text = ~ glue("<b>{str_wrap(categoria, 25)}</b>"),
        x = 0.5,
        y = 1.2,
        yref = "paper",
        xref = "paper",
        xanchor = "middle",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = 12)
      ) %>% 
      layout(title = NULL, xaxis = list(title = 'x', zeroline = F, showgrid = F, visible = F),
             yaxis = list(title = 'y', zeroline = F, showgrid = F,  visible = F))
  }
  map2(.x = categoria, .y = color,.f = ~ mapear(x = data, categoria = .x, color = .y))
  
}


empresas_map_nest_data <- empresas_map_nest_data %>% 
  mutate(mapas_av = map(nested_column_provs,
                        ~ mapas_prestadores2(.x, categoria = c("Agencias de Viaje",
                                                               "Alojamiento",
                                                               "Gastronomía",
                                                               "Transporte",
                                                               "Otros Servicios Turísticos",
                                                               "Total"),
                                             color = dnmye_col(1:4,6,7))))
# 
# mapas_al = map(nested_column_provs,
#                ~ mapas_prestadores2(.x, categoria = c("Alojamiento"),
#                                     color = dnmye_col(2))),
# mapas_g = map(nested_column_provs,
#                ~ mapas_prestadores2(.x, categoria = c("Gastronomía"),
#                                     color = dnmye_col(3))),
# mapas_t = map(nested_column_provs,
#                ~ mapas_prestadores2(.x, categoria = c("Transporte"),
#                                     color = dnmye_col(4))),
# mapas_o = map(nested_column_provs,
#                ~ mapas_prestadores2(.x, categoria = c("Otros Servicios Turísticos"),
#                                     color = dnmye_col(6))),
# mapas_n = map(nested_column_provs,
#              ~ mapas_prestadores2(.x, categoria = c("Total"),
#                                   color = dnmye_col(5)))


empresas_map_nest_data <- empresas_map_nest_data %>% 
  group_by(name_iso) %>% 
  mutate(.subplots = map(mapas_av, ~ {subplot(.x,  nrows = 3, shareX = F, shareY = F, margin = .07) %>% 
      config(scrollZoom = TRUE, modeBarButtonsToRemove = c('zoom2d','hoverCompareCartesian')) %>% 
      layout(title = "", xaxis = list(title = 'x', zeroline = F, showgrid = F, visible = F),
                         yaxis = list(title = 'y', zeroline = F, showgrid = F, visible = F),
      dragmode = F)})) %>% 
  ungroup()

empresas_map_nest_data <- empresas_map_nest_data %>% 
  select(-c(nested_column_provs, mapas_av))


write_rds(empresas_map_nest_data, "outputs/empresas_map_nest_data.RDS", compress = "gz")
