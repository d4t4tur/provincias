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
ubicacion_claes_turismo <- bind_rows(ubicacion_claes_turismo_empleo19)

ubicacion_claes_turismo <- ubicacion_claes_turismo %>% 
  filter(!clae6 %in% c(c(473000,681098,681099,780000,562091))) %>% 
  left_join(diccionario_claes) %>% 
  mutate(rct = ifelse(clae6 %in% ramas_turismo$`6D`, "RCT","NO RCT"),
         cat_rct = case_when(substr(clae6,1,3) %in% c(473,491,492,501,502,511,524,771) & rct == "RCT"~ "Transporte",
                             substr(clae6,1,3) %in% c(551,552) & rct == "RCT"~ "Alojamiento",
                             substr(clae6,1,3) %in% c(561,562) & rct == "RCT" ~ "Gastronomía",
                             substr(clae6,1,3) == 791 & rct == "RCT"~ "Agencias de Viaje",
                             substr(clae6,1,3) %in% c(591,592,681,780,854,900,910,920,931,939) & rct == "RCT"~ "Otros Servicios Turísticos")) 

prestadores_afip_loc <- ubicacion_claes_turismo %>% 
  group_by(provincia, departamento_arcgis, localidad_arcgis, cat_rct,cat_empresa, nombre_ubicacion) %>% 
  summarise(prestadores_cat = n()) %>% 
  filter(!is.na(provincia))


# prestadores_afip_loc_geo <- left_join(prestadores_afip_loc, coordenadas_afip, by = c("nombre_ubicacion" = "address") )

prestadores_afip_loc %>% 
  # filter(is.na(lat)) %>% 
  group_by(provincia, departamento_arcgis, localidad_arcgis) %>% 
  summarise(sum(prestadores_cat))

# prestadores_afip_loc_geo <- prestadores_afip_loc_geo %>% 
#   filter(!is.na(lat)) 
# st_as_sf(coords = c("long", "lat"), crs =4326)

prestadores_nest_data <- prestadores_afip_loc %>% 
  mutate(nombre_prov = provincia) %>% 
  ungroup() %>% 
  nest(nested_column_provs = -provincia)


tabla_prestadores <-  function(data) {
  
  data %>% 
    group_by(cat_rct) %>% 
    summarise(micro = sum(prestadores_cat[cat_empresa == "micro"], na.rm = T),
              pequeña = sum(prestadores_cat[cat_empresa == "pequeña"], na.rm = T),
              mediana = sum(prestadores_cat[cat_empresa == "mediana"], na.rm = T),
              grande = sum(prestadores_cat[cat_empresa == "grande"], na.rm = T),
              total = sum(prestadores_cat, na.rm = T)
              
    ) %>% 
    arrange(cat_rct) %>% 
    janitor::adorn_totals() %>%  
    mutate(across(, ~ ifelse(.x == 0, "-", as.character(.x)))) %>% 
    gt() %>% 
    cols_label(
      cat_rct  = md("Categoría"),
      micro = md("**Micro**"),
      pequeña = md("**Pequeña**"),
      mediana = md("**Mediana**"),
      grande = md("**Grande**"),
      total = md("**Total**")
    )  %>%
    cols_align(align = "center",
               columns = c(micro, pequeña, mediana, grande))  %>%
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
        rows =  cat_rct == "Total"
      )) %>% 
    tab_header(title = md(glue("**Empresas Registradas - {unique(data$nombre_prov)}**")),
               subtitle = "Cantidad de Empresas Registradas por Categoría. Año 2021") %>%
    tab_source_note(source_note = md("**Fuente:** Dirección Nacional de Mercados y Estadísticas a partir de datos de AFIP"
                                     #"**Fuente**: DNMyE ({unique(indicadores_tot$fuente)[1]}, {unique(indicadores_tot$fuente)[2]}, {unique(indicadores_tot$fuente)[3]})"
    )
    )
}  


prestadores_nest_data <- prestadores_nest_data %>% 
  mutate(.tablas_prov = map(nested_column_provs,  tabla_prestadores)) %>% 
  select(-nested_column_provs)

write_rds(prestadores_nest_data, "outputs/empresas_nest_data.RDS")
