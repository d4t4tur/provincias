#Llamo a las librerias.
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(plotly)
library(d4t4tur)
library(DT)
library(crosstalk)


# Data por por clasificacion
cabotaje <- arrow::read_parquet("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada_diaria.parquet") %>% 
  filter(clasificacion_vuelo == "Cabotaje",
         reg_no_reg == "Empresa de vuelos regulares") %>% 
  select(anio_local, #TipodeMovimiento,
         origen_aeropuerto_etiqueta, origen_localidad_etiqueta, origen_provincia_etiqueta,
         destino_aeropuerto_etiqueta, destino_localidad_etiqueta, destino_provincia_etiqueta,
         pasajeros = pax_ad) 

cabotaje <- bind_rows(
  cabotaje %>% 
    group_by(anio_local,
             prov_ad = destino_provincia_etiqueta, 
             loc_ad = destino_localidad_etiqueta,
             aero_ad = destino_aeropuerto_etiqueta) %>% 
    summarise(pasajeros = sum(pasajeros)),
  
  cabotaje %>% 
    group_by(anio_local,
             prov_ad = origen_provincia_etiqueta, 
             loc_ad = origen_localidad_etiqueta,
             aero_ad = origen_aeropuerto_etiqueta) %>% 
    summarise(pasajeros = sum(pasajeros))
) %>%
  group_by(anio_local, prov_ad, loc_ad, aero_ad) %>% 
  summarise(pasajeros = round(sum(pasajeros))) %>% 
  ungroup() %>% 
  mutate(prov_ad = str_replace(prov_ad, "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                                                    "Tierra del Fuego"))


  #mutate(
    # aero_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_aeropuerto_etiqueta,
    #                          TipodeMovimiento == "Despegue" ~ origen_aeropuerto_etiqueta),
    #      prov_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_provincia_etiqueta,
    #                          TipodeMovimiento == "Despegue" ~ origen_provincia_etiqueta),
    #      loc_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_localidad_etiqueta,
    #                          TipodeMovimiento == "Despegue" ~ origen_localidad_etiqueta),
         # prov_ad = str_replace(prov_ad, "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
         #                         "Tierra del Fuego"))


internacional <- arrow::read_parquet("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada_diaria.parquet") %>% 
  filter(clasificacion_vuelo == "Internacional",
         reg_no_reg == "Empresa de vuelos regulares") %>% 
  select(anio_local, #TipodeMovimiento,
         origen_aeropuerto_etiqueta, origen_localidad_etiqueta, origen_provincia_etiqueta,
         destino_aeropuerto_etiqueta, destino_localidad_etiqueta, destino_provincia_etiqueta,
         pasajeros = pax_ad) 


internacional <- bind_rows(
  internacional %>% 
    group_by(anio_local, 
             prov_ad = destino_provincia_etiqueta, 
             loc_ad = destino_localidad_etiqueta,
             aero_ad = destino_aeropuerto_etiqueta) %>% 
    summarise(pasajeros = sum(pasajeros)),
  
  internacional %>% 
    group_by(anio_local,
             prov_ad = origen_provincia_etiqueta, 
             loc_ad = origen_localidad_etiqueta,
             aero_ad = origen_aeropuerto_etiqueta) %>% 
    summarise(pasajeros = sum(pasajeros))
) %>%
  group_by(anio_local, prov_ad, loc_ad, aero_ad) %>% 
  summarise(pasajeros = round(sum(pasajeros))) %>% 
  ungroup() %>% 
  mutate(prov_ad = str_replace(prov_ad, "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                               "Tierra del Fuego"))

#%>% 
  # mutate(aero_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_aeropuerto_etiqueta,
  #                            TipodeMovimiento == "Despegue" ~ origen_aeropuerto_etiqueta),
  #        prov_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_provincia_etiqueta,
  #                            TipodeMovimiento == "Despegue" ~ origen_provincia_etiqueta),
  #        loc_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_localidad_etiqueta,
  #                           TipodeMovimiento == "Despegue" ~ origen_localidad_etiqueta),
  #        prov_ad = str_replace(prov_ad, "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
  #                              "Tierra del Fuego"))

# DT idioma
options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))


## TABLAS

tabla_cabotaje <- cabotaje %>% 
  filter(anio_local >= 2017 & anio_local <= 2022) %>% 
  group_by(anio_local, prov_ad, loc_ad) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(anio_local), prov_ad) %>% 
  rename(Año = anio_local,
         Provincia = prov_ad,
         Localidad = loc_ad,
         Pasajeros = pasajeros) %>% 
  drop_na()


tabla_internacional <- internacional %>% 
  filter(anio_local >= 2017 & anio_local <= 2022) %>% 
  group_by(anio_local, prov_ad, loc_ad) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(anio_local), prov_ad) %>% 
  rename(Año = anio_local,
         Provincia = prov_ad,
         Localidad = loc_ad,
         Pasajeros = pasajeros) %>% 
  drop_na()


## PLOTS + DT interactivos

plot_cabotaje <- highlight_key(tabla_cabotaje %>% mutate(Año = as.character(Año)), ~ Provincia)

dt_cabotaje <- datatable(plot_cabotaje, extensions = 'Buttons',
                         options = list(lengthMenu = c(10, 25, 50), pageLength = 10, 
                                        dom = 'lfrtipB',
                                        buttons = list('copy', 
                                                       list(
                                                         extend = 'collection',
                                                         buttons = list(list(extend = 'csv', filename = "conectividad"),
                                                                        list(extend = 'excel', filename = "conectividad")),
                                                         text = 'Download'
                                                       ))),
                              rownames= FALSE)

gg <- ggplot(plot_cabotaje) + 
  geom_line(aes(Año, Pasajeros, group = Localidad, color = Localidad)) +
  geom_point(aes(Año, Pasajeros, group = Localidad, color = Localidad, 
                 text = paste0(Localidad, ": ", format(Pasajeros, big.mark = "."), 
                               " Pasajeros"))) +
  scale_color_dnmye() +
  theme_minimal() +
  theme(legend.position = "none")


cabotaje <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12), 
         filter_select("id", "Elegir una provincia", plot_cabotaje, ~ Provincia,
                       multiple = FALSE),
                    dt_cabotaje,
         ggplotly(gg, dynamicTicks = TRUE, tooltip = "text") %>% 
           layout(title = 'Evolución de pasajeros en vuelos de cabotaje por localidad'))
)

write_rds(cabotaje, "outputs/graph_cabotaje.rds")


plot_internacional <- highlight_key(tabla_internacional %>% mutate(Año = as.character(Año)), ~ Provincia)

dt_internacional <- datatable(plot_internacional, extensions = 'Buttons',
                              options = list(lengthMenu = c(10, 25, 50), pageLength = 10, 
                                             dom = 'lfrtipB',
                                             buttons = list('copy', 
                                                            list(
                                                              extend = 'collection',
                                                              buttons = list(list(extend = 'csv', filename = "conectividad"),
                                                                             list(extend = 'excel', filename = "conectividad")),
                                                              text = 'Download'
                                                            ))),
                              rownames= FALSE)

gg <- ggplot(plot_internacional) + 
  geom_line(aes(Año, Pasajeros, group = Localidad, color = Localidad)) +
  geom_point(aes(Año, Pasajeros, group = Localidad, color = Localidad, 
                 text = paste0(Localidad, "<br>", format(Pasajeros, big.mark = "."), 
                               " Pasajeros"))) +
  scale_color_dnmye() +
  theme_minimal() +
  theme(legend.position = "none")


internacional <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12), 
         filter_select("id", "Elegir una provincia", plot_internacional, ~ Provincia,
                       multiple = FALSE),
                    dt_internacional,
         ggplotly(gg, dynamicTicks = TRUE, tooltip = "text") %>% 
           layout(title = 'Evolución de pasajeros en vuelos internacionales por localidad'))
)

write_rds(internacional, "outputs/graph_internacional.rds")
