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
cabotaje <- read_rds("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada.rds") %>% 
  filter(ClasificaciónVuelo == "Cabotaje",
         reg_no_reg == "Empresa de vuelos regulares") %>% 
  select(Año_Local, TipodeMovimiento,
         origen_aeropuerto_etiqueta, origen_localidad_etiqueta, origen_provincia_etiqueta,
         destino_aeropuerto_etiqueta, destino_localidad_etiqueta, destino_provincia_etiqueta,
         pasajeros, pax) %>% 
  mutate(aero_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_aeropuerto_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_aeropuerto_etiqueta),
         prov_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_provincia_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_provincia_etiqueta),
         loc_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_localidad_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_localidad_etiqueta))


internacional <- read_rds("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada.rds") %>% 
  filter(ClasificaciónVuelo == "Internacional",
         reg_no_reg == "Empresa de vuelos regulares") %>% 
  select(Año_Local, TipodeMovimiento,
         origen_aeropuerto_etiqueta, origen_localidad_etiqueta, origen_provincia_etiqueta,
         destino_aeropuerto_etiqueta, destino_localidad_etiqueta, destino_provincia_etiqueta,
         pasajeros) %>% 
  mutate(aero_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_aeropuerto_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_aeropuerto_etiqueta),
         prov_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_provincia_etiqueta,
                             TipodeMovimiento == "Despegue" ~ origen_provincia_etiqueta),
         loc_ad = case_when(TipodeMovimiento == "Aterrizaje" ~ destino_localidad_etiqueta,
                            TipodeMovimiento == "Despegue" ~ origen_localidad_etiqueta))

# DT idioma
options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))


## TABLAS

tabla_cabotaje <- cabotaje %>% 
  filter(Año_Local >= 2017 & Año_Local < 2022) %>% 
  group_by(Año_Local, prov_ad, loc_ad) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(Año_Local), prov_ad) %>% 
  rename(Año = Año_Local,
         Provincia = prov_ad,
         Localidad = loc_ad,
         Pasajeros = pasajeros) %>% 
  drop_na()


tabla_internacional <- internacional %>% 
  filter(Año_Local >= 2017 & Año_Local < 2022) %>% 
  group_by(Año_Local, prov_ad, loc_ad) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(Año_Local), prov_ad) %>% 
  rename(Año = Año_Local,
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
                                                         buttons = list(list(extend = 'csv', filename = "puna"),
                                                                        list(extend = 'excel', filename = "puna")),
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
                                                              buttons = list(list(extend = 'csv', filename = "puna"),
                                                                             list(extend = 'excel', filename = "puna")),
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
