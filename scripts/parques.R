#parques
library(d4t4tur)
library(readxl)
library(tidyverse)
library(glue)
library(plotly)
library(DT)
library(crosstalk)

ANIO <- 2023

base_pn <- read_excel("/srv/DataDNMYE/areas_protegidas/areas_protegidas_nacionales/pivot_pn.xlsx", sheet = 2) %>% 
  mutate(parque_nacional = limpiar_texto(parque_nacional)) %>% 
  filter(parque_nacional != "nahuel huapi") %>% 
  mutate(parque_nacional = ifelse(parque_nacional == "nahuel huapi 3p", "nahuel huapi", parque_nacional)) %>% 
  rename(area_protegida = parque_nacional)

provincias <- read_excel("/srv/DataDNMYE/areas_protegidas/areas_protegidas_nacionales/provincias_2.xlsx") %>% 
  mutate(area_protegida = ifelse(area_protegida == "isla pingüino", "isla pinguino",area_protegida))
  # add_row(parque_nacional = "Pizarro", provincia = "Salta") %>% 
  # mutate(parque_nacional = ifelse(parque_nacional == "Nogalar de los Toldos", "El Nogalar de Los Toldos", parque_nacional),
  #        etiq_parque = parque_nacional,
  #        parque_nacional = limpiar_texto(parque_nacional))

base_pn <- left_join(base_pn, provincias, by = "area_protegida") %>% 
  filter(anio >= 2012)

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

parques_data <- base_pn %>% 
  filter(anio >= 2017 & anio <= ANIO) %>% 
  mutate(visitantes = as.integer(visitantes),
         residencia = str_to_sentence(residencia),
         parque_nacional = str_to_title(parque_nacional)) %>% 
  group_by(anio, provincia, parque_nacional, residencia) %>% 
  summarise(visitantes = sum(visitantes, na.rm = TRUE),
            visitantes = case_when(visitantes == 0 ~ NA_integer_,
                                   TRUE ~ visitantes)) %>% 
  ungroup() %>% 
  # pivot_wider(id_cols = c(anio, provincia, etiq_parque), names_from = residencia, values_from = visitantes) %>% 
  # janitor::clean_names() %>% 
  arrange(desc(anio), provincia) %>% 
  rename(Provincia = provincia, Origen = residencia, Visitas = visitantes, Año = anio,
         Parque = parque_nacional) %>% 
  drop_na(Visitas)

graph_pn <- parques_data %>% 
  group_by(Año, Provincia, Parque) %>% 
  summarise(Visitas = sum(Visitas, na.rm = T)) %>% 
  ungroup()

graph_orig <- parques_data %>% 
  group_by(Año, Provincia, Origen) %>% 
  summarise(Visitas = sum(Visitas, na.rm = T)) %>% 
  ungroup()

tabla_pn <- SharedData$new(parques_data, ~ Provincia, group = "Provincia")

plot_parques <- SharedData$new(graph_pn, ~ Provincia, group = "Provincia")

plot_origen <- SharedData$new(graph_orig, ~ Provincia, group = "Provincia")


dt_parques <- datatable(tabla_pn,extensions = 'Buttons',
                        options = list(lengthMenu = c(10, 25, 50), pageLength = 10, 
                                       dom = 'lfrtipB',
                                       buttons = list('copy', 
                                                      list(
                                                        extend = 'collection',
                                                        buttons = list(list(extend = 'csv', filename = "parques"),
                                                                       list(extend = 'excel', filename = "parques")),
                                                        text = 'Download'
                                                      ))),
                        rownames= FALSE
)


gg_pn <- ggplot(plot_parques) + 
  geom_line(aes(Año, Visitas, color = Parque)) +
  geom_point(aes(Año, Visitas, color = Parque,
                 text = paste0(Parque,": ",
                               format(Visitas, big.mark = ".")))) +
  scale_color_dnmye() +
  theme_minimal() +
  theme(legend.position = "none")

gg_orig <- ggplot(plot_origen) + 
  geom_line(aes(Año, Visitas, color = Origen)) +
  geom_point(aes(Año, Visitas, color = Origen,
                 text = paste0(Provincia,": ",
                               format(Visitas, big.mark = "."), " ",Origen))) +
  scale_color_manual(values = c(dnmye_colores("purpura"),
                                dnmye_colores("cian"))) +
  theme_minimal() +
  theme(legend.position = "none")


parques <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12), 
         filter_select("id", "Elegir una provincia", tabla_pn, ~ Provincia,
                       multiple = FALSE),
         dt_parques,
         htmltools::br(),        htmltools::br(),
         ggplotly(gg_orig, dynamicTicks = TRUE, tooltip = "text") %>% 
           layout(xaxis=list(tickformat='.d')) %>% 
           layout(title = 'Evolución de visitas a Parques Nacionales según origen'),
         htmltools::br(),
         ggplotly(gg_pn, dynamicTicks = TRUE, tooltip = "text") %>% 
           layout(xaxis=list(tickformat='.d')) %>% 
           layout(title = 'Evolución de visitas por Parque Nacional'))
)

write_rds(parques, "outputs/graph_parques.rds")
