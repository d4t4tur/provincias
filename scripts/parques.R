#parques
library(d4t4tur)
library(readxl)
library(tidyverse)
library(glue)
library(plotly)
library(DT)
library(crosstalk)

base_pn <- read_excel("/srv/DataDNMYE/areas_protegidas/areas_protegidas_nacionales/pivot_pn.xlsx", sheet = 2) %>% 
  mutate(parque_nacional = limpiar_texto(parque_nacional))

provincias <- read_excel("/srv/DataDNMYE/areas_protegidas/areas_protegidas_nacionales/provincias.xlsx") %>% 
  add_row(parque_nacional = "Pizarro", provincia = "Salta") %>% 
  mutate(parque_nacional = ifelse(parque_nacional == "Nogalar de los Toldos", "El Nogalar de Los Toldos", parque_nacional),
         etiq_parque = parque_nacional,
         parque_nacional = limpiar_texto(parque_nacional))

base_pn <- left_join(base_pn, provincias) %>% 
  filter(anio >= 2012)

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

parques_data <- base_pn %>% 
  filter(anio >= 2017 & anio < 2022) %>% 
  mutate(visitantes = as.integer(visitantes),
         residencia = str_to_sentence(residencia)) %>% 
  group_by(anio, provincia, residencia) %>% 
  summarise(visitantes = sum(visitantes, na.rm = TRUE)) %>% 
  # pivot_wider(id_cols = c(anio, provincia, etiq_parque), names_from = residencia, values_from = visitantes) %>% 
  # janitor::clean_names() %>% 
  arrange(desc(anio), provincia) %>% 
  rename(Provincia = provincia, Origen = residencia, Visitas = visitantes, Año = anio) 



plot_parques <- highlight_key(parques_data, ~ Provincia)

dt_parques <- datatable(plot_parques,extensions = 'Buttons',
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


gg <- ggplot(plot_parques) + 
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
         filter_select("id", "Elegir una provincia", plot_parques, ~ Provincia,
                       multiple = FALSE),
         dt_parques,
         ggplotly(gg, dynamicTicks = TRUE, tooltip = "text") %>% 
           layout(xaxis=list(tickformat='.d')) %>% 
           layout(title = 'Evolución de visitas a Parques Nacionales según origen'))
)

write_rds(parques, "outputs/graph_parques.rds")
