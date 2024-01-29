library(tidyverse)
library(plotly)
library(crosstalk)
library(DT)
library(d4t4tur)
library(lubridate)

visitantes_dnm <-  read_rds("/srv/DataDNMYE/turismo_internacional/turismo_internacional_visitantes.rds")

# turismo receptivo (visitantes: turistas y excursionistas)
receptivo <-  visitantes_dnm %>%
  filter(turismo_internac == "Receptivo", anio >= 2017 & anio < year(today())) %>%
  group_by(anio, prov, paso_publ) %>%
  summarise(visitantes = round(sum(casos_ponderados, na.rm = T))) %>%
  ungroup() %>%
  mutate(
    prov = case_when(
      prov == "CABA-GBA" ~ "Buenos Aires",
      prov == "Resto prov. Bs. As." ~ "Buenos Aires",
      #prov == "Tierra del Fuego" ~ "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
      TRUE ~ prov),
    paso_publ = str_replace_all(paso_publ, "Aero ", "Aeropuerto "),
    concatenado = paste(paso_publ, prov)) %>%
  group_by(anio) %>% 
  mutate(participacion_pais = visitantes/sum(visitantes)) %>% 
  ungroup()

tabla_rec <- receptivo %>% 
  group_by(anio, prov) %>% 
  arrange(desc(visitantes)) %>% 
  mutate(rank = 1:n(),
         paso_publ = ifelse(rank <= 5,paso_publ,"Otros" )) %>% 
  ungroup() %>% 
  group_by(anio, prov, paso_publ) %>% 
  summarise(visitantes = sum(visitantes),
            participacion_pais = sum(participacion_pais)) %>% 
  mutate(orden= ifelse(paso_publ == "Otros",2,1)) %>% 
  ungroup() %>% 
  arrange(desc(anio),prov,-visitantes) %>% 
  select(-orden) 

data_rec <- SharedData$new(tabla_rec, ~ prov)

dt_rec <- datatable(data_rec, extensions = 'Buttons', 
                     colnames=c("Año","Provincia","Paso","Visitantes","% sobre total país"),
                     options = list(lengthMenu = c(10, 25, 50), pageLength = 10, 
                                    dom = 'lfrtipB',
                                    buttons = list('copy', 
                                                   list(
                                                     extend = 'collection',
                                                     buttons = list(list(extend = 'csv', filename = "receptivo"),
                                                                    list(extend = 'excel', filename = "receptivo")),
                                                     text = 'Download'
                                                   ))),
                     rownames= FALSE
) %>% 
  formatPercentage(columns = "participacion_pais", digits = 1)


gg_rec <- ggplot(data_rec) + 
  geom_line(aes(anio, visitantes, color = paso_publ)) +
  geom_point(aes(anio, visitantes, color = paso_publ,
                text = paste0(paso_publ,": ", visitantes, " viajes de visitantes"))) +
  scale_color_dnmye() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45),
        axis.title = element_blank())


graph_receptivo <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12, 10), 
         filter_select("prov", "Elegir una provincia:", data_rec, ~ prov,
                       multiple = FALSE),
         dt_rec,
         htmltools::br(),
         htmltools::br(),
         ggplotly(gg_rec, dynamicTicks = TRUE, tooltip = "text") %>%
           layout(xaxis=list(type='category')) %>% 
           layout(title = 'Evolución del volumen de viajes de visitantes por paso internacional'))
)

write_rds(graph_receptivo, "outputs/graph_receptivo.rds")
