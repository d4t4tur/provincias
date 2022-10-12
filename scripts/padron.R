library(tidyverse)
library(janitor)
library(DT)
library(crosstalk)
library(plotly)


options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Arial'});",
                            "}")))


serie_puna <- arrow::read_parquet("/srv/DataDNMYE/puna/serie_puna.parquet") %>% 
  filter(anio >= 2017) %>%
  rename(Año = anio,
         Provincia = provincia,
         Localidad = localidad,
         Tipo = tipo,
         Clasificación = clasificacion_mintur) %>% 
  arrange(desc(Año))

tabla_tipo <- serie_puna %>% 
  group_by(Año, Provincia) %>%
  mutate(plazas_prov = sum(plazas),
         establ_prov = sum(establecimientos)) %>% 
  ungroup() %>% 
  group_by(Año, Provincia, Tipo) %>% 
  summarise(Establecimientos = sum(establecimientos),
            "% establecimientos" = Establecimientos/unique(establ_prov),
            Plazas = sum(plazas),
            "% plazas" = Plazas/unique(plazas_prov)) %>% 
    ungroup()

localidades_data <- serie_puna %>% 
  filter(Año == 2020) %>% 
  group_by(Provincia) %>% 
  mutate(plazas_prov = sum(plazas)) %>% 
  ungroup() %>% 
  group_by(Provincia, Localidad) %>% 
  summarise(Plazas = sum(plazas),
            "% plazas" = format(round(Plazas/unique(plazas_prov), 3)*100, decimal.mark = ",")) %>% 
  ungroup() %>% 
  group_by(Provincia) %>% 
  slice_max(n = 15, Plazas) %>% 
  ungroup()


tabla_tipo <- SharedData$new(tabla_tipo, ~ Provincia, group = "Provincia")
localidades_data <- SharedData$new(localidades_data, ~ Provincia, group = "Provincia")

dt_puna <- datatable(tabla_tipo, extensions = 'Buttons',
                     options = list(lengthMenu = c(10, 25, 50), pageLength = 10, 
                                    dom = 'lfrtipB',
                                    buttons = list('copy', 
                                                   list(
                                                     extend = 'collection',
                                                     buttons = list(list(extend = 'csv', filename = "puna"),
                                                                    list(extend = 'excel', filename = "puna")),
                                                     text = 'Download'
                                                   ))),
                        rownames= FALSE
) %>% 
  formatPercentage(columns = c("% establecimientos","% plazas"), digits = 1)


gg_loc <- ggplot(localidades_data) + 
  geom_bar(aes(Localidad, Plazas, text = paste0("Porcentaje de plazas sobre total provincial: ", 
                                                `% plazas`, " %")),
           stat = "identity", position = "dodge", fill = dnmye_colores("cian")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45))


puna <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = c(12, 12), 
         filter_select("Provincia", "Elegir una provincia:", tabla_tipo, ~ Provincia,
                       multiple = FALSE),
         dt_puna,
         htmltools::br(),
         htmltools::br(),
         ggplotly(gg_loc, dynamicTicks = TRUE) %>% 
           layout(xaxis = list(categoryorder = "trace")) %>% 
           layout(title = 'Top 15 localidades de la provincia según cantidad de plazas'))
)

write_rds(puna, "outputs/graph_puna.rds")
