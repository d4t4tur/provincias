library(tidyverse)
library(arrow)
library(crosstalk)
library(survey)
library(plotly)
library(DT)
library(lubridate)
library(comunicacion)
source(here::here("scripts","auxiliar_evyth.R"))

#### cargo y preparo la base ####
# b_evyth <- read_csv("http://datos.yvera.gob.ar/dataset/b5819e9b-5edf-4aad-bd39-a81158a2b3f3/resource/645e5505-68ee-4cfa-90f9-fcc9a4a34a85/download/evyth_microdatos.csv")
b_evyth <- arrow::read_parquet("/srv/DataDNMYE/evyth/base_trabajo/evyth_base_de_trabajo.parquet",
                               as_data_frame = TRUE) #%>%
# rename_with(.cols = starts_with("gasto_pc_constantes"),
#             .fn = ~ str_remove(.x, "[^gasto_pc_constantes$].*"))
provincias_region <- b_evyth %>% distinct(provincia_destino, region_destino)

b_evyth <- b_evyth %>% crear_etiqueta(variables = c("px09_t",
                                                    "p006_agrup",
                                                    "px08_agrup",
                                                    "px10_1_t"))



b_evyth <- b_evyth %>%
  mutate ( transporte  = as_factor(px09_t),  #transporte
           edad = as_factor(p006_agrup), #Edad en tramos
           alojamiento = as_factor(px08_agrup),#Tipo de alojamiento (agrupado)
           motivo = as_factor(px10_1_t)) #Motivo ppal del viaje agrupado

b_evyth <- b_evyth %>% 
  mutate(turismo_cultura = case_when(px17_2_7 == 1|
                                       px17_2_8 == 1|
                                       px17_2_9 == 1|
                                       px17_2_10 == 1 ~ 1,
                                     T ~ 0),
         turismo_naturaleza = case_when(
           px17_2_1 == 1 |
             px17_2_5 == 1 |
             px17_2_11 == 1 |
             px17_2_4 == 1 |
             px17_2_6 == 1 ~ 1,
           T ~ 0
         )
  )



if (!is.numeric(b_evyth$gasto_pc)) {
  b_evyth$gasto_pc <- parse_number(b_evyth$gasto_pc, locale = locale(decimal_mark = ","))
}


b_evyth$fecha <- lubridate::ym(b_evyth$Mes)

b_evyth <- b_evyth %>% bind_ipc(var_join_x = "fecha")

b_evyth <-  b_evyth %>% 
  mutate(
    #var  = ipc_ng_nacional/b_evyth$ipc_ng_nacional[which.max(b_evyth$fecha)] -1,
    gasto_pc_constantes = b_evyth$ipc_ng_nacional[which.max(b_evyth$fecha)]/b_evyth$ipc_ng_nacional*gasto_pc,
    gasto_pc_real_indice = gasto_pc/ipc_ng_nacional
  ) 

# filtro para excluir trims incompletos
ultimo_trim <- b_evyth %>% filter(is.na(pondera)) %>% distinct(anio, trimestre)

b_evyth <- b_evyth %>%
  filter(anio >= 2017 & anio <year(today())) %>% 
  filter(
    arg_o_ext == 1
  )

# check que no haya na en pondera
print(any(is.na(b_evyth$pondera)))

b_evyth$vis <- ifelse(b_evyth$arg_o_ext == 1,1,0)


evyth_nest_anio <- b_evyth %>% 
  group_by(anio) %>% 
  nest()

#### disenios ####


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(disenios  = map(.x = data, .f = disenio_evyth))

#### ppales numeros nacionales ####

#b_evyth_cv <- b_evyth %>% filter(anio == 2019)



f_totales <- formula("~ vis+I(tipo_visitante==1)+gasto_pc+gasto_pc_constantes+px07")


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_totales = map(.x = disenios,
                                    .f = ~estimar_totales(disenio = .x, formulas = f_totales)))

f_promedios <- formula("~gasto_pc+gasto_pc_constantes+px07")


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_promedios = map(.x = disenios,
                                      .f = ~estimar_promedios(disenio = .x,
                                                              formulas = f_promedios)))

#### ppales numeros provinciales ####

f_totales_prov <- formula("~ vis+I(tipo_visitante==1)+gasto_pc+gasto_pc_constantes+px07")


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_totales_prov = map(.x = disenios,
                                         .f = ~estimar_totales_prov(disenio = .x,
                                                                    formulas = f_totales_prov)))

f_promedios_prov <- formula("~ gasto_pc+gasto_pc_constantes+px07")


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_promedios_prov = map(.x = disenios,
                                           .f = ~estimar_promedios_prov(disenio = .x,
                                                                        formulas = f_promedios_prov)))

#### ppales numeros por region ####

f_totales_region <- formula("~ vis+I(tipo_visitante==1)+gasto_pc+gasto_pc_constantes+px07")

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_totales_region = map(.x = disenios,
                                           .f = ~estimar_totales_region(disenio = .x,
                                                                        formulas = f_totales_region)))

f_promedios_region <- formula("~ gasto_pc+gasto_pc_constantes+px07")

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_promedios_region = map(.x = disenios,
                                             .f = ~estimar_promedios_region(disenio = .x,
                                                                            formulas = f_promedios_region)))


#### tablas de valores ####
# preparar tablas en formato apto gt

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_totales_prov  = map(.x = estimaciones_totales_prov,
                                          .f = ~armar_tabla(.x)))

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_promedios_prov  = map(.x = estimaciones_promedios_prov,
                                            .f = ~armar_tabla(.x)))

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_totales_region  = map(.x = estimaciones_totales_region,
                                            .f = ~armar_tabla(.x, var_group = "region_destino")))

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(estimaciones_promedios_region  = map(.x = estimaciones_promedios_region,
                                              .f = ~armar_tabla(.x, var_group = "region_destino")))


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(
    across(
      c(estimaciones_totales, estimaciones_totales_region, estimaciones_totales_prov),
      .fns = ~ map(.x = .x, .f = ~  mutate(.data = .x, indicadores = case_when(
        indicadores == "vis" ~ "visitantes",
        indicadores == "I(tipo_visitante == 1)FALSE" ~ "excursionistas",
        indicadores == "I(tipo_visitante == 1)TRUE" ~ "turistas",
        indicadores == "px07" ~ "pernoctaciones",
        T ~ indicadores
      )
      ))
    )
  )

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(
    across(
      c(estimaciones_promedios, estimaciones_promedios_region, estimaciones_promedios_prov),
      .fns = ~ map(.x = .x, .f = ~  mutate(.data = .x, indicadores = case_when(
        indicadores == "gasto_pc" ~ "gasto_pc_promedio",
        indicadores == "gasto_pc_constantes" ~ "gasto_pc_cte_promedio",
        indicadores == "px07" ~ "estadia_promedio"
      )
      ))
    )
  )


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(tabla_nac  = map2(.x = estimaciones_totales,
                           .y = estimaciones_promedios,
                           .f = bind_rows),
         tabla_prov = map2(.x = estimaciones_totales_prov,
                           estimaciones_promedios_prov,
                           .f = bind_rows),
         tabla_region = map2(.x = estimaciones_totales_region,
                             estimaciones_promedios_region,
                             .f = bind_rows)
  )




evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(
    tabla_conjunta = pmap(.l =  list(tabla_prov, tabla_region, tabla_nac),
                          .f = ~ tabla_conjunta(prov = ..1, reg = ..2, nac = ..3)
    )
  )

#### armado de perfiles ####

# la conversion a factor hacerla antes del armado de los disenios de encuesta
# respecto a las actividades recategorizar entre turismo de naturaleza y  otro turismo/turismo de cultura
# esa categoría hay que convertirla en factor

# b_evyth <- b_evyth %>%
#   mutate ( transporte  = as_factor(px09_t),  #transporte
#            edad = as_factor(p006_agrup), #Edad en tramos
#            alojamiento = as_factor(px08_agrup),#Tipo de alojamiento (agrupado)
#            motivo = as_factor(px10_1_t)) #Motivo ppal del viaje agrupado
# 
# 
# 
# b_evyth <- b_evyth %>% 
#   mutate(tipo_turismo 

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(perfil_transporte  = map(.x = disenios,
                                  .f = ~calcular_prop(disenio = .x,
                                                      variable = "transporte")),
         perfil_edad  = map(.x = disenios,
                            .f = ~calcular_prop(disenio = .x,
                                                variable = "edad")),
         perfil_alojamiento  = map(.x = disenios,
                                   .f = ~calcular_prop(disenio = .x,
                                                       variable = "alojamiento")),
         perfil_motivo  = map(.x = disenios,
                              .f = ~calcular_prop(disenio = .x,
                                                  variable = "motivo")),
         perfil_turismo_naturaleza  = map(.x = disenios,
                                          .f = ~calcular_prop(disenio = .x, variable = "turismo_naturaleza",
                                                              f = "I(turismo_naturaleza == 1)")),
         perfil_turismo_cultura  = map(.x = disenios,
                                       .f = ~calcular_prop(disenio = .x, variable = "turismo_cultura",
                                                           f = "I(turismo_cultura == 1)"))
  )

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(perfil_transporte_reg  = map(.x = disenios,
                                      .f = ~calcular_prop_by(disenio = .x,  agrupamiento = "region_destino",
                                                             variable = "transporte")),
         perfil_edad_reg = map(.x = disenios,
                               .f = ~calcular_prop_by(disenio = .x, agrupamiento = "region_destino",
                                                      variable = "edad")),
         perfil_alojamiento_reg = map(.x = disenios,
                                      .f = ~calcular_prop_by(disenio = .x, agrupamiento = "region_destino",
                                                             variable = "alojamiento")),
         perfil_motivo_reg = map(.x = disenios,
                                 .f = ~calcular_prop_by(disenio = .x, agrupamiento = "region_destino",
                                                        variable = "motivo")),
         perfil_turismo_naturaleza_reg = map(.x = disenios,
                                             .f = ~calcular_prop_by(disenio = .x, agrupamiento = "region_destino",   variable = "turismo_naturaleza",
                                                                    f = "I(turismo_naturaleza == 1)")),
         perfil_turismo_cultura_reg = map(.x = disenios,
                                          .f = ~calcular_prop_by(disenio = .x, agrupamiento = "region_destino", variable = "turismo_cultura",
                                                                 f = "I(turismo_cultura == 1)")
         )
  )

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(perfil_transporte_prov  = map(.x = disenios,
                                       .f = ~calcular_prop_by(disenio = .x,  agrupamiento = "provincia_destino",
                                                              variable = "transporte")),
         perfil_edad_prov = map(.x = disenios,
                                .f = ~calcular_prop_by(disenio = .x, agrupamiento = "provincia_destino",
                                                       variable = "edad")),
         perfil_alojamiento_prov = map(.x = disenios,
                                       .f = ~calcular_prop_by(disenio = .x, agrupamiento = "provincia_destino",
                                                              variable = "alojamiento")),
         perfil_motivo_prov = map(.x = disenios,
                                  .f = ~calcular_prop_by(disenio = .x, agrupamiento = "provincia_destino",
                                                         variable = "motivo")),
         perfil_turismo_naturaleza_prov = map(.x = disenios,
                                              .f = ~calcular_prop_by(disenio = .x, agrupamiento = "provincia_destino",   variable = "turismo_naturaleza",
                                                                     f = "I(turismo_naturaleza == 1)")),
         perfil_turismo_cultura_prov = map(.x = disenios,
                                           .f = ~calcular_prop_by(disenio = .x, agrupamiento = "provincia_destino", variable = "turismo_cultura",
                                                                  f = "I(turismo_cultura == 1)"))
  )


# para variable de perfil hay que hacer el join de tablas igual que hicimos
# antes con los promedios

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(
    perfil_transporte_prov  = map(.x = perfil_transporte_prov,
                                  .f = ~armar_tabla(.x)),
    perfil_edad_prov = map(.x = perfil_edad_prov,
                           .f = ~armar_tabla(.x)),
    perfil_alojamiento_prov = map(.x = perfil_alojamiento_prov,
                                  .f = ~armar_tabla(.x)),
    perfil_motivo_prov = map(.x = perfil_motivo_prov,
                             .f = ~armar_tabla(.x)),
    perfil_turismo_cultura_prov = map(.x = perfil_turismo_cultura_prov,
                                      .f = ~armar_tabla(.x)),
    perfil_turismo_naturaleza_prov = map(.x = perfil_turismo_naturaleza_prov,
                                         .f = ~armar_tabla(.x))
  )

evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(
    perfil_transporte_reg  = map(.x = perfil_transporte_reg,
                                 .f = ~armar_tabla(.x, var_group = "region_destino")),
    perfil_edad_reg = map(.x = perfil_edad_reg,
                          .f = ~armar_tabla(.x, var_group = "region_destino")),
    perfil_alojamiento_reg = map(.x = perfil_alojamiento_reg,
                                 .f = ~armar_tabla(.x, var_group = "region_destino")),
    perfil_motivo_reg = map(.x = perfil_motivo_reg,
                            .f = ~armar_tabla(.x, var_group = "region_destino")),
    perfil_turismo_cultura_reg = map(.x = perfil_turismo_cultura_reg,
                                     .f = ~armar_tabla(.x, var_group = "region_destino")),
    perfil_turismo_naturaleza_reg = map(.x = perfil_turismo_naturaleza_reg,
                                        .f = ~armar_tabla(.x, var_group = "region_destino")))


evyth_nest_anio <- evyth_nest_anio %>% 
  mutate(
    tabla_conjunta_transporte = pmap(.l =  list(perfil_transporte_prov, perfil_transporte_reg, perfil_transporte),
                                     .f = ~ tabla_conjunta(prov = ..1, reg = ..2, nac = ..3)),
    tabla_conjunta_edad = pmap(.l =  list(perfil_edad_prov, perfil_edad_reg, perfil_edad),
                               .f = ~ tabla_conjunta(prov = ..1, reg = ..2, nac = ..3)),
    tabla_conjunta_alojamiento = pmap(.l =  list(perfil_alojamiento_prov, perfil_alojamiento_reg, perfil_alojamiento),
                                      .f = ~ tabla_conjunta(prov = ..1, reg = ..2, nac = ..3)),
    tabla_conjunta_motivo = pmap(.l =  list(perfil_motivo_prov, perfil_motivo_reg, perfil_motivo),
                                 .f = ~ tabla_conjunta(prov = ..1, reg = ..2, nac = ..3)),
    tabla_conjunta_tur_naturaleza = pmap(.l =  list(perfil_turismo_naturaleza_prov, perfil_turismo_naturaleza_reg, perfil_turismo_naturaleza),
                                         .f = ~ tabla_conjunta(prov = ..1, reg = ..2, nac = ..3)),
    tabla_conjunta_tur_cultura = pmap(.l =  list(perfil_turismo_cultura_prov, perfil_turismo_cultura_reg, perfil_turismo_cultura),
                                      .f = ~ tabla_conjunta(prov = ..1, reg = ..2, nac = ..3))
  )

# tabla de localidades como absolutos y como proporciones sobre el total pais


# salidas ----


salidas <- evyth_nest_anio %>% 
  select(anio, starts_with("tabla_conjunta"))

salidas <- salidas %>% 
  mutate(tabla_conjunta = map(.x = tabla_conjunta,
                              .f = ~ mutate(.data = .x, 
                                            indicadores = case_when(
                                              indicadores == "gasto_pc" ~ "gasto total",
                                              indicadores == "gasto_pc_constantes" ~ "gasto total (pesos constantes)",
                                              indicadores == "gasto_pc_promedio" ~ "gasto por turista",
                                              indicadores == "gasto_pc_cte_promedio" ~ "gasto por turista (pesos constantes)",
                                              indicadores == "estadia_promedio" ~ "estadía promedio",
                                              T ~ indicadores
                                            )
                              )
  ),
  tabla_conjunta_tur_cultura = map(.x = tabla_conjunta_tur_cultura, 
                                   .f = ~ .x %>% mutate(
                                     indicadores = case_when(
                                       indicadores == "I( == 1)FALSE" ~ "no realizó turismo de cultura",
                                       indicadores == "I( == 1)TRUE" ~ "realizó turismo de cultura", 
                                     )
                                   )),
  tabla_conjunta_tur_naturaleza = map(.x = tabla_conjunta_tur_naturaleza, 
                                      .f = ~ .x %>% mutate(
                                        indicadores = case_when(
                                          indicadores == "I( == 1)FALSE" ~ "no realizó turismo de naturaleza",
                                          indicadores == "I( == 1)TRUE" ~ "realizó turismo de naturaleza" 
                                        )
                                      ))
  )



salidas <- salidas %>%
  mutate(tabla_conjunta =  map(
    .x = tabla_conjunta,
    .f = ~ filter(
      .data = .x,
      !indicadores %in% c(
        "visitantes",
        "gasto total",
        "gasto por turista",
        "gasto por turista (pesos constantes)",
        "estadía promedio"
      )
    )
  ))

salidas <- salidas %>%
  mutate(tabla_conjunta_alojamiento =  map(
    .x = tabla_conjunta_alojamiento,
    .f = ~ filter(
      .data = .x,
      !indicadores %in% c("Sin uso de alojamiento (Visitas de un dia)",
                          "Sin uso de  (visitas de un día)"))
  )
  )


salidas <- salidas %>%
  mutate(tabla_conjunta =  map(
    .x = tabla_conjunta,
    .f = ~ .x %>% mutate(indicadores = as_factor(indicadores))
  ))

salidas <- salidas %>%
  mutate(tabla_conjunta_tipo_turismo =  map2(
    .x = tabla_conjunta_tur_naturaleza,.y = tabla_conjunta_tur_cultura,
    .f = ~ bind_rows(.x, .y)
  ))

salidas <- salidas %>%
  mutate(tabla_conjunta_tipo_turismo =  map(
    .x = tabla_conjunta_tipo_turismo, 
    .f = ~ .x %>% mutate(indicadores = as_factor(indicadores))
  ))


## PLOTS + DT interactivos ----

env_indicadores_ppales <- SharedData$new(data =   salidas %>% select(anio, tabla_conjunta) %>% unnest(),
                                         key = ~ provincia_destino,
                                         group = "provincia_destino")


dt_indicadores_ppales <- datatable(env_indicadores_ppales, extensions = 'Buttons',
                                   options = list(lengthMenu = c(10, 25), 
                                                  pageLength = 10, 
                                                  dom = 'lrtipB',buttons = list(list(
                                                    extend = "copy",
                                                    text = "Copiar"
                                                  ),
                                                  list(extend = 'collection',
                                                       buttons = list(list(extend = 'csv', filename = "indicadores_evyth"),
                                                                      list(extend = 'excel', filename = "indicadores_evyth")),
                                                       text = 'Descargar'
                                                  ))),
                                   rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                                   colnames = c('Año', 'Provincia', 'Indicador', 'Valor', 'CV %', 'Valor Región', 'CV % Región', 'Valor Nacional', 'CV % Nacional')
) %>% 
  formatPercentage(columns = c(5,7,9), dec.mark = ",", digits = 1) %>% 
  formatRound(columns = c(4,6,8), digits = 0, dec.mark = ",", mark = ".")


gg_indicadores_ppales <- ggplot(env_indicadores_ppales) + 
  geom_line(aes(anio, valor_prov, group = indicadores, color = indicadores)) +
  geom_point(aes(anio, valor_prov, group = indicadores, color = indicadores, text = paste0(indicadores, "<br>", format(valor_prov, big.mark = "."))),
             size = 3) +
  theme_minimal() +
  facet_wrap(~ indicadores, ncol = 1, scales = "fixed") +
  theme(legend.position = "none") +
  xlab("Año") +
  ylab("")

indicadores_ppales <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12,
         filter_select("interno-ppales", "Elegir una provincia", env_indicadores_ppales, ~ provincia_destino,
                       multiple = F),
         htmltools::br(),
         dt_indicadores_ppales,
         htmltools::p("Evolución de los principales indicadores"),
         ggplotly(gg_indicadores_ppales, dynamicTicks = TRUE, tooltip = "text") %>%
           layout(autosize = F))#, height = 800, widths = 1000
  
)

write_rds(indicadores_ppales, "outputs/interno_ppales.rds")

## alojamientos
env_alojamiento <- SharedData$new(data = salidas %>% 
                                    select(anio, tabla_conjunta_alojamiento) %>% unnest(),
                                  key = ~ provincia_destino,
                                  group = "provincia_destino")

dt_alojamiento <- datatable(env_alojamiento, extensions = 'Buttons',
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
                            colnames = c('Año', 'Provincia', 'Tipo de Alojamiento', 'Valor', 'CV %', 'Valor Región', 'CV % Región', 'Valor Nacional', 'CV % Nacional')
) %>% 
  formatPercentage(columns = 4:9, dec.mark = ",", digits = 2)


gg_alojamientos <- ggplot(env_alojamiento) + 
  geom_bar(aes(anio, 100*valor_prov, group = indicadores, fill = indicadores, text = paste0(indicadores, "<br>", scales::label_percent(decimal.mark = ",", accuracy = 0.1)(valor_prov))),
           stat = "identity", position = "dodge", color = "white") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Año") +
  ylab("") 


alojamientos <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12,
         # filter_select("alojamiento", "Elegir una provincia", env_alojamiento, ~ provincia_destino,
         #               multiple = F),
         dt_alojamiento,
         htmltools::br(),
         htmltools::p("Tipo de alojamiento como porcentaje del total de turistas en el destino"),
         ggplotly(gg_alojamientos, dynamicTicks = TRUE, tooltip = "text") %>%
           layout(autosize = F))
  
)

write_rds(alojamientos, "outputs/alojamiento.rds")

## edad

env_edad <- SharedData$new(data = salidas %>% 
                             select(anio, tabla_conjunta_edad) %>% unnest(),
                           key = ~ provincia_destino,
                           group = "provincia_destino")

dt_edad <- datatable(env_edad, extensions = 'Buttons',
                     options = list(lengthMenu = c(10, 25), 
                                    pageLength = 10, 
                                    dom = 'lrtipB',buttons = list(list(
                                      extend = "copy",
                                      text = "Copiar"
                                    ),
                                    list(extend = 'collection',
                                         buttons = list(list(extend = 'csv', filename = "edad"),
                                                        list(extend = 'excel', filename = "edad")),
                                         text = 'Descargar'
                                    ))),
                     rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                     colnames = c('Año', 'Provincia', 'Grupo Etario', 'Valor', 'CV %', 'Valor Región', 'CV % Región', 'Valor Nacional', 'CV % Nacional')
) %>% 
  formatPercentage(columns = 4:9, dec.mark = ",", digits = 2)


gg_edad <- ggplot(env_edad) + 
  geom_bar(aes(anio, 100*valor_prov, group = indicadores, fill = indicadores,  text = paste0(indicadores, "<br>", scales::label_percent(decimal.mark = ",", accuracy = 0.1)(valor_prov))),
           stat = "identity", position = "dodge", color = "white") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Año") +
  ylab("") 


edad <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12,
         # filter_select("edad", "Elegir una provincia", env_edad, ~ provincia_destino,
         #               multiple = F),
         dt_edad,
         htmltools::br(),
         htmltools::p("Grupo de edad como porcentaje del total de turistas en el destino"),
         ggplotly(gg_edad, dynamicTicks = TRUE, tooltip = "text") %>%
           layout(autosize = F))
  
)

write_rds(edad, "outputs/edad.rds")

## transporte

env_transporte <- SharedData$new(data  = salidas %>% 
                                   select(anio, tabla_conjunta_transporte) %>% unnest(),
                                 key = ~ provincia_destino,
                                 group = "provincia_destino")

dt_transporte <- datatable(env_transporte, extensions = 'Buttons',
                           options = list(lengthMenu = c(10, 25), 
                                          pageLength = 10, 
                                          dom = 'lrtipB',buttons = list(list(
                                            extend = "copy",
                                            text = "Copiar"
                                          ),
                                          list(extend = 'collection',
                                               buttons = list(list(extend = 'csv', filename = "transporte"),
                                                              list(extend = 'excel', filename = "transporte")),
                                               text = 'Descargar'
                                          ))),
                           rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                           colnames = c('Año', 'Provincia', 'Tipo de Transporte', 'Valor', 'CV %', 'Valor Región', 'CV % Región', 'Valor Nacional', 'CV % Nacional')
) %>% 
  formatPercentage(columns = 4:9, dec.mark = ",", digits = 2)


gg_transporte <- ggplot(env_transporte) + 
  geom_bar(aes(anio, 100*valor_prov, group = indicadores, fill = indicadores, text = paste0(indicadores, "<br>", scales::label_percent(decimal.mark = ",", accuracy = 0.1)(valor_prov))),
           stat = "identity", position = "dodge", color = "white") +
  theme_minimal() +
  scale_fill_dnmye() +
  theme(legend.position = "none") +
  xlab("Año") +
  ylab("") 




transporte <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12,
         dt_transporte,
         htmltools::br(),
         htmltools::p("Tipo de transporte utilizado como porcentaje del total de turistas en el destino"),
         ggplotly(gg_transporte, dynamicTicks = TRUE, tooltip = "text") %>%
           layout(autosize = F))
  
)

write_rds(transporte, "outputs/transporte.rds")

## motivo

env_motivo <- SharedData$new(data  = salidas %>% 
                               select(anio, tabla_conjunta_motivo) %>% unnest(),
                             key = ~ provincia_destino,
                             group = "provincia_destino")

dt_motivo <- datatable(env_motivo, extensions = 'Buttons',
                       options = list(lengthMenu = c(10, 25), 
                                      pageLength = 10, 
                                      dom = 'lrtipB',buttons = list(list(
                                        extend = "copy",
                                        text = "Copiar"
                                      ),
                                      list(extend = 'collection',
                                           buttons = list(list(extend = 'csv', filename = "motivo"),
                                                          list(extend = 'excel', filename = "motivo")),
                                           text = 'Descargar'
                                      ))),
                       rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                       colnames = c('Año', 'Provincia', 'Motivo del Viaje', 'Valor', 'CV %', 'Valor Región', 'CV % Región', 'Valor Nacional', 'CV % Nacional')
) %>% 
  formatPercentage(columns = 4:9, dec.mark = ",", digits = 2)


gg_motivo <- ggplot(env_motivo) + 
  geom_bar(aes(anio, 100*valor_prov, group = indicadores, fill = indicadores, text = paste0(indicadores, "<br>", scales::label_percent(decimal.mark = ",", accuracy = 0.1)(valor_prov))),
           stat = "identity", position = "dodge", color = "white") +
  theme_minimal() +
  scale_fill_dnmye() +
  theme(legend.position = "none") +
  xlab("Año") +
  ylab("") 


motivo <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12,
         dt_motivo,
         htmltools::br(),
         htmltools::p("Principal motivo del viaje como porcentaje del total de turistas en el destino"),
         ggplotly(gg_motivo, dynamicTicks = TRUE, tooltip = "text") %>%
           layout(autosize = F))
  
)

write_rds(motivo, "outputs/motivo.rds")


## tipo turismo

env_tipo_turismo <- SharedData$new(salidas %>% 
                                     select(anio, tabla_conjunta_tipo_turismo) %>% unnest(),
                                   key = ~ provincia_destino,
                                   group = "provincia_destino")

env_tipo_turismo_plot <- SharedData$new(salidas %>% 
                                          select(anio, tabla_conjunta_tipo_turismo) %>% unnest() %>% filter(!str_detect(indicadores, "no")),
                                        key = ~ provincia_destino,
                                        group = "provincia_destino")

dt_tipo_turismo <- datatable(env_tipo_turismo, extensions = 'Buttons',
                             options = list(lengthMenu = c(10, 25), 
                                            pageLength = 10, 
                                            dom = 'lrtipB',buttons = list(list(
                                              extend = "copy",
                                              text = "Copiar"
                                            ),
                                            list(extend = 'collection',
                                                 buttons = list(list(extend = 'csv', filename = "tipo_turismo"),
                                                                list(extend = 'excel', filename = "tipo_turismo")),
                                                 text = 'Descargar'
                                            ))),
                             rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                             colnames = c('Año', 'Provincia', 'Tipo de Turismo', 'Valor', 'CV %', 'Valor Región', 'CV % Región', 'Valor Nacional', 'CV % Nacional')
) %>% 
  formatPercentage(columns = 4:9, dec.mark = ",", digits = 2)


gg_tipo_turismo <- ggplot(env_tipo_turismo_plot) + 
  geom_bar(aes(anio, 100*valor_prov, group = indicadores, fill = str_detect(indicadores, "cultura"),
               text = paste0(indicadores, "<br>",
                             scales::label_percent(decimal.mark = ",", accuracy = 0.1)(valor_prov))),
           stat = "identity", color = "white", position  = "dodge") +
  scale_fill_manual(values = c("#F7941E", "#50B8B1"))+
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Año") +
  ylab("") 


tipo_turismo <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12,
         # filter_select("tipo_turismo", "Elegir una provincia", env_tipo_turismo, ~ provincia_destino,
         #               multiple = F),
         dt_tipo_turismo,
         htmltools::br(),
         htmltools::p("Participación en actividades de turismo de naturaleza y de turismo cultural"),
         ggplotly(gg_tipo_turismo, dynamicTicks = TRUE, tooltip = "text") %>%
           layout(autosize = F))
  
)

write_rds(tipo_turismo, "outputs/tipo_turismo.rds")

