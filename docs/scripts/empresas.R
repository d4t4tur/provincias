library(geoAr)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(herramientas)
library(comunicacion)
library(crosstalk)
library(htmltools)
source(file = "scripts/aux_function.R")

ramas_turismo <- readxl::read_excel("/srv/DataDNMYE/padron_afip/ramas turismo 6 D.xlsx")
diccionario_claes <- read.csv("/srv/DataDNMYE/padron_afip/clae_diccionario.csv") %>% 
  select(clae6,clae6_desc)

#..............Armo base final con join de lat/long..............

ubicacion_claes_turismo <- read_delim("/srv/DataDNMYE/padron_afip/ubicacion_claes_turismo_empleo_v2023.07_v2.csv",
                                      delim = ",", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(empleadora_jun_22 == 1) %>%
  select(-empleadora_jun_22) %>% 
  rename(clae6 = actividad_principal)

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

data_emp <- ubicacion_claes_turismo %>% 
  mutate(provincia = case_when(localidad == "Rosario" & provincia == "Capital" ~ "Santa Fe",
                               localidad == "Cordoba" & provincia == "Chubut" ~ "Córdoba",
                               T ~ provincia),
         provincia = recode(provincia,
                            "Capital" = "Ciudad Autónoma de Buenos Aires"))

# Normalizamos la variable localidad.
data_emp <- data_emp %>% 
  mutate(localidad = str_to_title(localidad), 
         localidad = str_remove_all(localidad, "[:punct:]"),
         localidad = str_remove_all(localidad, "˝"),
         localidad = str_replace_all(localidad, "Ďż|ďż|�", "ñ"),
         localidad = str_replace_all(localidad, "ññ", "ñ"),
         localidad = str_squish(localidad),
         localidad = str_to_title(localidad), 
         localidad = str_remove_all(localidad, "Sur$|Sud$|Norte$"),
         localidad = str_remove_all(localidad, "Sudeste|Noroeste"),
         localidad = str_remove_all(localidad, "ipv"),
         localidad = str_squish(localidad),
         # Y recodificamos los casos mal nombrados.
         localidad = case_when(grepl("Moreno", localidad) ~ "Moreno",
                               grepl("Escobar", localidad) ~ "Escobar",
                               grepl("Ezeiza", localidad) ~ "Ezeiza",
                               grepl("La Plata", localidad) ~ "La Plata",
                               grepl("Mar De Ajo", localidad) ~ "Mar De Ajo",
                               grepl("Tigre", localidad) ~ "Tigre",
                               grepl("Quilmes", localidad) ~ "Quilmes",
                               grepl("Bolivar", localidad) ~ "Bolivar",
                               grepl("Lanus", localidad) ~ "Lanus",
                               grepl("Villa Lynch", localidad) ~ "Villa Lynch",
                               grepl("Villa Adelina", localidad) ~ "Villa Adelina",
                               grepl("Chiquita", localidad) ~ "Mar Chiquita", 
                               grepl("Roque", localidad) ~ "Roque Saenz Peña", 
                               grepl("Cordoba", localidad) ~ "Cordoba",
                               grepl("San Francisco Del Mo", localidad) ~ "San Francisco Del Monte",
                               grepl("La Punta", localidad) ~ "La Punta",
                               grepl("Mina Carolina Cnel", localidad) ~ "La Carolina",
                               grepl("Santa Rosa Del Conla", localidad) ~ "Santa Rosa Del Conlara",
                               grepl("Villa San Agustin De", localidad) ~ "Villa San Agustin",
                               grepl("Dique Florentino", localidad) ~ "Dique Florentino Ameghino",
                               grepl("Barilo", localidad) ~ "Bariloche",
                               grepl("Dina Huapi", localidad) ~ "Dina Huapi",
                               grepl("Las Grutas", localidad) ~ "Las Grutas",
                               grepl("Enrique Godo", localidad) ~ "General Enrique Godoy",
                               grepl("General Fernandez Or", localidad) ~ "General Fernandez Oro",
                               grepl("Comandante Luis Pied", localidad) ~ "Comandante Luis Piedrabuena",
                               grepl("Calafate", localidad) ~ "El Calafate",
                               grepl("28 De Noviembre", localidad) ~ "Veintiocho de Noviembre",
                               grepl("Virasor", localidad) ~ "Virasoro",
                               grepl("Carlos Pelle", localidad) ~ "Carlos Pellegrini",
                               grepl("Dpto Sauce", localidad) ~ "Sauce",
                               grepl("Liebig", localidad) ~ "Liebig",
                               grepl("Eldorado", localidad) ~ "Eldorado",
                               grepl("Capiovy", localidad) ~ "Capiovi",
                               grepl("25 De Mayo", localidad) ~ "Veinticinco De Mayo",
                               grepl("Concepcion De La Sie", localidad) ~ "Concepcion De La Sierra",
                               grepl("Manantial", localidad) ~ "Manantial",
                               grepl("Juan Bautista Alberd", localidad) ~ "Juan Bautista Alberdi",
                               grepl("Raco", localidad) ~ "Raco",
                               grepl("Juan Bautista Alberd", localidad) ~ "Juan Bautista Alberdi",
                               grepl("La Trinidad", localidad) ~ "La Trinidad",
                               grepl("Virginia", localidad) ~ "Gobernador Piedrabuena",
                               grepl("San Fernando Del Val", localidad) ~ "San Fernando Del Valle",
                               grepl("Antofagasta De La Si", localidad) ~ "Antofagasta De La Sierra",
                               grepl("Godoy Cruz", localidad) ~ "Godoy Cruz",
                               grepl("guaymallen", localidad) ~ "Guaymallen",
                               grepl("Guaymallen", localidad) ~ "Guaymallen",
                               grepl("Iguazu", localidad) ~ "Puerto Iguazú",
                               grepl("Mayor Drumond", localidad) ~ "Mayor Drummond",
                               grepl("Sumampa", localidad) ~ "Sumampa",
                               grepl("Presidente Avellaned", localidad) ~ "Presidente Avellaneda",
                               grepl("Pampa De Los Guanaco", localidad) ~ "Pampa De Los Guanacos",
                               grepl("Colonia Italia", localidad) ~ "Lavalle",
                               grepl("Tres Esquinas De Cru", localidad) ~ "Cruz De Piedra",
                               grepl("Villanueva", localidad) ~ "Guaymallen",
                               grepl("Barrio San Agustin", localidad) ~ "Paraná",
                               grepl("Profesor Salvador Ma", localidad) ~ "Profesor Salvador Mazza",
                               grepl("Barrio Aeronautico", localidad) ~ "Las Heras",
                               grepl("Barrio Unimev Dep", localidad) ~ "Mendoza",
                               grepl("San Jose Dep 63", localidad) ~ "Mendoza",
                               grepl("Kilometro 8", localidad) ~ "Mendoza",
                               grepl("Seccion Sexta 6Ta", localidad) ~ "Mendoza",
                               grepl("San Martin De Los A|Smdlos Andes|Sana Martin De Los A", localidad) ~ "San Martín de los Andes",
                               grepl("San Patricio Del Cha", localidad) ~ "San Patricio Del Chañar",
                               grepl("Barrio Bouquet Rolda|Barrio El Progreso", localidad) ~ "Neuquen",
                               grepl("Correntoso", localidad) ~ "Villa La Angostura",
                               grepl("San Blas De Los Sauc", localidad) ~ "San Blas De Los Sauces",
                               grepl("Vinchina", localidad) ~ "San Jose De Vinchina",
                               grepl("Baldecito", localidad) ~ "Baldecitos",
                               grepl("Puerto Piramide", localidad) ~ "Puerto Piramides",
                               grepl("28 De Julio", localidad) ~ "Veintiocho De Julio",
                               grepl("Quemu Quemu", localidad) ~ "Quemú Quemú",
                               grepl("Beron De Astrada", localidad) ~ "San Antonio De Itatí",
                               grepl("Agua Amarga", localidad) ~ "Tunuyan",
                               grepl("Fray Luis Beltran", localidad) ~ "Fray Luis Beltrán",
                               grepl("Guaymallen", localidad) ~ "Guaymallén",
                               grepl("San Ramon De La Nuev", localidad) ~ "Orán",
                               grepl("Villa San Lorenzo", localidad) ~ "San Lorenzo",
                               grepl("San Antonio De Los", localidad) ~ "San Antonio De Los Cobres",
                               grepl("Rosario De La Fronte", localidad) ~ "Rosario De La Frontera",
                               grepl("General Gñemes", localidad) ~ "General Guemes",
                               grepl("General Enrique Mosc", localidad) ~ "General Enrique Mosconi",
                               grepl("Santa Rosa De Los Pa", localidad) ~ "Santa Rosa de los Pastos Grandes",
                               grepl("Barrio Parque Tres C", localidad) ~ "Tres Cerritos",
                               grepl("Joaquin V Gonzalez", localidad) ~ "Joaquin V. Gonzalez",
                               grepl("Concepcion Del Urugu", localidad) ~ "Concepcion Del Uruguay",
                               grepl("Pueblo General Belgr", localidad) ~ "Pueblo General Belgrano",
                               grepl("Libertador San Martin", localidad) ~ "Libertador San Martin",
                               grepl("Libertador San Martin", localidad) ~ "Libertador San Martin",
                               grepl("San Jaime De La Fron", localidad) ~ "San Jaime De La Frontera",
                               grepl("Coronel Rodolfo S D", localidad) ~ "Coronel Rodolfo S. Dominguez",
                               grepl("Villa Gobernador Gal", localidad) ~ "Villa Gobernador Galvez",
                               grepl("Puerto General San M", localidad) ~ "Puerto General San Martin",
                               grepl("San Jose De La Esqui", localidad) ~ "San Jose De La Esquina",
                               grepl("Empalme Villa Consti", localidad) ~ "Empalme Villa Constitución",
                               grepl("San Martin De Las Es", localidad) ~ "San Martin De Las Escobas",
                               grepl("Santa Maria De Punil", localidad) ~ "Santa Maria De Punilla",
                               grepl("Los Reartes", localidad) ~ "Los Reartes",
                               grepl("Yacanto", localidad) ~ "Yacanto",
                               grepl("San Antonio De Arred", localidad) ~ "San Antonio De Arredondo",
                               grepl("Santa Rosa De Calamu", localidad) ~ "Santa Mónica",
                               grepl("Villa General Belgra", localidad) ~ "Villa General Belgrano",
                               grepl("Villas Ciudad De Ame", localidad) ~ "Villas Ciudad De América",
                               grepl("Quintas De Argu", localidad) ~ "Quintas De Arguello",
                               grepl("Santa Rosa De Rio I", localidad) ~ "Villa Santa Rosa",
                               grepl("Villa Parque Santa A", localidad) ~ "Santa Ana",
                               grepl("Sarsfield", localidad) ~ "Velez Sarsfield",
                               grepl("Velez", localidad) ~ "Velez Sarsfield",
                               grepl("Colinas De Velez Sar", localidad) ~ "Colinas De Velez Sarsfiel",
                               grepl("Barrio Altos Del Tal", localidad) ~ "El Talar",
                               grepl("Ciudad Jardin", localidad) ~ "Ciudad Jardin",
                               grepl("Los Hornos Las Call", localidad) ~ "Los Hornos",
                               grepl("Lisandro Olmosnoroe", localidad) ~ "Lisandro Olmos",
                               grepl("Lisandro Olmos", localidad) ~ "Lisandro Olmos",
                               grepl("Balneario Monte Herm", localidad) ~ "Monte Hermoso",
                               grepl("Ingeniero Pablo Nogu", localidad) ~ "Pablo Nogues",
                               grepl("Norberto De La Riest", localidad) ~ "Norberto De La Riestra",
                               grepl("Mercado Central Buen", localidad) ~ "Mercado Central",
                               grepl("Fatima Estacion Empa", localidad) ~ "Fatima",
                               grepl("Doctor Domingo Cabre", localidad) ~ "Open Door",
                               grepl("San Bernardo", localidad) ~ "San Bernardo",
                               grepl("Balneario Mar Chiqui", localidad) ~ "Mar Chiquita",
                               grepl("Camino Centenario Ki", localidad) ~ "City Bell",
                               grepl("Barrio Los Troncos D", localidad) ~ "Los Troncos",
                               grepl("La Lonja", localidad) ~ "La Lonja",
                               grepl("Villa Gobernador Uda", localidad) ~ "Villa Gobernador Udaondo",
                               grepl("Capilla Del Seqor E", localidad) ~ "Capilla Del Señor",
                               grepl("Adolfo Gonzales Chav", localidad) ~ "Adolfo Gonzáles Chaves",
                               grepl("Domingo Faustino Sar", localidad) ~ "Domingo Faustino Sarmiento",
                               grepl("Maquinista Francisco", localidad) ~ "Maquinista Francisco Savio",
                               grepl("Mar De Cob", localidad) ~ "Mar De Cobo",
                               grepl("Manuel Jose Cobo", localidad) ~ "Lezama",
                               grepl("Ingeniero Adolfo Sou", localidad) ~ "Ingeniero Adolfo Sourdeaux",
                               grepl("Juan Nepomuceno Fern", localidad) ~ "Juan Nepomuceno Fernandez",
                               grepl("Centro Agricola El P", localidad) ~ "El Pato",
                               grepl("Comandante Nicanor O", localidad) ~ "Comandante Nicanor Otamendi",
                               grepl("Exaltacion De La Cru", localidad) ~ "Exaltacion De La Cruz",
                               grepl("Badvcabrera", localidad) ~ "Córdoba",
                               grepl("Blos Patricios", localidad) ~ "Córdoba",
                               grepl("Bqtasarguello", localidad) ~ "Córdoba",
                               grepl("Bresidamerica", localidad) ~ "Córdoba",
                               grepl("Bsan Martin", localidad) ~ "Córdoba",
                               grepl("Bsan Pablo", localidad) ~ "Córdoba",
                               grepl("Corral De Palo", localidad) ~ "Córdoba",
                               grepl("Country Tejas Del Su", localidad) ~ "Córdoba",
                               grepl("Jardin General Arena", localidad) ~ "Córdoba",
                               grepl("Jose Ignacio Diaz", localidad) ~ "Córdoba",
                               grepl("Villa Quisquizacate", localidad) ~ "Córdoba",
                               grepl("Villa Quisquizacate", localidad) ~ "Córdoba",
                               grepl("Bcabdel Pilar", localidad) ~ "Córdoba",
                               grepl("Chacra De La Merced", localidad) ~ "Córdoba",
                               grepl("Villa Aspacia Aatra", localidad) ~ "Córdoba",
                               grepl("Villa Eucaristia", localidad) ~ "Córdoba",
                               grepl("Villa Los Pinos", localidad) ~ "Córdoba",
                               grepl("Villa Mafekñn", localidad) ~ "Córdoba",
                               grepl("Villa Maurizzi", localidad) ~ "Córdoba",
                               grepl("B Jardin Del", localidad) ~ "Córdoba",
                               grepl("Villa Rio Ycho Cruz", localidad) ~ "Icho Cruz",
                               grepl("San Martin De Las Esquina", localidad) ~ "San Martín De Las Escobas",
                               grepl("Colonia Francesa", localidad) ~ "San Javier",
                               grepl("Empalme Granero", localidad) ~ "Rosario",
                               grepl("Barrio Parque Field", localidad) ~ "Rosario",
                               grepl("Barrio Remedios De E", localidad) ~ "Rosario",
                               grepl("Barrio Ludueña", localidad) ~ "Rosario",
                               grepl("Campo Del Medio", localidad) ~ "Helvecia",
                               grepl("Barrio Nueva Roma", localidad) ~ "Casilda",
                               grepl("Ibicuy", localidad) ~ "Puerto Ibicuy",
                               grepl("General Obrien", localidad) ~ "Eduardo O'Brien",
                               grepl("Gregorio De Lafe", localidad) ~ "Laferrere",
                               grepl("Juan M Gutierrez", localidad) ~ "Guernica",
                               grepl("Muññiz", localidad) ~ "Muñiz",
                               grepl("Pueblo Santa Maria", localidad) ~ "Santa María",
                               grepl("San Clemente Del Tuy", localidad) ~ "San Clemente Del Tuyu",
                               grepl("Villa Santos Tesei", localidad) ~ "Villa Tesei",
                               grepl("Barrio Los Cachorros", localidad) ~ "Pilar",
                               grepl("Barrio Mariano Moren", localidad) ~ "Mariano Moreno",
                               grepl("Barrio Maritimo", localidad) ~ "Berazategui",
                               grepl("Barrio Parque Luro", localidad) ~ "Mar Del Plata",
                               grepl("Barrio Acevedo", localidad) ~ "Mar Del Plata",
                               grepl("Villa Lhermosa", localidad) ~ "Mar Del Plata",
                               grepl("Dfsarmiento", localidad) ~ "Mar Del Plata",
                               grepl("Barrio Parque San Ma", localidad) ~ "Mar Del Plata",
                               grepl("Barrio P Rocco", localidad) ~ "Mar Del Plata",
                               grepl("Barrio Pasco", localidad) ~ "Quilmes",
                               grepl("Barrio San Pablo", localidad) ~ "Pilar",
                               grepl("Adolfo Gonzales Chavez", localidad) ~ "Adolfo Gonzáles Chaves",
                               grepl("Del Talar", localidad) ~ "El Talar",
                               grepl("Claromeco", localidad) ~ "Claromeco",
                               grepl("Villa San Andres", localidad) ~ "San Martín",
                               grepl("Barrio Troncos Del T", localidad) ~ "El Talar",
                               grepl("Barrio Loma Verde", localidad) ~ "La Reja",
                               grepl("Moreras", localidad) ~ "Luis Guillon",
                               grepl("Barrio Juan B Justo", localidad) ~ "Berisso",
                               grepl("Barrio Iparraguirre", localidad) ~ "Ituzaingó",
                               grepl("Barrio Faro", localidad) ~ "Mar Del Plata",
                               grepl("Barrio Alfar", localidad) ~ "Mar Del Plata",
                               grepl("San Salvador De Juju", localidad) ~ "San Salvador De Jujuy",
                               grepl("Perico", localidad) ~ "Perico",
                               grepl("Telen", localidad) ~ "Telén",
                               grepl("Baños Pismanta", localidad) ~ "Pismanta",
                               grepl("General Manuel Belgr", localidad) ~ "General Manuel Belgrano",
                               grepl("Mayor Edmundo Villaf", localidad) ~ "Mayor Villafañe",
                               grepl("Baños Termales", localidad) ~ "Fiambalá",
                               grepl("San Amesquiu", localidad) ~ "Fray Mamerto Esquiú",
                               grepl("Termas De Reyes|Villa Jardin De Reye", localidad) ~ "Villa Jardín De Reyes",
                               grepl("Bo Vte Sola Sal", localidad) ~ "Salta",
                               grepl("Gal Santo Domingo", localidad) ~ "Córdoba",
                               grepl("Bampramerica|Bampamerica|Bpanamericano|Bpanamerican|Bapanamerican", localidad) ~ "Córdoba",
                               grepl("Byofre", localidad) ~ "Córdoba",
                               grepl("Bpueyrredon", localidad) ~ "Córdoba",
                               grepl("Bparque Liceo", localidad) ~ "Córdoba",
                               grepl("Bituzaingo An", localidad) ~ "Córdoba",
                               grepl("Cerro Colorado Tulum", localidad) ~ "Cerro Colorado",
                               grepl("Bñsan Jose", localidad) ~ "Córdoba",
                               grepl("Falda Del Carmen", localidad) ~ "Falda Del Carmen",
                               grepl("Country Fortñn Del P|Balto Gralpaz|B1ro De Mayo", localidad) ~ "Córdoba",
                               grepl("Bgralbustos|Bo Villa Cabrer|Residencial Ame", localidad) ~ "Córdoba",
                               grepl("Secc 10 Alberdi", localidad) ~ "Rosario",
                               grepl("San Jeronimo Del Sau", localidad) ~ "San Jeronimo Del Sauce",
                               TRUE ~ localidad))

# luego, creamos una variable dirección.
data_emp <- data_emp %>% 
  mutate(llave = paste0(localidad,", ", provincia, ", Argentina")) 
# %>% 
#   mutate(across(provincia:localidad, tolower),
#          localidad = str_replace(localidad, "�", "ñ"),
#          localidad = str_remove_all(localidad, "[:punct:]")) %>% 
#   mutate(localidad = ifelse(localidad=="capital federal", "ciudad de buenos aires", localidad),
#          provincia = ifelse(provincia == "ciudad autónoma de buenos aires", "ciudad de buenos aires", provincia))

empresas_geo <- read_file_srv("/srv/DataDNMYE/padron_afip/localidades_padron.geojson") %>% 
  distinct(provincia, localidad, llave, .keep_all = T)

#Para join geo
data_emp_geo <- data_emp %>% 
  left_join(empresas_geo %>% select(llave), by = "llave") %>% 
  st_as_sf()


#agrego deptos ign
deptos <- read_sf("/srv/DataDNMYE/capas_sig/departamento.json") %>% 
  filter(nam != "Antártida Argentina") %>% 
  st_make_valid()

#joineo y unifico CABA (no está por comuna)
data_emp_geo <- st_join(data_emp_geo, deptos) %>% 
  mutate(departamento_arcgis = ifelse(str_detect(nam, "Comuna"), "CABA", nam),
         provincia = ifelse(str_detect(nam, "Comuna"), 
                            "Ciudad Autónoma de Buenos Aires", provincia),
         in1 = ifelse(departamento_arcgis == "CABA", "02007", in1))

empresas_afip_loc <- data_emp_geo %>% 
  group_by(provincia, in1, departamento_arcgis, cat_rct,cat_empresa) %>% 
  summarise(cantidad = n()) %>% 
  filter(!is.na(provincia))


env_empresas_categoria_tamaño <- SharedData$new(empresas_afip_loc %>% 
                                                  st_drop_geometry() %>% 
                                                  group_by(provincia, cat_rct,cat_empresa) %>% 
                                                  summarise(cantidad = sum(cantidad)) %>% 
                                                  pivot_wider(names_from = cat_empresa, values_from = cantidad) %>% 
                                                  select(provincia, cat_rct, micro, pequeña = pequenia, mediana, grande),
                                                key = ~ provincia,
                                                group = "provincia")

env_empresas_dpto_cat <- SharedData$new(empresas_afip_loc %>% 
                                          st_drop_geometry() %>% 
                                          group_by(provincia,  departamento_arcgis, cat_rct) %>% 
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
                                     buttons = list(list(extend = 'csv', filename = "empresas"),
                                                    list(extend = 'excel', filename = "empresas")),
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
                                     buttons = list(list(extend = 'csv', filename = "empresas"),
                                                    list(extend = 'excel', filename = "empresas")),
                                     text = 'Descargar'
                                ))),
                 rownames= FALSE,  filter = list(position = 'top', clear = FALSE),
                 colnames = c('Provincia', 'Departamento', 'Alojamiento', 'Gastronomía', 'Transporte', 'Agencias de Viaje', 'Otros Servicios Turísticos')
  )



# 
# mapa_base <- geoAr::get_geo("ARGENTINA") %>% 
#   geoAr::add_geo_codes()
# 
# mapa_base <- mapa_base %>% 
#   mutate(name_prov = limpiar_texto(name_prov, enie = F),
#          nomdepto_censo = limpiar_texto(nomdepto_censo, enie = F)) %>% 
#   mutate(nomdepto_censo = case_when(
#     nomdepto_censo == "comuna 01" ~ "comuna 1",
#     nomdepto_censo == "comuna 02" ~ "comuna 2",
#     nomdepto_censo == "comuna 03" ~ "comuna 3",
#     nomdepto_censo == "comuna 04" ~ "comuna 4",
#     nomdepto_censo == "comuna 05" ~ "comuna 5",
#     nomdepto_censo == "comuna 06" ~ "comuna 6",
#     nomdepto_censo == "comuna 07" ~ "comuna 7",
#     nomdepto_censo == "comuna 08" ~ "comuna 8",
#     nomdepto_censo == "comuna 09" ~ "comuna 9", 
#     T ~ nomdepto_censo),
#     key = paste(name_prov, nomdepto_censo))


# empresas_afip_loc_mapa <- empresas_afip_loc %>% 
#   group_by(provincia, departamento_arcgis, cat_rct) %>% 
#   summarise(cantidad = sum(cantidad)) %>%
#   filter(cantidad >= 3) %>% 
#   mutate(key = paste( limpiar_texto(provincia, enie = F),limpiar_texto(departamento_arcgis,enie =F))) %>% 
#   filter(!is.na(provincia) & !is.na(departamento_arcgis))
# 
# sum(empresas_afip_loc_mapa$cantidad[(empresas_afip_loc_mapa$key) %in% mapa_base$key])/sum(empresas_afip_loc_mapa$cantidad)
# 
# 
# empresas_afip_dpto_geo <- left_join(mapa_base, empresas_afip_loc_mapa, by = "key")
# 
# empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>% 
#   group_by(provincia, departamento_arcgis, cat_rct) %>% 
#   summarise(empresas = sum(cantidad, na.rm = T))
# 
# empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>%
#   st_cast("MULTIPOLYGON") %>% 
#   st_centroid() 
# 
# empresas_afip_dpto_geo <- bind_cols(empresas_afip_dpto_geo, st_coordinates(empresas_afip_dpto_geo)) %>% 
#   st_drop_geometry() %>% 
#   rename(long  = X, lat = Y)

#empresas por depto
data_emp_depto <- empresas_afip_loc %>% 
  st_drop_geometry() %>%
  group_by(provincia, in1, departamento_arcgis, cat_rct) %>% 
  filter(cantidad >= 3) %>% 
  summarise(empresas = sum(cantidad, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(deptos %>% select(in1,geometry), by = "in1") %>% 
  st_as_sf() %>% 
  st_centroid()


# empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>% 
#   st_centroid()
# 
# empresas_afip_dpto_geo <- empresas_afip_dpto_geo %>%  
#   mutate(long = unlist(map(geometry,1)),
#          lat = unlist(map(geometry,2)))

# empresas_afip_dpto_geo_wider <- empresas_afip_dpto_geo %>% 
#   pivot_wider(names_from = cat_rct, values_from = empresas) %>% 
#   select(-c("NA"))
# 
# datachart <- empresas_afip_dpto_geo_wider %>% ungroup() %>% select(Gastronomía, Transporte, Alojamiento, `Agencias de Viaje`, `Otros Servicios Turísticos`) %>% 
#   ungroup()
# 
# leaflet() %>% 
#   addArgTiles() %>% 
#   addMinicharts(
#     empresas_afip_dpto_geo_wider$long, empresas_afip_dpto_geo_wider$lat,
#     chartdata = datachart,
#     # colorPalette =,
#     width = 45, height = 45
#   )

paleta1 <- RColorBrewer::brewer.pal(n = 5, "Reds")

empresas_afip_dpto_geo <- data_emp_depto %>%
  group_by(provincia, departamento_arcgis, cat_rct = "Total") %>%
  summarise(empresas = sum(empresas)) %>%
  bind_rows(data_emp_depto)

empresas_map_data <- empresas_afip_dpto_geo %>% 
  filter(!is.na(provincia)) %>% 
  #ungroup() %>% 
  mutate(
    etiqueta = paste0(departamento_arcgis, "<br>", cat_rct,"<br>Empresas Registradas:", empresas))

empresas_map_data <- empresas_map_data %>% 
  mutate(key = paste(provincia, departamento_arcgis, cat_rct, sep = "-")) %>% 
  st_cast("POINT") %>% 
  st_as_sf()


fix_chaco <- empresas_map_data %>%
  filter(provincia == "Chaco" & departamento_arcgis == "Almirante Brown") %>% 
  mutate(geometry = st_geometry(st_point(c(-61.592346, -25.589289)))) %>% 
  st_set_crs(st_crs(empresas_map_data))

empresas_map_data <- empresas_map_data %>% 
  filter(!(provincia == "Chaco" & departamento_arcgis == "Almirante Brown")) %>% 
  rbind(fix_chaco)

prov_lims <-  geoAr::get_geo("ARGENTINA", level = "provincia") %>% geoAr::add_geo_codes() %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  bind_cols(geoAr::get_geo("ARGENTINA", level = "provincia") %>% geoAr::add_geo_codes()  %>% st_drop_geometry(),.) %>% 
  rename(long  = X, lat = Y) %>% 
  mutate(join_key = herramientas::limpiar_texto(ifelse(str_detect(name_iso, "Ciudad"), "CABA", name_iso))) 

master_mapa <- SharedData$new(empresas_map_data %>% 
                                mutate(join_key = herramientas::limpiar_texto(provincia)) %>% 
                                left_join(prov_lims),
                              key = ~ key, group = "mapa") 

env_total_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                   cat_rct == "Total") %>% 
                                       mutate(hexcolor = colorNumeric(domain = log(empresas),
                                                                      palette =paleta1 )(log(empresas))),
                                     key = ~ key, group = "mapa")


env_alojamientos_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                          cat_rct == "Alojamiento") %>% 
                                              mutate(hexcolor = colorNumeric(domain = log(empresas),
                                                                             palette =paleta1 )(log(empresas))),
                                            key = ~ key, group = "mapa")

env_gastro_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                    cat_rct == "Gastronomía") %>% 
                                        mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                      key = ~ key, group = "mapa")

env_transporte_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                        cat_rct == "Transporte") %>% 
                                            mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                          key = ~ key, group = "mapa")

env_agencias_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                      cat_rct == "Agencias de Viaje") %>% 
                                          mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                        key = ~ key, group = "mapa")

env_otros_map_data <- SharedData$new(data = filter(empresas_map_data,
                                                   cat_rct == "Otros Servicios Turísticos") %>% 
                                       mutate(hexcolor = colorNumeric(domain = log(empresas), palette =paleta1 )(log(empresas))),
                                     key = ~ key, group = "mapa")

legendTotal <- filter(empresas_map_data,
                      cat_rct == "Total") %>%
  mutate(hexcolor = colorNumeric(domain = log(empresas),
                                 palette =
                                   paleta1)(log(empresas)),
         log_empresas = log(empresas)) %>% 
  select(hexcolor, empresas, log_empresas)

palTotal <- colorNumeric(domain = legendTotal$log_empresas,
                         palette = paleta1)

legendAlojamiento <- filter(empresas_map_data,
                            cat_rct == "Alojamiento") %>%
  mutate(hexcolor = colorNumeric(domain = log(empresas),
                                 palette =
                                   paleta1)(log(empresas)),
         log_empresas = log(empresas)) %>% 
  select(hexcolor, empresas, log_empresas)

palAlojamiento <- colorNumeric(domain = legendAlojamiento$log_empresas,
                               palette = paleta1)

legendTransporte <- filter(empresas_map_data,
                           cat_rct == "Transporte") %>%
  mutate(hexcolor = colorNumeric(domain = log(empresas),
                                 palette =
                                   paleta1)(log(empresas)),
         log_empresas = log(empresas)) %>% 
  select(hexcolor, empresas, log_empresas)

palTransporte <- colorNumeric(domain = legendTransporte$log_empresas,
                              palette = paleta1)

legendGastronomia <- filter(empresas_map_data,
                            cat_rct == "Gastronomía") %>%
  mutate(hexcolor = colorNumeric(domain = log(empresas),
                                 palette =
                                   paleta1)(log(empresas)),
         log_empresas = log(empresas)) %>% 
  select(hexcolor, empresas, log_empresas)

palGastronomia <- colorNumeric(domain = legendGastronomia$log_empresas,
                               palette = paleta1)

legendAgencias <- filter(empresas_map_data,
                         cat_rct == "Agencias de Viaje") %>%
  mutate(hexcolor = colorNumeric(domain = log(empresas),
                                 palette =
                                   paleta1)(log(empresas)),
         log_empresas = log(empresas)) %>% 
  select(hexcolor, empresas, log_empresas)

palAgencias <- colorNumeric(domain = legendAgencias$log_empresas,
                            palette = paleta1)

legendOtros <- filter(empresas_map_data,
                      cat_rct == "Otros Servicios Turísticos") %>%
  mutate(hexcolor = colorNumeric(domain = log(empresas),
                                 palette =
                                   paleta1)(log(empresas)),
         log_empresas = log(empresas)) %>% 
  select(hexcolor, empresas, log_empresas)

palOtros <- colorNumeric(domain = legendOtros$log_empresas,
                         palette = paleta1)

mapa <- leaflet(height = 600) %>%
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addArgTiles() %>%
  addCircleMarkers(data = env_total_map_data, radius = 10, weight = 2, color = "black", stroke = T, 
                   fillColor = ~ hexcolor, fillOpacity =  .8, 
                   group = "Total", popup = ~ lapply(etiqueta, htmltools::HTML),
                   label = ~ lapply(etiqueta, htmltools::HTML)) %>% 
  addCircleMarkers(data = env_alojamientos_map_data, radius = 10,  weight = 2, color = "black",
                   fillColor = ~ hexcolor, stroke = T, fillOpacity =  .8,  popup = ~ lapply(etiqueta, htmltools::HTML),
                   group = "Alojamiento", 
                   label = ~ lapply(etiqueta, htmltools::HTML)) %>%
  addCircleMarkers(data = env_transporte_map_data, radius = 10,  weight = 2, color = "black",
                   fillColor = ~ hexcolor, stroke = T, fillOpacity =  .8, popup = ~ lapply(etiqueta, htmltools::HTML),
                   group = "Transporte", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
  addCircleMarkers(data = env_gastro_map_data, radius = 10,  weight = 2, color = "black",
                   fillColor = ~ hexcolor, stroke = T, fillOpacity =  .8, popup = ~ lapply(etiqueta, htmltools::HTML),
                   group = "Gastronomía", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
  addCircleMarkers(data = env_agencias_map_data, radius = 10,  weight = 2, color = "black",
                   fillColor = ~ hexcolor, stroke = T, fillOpacity =  .8, popup = ~ lapply(etiqueta, htmltools::HTML),
                   group = "Agencias de Viaje", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
  addCircleMarkers(data = env_otros_map_data, radius = 10,  weight = 2, color = "black",
                   fillColor = ~ hexcolor, stroke = T, fillOpacity =  .8, popup = ~ lapply(etiqueta, htmltools::HTML),
                   group = "Otros rubros", label = ~ lapply(etiqueta, htmltools::HTML)) %>%
  addLayersControl(
    baseGroups = c("Total","Alojamiento", "Transporte", "Gastronomía", "Agencias de Viaje", "Otros rubros"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(layerId = "Total", bins = seq(from = min(legendTotal$log_empresas),
                                          to = max(legendTotal$log_empresas),
                                          length.out = 5),
            pal = palTotal, values = legendTotal$log_empresas ,  
            labFormat = labelFormat(transform = function(x) exp(x), digits = 0, big.mark = ".")
  ) %>% 
  
  addLegend(layerId = "Alojamiento",  bins = seq(from = min(legendAlojamiento$log_empresas), to = max(legendAlojamiento$log_empresas), length.out = 5),
            pal = palAlojamiento, values = legendAlojamiento$log_empresas, 
            labFormat = labelFormat(transform = function(x) exp(x), digits = 0, big.mark = ".")) %>% 
  
  addLegend(layerId = "Transporte", bins = seq(from = min(legendTransporte$log_empresas), to = max(legendTransporte$log_empresas), length.out = 5),
            pal = palTransporte, values = legendTransporte$log_empresas, 
            labFormat = labelFormat(transform = function(x) exp(x), digits = 0, big.mark = ".")) %>% 
  
  addLegend(layerId = "Gastronomía", bins = seq(from = min(legendGastronomia$log_empresas), to = max(legendGastronomia$log_empresas), length.out = 5),
            pal = palGastronomia, values = legendGastronomia$log_empresas, 
            labFormat = labelFormat(transform = function(x) exp(x), digits = 0, big.mark = ".")) %>% 
  
  addLegend(layerId = "Agencias de Viaje", bins = seq(from = min(legendAgencias$log_empresas), to = max(legendAgencias$log_empresas), length.out = 5),
            pal = palAgencias, values = legendAgencias$log_empresas, 
            labFormat = labelFormat(transform = function(x) exp(x), digits = 0, big.mark = ".")) %>%
  addLegend(layerId = "Otros rubros", bins = seq(from = min(legendOtros$log_empresas), to = max(legendOtros$log_empresas), length.out = 5),
            pal = palOtros, values = legendOtros$log_empresas, 
            labFormat = labelFormat(transform = function(x) exp(x), digits = 0, big.mark = ".")) %>% 
  # addEasyButton(easyButton(
  #   icon="fa-globe", title="Zoom to Level 1",
  #   onClick=JS("function(btn, map){ map.setZoom(3); }"))) %>% 
  # https://gist.github.com/noamross/98c2053d81085517e686407096ec0a69
  htmlwidgets::onRender("
    function(el, x) {
      var initialLegend = 'Total' // Set the initial legend to be displayed by layerId
      var myMap = this;
      for (var legend in myMap.controls._controlsById) {
        var el = myMap.controls.get(legend.toString())._container;
        if(legend.toString() === initialLegend) {
          el.style.display = 'block';
        } else {
          el.style.display = 'none';
        };
      };
    myMap.on('baselayerchange',
      function (layer) {
        for (var legend in myMap.controls._controlsById) {
          var el = myMap.controls.get(legend.toString())._container;
          if(legend.toString() === layer.name) {
            el.style.display = 'block';
          } else {
            el.style.display = 'none';
          };
        };
      });
    }") %>% 
  htmlwidgets::onRender(
    "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")

empresas <- withr::with_options(
  list(persistent = TRUE), 
  bscols(widths = 12, 
         filter_select("empresas", "Elegir una provincia", env_empresas_categoria_tamaño, ~ provincia,
                       multiple = F),
         htmltools::h3('Cantidad de empresas registradas por provincia y por actividad según tamaño de la empresa'),
         dt_empresas_categoria_tamaño,
         htmltools::br(),
         htmltools::h3('Cantidad de empresas registradas por provincia, departamento y actividad'),
         dt_empresas_dpto_cat,
         htmltools::br(),
         htmltools::h3('Mapa de la distribución de empresas registradas por departamento y actividad'),
         filter_select("mapa", "Elegir una provincia", master_mapa, ~ provincia,
                       multiple = F),
         htmltools::br(),
         mapa,
         htmltools::br()
  )
)

write_rds(empresas, "outputs/empresas.rds")
