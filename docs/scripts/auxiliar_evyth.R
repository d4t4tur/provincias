crear_etiqueta <- function (base, variables = NULL, drop_vars = T) 
{
  diccionario <- data.table::fread("entradas/diccionario_evyth.csv")
  diccionario <- diccionario %>% filter(!is.na(etiquetas) & !is.na(valores)) 
  diccionario$valores <- diccionario$valores %>% as.double()
  if (is.null(variables)) {
    variables <- names(base)
  }
  if (is.numeric(variables)) {
    variables <- names(base[variables])
  }
  coincidencias <- c()
  for (i in variables) {
    labels <- diccionario[diccionario$variable == i & !is.na(diccionario$valores), 
                          c("valores", "etiquetas")]
    if (nrow(labels) == 0) {
      next
    }
    else {
      coincidencias <- c(coincidencias, i)
      names(labels) <- c(i, paste0(i, "_label"))
      base <- dplyr::left_join(base, labels)
    }
    if (drop_vars == T) {
      base <- dplyr::select(base, -dplyr::any_of(i))
      base <- dplyr::rename_with(base, .cols = dplyr::matches(match = paste0(i, 
                                                                             "_label")), .fn = ~i)
    }
  }
  message(paste(length(coincidencias), ifelse(length(coincidencias) == 
                                                1, "variable etiquetada:", "variables etiquetadas:")))
  message(paste(coincidencias, collapse = ", "))
  base
}



bind_ipc <- function(base, var_join_x, serie = "mensual", var_join_y ="indice_tiempo") {
  
  assertthat::assert_that(serie %in% c("mensual", "trimestral"),
                          msg = "'serie' debe ser igual a 'trimestral' o 'mensual'")
  
  if (serie == "mensual") {
    url <- "https://infra.datos.gob.ar/catalog/sspm/dataset/145/distribution/145.3/download/indice-precios-al-consumidor-nivel-general-base-diciembre-2016-mensual.csv"
    ipc <- read_csv(url) %>% select(indice_tiempo, ipc_ng_nacional)
  }
  
  if (serie == "trimestral") {
    url <- "https://infra.datos.gob.ar/catalog/sspm/dataset/145/distribution/145.1/download/indice-precios-al-consumidor-nivel-general-base-diciembre-2016-trimestral.csv"
    ipc <- read_csv(url) %>% select(indice_tiempo, ipc_ng_nacional)
  }
  
  vector <- var_join_y
  
  names(vector) <- var_join_x
  
  base <- left_join(base, ipc, by = vector)
  
  base
}



disenio_evyth <- function(x) {
  svydesign(ids = ~id_hogar, 
            strata = ~i16_region,
            weights  = ~ pondera,
            data = x, nest = T)
}

estimar_totales <- function(disenio, formulas) {
  svytotal(x = formulas, design = disenio) %>% 
    cbind(., cv(.)) %>%
    as_tibble(rownames = "indicadores") %>%
    rename(valor = 2, cv = V2) 
}


estimar_promedios <- function(disenio, formulas) {
  svymean(x = formulas, design = subset(disenio, tipo_visitante==1)) %>% 
    cbind(., cv(.)) %>%
    as_tibble(rownames = "indicadores") %>%
    rename(valor = 2, cv = V2) 
}

estimar_totales_prov <- function(disenio, formulas) {
  svyby(formula = formulas, by = ~provincia_destino,
        design = disenio, FUN = svytotal,
        vartype = c("cv")) %>%
    as_tibble()
}


estimar_promedios_prov <- function(disenio, formulas) {
  svyby(formula = formulas, by = ~provincia_destino,
        design = subset(disenio,  tipo_visitante==1), FUN = svymean,
        vartype = c("cv")) %>%
    as_tibble()
}


estimar_totales_region <- function(disenio, formulas) {
  svyby(formula = formulas, by = ~region_destino,
        design = disenio, FUN = svytotal,
        vartype = c("cv")) %>%
    as_tibble()
}


estimar_promedios_region <- function(disenio, formulas) {
  svyby(formula = formulas, by = ~region_destino,
        design = subset(disenio,  tipo_visitante==1), FUN = svymean,
        vartype = c("cv")) %>%
    as_tibble()
}

armar_tabla <- function(x, var_group = "provincia_destino") {
  x %>%
    select(all_of(var_group), !starts_with("cv")) %>%  pivot_longer(cols= -var_group, 
                                                                    names_to = "indicadores",
                                                                    values_to = "valor") %>%
    left_join(
      # tablas de cv
      x %>% 
        select(all_of(var_group), starts_with("cv")) %>%  pivot_longer(cols= -var_group, 
                                                                       names_to = "names",
                                                                       values_to = "valor") %>%
        separate(names, remove = F, into =c( "medida", "indicadores"), sep = "\\.") %>% 
        filter(medida == "cv") %>% select(-names),
      by = c(var_group, "indicadores")
    ) %>% 
    select(-medida) %>% rename(cv = valor.y, valor = valor.x)
}

tabla_conjunta <- function(prov, reg, nac) {
  
  prov %>% 
    left_join(provincias_region) %>% 
    left_join(reg,
              by = c("region_destino", "indicadores")) %>% 
    left_join(nac,
              by = "indicadores") %>% 
    select(-c(region_destino)) %>% 
    evyth::crear_etiqueta() %>% 
    relocate(provincia_destino) %>% 
    rename(valor_prov = valor.x , cv_prov = cv.x,
           valor_reg = valor.y, cv_reg = cv.y,
           valor_nac = valor, cv_nac = cv) 
}


calcular_prop <- function(variable, disenio, f = NULL) {
  var_name <- variable
  
  if (is.null(f)) {
    formula_perfil <- formula(paste("~", var_name))
  } else {
    formula_perfil <- formula(paste("~", f))
  }
  
  
  svymean(x = formula_perfil,
          design = subset(disenio,
                          tipo_visitante==1 & !is.na(get(variable)))) %>% 
    cbind(., cv(.)) %>% 
    as_tibble(rownames = "indicadores") %>% 
    rename(valor = ".", cv = V2) %>% 
    mutate(indicadores = str_remove_all(pattern = var_name,
                                             indicadores))
}


calcular_prop_by <- function(variable, disenio, agrupamiento, f = NULL) {
  
  var_name <- variable
  
  if (is.null(f)) {
    formula_perfil <- formula(paste("~", var_name))
  } else {
    formula_perfil <- formula(paste("~", f))
  }
  
  formula_by <- formula(paste("~", agrupamiento))
  
  svyby(formula = formula_perfil, by = formula_by,
        design = subset(disenio,
                        tipo_visitante==1 & !is.na(get(variable))),
        FUN = svymean,
        vartype = c("cv")) %>%
    as_tibble() %>% 
    rename_with(.cols = -1, .fn = ~ str_remove_all(.x, pattern = var_name))
}

