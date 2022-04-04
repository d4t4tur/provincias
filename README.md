# Monitor de Estadísticas Turísticas de la Provincias

## Descripción :speech_balloon:

**Este proyecto busca presentar resumidamente los principales indicadores sobre el turismo a nivel provincial**

:calendar: Frecuencia de publicación de los productos:

- **Los datos se irán actualizando en función de los insumos que usa cada sección**

:globe_with_meridians: Se publican en: **https://provincias.yvera.tur.ar/**

## Contenidos :test_tube:

**Detalle del contenido**

* La web está dividida por grandes ejes temáticos: turismo interno, turismo receptivo, padrón de alojamientos, áreas naturales, conectividad aérea, y prestadores turísticos. Para cada uno de los ejes se presentan los datos correspondientes a cada provincia.

* Para la creación de los paneles que permiten seleccionar las provincias nos basamos en el código orignal de **📦[{sknifedatar}](https://rafzamb.github.io/sknifedatar/reference/automagic_tabs.html)**. 
 
[Ejemplo de uso para el indicador de prestadores turísticos:](https://github.com/dnme-minturdep/provincias/blob/main/prestadores.Rmd) 

`r rmdautotabs(input_data = prestadores_nest_data, panel_name = "provincia", .tablas_prov)`

## Cómo contribuir con el proyecto :twisted_rightwards_arrows:

Para colaborar en este proyecto, se recomienda hacer un Fork, trabajar sobre ese repositorio forkeado y hacer contribución para que el propietario vea qué cambios se realizaron, esté al tanto y lo actualice.
