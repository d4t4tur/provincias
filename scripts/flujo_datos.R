library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

graph <- grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]

  # several 'node' statements
  node [shape = rectangle,
        fontname = Helvetica]
        
  dataEvyth [label = 'EVYTH']
  dataEti [label = 'ETI']
  dataDnm [label = 'DNM']
  dataPuna [label = 'PUNA']
  dataAnac [label = 'ANAC']
  dataAfip [label = 'AFIP']
  dataParques [label = 'PARQUES']
  
  dataEvyth; dataEti; dataDnm; dataPuna; dataAnac; dataAfip; dataParques

  node [shape = ellipse,
        fixedsize = false] // sets as parallelograms
        
  rInterno [label = 'interno.R']
  rReceptivo [label = 'receptivo.R']
  rAereo [label = 'aereo.R']
  rPadron [label = 'padron.R']
  rPrestadoresMapas [label = 'prestadores_mapas.R']
  rPrestadores [label = 'prestadores.R']
  rParques [label = 'parques.R']

  rInterno; rReceptivo; rAereo; rPadron; rPrestadoresMapas; rPrestadores; rParques
  
  dataEvyth->rInterno dataEti->rReceptivo dataDnm->rReceptivo dataPuna->rPadron 
  dataAnac->rAereo dataDnm->rAereo dataAfip->rPrestadores
  dataAfip->rPrestadoresMapas dataParques->rParques
  
   node [shape = cds,
        fixedsize = false] // sets as parallelograms
        
  rdsInterno [label = 'evyth_nest_data.RDS']
  rdsReceptivo [label = 'receptivo_nest_data.RDS']
  rdsAereoCabotaje [label = 'aero_cabotaje_nest_data.RDS']
  rdsAereoInternacional [label = 'aero_internacional_nest_data.RDS']
  rdsPadron [label = 'serie_puna_nest_data.RDS']
  rdsPrestadoresMapas [label = 'prestadores_map_nest_data.RDS']
  rdsPrestadores [label = 'prestadores_nest_data.RDS']
  rdsParques [label = 'parques_nest_data.RDS']

  rdsInterno; rdsReceptivo; rdsAereoCabotaje; rdsAereoInternacional; rdsPadron;
  rdsPrestadoresMapas; rdsPrestadores; rdsParques;
  
  rInterno->rdsInterno rReceptivo->rdsReceptivo rPadron->rdsPadron
  rAereo->rdsAereoCabotaje rAereo->rdsAereoInternacional
  rPrestadoresMapas->rdsPrestadoresMapas
  rPrestadores->rdsPrestadores rParques->rdsParques
  
  node [shape = ellipse,
        fixedsize = false] // sets as parallelograms
        
  rmdInterno [label = 'interno.Rmd']
  rmdReceptivo [label = 'receptivo.Rmd']
  rmdAereo [label = 'aereo.Rmd']
  rmdParques [label = 'parques.Rmd']
  rmdPrestadores [label = 'prestadores.Rmd']
  rmdPadron [label = 'padron.Rmd']

  rdsInterno->rmdInterno; rdsReceptivo->rmdReceptivo; rdsAereoCabotaje->rmdAereo;
  rdsAereoInternacional->rmdAereo; rdsParques->rmdParques;
  rdsPrestadores->rmdPrestadores; rdsPrestadoresMapas->rmdPrestadores; rdsPadron->rmdPadron

    
  node [shape = box,
        fixedsize = false] // sets as boxs
        
  htmlInterno [label = 'interno.html']
  htmlReceptivo [label = 'receptivo.html']
  htmlAereo [label = 'aereo.html']
  htmlParques [label = 'parques.html']
  htmlPrestadores [label = 'prestadores.html']
  htmlPadron [label = 'padron.html']
  
  rmdInterno->htmlInterno; rmdReceptivo->htmlReceptivo; rmdAereo->htmlAereo; rmdParques->htmlParques;
  rmdPrestadores->htmlPrestadores; rmdPadron->htmlPadron

  # several 'edge' statements
  
  }
")

graph

graph %>%
  export_svg %>% charToRaw %>% rsvg_png("outputs/flujo_de_datos.png")

export_graph(graph,
             file_name = "outputs/flujo_de_datos.png",
             file_type = "png")
