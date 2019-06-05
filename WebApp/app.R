library(shiny)
library(DT)
library(shinyBS)
library(markdown)
library(knitr)
library(leaflet)
library(dplyr)
library(leaflet.extras)

bd <-  read.table(file = "db/df_depurada_1.csv", header = T, sep = ",")
bd2014 <- bd[bd$PERIODO==2014,]
bd2015 <- bd[bd$PERIODO==2015,]
bd2016 <- bd[bd$PERIODO==2016,]
bd2017 <- bd[bd$PERIODO==2017,]
bd2018 <- bd[bd$PERIODO==2018,]

columnas_labels <-
  c(
    "PERIODO",
    "MES",
    "DIA",
    "CLASE",
    "GRAVEDAD",
    "BARRIO",
    "COMUNA"
  )

# Variables related to css stylesheets

style <- "my_css.css"


# Variables related to header and footer

title <- "Accidentalidad en Medellín"

first_menu_button <- "Visualización de datos"

second_menu_button <- "Predictor de satisfacción"

third_menu_button <- "Acerca del estudio"

fourth_menu_button <- "Créditos"


# Submenus

data_2018_button <- "Datos analizados 2018"

data_2017_button <- "Datos analizados 2017"

data_2016_button <- "Datos analizados 2016"

data_2015_button <- "Datos analizados 2015"

data_2014_button <- "Datos analizados 2014"

# Footer

footer_text <- "Este sera el footer"

logo_tag <- '<img src="logo8.png" alt="logo">'


# Variables related to Home
home_button <- "Home"

home_title <- "¡Bienvenidos!"

home_text <-
  "En esta página web encontraras datos entre los años 2014 y 2018 referentes a accidentes de tránsito en Medellín. Con ayuda de herramientas informáticas hemos analizado dichos datos para crear un predictor del número de accidentes en un barrio o comuna bajo alguna unidad de tiempo y también hemos agrupado barrios con características comunes. El vídeo aquí arriba explica cómo usar las diferentes opciones de la aplicación web."

youtube_video <- '<iframe width="560" height="315" src="https://www.youtube.com/embed/fFqBpbgRHSM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'

# Variables related to 2017 data table
first_table_title <- "Tabla de datos 2014"
first_table_text <- "Estos son los datos de 2014 que se utilizaron para entrenar los modelos predictivos y el algoritmo de agrupamiento"

second_table_title <- "Tabla de datos 2015"
second_table_text <- "Estos son los datos de 2015 que se utilizaron para entrenar los modelos predictivos y el algoritmo de agrupamiento"

third_table_title <- "Tabla de datos 2016"
third_table_text <- "Estos son los datos de 2016 que se utilizaron para entrenar los modelos predictivos y el algoritmo de agrupamiento"

fourth_table_title <- "Tabla de datos 2017"
fourth_table_text <- "Estos son los datos de 2017 que se utilizaron para validar el primer modelo y para entrenar el segundo modelo y el algoritmo de agrupamiento"

fifth_table_title <- "Tabla de datos 2018"
fifth_table_text <- "Estos son los datos de 2018 que se utilizaron para validar los modelos predictivos y entrenar el algoritmo de agrupamiento"

About_title_1 <- "Acerca del estudio:"
About_text_1 <- "El siguiente reporte describe el análisis estadístico realizado en este estudio:"

credits_title <- "¡Créditos!"
credits_data_analysis <-
  "Análisis descriptivo de datos: Juan Felipe Munera"
credits_database <-
  "Depuración y tratamiento de las bases de datos: Sebastian Parra y Juan Felipe Munera"
credits_front_end <-
  "Elaboración de la aplicación web: David Chaverra y Andres Felipe Aguilar"
credits_video <-
  "Elaboración del video promocional: Andres Felipe Aguilar"

labels_regimen = c("Contributivo", "Especial", "Subsidiado")

labels_actividad = c("Trabajando",
                     "Buscando trabajo",
                     "Estudiando",
                     "Oficios del hogar",
                     "Incapacitado")

labels_vivienda = c("Propia",
                    "Arriendo",
                    "Usufructuario",
                    "Sin titulo/colectiva")

labels_seguridad = c("Segura", "Insegura")

labels_salud = c("Muy bueno", "Bueno", "Regular", "Malo")

labels_educacion = c(
  "Ninguno",
  "Preescolar",
  "Primaria(1-5)",
  "Secundaria(6-9)",
  "Media(10-13)",
  "Tecnico sin titulo",
  "Tecnico con titulo",
  "Teconologico sin titulo",
  "Tecnologico con titulo",
  "Universitario sin titulo",
  "Universitario con titulo",
  "Postgrado sin titulo",
  "Postgrado con titulo"
)

labels_estrato <- 1:6

labels_bienes <- 0:23

bienes <-
  c(
    "Lavadora",
    "Nevera",
    "Estufa",
    "Horno a gas o eléctrico",
    "Microondas",
    "Calentador de agua",
    "TV Convencional",
    "TV pantalla plana",
    "Reproductor de video",
    "Equipo de sonido",
    "Aire acondicionado"
  )

bienes2 <- c(
  "Ventilador",
  "Reproductores de música como mp3",
  "Consola de videojuegos",
  "Carro particular",
  "Motocicleta",
  "Bicicleta",
  "Casa, apartamento, finca",
  "Computador",
  "Cámara de video",
  "Servicio de televisión"
)

# load(file = "txt/modelo.RData")

original_columns <-c("edad","regimen","estado_de_salud","nivel_educativo",
                     "actividad_semana_pasada","arriendo","vivienda_propia","seguridad",
                     "bienes","estrato","ingreso_percapita","personas_hogar")

educacion_numbers <- c(1:13)
names(educacion_numbers) <- labels_educacion

salud_numbers <- c(1:4)
names(salud_numbers) <- labels_salud

ui <- navbarPage(
  title,
  theme = style,
  footer =   tags$footer(class = "footer-distributed",
                         fluidRow(
                           column(width = 6,
                                  tags$p(
                                    class = "footer-links",
                                    tags$a(
                                      href = "https://github.com/afaguilarr/accidentes_medellin",
                                      "Click here to see the github repo with all code used to make this app."
                                    )
                                  )),
                           column(
                             width = 6,
                             tags$p(
                               class = "footer-company-name",
                               "Made by Andres Aguilar, Sebastian Parra, Juan Felipe Munera and David Chaverra 2019"
                             )
                           )
                         )
                      ),
  tabPanel(
    home_button,
    tags$div(
      class = "container center",
      tags$h1(home_title),
      fluidRow(
        tags$div(class = "col-md-10  col-md-offset-1 videoWrapper",
                 HTML(youtube_video))
      ),
      tags$p(home_text)
    )
  ),
  navbarMenu(
    first_menu_button,
    tabPanel(
      data_2014_button,
      tags$h1(first_table_title),
      tags$p(first_table_text),
      DT::dataTableOutput(outputId = "data_2014")
    ),
    tabPanel(
      data_2015_button,
      tags$h1(second_table_title),
      tags$p(second_table_text),
      DT::dataTableOutput(outputId = "data_2015")
    ),
    tabPanel(
      data_2016_button,
      tags$h1(third_table_title),
      tags$p(third_table_text),
      DT::dataTableOutput(outputId = "data_2016")
    ),
    tabPanel(
      data_2017_button,
      tags$h1(fourth_table_title),
      tags$p(fourth_table_text),
      DT::dataTableOutput(outputId = "data_2017")
    ),
    tabPanel(
      data_2018_button,
      tags$h1(fifth_table_title),
      tags$p(fifth_table_text),
      DT::dataTableOutput(outputId = "data_2018")
    )
  ),
  navbarMenu(
    "Predicciones",
    tabPanel(
      class = "panel1",
      "Predicción usando 2014-2016",
      navbarPage("asdsasd")
      ),
    tabPanel(
      class = "panel1",
      "Predicción usando 2014-2017",
      navbarPage("asdsasd")
    )
  ),
  tabPanel(
    "Grupos de barrios en Medellín",
    tags$h1("Grupos de barrios en Medellín"),
    fluidRow(
      column(width = 1),
      column(
        width = 10,
        leafletOutput(outputId = "mymap")
      ),
      column(width = 1)
    )
  ),
  tabPanel(
    id = "report",
    third_menu_button,
    tags$div(class = "container center",
             tags$h1(About_title_1),
             tags$p(id = "about", About_text_1)),
    fluidRow(
      column(width = 1),
      column(
        width = 10,
        uiOutput('markdown')
      ),
      column(width = 1)
    )
  ),
  tabPanel(
    fourth_menu_button,
    tags$div(
      class = "container center",
      tags$h1(credits_title),
      tags$p(id = "analisis", credits_data_analysis),
      tags$p(credits_database),
      tags$p(credits_front_end),
      tags$p(credits_video)
    )
  )
)

server <- function(input, output, session) {
  values <-
    reactiveValues(
      data1 = "6000001.1",
      data2 = "5000001.1",
      extra_data1 = 1,
      extra_data2 = 2,
      satisfaction_data = 1,
      arriendo = 0,
      ingreso = 0
    )
  
  output$data_2014 <- DT::renderDataTable({
    datatable(bd2014[, columnas_labels],
              selection = 'none',
              class = 'cell-border stripe')
  })
  
  output$data_2015 <- DT::renderDataTable({
    datatable(bd2015[, columnas_labels],
              selection = 'none',
              class = 'cell-border stripe')
  })
  
  output$data_2016 <- DT::renderDataTable({
    datatable(bd2016[, columnas_labels],
              selection = 'none',
              class = 'cell-border stripe')
  })
  
  output$data_2017 <- DT::renderDataTable({
    datatable(bd2017[, columnas_labels],
              selection = 'none',
              class = 'cell-border stripe')
  })
  
  output$data_2018 <- DT::renderDataTable({
    datatable(bd2018[, columnas_labels],
              selection = 'none',
              class = 'cell-border stripe')
  })
  
  output$markdown <- renderUI({
    includeMarkdown('www/Informe_Final.rmd')
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
    setView(lng = -99, lat = 45, zoom = 2) %>% addTiles()
  })
  
  # output$network2 <- renderVisNetwork({
  #   grafica_hogar_de_persona2016(values$data2)
  # })
  # 
  # output$extra_data2 <- DT::renderDataTable({
  #   datatable(
  #     values$extra_data2,
  #     selection = 'none',
  #     filter = 'none',
  #     options = list(paging = FALSE, searching = FALSE),
  #     colnames = column_names,
  #     class = 'cell-border stripe'
  #   )
  # })
  # 
  # output$madres_solteras_f2016 <- DT::renderDataTable({
  #   datatable(madres_solteras_f2016[, columnas_labels2016],
  #             # colnames = columnas_labels,
  #             selection = 'none',
  #             class = 'cell-border stripe')  %>% formatStyle(0:ncol(madres_solteras_f2016) +
  #                                                              1, cursor = 'pointer')
  # })
  # 
  # observeEvent(input$madres_solteras_f2016_cell_clicked, {
  #   info = input$madres_solteras_f2016_cell_clicked
  #   if (is.null(info$value)){return()}
  #   # print(madres_solteras_f2[info$row,])
  #   # values$data1 = madres_solteras_f2[info$row, "Edad"]
  #   home_id <-
  #     paste(bd_estudio2016[filas_madres2016[info$row], "DIRECTORIO"], ".", bd_estudio2016[filas_madres2016[info$row], "SECUENCIA_P"], sep =
  #             "")
  #   print(home_id)
  #   values$data2 = home_id
  #   values$extra_data2 = data.frame(t(madres_solteras_f2016[info$row, ]))
  #   # print(values$extra_data1)
  #   toggleModal(session, "modal2", toggle = "toggle")
  # })
  
  output$extra_data3 <- DT::renderDataTable({
    datatable(
      values$extra_data3,
      selection = 'none',
      filter = 'none',
      options = list(paging = FALSE, searching = FALSE),
      colnames = column_names,
      class = 'cell-border stripe'
    )
  })
  
  output$satisfaction <- renderText(
    values$satisfaction_data
  )
  
  observeEvent(input$submit_button, {
    values$arriendo <- as.numeric(input$arriendo)
    values$arriendo <- prettyNum(values$arriendo, scientific=FALSE, big.mark=".")
    values$arriendo <- paste("$", values$arriendo)
    values$ingreso <- as.numeric(input$ingreso)
    values$ingreso <- prettyNum(values$ingreso, scientific=FALSE, big.mark=".")
    values$ingreso <- paste("$", values$ingreso)
    
    #match with values for prediction
    educacion <- educacion_numbers[[input$educacion]]
    salud <- salud_numbers[[input$salud]]
    
    prediction_data <- as.data.frame(list(as.numeric(input$edad), input$regimen, salud, educacion, input$actividad,
                        as.numeric(input$arriendo), input$vivienda, input$seguridad, as.numeric(input$bienes), as.numeric(input$estrato),
                        as.numeric(input$ingreso), as.numeric(input$personas)), col.names = original_columns)

    prediction_value <- round(10-predict(MG1, newdata = prediction_data, type="response"))
    
    form_data = c(input$edad, input$regimen, input$salud, input$educacion, input$actividad,
                  values$arriendo, input$vivienda, input$seguridad, input$bienes, input$estrato,
                  values$ingreso, input$personas)
    values$extra_data3 = data.frame(filas_names[2:13], form_data)
    values$satisfaction_data <- prediction_value
    toggleModal(session, "modal3", toggle = "toggle")
    
    
    })
}

shinyApp(ui = ui, server = server)