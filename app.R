#carpeta p-Proy6 app
#install.packages("Rcpp")
library(rsconnect)
require(dplyr)
library(class)
require(stringr)
library(sf)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)

#l--------------------lectura de datos
setwd("/Users/tamarapervaz/Documents/Proyecto final/")
viofamyearstate<-read.csv("/Users/tamarapervaz/Documents/Proyecto final/Data/viofamyearstate.csv")
mex_map <- st_read("/Users/tamarapervaz/Documents/Proyecto final/Data/areas_geoestadisticas_estatales.shp")
volenciafamiliarmap<-data.frame(read.csv("/Users/tamarapervaz/Documents/Proyecto final/Data/volenciafamiliarmap.csv"))
df<-select(data.frame(read.csv("/Users/tamarapervaz/Documents/Proyecto final/Data/df.csv")),-X)
viofamyearstate<-select(viofamyearstate, -X)
vf.ts <- read.csv("/Users/tamarapervaz/Documents/Proyecto final/Data/TimeSeries_VF.csv")


#-------------sidebar menu
sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
                #Top por Anio
                menuItem("TOP POR AÑO", tabName = "top", icon = icon("line-chart")),
                #Comparativa de anios
                menuItem("CONTRASTE ENTRE AÑOS", tabName = "contra", icon = icon("sort-amount-desc")),
                #Histogramas por Por Year
                menuItem("HISTOGRAMAS", tabName = "histo", icon = icon("bar-chart")),
                #mapas coroplecticos
                menuItem("MAPAS COROPLECTICOS", tabName = "mapa", icon = icon("map-marker")),
                #Series de Tiempo
                menuItem("SERIES DE TIEMPO", tabName = "serie", icon = icon("calendar"))
                
                
    )
)
#------------cuerpo
#Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
body <- dashboardBody(
    tabItems(
        #Top por anio
        tabItem(tabName = "top", 
                fluidRow(
                    box(solidHeader = TRUE, background = "navy",
                        conditionalPanel(condition="input.menu1=='top'",
                                         
                                         p(enc2utf8("Elige el anio para ver el top")),
                                         selectInput("year", "Seleccione el anio",
                                                     choices = unique(viofamyearstate$Year)),
                                         p("Elige el mes para ver el top"),
                                         selectInput("mes", "Seleccione el mes",
                                                     choices = c("Enero", "Febrero", "Marzo",
                                                                 "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
                                         radioButtons("choice",strong("Elige la incidencia"),
                                                      choices=c("Menor incidencia" = 1,
                                                                "Mayor incidencia" = 2))
                        )
                    ),
                    #fluidRow(#h3("Tabla de Contraste", align = "center" ),
                    
                    box(solidHeader = TRUE, background = "teal",
                        h3("Top 10 de Estados", align = "center"),
                        conditionalPanel(condition="input.choice==1", 
                                         tableOutput("table")),
                        conditionalPanel(condition="input.choice==2",
                                         tableOutput("table2"))
                    )
                    
                )
        ),
        #-------#Comparativa de anios   enc2utf8
        tabItem(tabName = "contra",
                fluidRow(
                    conditionalPanel(condition="input.menu1=='contra'",
                                     column(3,
                                            p(enc2utf8("Elige los años para ver la comparacion entre ambos")),
                                            selectInput("year2", "Seleccione el anio",
                                                        choices = unique(viofamyearstate$Year)),
                                            selectInput("year3", "Seleccione el anio a comparar",
                                                        choices = unique(viofamyearstate$Year))
                                     ), 
                                     column(3,offset = 1, 
                                            p("Elige el mes para ver el top"),
                                            selectInput("mes2", "Seleccione el mes",
                                                        choices = c("Enero", "Febrero", "Marzo",               
                                                                    "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))
                                     ), 
                                     column(3,offset = 1,
                                            radioButtons("choice2","Elige la incidencia",
                                                         choices=c("Menor incidencia" = 1,
                                                                   "Mayor incidencia" = 2))
                                     ) 
                                     
                    )    
                ),
                fluidRow(h3("Tabla de Contraste", align = "center" ),
                         
                         conditionalPanel(condition="input.choice2==1",
                                          tableOutput("table3")),
                         conditionalPanel(condition="input.choice2==2",
                                          tableOutput("table4"))
                         
                )
        ),
        #---------#mapas coroplecticos
        tabItem(tabName = "mapa",
                fluidRow(
                    #titlePanel(h2("")),
                    box(solidHeader = TRUE, background = "light-blue",
                        conditionalPanel(condition="input.menu1=='mapa'",
                                         p(strong("Elige el mes y anio para graficar")),
                                         selectInput("year4", "Seleccione el anio",
                                                     choices = unique(viofamyearstate$Year)),
                                         selectInput("mes3", "Seleccione el mes",
                                                     choices = c("Enero", "Febrero", "Marzo",               
                                                                 "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))
                        )
                    )
                ),
                fluidRow( 
                    plotOutput("plot")
                )
                
        ),
#--------#histigramas por estado
        tabItem(tabName = "histo",
                fluidRow(#titlePanel(h2("cargando...")),
                    box(solidHeader = TRUE, background = "yellow",
                        conditionalPanel(condition="input.menu1=='histo'",
                                         p(enc2utf8("Elige el anio para graficar")),
                                         selectInput("year5", "Seleccione el anio",
                                                     choices = unique(viofamyearstate$Year))
                        )
                    )
                ),
                fluidRow(
                    
                    plotOutput("plot2")
                )
        ),
#--------#Series de tiempo
        tabItem(tabName = "serie",
                fluidRow(#titlePanel(h2("cargando...")),
                    box(solidHeader = TRUE, background = "maroon",
                        conditionalPanel(condition="input.menu1=='serie'",
                                         p("Elige el estado para graficar"),
                                         selectInput("estado", "Seleccione el estado",
                                                     choices = unique(vf.ts$Entidad))
                        )
                    )
                ),
                fluidRow(
                    
                    plotOutput("plot3")
                )
        )
    )#cierra tabItems()
)#cierra body

#----------------interfaz UI------------------
ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "PROYECTO R, EQ23"),
    sidebar,
    body
)
#-------------------server------------------------
server <- function(input, output) {
    
    
    
    
    output$table <- renderTable({ 
        table1<- switch (input$mes,
                         "Enero" = arrange(filter(viofamyearstate, Year==input$year), Enerosum )[1:10,c(1:3,4)],
                         "Febrero"=arrange(filter(viofamyearstate, Year==input$year), Febrerosum )[1:10,c(1:3,6)],
                         "Marzo"=arrange(filter(viofamyearstate, Year==input$year), Marzosum )[1:10,c(1:3,8)],
                         "Abril"=arrange(filter(viofamyearstate, Year==input$year), Abrilsum )[1:10,c(1:3,10)],
                         "Mayo"=arrange(filter(viofamyearstate, Year==input$year), Mayosum )[1:10,c(1:3,12)],
                         "Junio"=arrange(filter(viofamyearstate, Year==input$year), Juniosum )[1:10,c(1:3,14)],
                         "Julio"=arrange(filter(viofamyearstate, Year==input$year), Juliosum )[1:10,c(1:3,16)],
                         "Agosto"=arrange(filter(viofamyearstate, Year==input$year), Agostosum )[1:10,c(1:3,18)],
                         "Semptiembre"=arrange(filter(viofamyearstate, Year==input$year), Semptiembresum )[1:10,c(1:3,20)],
                         "Octubre"=arrange(filter(viofamyearstate, Year==input$year), Octubresum )[1:10,c(1:3,22)],
                         "Noviembre"=arrange(filter(viofamyearstate, Year==input$year), Noviembresum )[1:10,c(1:3,24)],
                         "Diciembre"=arrange(filter(viofamyearstate, Year==input$year), Diciembresum )[1:10,c(1:3,26)],
                         
        )
    })
    
    output$table2 <- renderTable({
        switch (input$mes,
                "Enero" = arrange(filter(viofamyearstate, Year==input$year), desc(Enerosum) )[1:10,c(1:3,4)],
                "Febrero"=arrange(filter(viofamyearstate, Year==input$year), desc(Febrerosum) )[1:10,c(1:3,6)],
                "Marzo"=arrange(filter(viofamyearstate, Year==input$year), desc(Marzosum) )[1:10,c(1:3,8)],
                "Abril"=arrange(filter(viofamyearstate, Year==input$year), desc(Abrilsum) )[1:10,c(1:3,10)],
                "Mayo"=arrange(filter(viofamyearstate, Year==input$year), desc(Mayosum) )[1:10,c(1:3,12)],
                "Junio"=arrange(filter(viofamyearstate, Year==input$year), desc(Juniosum) )[1:10,c(1:3,14)],
                "Julio"=arrange(filter(viofamyearstate, Year==input$year), desc(Juliosum) )[1:10,c(1:3,16)],
                "Agosto"=arrange(filter(viofamyearstate, Year==input$year), desc(Agostosum) )[1:10,c(1:3,18)],
                "Semptiembre"=arrange(filter(viofamyearstate, Year==input$year), desc(Semptiembresum) )[1:10,c(1:3,20)],
                "Octubre"=arrange(filter(viofamyearstate, Year==input$year), desc(Octubresum) )[1:10,c(1:3,22)],
                "Noviembre"=arrange(filter(viofamyearstate, Year==input$year), desc(Noviembresum) )[1:10,c(1:3,24)],
                "Diciembre"=arrange(filter(viofamyearstate, Year==input$year), desc(Diciembresum) )[1:10,c(1:3,26)],
                
        )
    })
    
    output$table3 <- renderTable({
        switch (input$mes2,
                "Enero" = cbind(arrange(filter(viofamyearstate, Year==input$year2), (Enerosum) )[1:10,c(1:3,4)],
                                arrange(filter(viofamyearstate, Year==input$year3), (Enerosum) )[1:10,c(1:3,4)]),
                "Febrero"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Febrerosum) )[1:10,c(1:3,6)],
                                arrange(filter(viofamyearstate, Year==input$year3), (Febrerosum) )[1:10,c(1:3,6)]),
                "Marzo"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Marzosum) )[1:10,c(1:3,8)],
                              arrange(filter(viofamyearstate, Year==input$year3), (Marzosum) )[1:10,c(1:3,8)]),
                "Abril"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Abrilsum) )[1:10,c(1:3,10)],
                              arrange(filter(viofamyearstate, Year==input$year3), (Abrilsum) )[1:10,c(1:3,10)]),
                "Mayo"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Mayosum) )[1:10,c(1:3,12)],
                             arrange(filter(viofamyearstate, Year==input$year3), (Mayosum) )[1:10,c(1:3,12)]),
                "Junio"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Juniosum) )[1:10,c(1:3,14)],
                              arrange(filter(viofamyearstate, Year==input$year3), (Juniosum) )[1:10,c(1:3,14)]),
                "Julio"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Juliosum) )[1:10,c(1:3,16)],
                              arrange(filter(viofamyearstate, Year==input$year3), (Juliosum) )[1:10,c(1:3,16)]),
                "Agosto"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Agostosum) )[1:10,c(1:3,18)],
                               arrange(filter(viofamyearstate, Year==input$year3), (Agostosum) )[1:10,c(1:3,18)]),
                "Semptiembre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Semptiembresum) )[1:10,c(1:3,20)],
                                    arrange(filter(viofamyearstate, Year==input$year3), (Semptiembresum) )[1:10,c(1:3,20)]),
                "Octubre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Octubresum) )[1:10,c(1:3,22)],
                                arrange(filter(viofamyearstate, Year==input$year3), (Octubresum) )[1:10,c(1:3,22)]),
                "Noviembre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Noviembresum) )[1:10,c(1:3,24)],
                                  arrange(filter(viofamyearstate, Year==input$year3), (Noviembresum) )[1:10,c(1:3,24)]),
                "Diciembre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), (Diciembresum) )[1:10,c(1:3,26)],
                                  arrange(filter(viofamyearstate, Year==input$year3), (Diciembresum) )[1:10,c(1:3,26)])
                
        )
    })
    
    output$table4 <- renderTable({
        switch (input$mes2,
                "Enero" = cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Enerosum) )[1:10,c(1:3,4)],
                                arrange(filter(viofamyearstate, Year==input$year3), desc(Enerosum) )[1:10,c(1:3,4)]),
                "Febrero"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Febrerosum) )[1:10,c(1:3,6)],
                                arrange(filter(viofamyearstate, Year==input$year3), desc(Febrerosum) )[1:10,c(1:3,6)]),
                "Marzo"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Marzosum) )[1:10,c(1:3,8)],
                              arrange(filter(viofamyearstate, Year==input$year3), desc(Marzosum) )[1:10,c(1:3,8)]),
                "Abril"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Abrilsum) )[1:10,c(1:3,10)],
                              arrange(filter(viofamyearstate, Year==input$year3), desc(Abrilsum) )[1:10,c(1:3,10)]),
                "Mayo"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Mayosum) )[1:10,c(1:3,12)],
                             arrange(filter(viofamyearstate, Year==input$year3), desc(Mayosum) )[1:10,c(1:3,12)]),
                "Junio"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Juniosum) )[1:10,c(1:3,14)],
                              arrange(filter(viofamyearstate, Year==input$year3), desc(Juniosum) )[1:10,c(1:3,14)]),
                "Julio"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Juliosum) )[1:10,c(1:3,16)],
                              arrange(filter(viofamyearstate, Year==input$year3), desc(Juliosum) )[1:10,c(1:3,16)]),
                "Agosto"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Agostosum) )[1:10,c(1:3,18)],
                               arrange(filter(viofamyearstate, Year==input$year3), desc(Agostosum) )[1:10,c(1:3,18)]),
                "Semptiembre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Semptiembresum) )[1:10,c(1:3,20)],
                                    arrange(filter(viofamyearstate, Year==input$year3), desc(Semptiembresum) )[1:10,c(1:3,20)]),
                "Octubre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Octubresum) )[1:10,c(1:3,22)],
                                arrange(filter(viofamyearstate, Year==input$year3), desc(Octubresum) )[1:10,c(1:3,22)]),
                "Noviembre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Noviembresum) )[1:10,c(1:3,24)],
                                  arrange(filter(viofamyearstate, Year==input$year3), desc(Noviembresum) )[1:10,c(1:3,24)]),
                "Diciembre"=cbind(arrange(filter(viofamyearstate, Year==input$year2), desc(Diciembresum) )[1:10,c(1:3,26)],
                                  arrange(filter(viofamyearstate, Year==input$year3), desc(Diciembresum) )[1:10,c(1:3,26)])
                
        )
    })
    
    output$plot <- renderPlot({
        mex_map <- mex_map %>%  
            mutate(CVE_ENT = as.numeric(CVE_ENT))
        mex_map_covid <- mex_map %>%
            # unir tablas
            left_join(filter(volenciafamiliarmap, Year==input$year4),
                      # indicar explicitamente las columnas indice,
                      # necesario cuando no tienen el mismo nombre
                      by = c("CVE_ENT" = "Clave_Ent"))
        switch (input$mes3,
                "Enero" = (mex_map_covid %>%
                               # usamos el aesthetic fill para indicar la columna de casos
                               ggplot(aes(fill = Enerosum)) +
                               geom_sf() +
                               scale_fill_gradient("Enerosum", high = "red", low = "white")),
                "Febrero"=(mex_map_covid %>%
                               # usamos el aesthetic fill para indicar la columna de casos
                               ggplot(aes(fill = Febrerosum)) +
                               geom_sf() +
                               scale_fill_gradient("Febrerosum", high = "red", low = "white")),
                "Marzo"=(mex_map_covid %>%
                             # usamos el aesthetic fill para indicar la columna de casos
                             ggplot(aes(fill = Marzosum)) +
                             geom_sf() +
                             scale_fill_gradient("Marzosum", high = "red", low = "white")),
                "Abril"=(mex_map_covid %>%
                             # usamos el aesthetic fill para indicar la columna de casos
                             ggplot(aes(fill = Abrilsum)) +
                             geom_sf() +
                             scale_fill_gradient("Abrilsum", high = "red", low = "white")),
                "Mayo"=(mex_map_covid %>%
                            # usamos el aesthetic fill para indicar la columna de casos
                            ggplot(aes(fill = Mayosum)) +
                            geom_sf() +
                            scale_fill_gradient("Mayosum", high = "red", low = "white")),
                "Junio"=(mex_map_covid %>%
                             # usamos el aesthetic fill para indicar la columna de casos
                             ggplot(aes(fill = Juniosum)) +
                             geom_sf() +
                             scale_fill_gradient("Juniosum", high = "red", low = "white")),
                "Julio"=(mex_map_covid %>%
                             # usamos el aesthetic fill para indicar la columna de casos
                             ggplot(aes(fill = Juliosum)) +
                             geom_sf() +
                             scale_fill_gradient("Juliosum", high = "red", low = "white")),
                "Agosto"=(mex_map_covid %>%
                              # usamos el aesthetic fill para indicar la columna de casos
                              ggplot(aes(fill = Agostosum)) +
                              geom_sf() +
                              scale_fill_gradient("Agostosum", high = "red", low = "white")),
                "Semptiembre"=(mex_map_covid %>%
                                   # usamos el aesthetic fill para indicar la columna de casos
                                   ggplot(aes(fill = Septiembresum)) +
                                   geom_sf() +
                                   scale_fill_gradient("Septiembresum", high = "red", low = "white")),
                "Octubre"=(mex_map_covid %>%
                               # usamos el aesthetic fill para indicar la columna de casos
                               ggplot(aes(fill = Octubresum)) +
                               geom_sf() +
                               scale_fill_gradient("Octubresum", high = "red", low = "white")),
                "Noviembre"=(mex_map_covid %>%
                                 # usamos el aesthetic fill para indicar la columna de casos
                                 ggplot(aes(fill = Noviembresum)) +
                                 geom_sf() +
                                 scale_fill_gradient("Noviembresum", high = "red", low = "white")),
                "Diciembre"=(mex_map_covid %>%
                                 # usamos el aesthetic fill para indicar la columna de casos
                                 ggplot(aes(fill = Diciembresum)) +
                                 geom_sf() +
                                 scale_fill_gradient("Diciembresum", high = "red", low = "white")),
                
        )
    })#termina output$plot <- renderPlot [166]
    output$plot2 <- renderPlot ({
        
        histogr <- ggplot(filter(df, Year== input$year5), aes(x=SumaAnual, y = Entidad, 
                                                              colour = Tipo.de.delito)) + 
            geom_col(colour = "black", fill = "white") +
            geom_point() +  
            facet_grid( ~ Tipo.de.delito) +
            theme_bw() +
            guides(col = FALSE) +
            ggtitle (paste("Delitos Intrafamiliares en la Republica Mexicana en",input$year5)) + 
            theme (plot.title = element_text(family="Comic Sans MS",
                                             size=rel(1.5),
                                             face="bold",
                                             hjust = 0.5,
                                             lineheight = 1.5)) +
            labs(x = "Delitos Anuales",y = "Estados") +
            theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1))) +
            theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1))) 
        
        histogr 
    })
    output$plot3 <- renderPlot({
        
        vf.ts <- select(vf.ts,-X)
        
        #Al leerlo solo cambiar la fecha a formato Date 
        
        vf.ts <- mutate(vf.ts, Date = as.Date(Date,"%Y-%m-%d"))
        
        # AQUI ES DONDE SE HACE EL INPUT DE LOS ESTADOS 
        
        Entidad.vf = input$estado
        vf.ts <- filter(vf.ts, Entidad == Entidad.vf)
        # Grafica 
        
        vf.plot <- ggplot(vf.ts, aes(Date, Suma)) +
            geom_line(color = "black", size = 2) +
            theme_minimal() + 
            geom_area(aes(fill = format(Date,"%Y"))) +
            scale_fill_manual(values=c("#005EFF","#0000FF","#1E0BD0",
                                       "#2510A3","#241178","#1F104F")) +
            labs(fill = "Year") +
            ggtitle (paste("Casos de Violencia Familiar en",Entidad.vf)) + 
            theme (plot.title = element_text(family="Comic Sans MS",
                                             size=rel(1.5),
                                             face="bold",
                                             hjust = 0.5,
                                             lineheight = 1.5)) +
            labs(x = "Tiempo", y = "Delitos") +
            theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1))) +
            theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1)))
        
        vf.plot
        
    })
    
} #termina server

shinyApp(ui, server)


