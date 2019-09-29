library(shiny)
library(DT)
library("rio")
library(dplyr)
library("highcharter")

# Define UI for application that draws a histogram (crea el nombre del objeto que queremos crear (ui)// sho a plot (visualizo el objeto en el grafico)) 
#ui nombre del objeto que se va a crear
#server para mostrar proceso.. guardar en lista output y mostrar en el ui
# no mostrar dos veces el mismo objeto en ui
#colum te permite poner varias columnas en el shiny

#Autor: Jenny Tenisaca
#Fecha:29/09/2019
#Modulo3

ui <- fluidPage(
    
    # Application title
    titlePanel("Datos Consumo de alcohol-libadores"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(2,
               
               
               #sidebarLayout(
               #sidebarPanel(
               conditionalPanel(condition = 'input.conditionPanels=="Texto"',
                                textInput(inputId = "texto",label = "Ingrese un texto"),
                                actionButton("boton","Ejecutar")
                                
               ),
               conditionalPanel(condition = 'input.conditionPanels=="Tabla"',
                                downloadLink("downloadData", "Descargar datos")
                                #selectInput(inputId = "df",label = "Presione data frame",choices = c("mtcars","iris"),selected = "mtcars")
               )
        ),
               
               column(1,
                      sliderInput("bins",
                                  "Num. Barras del Histograma:",
                                  min = 1,
                                  max = 50,
                                  value = 30)
                      ),
               
               
               
               #),
         # cierra el colum
        column(7,
               # main panel
               # Show a plot of the generated distribution
               
               #mainPanel( ## main panel
               tabsetPanel(

                   tabPanel("Tabla",
                            div(DTOutput("tabla"),style="font-size:80%"), 
                            br(),
                            div(DTOutput("tabla1"),style="font-size:80%")
                   ),
                   
                   
                  
                   tabPanel("Documento", 
                            htmlOutput('documento')
                   ),
                   
                   
                   tabPanel("Graficos",
                            plotOutput("distPlot"),
                            highchartOutput("barras")
                            
                           
                   ), id="conditionPanels"
               )
        ),
        
        column(2,
               img(src="licor.jpg",height=245, width=285),
               h5("Libador", aling="center"),
               br()
               
        )
    )
)
#)

#)

# Define server logic required to draw a histogram (lista input /output) se crea el objeto en el server por denntro
server <- function(input, output) {
    
#lectura base
        ecu3 <- import("Mi_base.dta")
    
    #tabla completa con unas pocas variables
    output$tabla <- renderDT({
        Data <- select(ecu3,c(Provincia, Distrito, Circuito, Subcircuito,IncidentTime, mes, DIA,IncidentGradeName_Incidente,N2))
        print(Data)
        
        },
        options=list(lengthChange=TRUE,
                     scrollY=360,
                     scrolly=TRUE)
    )
    #tabla filtrada por tipo de claves
        output$tabla1 <- renderDT({
        table(ecu3$IncidentGradeName_Incidente,ecu3$N2)
            })
    #descargas
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("data_", Sys.Date(), ".csv", sep="")
                },
            content = function(filename){
                write.csv(ecu3, filename)
            }
        )
        
        
        #############
        
        #histograma
        
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
        #documento consumo de alcohol
    output$documento <- renderText({
        return(paste0('<iframe style="height:600px; width:100%" src="',
                      "consumo_alcohol.pdf", '"></iframe>'))
    })
    
    #grafico horas de incidentes de libadores
    
    output$barras <- renderHighchart({
       # plot(ecu3$IncidentGradeName_Incidente,ecu3$libadores)
       hchart(ecu3$hora, type = "column", name = "hora")
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

## ctrl+a .... crtl+i= arregla el codigo 
# como maximo 5 aplicaciones activas, hay que archivarles. craendo una cuenta 
#5 visualizaciones

# instalar en una srvidor propio shiny server, en una red interna , trabajo universidad gratituita y de pago
# grat n aplicaciones , limitante de usuarios 25 conexiones simultaneas , la 26 no puede ver (hay trucos para que el servidor permita mas usuarios. 
#
