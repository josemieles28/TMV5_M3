# La App se construyó en dos archivos separados ui.R y server.R

# UI
shinyUI(fluidPage(
    fluidRow(
        column(
            width = 10, offset = 1,
            titlePanel("Titanic: Exploratory Data Analysis"), 
            p("Este es un EDA básico para el conjunto de datos titanic. 
             Este análisis fue desarrollado con el fin de proporcionar información rápida sobre 
             los datos para las personas que solo desean tener una idea general de los mismos."),
            br(),
            tabsetPanel(
                tabPanel(
                    title = "Antes de Empezar", br(),
                    tags$h3("Algunas aclaraciones importante:"),
                    tags$ul(
                        tags$li("Survived.- 0 = No, 1 = Yes"),
                        tags$li("Pclass.- una aproximación para el estatus socioeconómico"),
                            tags$ul(
                                tags$li("1st = superior"),
                                tags$li("2do = medio"),
                                tags$li("3ro = inferior")
                            )
                    ), br(),
                    tags$h3("Lectura de la data"),
                    dataTableOutput("table_df"), br(),
                    actionButton(
                        inputId = "resumen", 
                        label = "Calcular resúmenes", 
                        icon = icon("table"),
                        width = '20%'
                    ), br(), br(),
                    fluidRow(
                        column(width = 4, tableOutput("summary_num")),
                        column(width = 8, tableOutput("summary_factor"))
                    )
                ),
                tabPanel(
                    title = "Análisis Univariado", br(),
                    sidebarLayout(
                        sidebarPanel(
                            width = 3,
                            radioButtons(
                                inputId = "tipo_var",
                                label = "Tipo de variable", 
                                choices = c("Numérica" = "num", "Categórica" = "cat")
                            ),
                            conditionalPanel(
                                condition = "input.tipo_var == 'num'",
                                selectInput(
                                    inputId = "var_numeric", 
                                    label = "Variables numéricas:", 
                                    choices = names_numeric
                                )
                            ),
                            conditionalPanel(
                                condition = "input.tipo_var == 'cat'",
                                selectInput(
                                    inputId = "var_factor", 
                                    label = "Variables categóricas:", 
                                    choices = names_factor
                                )
                            )
                        ),
                        
                        mainPanel(
                            width = 9,
                            highchartOutput("plot_univariado")
                        )
                    )
                ),
                tabPanel(
                    title = "Análisis Bivariado", br(),
                    sidebarLayout(
                        sidebarPanel(
                            width = 3,
                            checkboxInput(
                                inputId = "var_obj", 
                                label = "Variable objetivo",
                                value = TRUE
                            ),
                            radioButtons(
                                inputId = "tipo_var2",
                                label = "Tipo de variable", 
                                choices = c("Numérica" = "num", "Categórica" = "cat")
                            ),
                            conditionalPanel(
                                condition = "input.tipo_var2 == 'num'",
                                selectInput(
                                    inputId = "var_numeric2", 
                                    label = "Variables numéricas:", 
                                    choices = names_numeric
                                )
                            ),
                            conditionalPanel(
                                condition = "input.tipo_var2 == 'cat'",
                                selectInput(
                                    inputId = "var_factor2", 
                                    label = "Variables categóricas:", 
                                    choices = names_factor
                                )
                            ), 
                            downloadButton("plot_download", "Descargar plot")
                        ),
                        mainPanel(
                            width = 9,
                            plotOutput("plot_bivariado")
                        )
                    )
                )
            )
        )
    )
))


# SERVER

shinyServer(function(input, output) {
    
    # +++++++++++++++++++++
    # Antes de Empezar ----
    # +++++++++++++++++++++
    
    output$table_df <- renderDataTable(
        df, options = list(pageLength = 5, scrollX = TRUE)
    )
    
    observeEvent(input$resumen, {
        output$summary_num <- renderTable({
            describe_num <- function(bdd, digitos = 4) {
                n_length <- nrow(bdd)
                NA_s <- round(purrr::map_dbl(bdd, function(x) sum(is.na(x))), digitos)
                PoctNA_s <- round(NA_s / n_length, digitos)
                conteo <- round(purrr::map_dbl(bdd, length), digitos)
                media <- round(purrr::map_dbl(bdd, mean, na.rm = T), digitos)
                std <- round(purrr::map_dbl(bdd, sd, na.rm = T), digitos)
                minimo <- round(purrr::map_dbl(bdd, min, na.rm = T), digitos)
                Q_25 <- round(purrr::map_dbl(bdd, quantile, probs = 0.25, na.rm = T), digitos)
                Q_50 <- round(purrr::map_dbl(bdd, quantile, probs = 0.50, na.rm = T), digitos)
                Q_75 <- round(purrr::map_dbl(bdd, quantile, probs = 0.75, na.rm = T), digitos)
                maximo <- round(purrr::map_dbl(bdd, max, na.rm = T), digitos)
                bdd_summary <- rbind(
                    NA_s, PoctNA_s, conteo, media, std, minimo, Q_25, Q_50, Q_75, maximo
                ) %>% as.data.frame()
                return(bdd_summary)
            }
            describe_num(select_if(df, is.numeric), 2)
        }, align = 'c', rownames = TRUE)
        
        output$summary_factor <- renderTable({
            describe_cat <- function(bdd, digitos = 4) {
                n_length <- nrow(bdd)
                NA_s <- round(purrr::map_dbl(bdd, function(x) sum(is.na(x))), digitos)
                PoctNA_s <- round(NA_s / n_length, digitos)
                conteo <- round(purrr::map_dbl(bdd, length), digitos)
                category <- round(
                    purrr::map_dbl(bdd, function(x) length(unique(na.omit(x)))), digitos
                )
                bdd_summary <- rbind(
                    NA_s, PoctNA_s, conteo, category
                ) %>% as.data.frame()
                return(bdd_summary)
            }
            describe_cat(select_if(df, is.factor), 2)
        }, align = 'c', rownames = TRUE)
    })
    
    # ++++++++++++++++++++++++
    # Análisis Univariado ----
    # ++++++++++++++++++++++++
    
    output$plot_univariado <- renderHighchart({
        if (input$tipo_var == "num") {
            hchart(
                name = input$var_numeric, na.omit(df[, input$var_numeric]), 
                type = "column", color = "#008B8B"
            ) %>% 
                hc_title(text = paste("Histograma de", input$var_numeric)) %>% 
                hc_add_theme(hc_theme_elementary())
        } else {
            hchart(
                name = input$var_factor, na.omit(df[, input$var_factor]), 
                color = "#FFA500"
            ) %>% 
                hc_title(text = paste("Diagrama de barras de", input$var_factor)) %>% 
                hc_add_theme(hc_theme_elementary())
        }
    })
    
    # +++++++++++++++++++++++
    # Análisis Bivariado ----
    # +++++++++++++++++++++++
    
    plot_Output <- reactive({
        if(input$var_obj) {
            if(input$tipo_var2 == "num") {
                ggplot(df, aes_string(x = input$var_numeric2, fill = var_objetivo)) + 
                    geom_histogram(bins = 25) + scale_fill_tableau() + theme_bw()
            } else {
                ggplot(df, aes_string(x = input$var_factor2, fill = var_objetivo)) + 
                    geom_bar(position = "fill") +
                    scale_fill_tableau() + theme_bw()
            }
        } else {
            if(input$tipo_var2 == "num") {
                ggplot(df, aes_string(x = input$var_numeric2)) + 
                    geom_histogram(bins = 25) + scale_fill_tableau() + theme_bw()
            } else {
                ggplot(df, aes_string(x = input$var_factor2)) + 
                    geom_bar(position = "fill") +
                    scale_fill_tableau() + theme_bw()
            }
        }
    })
    
    output$plot_bivariado <- renderPlot({
        plot_Output()
    })
    
    output$plot_download <- downloadHandler(
        filename = function() {"plot.png"},
        content = function(file) {
            png(file)
            print(plot_Output())
            dev.off()
        }
    )

})
