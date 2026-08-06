library(shiny)
library(bslib)

# Sidebar Accordion ============================================================
sb_acc <- accordion(id = "sb_acc"
                    , open = FALSE
                    , accordion_panel(title = "IMPORT"
                                      , icon = bsicons::bs_icon("database-up")
                                      , fileInput("upload"
                                                  , label = "Select a file:"
                                                  , buttonLabel = "Upload..."
                                                  , multiple = TRUE
                                                  , accept = c(".csv", ".arw", ".txt")
                                                  , placeholder = "browse a file"))
                    
                    , accordion_panel(title = "PROCESS"
                                      , icon = bsicons::bs_icon("plus-slash-minus")
                                      , sliderInput("time_rng", "Adjust sliders to crop out Retention time segment"
                                                    , value = c(0, 100)
                                                    , min = 0
                                                    , max = 100
                                                    , width = "100%"
                                                    , step = 1
                                                    , post = " min"))
                    
                    , accordion_panel(title = "ANALYSIS"
                                      , icon = bsicons::bs_icon("puzzle")))

# Multi-page ===================================================================
ui <- page_navbar(title = "UVizor"
                  , nav_panel("IMPORT"
                              ,layout_sidebar(
                                sidebar = sidebar(
                                  title = "Import Control"
                                  , fileInput("upload"
                                              , label = "Select a file:"
                                              , buttonLabel = "Upload..."
                                              , multiple = TRUE
                                              , accept = c(".csv", ".arw", ".txt")
                                              , placeholder = "browse a file")
                      ),
                      plotOutput("plot1")
                    ))
                  
                  , nav_panel("PROCESSING"
                              , layout_sidebar(sidebar = sidebar(
                                  title = "Processing controls"
                                  , textInput("txt", "Enter text:", "Hello")
                                  ),
                      textOutput("txt_out"))))

# Server Logic =================================================================
server <- function(input, output, session){
  
  output$txt_out <- renderText("Welcome to the HELL!")
  output$plot1 <- renderPlot({rnorm(100,0,2)|> hist(main = "Histogram")})
  
  }

shinyApp(ui, server)

x <- card("A simple card")

page_fillable(
  layout_columns(x, x, x, x)
)

page_fillable(
  layout_columns(
    col_widths = c(6, 6, 12),
    x, x, x
  )
)
