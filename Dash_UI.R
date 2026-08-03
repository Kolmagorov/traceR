library(shiny)
library(bslib)


acc <- accordion(
  accordion_panel("UPLOAD"
                  , icon = bsicons::bs_icon("files")
                  , fileInput("upload"
                              , label = "Select a file:"
                              , buttonLabel = "Upload..."
                              , multiple = TRUE
                              , accept = c(".csv", ".arw", ".txt")
                              , placeholder = "browse a file")),
  
  accordion_panel("PROCESS", icon = bsicons::bs_icon("plus-slash-minus")),
  accordion_panel("ANALYSIS", icon = bsicons::bs_icon("puzzle"))
)

ui <- page_sidebar(title = "UVizor"
                   , sidebar = sidebar("Sidebar"
                                       , id = "SD"
                                       , open = "closed"
                                       , resizable = FALSE
                                       , accordion(id = "sb_accorion"
                                                   , accordion_panel(title = "UPLOAD"
                                                                     , fileInput("upload"
                                                                                 , label = "Select a file:"
                                                                                 , buttonLabel = "Upload..."
                                                                                 , multiple = TRUE
                                                                                 , accept = c(".csv", ".arw", ".txt")
                                                                                 , placeholder = "browse a file")
                                                    )
                                                   , accordion_panel(title = "PROCESS"
                                                                     , sliderInput("time_rng", "Adjust sliders to crop out Retention time segment"
                                                                                   , value = c(0, 100)
                                                                                   , min = 0
                                                                                   , max = 100
                                                                                   , width = "100%"
                                                                                   , step = 1
                                                                                   , post = " min"))
                                  )
                      ),
    plotOutput("p")
    )

server <- function(input, output, session){
  output$p <- renderPlot({
    rnorm(n = 500, mean = 0,sd = 2)|>
      hist()
    })
  }


# Run the application 
shinyApp(ui = ui, server = server)

