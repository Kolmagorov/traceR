library(shiny)
library(bslib)

# Sidebar Accordion ============================================================
sb_acc <- accordion(id = "sb_accorion"
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

nav_set <- navset_tab(
  nav_panel(title = "PREVIEW"
            , plotOutput("p")),
  nav_panel(title = "META DATA EDITOR", p("Second tab content.")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item("link_shiny"),
    nav_item("link_posit")
  )
)



# USER INTERFACE ===============================================================
ui <- page_sidebar(title = "UVizor"
                   , sidebar = sidebar(title = NULL
                                       , id = "SD"
                                       , open = "open"
                                       , width = "300px"
                                       , resizable = TRUE
                                       , sb_acc)
                   , nav_set)

# Server Logic =================================================================
server <- function(input, output, session){
  output$p <- renderPlot({
    rnorm(n = 500, mean = 0, sd = 2)|>
      hist()
    })
  }


# Run the application ========================================================= 
shinyApp(ui = ui, server = server)

