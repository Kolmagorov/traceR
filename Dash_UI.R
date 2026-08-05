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
# NavSet =======================================================================
tabset_hdn <- navset_hidden(
  id = "main_tabs",
  nav_panel_hidden("IMPORT", textOutput("text1")),
  nav_panel_hidden("PROCESS", textOutput("text2")),
  nav_panel_hidden("ANALYSIS", textOutput("text3"))
)



# USER INTERFACE ===============================================================
ui <- page_sidebar(title = "UVizor"
                   , sidebar = sidebar(title = NULL
                                       , id = "SD"
                                       , open = "open"
                                       , width = "300px"
                                       , resizable = TRUE
                                       , sb_acc)
                   , tabset_hdn
                   , textOutput("txt"))

# Server Logic =================================================================
server <- function(input, output, session){
  
  observeEvent(input$sb_acc, {
    nav_select("main_tabs", input$sb_acc[length(input$sb_acc)])
    })

  
  output$text1 <- renderText("Welcome to the Dashboard Page!")
  output$text2 <- renderText("Welcome to the Analytics Page!")
  output$text3 <- renderText("Welcome to the Settings Page!")
  
  output$txt <- renderText({ input$sb_acc})
  }



# Run the application ========================================================= 
shinyApp(ui = ui, server = server)


library(shiny)
library(bslib)
library(bsicons)

ui <- page_sidebar(
  title = "Global Sidebar Navigation",
  
  # 1. Define the shared global sidebar
  sidebar = sidebar(gap = "0px",
    title = "Controls",
    # Input buttons acting as sidebar menu items
    actionButton("go_page1", "Dashboard", icon = icon("dashboard"), class = "w-100 mb-2 btn-primary"),
    actionButton("go_page2", "Analytics", icon = icon("bar-chart"), class = "w-100 mb-2 btn-secondary"),
    actionButton("go_page3", "Settings", icon = icon("gear"), class = "w-100 btn-secondary")
  ),
  
  # 2. Define the hidden container for pages in the main body
  navset_hidden(
    id = "main_tabs",
    nav_panel_hidden("page_1", textOutput("text1")),
    nav_panel_hidden("page_2", textOutput("text2")),
    nav_panel_hidden("page_3", textOutput("text3"))
  )
)

server <- function(input, output, session) {
  # 3. Handle navigation clicks
  observeEvent(input$go_page1, {
    nav_select("main_tabs", "page_1")
  })
  
  observeEvent(input$go_page2, {
    nav_select("main_tabs", "page_2")
  })
  
  observeEvent(input$go_page3, {
    nav_select("main_tabs", "page_3")
  })
  
  # Placeholder page content outputs
  output$text1 <- renderText("Welcome to the Dashboard Page!")
  output$text2 <- renderText("Welcome to the Analytics Page!")
  output$text3 <- renderText("Welcome to the Settings Page!")
}

shinyApp(ui, server)

