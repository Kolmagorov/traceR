library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "UVisor"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("IMPORT", tabName = "import", icon = NULL ),
      menuItem("PROCESSING", tabName = "prc", icon = NULL),
      menuItem("ANALYSIS", tabName = "anls", icon = NULL))),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "import"
              , fileInput("upload"
                          , label = "Select a file:"
                          , buttonLabel = "Upload..."
                          , multiple = TRUE
                          , accept = c(".csv", ".arw", ".txt")
                          , placeholder = "browse a file") ),
      
      tabItem(tabName = "prc"
              , plotOutput("p")),
      tabItem(tabName = "anls", "ANALYSIS")),
    )
)

server <- function(input, output, session){
  
  output$p <- renderPlot({
    
    rnorm(n = 500, sd = 2, mean = 0)|>
      hist()

  })
  
}

shinyApp(ui, server)
