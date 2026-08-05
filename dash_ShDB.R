library(shiny)
library(shinydashboard)



dbb <- dashboardBody(
  tabItems(
    tabItem(tabName = "import", tags$p("IMPORT")),
    tabItem(tabName = "prc", plotOutput("p")),
    tabItem(tabName = "anls", "ANALYSIS")))



# Collapsible Side Bar ====================================
dbs <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("IMPORT", icon = icon("folder-open"), expandedName = "filters"
             , fileInput("upload"
                         , label = "Select a file:"
                         , buttonLabel = "Upload..."
                         , multiple = TRUE
                         , accept = c(".csv", ".arw", ".txt")
                         , placeholder = "browse a file")),
    menuItem("DATA PROCESSING", tabName = "prc", icon = icon("mortar-pestle")),
    menuItem("ANALYSIS", tabName = "anls", icon = icon("brain"))
  ))




dbs <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("IMPORT"
             , tabName = "import"
             , icon = icon("filter")
             , expandedName = "filters"
             , fileInput("upload"
                         , label = "Select a file:"
                         , buttonLabel = "Upload..."
                         , multiple = TRUE
                         , accept = c(".csv", ".arw", ".txt")
                         , placeholder = "browse a file")),
    
    menuItem("PROCESSING", tabName = "prc", icon = NULL),
    menuItem("ANALYSIS", tabName = "anls", icon = NULL)))




dbs <- dashboardSidebar(
  sidebarMenu(id = "tabs", # Unique ID to track the active tab
              menuItem("Overview", tabName = "overview_tab", icon = icon("home")),
              menuItem("Detailed Analysis", tabName = "analysis_tab", icon = icon("chart-bar")),
              
              # Only displays when the "Detailed Analysis" tab is selected
              conditionalPanel(
                condition = "input.tabs == 'analysis_tab'",
                numericInput("threshold", "Analysis Threshold:", value = 10, min = 1)
              )
  )
)



ui <- dashboardPage(
  dashboardHeader(title = "UVisor"),
  dbs,
  dashboardBody())

server <- function(input, output, session){
  
  output$p <- renderPlot({
    
    rnorm(n = 500, sd = 2, mean = 0)|>
      hist()

  })
  
}

shinyApp(ui, server)
