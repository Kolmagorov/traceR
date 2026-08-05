library(shiny)
library(shinydashboard)



dbb <- dashboardBody(
  tabItems(
    tabItem(tabName = "import", tags$p("IMPORT")),
    tabItem(tabName = "prc", plotOutput("p")),
    tabItem(tabName = "anls", "ANALYSIS")))




dbs <- dashboardSidebar(
  sidebarMenu(
    
    # This item expands to reveal the nested inputs
    menuItem("Filter Options", icon = icon("filter"), expandedName = "filters",
             selectInput("category", "Select Category:", choices = c("A", "B", "C")),
             dateInput("date", "Select Date:")),
    
    menuItem("Main Dashboard", tabName = "dashboard", icon = icon("dashboard"))
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
