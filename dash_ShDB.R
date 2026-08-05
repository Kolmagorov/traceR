library(shiny)
library(shinydashboard)
library(bslib)




# Collapsible Side Bar ====================================
dbs <- dashboardSidebar(
  #width = 300,
  
  sidebarMenu(
    menuItem("IMPORT"
             , tabName = "import"
             , icon = icon("folder-open")),
    
    menuItem("DATA PROCESSING"
             , tabName = "prc"
             , icon = icon("sliders")),
    
    menuItem("ANALYSIS"
             , tabName = "anls"
             , icon = icon("brain"))
    ))



dbb <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "import"
            , fileInput("upload"
                        , label = "Select a file:"
                        , buttonLabel = "Upload..."
                        , multiple = TRUE
                        , accept = c(".csv", ".arw", ".txt")
                        , placeholder = "browse a file"),
            
            actionButton(inputId = "all_btn"
                         , label = "All"
                         , icon = NULL
                         , disabled = FALSE)),
    
    tabItem(tabName = "prc"
            , "PROCESS"
            , navset_tab(nav_panel("FORGE", "Page A content"),
                         nav_panel("EDITOR", "Page B content"))),
    
    tabItem(tabName = "anls", "ANALYSIS")
    )
  )





ui <- dashboardPage(
  dashboardHeader(title = "UVisor"),
  dbs,
  dbb)

server <- function(input, output, session){
  
}

shinyApp(ui, server)


library(shiny)
library(bslib)

library(shiny)
library(bslib)
library(bsicons)

ui <- page_sidebar(
  title = "Global Sidebar Navigation",
  
  # 1. Define the shared global sidebar
  sidebar = sidebar(
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


