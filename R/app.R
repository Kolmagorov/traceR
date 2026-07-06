library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UVizor - test"),

    # Sidebar with a slider input for number of bins 
    fileInput("upload"
              , label = "Select a file:"
              , buttonLabel = "Upload..."
              , multiple = TRUE
              , accept = c(".csv", ".arw", ".txt")
              , placeholder = "browse a file"),
    
    tableOutput("files")
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$files <- renderTable(input$upload)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
