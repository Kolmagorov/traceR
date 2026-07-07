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
    h3("UPLOAD LOG"),
    DT::DTOutput("log"),
    plotOutput("plot")
  )
    


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  data <- reactive({
    
    req(input$upload)
    obj <- traceR::load_trace(fls = input$upload$datapath)
    obj$LOG$FILE_NAME <- input$upload$name
    obj$LOG |> dplyr::select(!FILE)
    
    
  })
  
  output$log <- DT::renderDT({
    
    DT::datatable(data()
                  , filter = "none"
                  , rownames = TRUE
                  , options = list( 
                    scrollY = "200px",
                    scrollX = TRUE,
                    paging = FALSE)
                  )
    
    })
  
  output$plot <- renderPlot({
    
    traceR::plt_gg(x = )
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

