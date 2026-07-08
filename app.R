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
    hr(),
    h2("Preview LOG"),
    DT::DTOutput("preview"),
    actionButton(inputId = "submit_btn"
                 , label = "Submit"
                 , icon = icon("stats", lib = "glyphicon")),
    hr(),
    h2("Traces"),
    
    selectInput(
      inputId = "Scaling",
      label = "Select a func.:",
      choices = c("NONE" = "none", "MINMAX" = "minmax", "MAXNORM" = "maxnorm"),
      selected = "none"
    ),
    
    plotOutput("plot"),
    sliderInput("time_rng", "Retention Time"
                , value = c(0, 100)
                , min = 0
                , max = 100
                , width = "100%"
                , step = 1
                , post = "min"),
    
    textOutput("txt")
    
  )
    


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Get preview data
  prev_dt <- reactive({
    
    req(input$upload)
    obj <- traceR::load_trace(fls = input$upload$datapath)
    obj$LOG$FILE_NAME <- input$upload$name
    
    obj$LOG |>
      dplyr::select(!c(FILE, SOURCE))|>
      merge(traceR::trace_info(obj, force_raw = TRUE), by ="ID")

  })
  
  observeEvent(input$submit_btn, {
    
    obj <- traceR::load_trace(fls = input$upload$datapath) # indexes are changed
    what <- NULL
    
    if(!is.null(input$preview_rows_selected)){
      
      what <- prev_dt()[["ID"]][input$preview_rows_selected]
      
      obj <- traceR::copy_trace(x = obj, what = what)
    }
    
    output$txt <- renderPrint(what)
    
  })
  
  
  
  
  
  # Update sliderInput with uploaded data
  #observeEvent(input$upload, {
    
    # Extract Meta data table
    #dt_meta <- traceR::trace_info(data(), force_raw = TRUE)
    
    # Set limits
    #rng_min <- min(dt_meta$minRT, na.rm = TRUE)
    #rng_max <- max(dt_meta$maxRT, na.rm = TRUE)
    
    # update Slider
    #updateSliderInput(inputId = "time_rng"
   #                   , session = session
    #                  , min = rng_min
    #                  , max = rng_max
    #                  , value = c(rng_min, rng_max))
 # })
  
  # Render Preview Data
  output$preview <- DT::renderDT({
    
    DT::datatable(prev_dt()
                  , filter = "none"
                  , rownames = TRUE
                  , selection = "multiple"
                  , options = list( 
                    scrollY = "200px",
                    scrollX = TRUE,
                    paging = FALSE)
                  )
    
    })
  # Render Chromatograms
  #output$plot <- renderPlot({
    
    #obj <- data()|>
      #traceR::tr_crop(crop_to = c(input$time_rng[1]
                                  #,input$time_rng[2]))
    
   # if(input$Scaling != "none"){
     # obj <- traceR::tr_rescale(x = data(), type = input$Scaling)
    #}
    
   # traceR::plt_gg(x = obj)
    
  #})
  
  
  #output$txt <- renderPrint(input$Scaling)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

