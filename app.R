library(shiny)
#library(bslib)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Add theme
  #theme = bs_theme(version = 5),
  includeCSS("www/style.css"),
  #tags$head(
   # tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  #),
  
  # Application title
  titlePanel("UVizor TEST", windowTitle = "UVizor"),
  
  # File Upload -----------------------------------------------------
  fileInput("upload"
            , label = "Select a file:"
            , buttonLabel = "Upload..."
            , multiple = TRUE
            , accept = c(".csv", ".arw", ".txt")
            , placeholder = "browse a file"),
  br(),
  # Preview Table --------------------------------------------------
  h4("Summary Preview"),
  
  checkboxGroupInput(
    inputId = "info_prev",       
    label = "display options",         
    choices = c(Common = "com", Computed = "extra"),       
    selected = c("com", "extra"),      
    inline = TRUE
  ),
  
  DT::DTOutput("preview"),
  
  actionButton(inputId = "submit_btn"
               , label = "Submit"
               , icon = icon("stats", lib = "glyphicon")
               , disabled = TRUE),
  br(),
  # Trace Plot Section ------------------------------------------------------
  h4("Traces"),
  
  # Signal Normalization options
  selectInput(
    inputId = "Scaling",
    label = "normalize by:",
    choices = c("NONE" = "none", "MINMAX" = "minmax", "MAXNORM" = "maxnorm"),
    selected = "none"
    ),
  
  # Facet Labels
  varSelectizeInput(
    inputId = "facet_lab",
    label = "Select Label:",
    data = data.frame(ID = "none"),
    multiple = TRUE,
    options = list(plugins = list("drag_drop")),
    selected = "ID"
  ),
  
  # Alignment button
  actionButton(inputId = "align_btn"
               , label = "Aligne"
               , icon = icon(name = "object-align-vertical",
                             lib = "glyphicon")),
  
  # PLOT AREA
  plotOutput("plot"),
  
  # RETENTION Time cropping
  sliderInput("time_rng", "Retention Time"
              , value = c(0, 100)
              , min = 0
              , max = 100
              , width = "100%"
              , step = 1
              , post = "min"),
  
  # NEW Obj meta info --------------------------------------------------
  tableOutput("tbl")
  )

# Define server logic
server <- function(input, output, session) {
  
  # Create container to store an object
  
  act_obj <- reactiveVal(NULL)
  
  # Get loaded data reactive
  spc <- reactive({
    
    req(input$upload)
    obj <- traceR::load_trace(fls = input$upload$datapath)
    obj$LOG$FILE_NAME <- input$upload$name
    obj
   
  })
  
  # Get the submit button label reactive
  sub_lab <- reactive({
   
    req(input$upload)
    
    if(is.null(input$preview_rows_selected)){ suf <- "(All)"}
    else{suf <- sprintf("(%i)", length(input$preview_rows_selected))}
    paste("Submit", suf)
    
  })
  
  # Get preview meta data reactive
  dt_prev <- reactive({
    
    opt_status <- c("com", "extra") %in% input$info_prev
    
    traceR::trace_info(spc()
                       , common = opt_status[1]
                       , extra = opt_status[2]
                       , force_raw = TRUE)|>
      dplyr::select(!c(FILE, SOURCE))|>
      merge(spc()[["LOG"]], by = "ID")|>
      dplyr::select(!FILE)

  })
  
  # Update the Submit button
  observeEvent(sub_lab(),{
    
    updateActionButton(
      session = session,
      inputId = "submit_btn",
      label = sub_lab(),
      icon = icon("stats", lib = "glyphicon"),
      disabled = FALSE)
  })
  
  # Alignment on click <------ CHANGE TO TOGGLE OR SWITCH
  observeEvent(input$align_btn, {
    
    obj <- traceR::tr_align(act_obj(), ref = 1)
    act_obj(obj)
    
  })

  # Submission on Click
  observeEvent(input$submit_btn, {
    
    # Check if any row has been selected
    if(is.null(input$preview_rows_selected)){what <- NULL}
    else{what <- dt_prev()[["ID"]][input$preview_rows_selected]}
    
    # Subset spc and create a new object of class tracer
    obj <- traceR::copy_trace(spc(), what = what)
    
    # Get Meta data of the submitted object
    dt_meta <- traceR::trace_info(obj
                                  , extra = TRUE
                                  , common = TRUE
                                  , force_raw = TRUE) # ? Processed ?
    
    # Keep character fields only
    facet_labs <- dt_meta |> dplyr::select(dplyr::where(is.character))
    
    # Set limits
    rng_min <- min(dt_meta$minRT, na.rm = TRUE)
    rng_max <- max(dt_meta$maxRT, na.rm = TRUE)
    
    # Update Slider Range Limits
    updateSliderInput(inputId = "time_rng"
                      , session = session
                      , min = rng_min
                      , max = rng_max
                      , value = c(rng_min, rng_max))
    
    
    # Update Facet Labs input
    updateVarSelectizeInput(inputId = "facet_lab"
                         , session = session
                         , data = facet_labs
                         , selected = "ID") 
    # Render Traces
    output$plot <- renderPlot({
      
      # Cropping
      obj <- traceR::tr_crop(obj, crop_to = c(input$time_rng[1]
                                              , input$time_rng[2]))
      #act_obj(obj)
      
      # Re-scaling
      if(input$Scaling != "none"){
        obj <- traceR::tr_rescale(obj, type = input$Scaling)
        #act_obj(obj)
      }
      
      act_obj(obj)
      
      traceR::plt_gg(x = act_obj(),
                     facet_lab = paste(input$facet_lab, collapse = "+")
                   )
      
    })
    
    output$tbl <- renderTable({dt_meta})
  })
  
  # Render Preview Data
  output$preview <- DT::renderDT({
    
    DT::datatable(data = dt_prev()
                  , filter = "none"
                  , rownames = TRUE
                  , extensions = "ColReorder"
                  , selection = "multiple"
                  , options = list(
                    colReorder = TRUE,
                    scrollY = "200px",
                    scrollX = TRUE,
                    paging = FALSE)
                  )
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

