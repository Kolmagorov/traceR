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
    inline = TRUE),
  
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
               , label = "Align"
               , disabled = TRUE
               , icon = icon(name = "object-align-vertical",
                             lib = "glyphicon")),
  # Alignment Info
  verbatimTextOutput("align_info", placeholder = TRUE),
  
  # PLOT AREA
  plotOutput("plot"),
  
  # RETENTION Time cropping
  sliderInput("time_rng", "Adjust sliders to crop out Retention time segment"
              , value = c(0, 100)
              , min = 0
              , max = 100
              , width = "100%"
              , step = 1
              , post = " min"),
  
  # Editable Object meta info --------------------------------------------------
  DT::DTOutput("obj_ed")

  )

# Define server logic
server <- function(input, output, session) {
  
  # Create container to store an object 
  act_obj <- reactiveVal(NULL)
  
  # Reactive values to track the previously selected row
  row_sel_ed <- reactiveValues(selected_row = 1)
  
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
    
    obj <- traceR::tr_align(act_obj(), ref = row_sel_ed$selected_row)
    act_obj(obj)
    
  })
  
  # Selecting a reference from table
  observeEvent(input$obj_ed_rows_selected, {
    
    if(length(input$obj_ed_rows_selected) > 0) {
      # Save the newly selected row
      row_sel_ed$selected_row <- input$obj_ed_rows_selected
    }else{
      # If user deselects, force the previously selected row to remain active
      DT::selectRows(DT::dataTableProxy("obj_ed"), row_sel_ed$selected_row)
    }
  }, ignoreNULL = FALSE)
  
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
                                  , force_raw = TRUE) # ? add a Switch to Processed ?
    
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
    
    # Update Align btn state 
    updateActionButton(session = session
                       , inputId = "align_btn"
                       , disabled = FALSE)
    
    
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
    
    # Render child Object meta data
    output$obj_ed <- DT::renderDT({
      
      DT::datatable(data = dt_meta
                    , filter = "none"
                    , rownames = TRUE
                    , extensions = "ColReorder"
                    , selection = "single"
                    , options = list(
                      colReorder = TRUE,
                      scrollY = "200px",
                      scrollX = TRUE,
                      paging = FALSE)
      )
    })
    
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
  
  # Render alignment info
  output$align_info <- renderPrint({
    
    act_obj()$HISTORY
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

