library(shiny)
library(bslib)
devtools::load_all()


# IMPORT PAGE ==================================================================
import_page <- layout_columns(
  col_widths = c(6,6),
  # IMPORT LOG
  card(
    full_screen = TRUE,
    card_header(
      "IMPORT STATUS",
      class = "bg-blue",
      toolbar(
        align = "right",
        toolbar_input_select(
          id = "filter",
          label = "Filter",
          choices = c("All", "BAD"),
          icon = icon("filter")
        )
      )
    ),
    card_body( DT::DTOutput("status") )
  ),
  
  # PREVIEW SECTION
  navset_card_pill(
    id = "preview_navpill",
    full_screen = TRUE,
    title = "PREVIEW : ",
    
    # RAW data
    nav_panel(
      "Raw",
      icon = icon("binoculars"),
      shiny::verbatimTextOutput("preview")),
    
    # Parsed Data
    nav_panel(
      "DataTable",
      icon = icon("refresh"),
      # Column assignment
      layout_column_wrap(
        # Retention Time Column
        shiny::selectInput(inputId = "fld_time",
                           label = "Time: ", 
                           choices = ""),
        # Response column
        shiny::selectInput(inputId = "fld_response",
                           label = "Response: ", 
                           choices = ""),
        
      ),
      # Data Table View
      DT::DTOutput("data_tab"),
      actionButton(inputId = "accpt", label = "Accept", icon = icon("check")))
))


# IMPORT SIDEBAR ===============================================================
input_sidebar <- bslib::layout_sidebar(
  sidebar = bslib::sidebar(
    title = "Import Control",
    width = 300,
    style = "min-width: 250px; max-width: 400px",
    # File IMPORT 
    shiny::fileInput(inputId = "upload"
                     , label = "Select a file:"
                     , buttonLabel = "Upload..."
                     , multiple = TRUE
                     , accept = c(".csv", ".arw", ".txt")
                     , placeholder = "browse a file"),
    htmltools::hr(),
    htmltools::h5("Parser controls:"),
    
    shiny::selectInput(inputId = "dec",
                label = "Decimal:", 
                choices = list("Comma" = ",",
                               "Period" = "."), 
                selected = "Comma",
                width = "130px"),

    shiny::selectInput(inputId = "sep",
                label = "Delim:", 
                choices = list("Tab" = "\t", 
                               "Comma" = ",",
                               "Semicolon" = ";",
                               "Colon" = ":",
                               "Newline" = "\n"), 
                selected = "Tab",
                width = "130px"),

    shiny::numericInput(inputId = "skip",
              label = "Skip rows:", 
              value = 1,
              min = 0,
              width = "70px"),
    
    shiny::numericInput(inputId = "nrow",
              label = "Rows to show:", 
              value = 25,
              min = 1,
              max = 99,
              width = "70px"),
    
    shiny::checkboxInput(inputId = "hdr", 
                         label = "Include Header",
                         value = FALSE),
    
    shiny::actionButton(inputId = "btn_reload"
                        , label = "Reload"
                        , icon = shiny::icon("refresh")
                        , disabled = FALSE),
    
    shiny::verbatimTextOutput("deb")
    ), 
  
  import_page
  
  )

# FORGE SIDEBAR ================================================================
forge_sidebar <- bslib::layout_sidebar(
  sidebar = bslib::sidebar(
  title = "FORGE", 
  textInput("txt", "Enter text:", "Hello")
  ))

proc_sidebar <- bslib::layout_sidebar(
  sidebar = bslib::sidebar(
    title = "PROCESSING", 
    textInput("txt", "Enter text:", "Hello")
  ))




# MAIN UI ======================================================================
ui <- bslib::page_navbar(
  title = "UVizor",
  
  bslib::nav_panel("IMPORT", 
            icon = bsicons::bs_icon("database-up"),
            input_sidebar), 
  
  bslib::nav_panel("FORGE", 
            icon = bsicons::bs_icon("tools"), 
            forge_sidebar, 
            textOutput("txt_out")),
  
  bslib::nav_panel("PROCESSING", 
            icon = bsicons::bs_icon("plus-slash-minus"), 
            forge_sidebar, 
            textOutput("txt_out"))
  )



# Server Logic =================================================================
server <- function(input, output, session){
  
  # Get loaded data reactive
  spc <- reactive({

    req(input$upload)
    obj <- traceR::load_trace(fls = input$upload$datapath)
    obj$LOG$FILE_NAME <- input$upload$name
    obj
    
  })
  
  # Container to keep path to the file
  bfl <- reactiveVal(NULL)
  
  # Container to keep Time and  Response columns
  fld_choices <- reactiveVal("")
  
  # Update Preview btn state 
  observeEvent(input$upload,{
    
    updateActionButton(session = session
                       , inputId = "btn_preview"
                       , disabled = FALSE)
    })
  
  # Handling numeric input limits 
  observeEvent(input$nrow, {
    # Check if the input is not empty and exceeds 100
    if (!is.na(input$nrow) && input$nrow > 100 && input$nrow < 1) {
      
      # Force the input back to the maximum allowed value
      shiny::updateNumericInput(session, "nrow", value = 100)
      
      # Optional: Notify the user with a toast or alert
      shiny::showNotification("Maximum number of rows to preview is 100", type = "warning")}
      
      })
  
  # Activate Preview
  observeEvent(input$status_rows_selected,{
    
    if(!is.null(input$status_rows_selected)){
      
      # BUG Does NOT Match rows if filtered !!!!
      spc()$LOG[["FILE"]][input$status_rows_selected] |> 
        bfl() 
      prv_tab <- readLines(con = bfl(), n = input$nrow)}
    
    else{prv_tab <- "No Data Selected"}
    
  # Render Preview 
    output$preview <- shiny::renderPrint({
      
      prv_tab
      
    })
    
  })
  
  # Reloading and Render data Table
  observeEvent(input$btn_reload,{
    
    if(!is.null(bfl())){
      
      dt_reload <- read.csv(file = bfl()
                            , header = input$hdr
                            , sep = input$sep
                            , dec = input$dec
                            , skip = input$skip
                            , nrows = input$nrow)
      
      fld_choices(names(dt_reload))
      
      updateSelectInput(session, "fld_time",
                        choices = c("", fld_choices()),
                        selected = "")
      
      updateSelectInput(session, "fld_response",
                        choices = c("", fld_choices()),
                        selected = "")
      
      # Render DataTable Reloaded
      output$data_tab <- DT::renderDT({
        
        DT::datatable(data = dt_reload
                      , filter = "none"
                      , rownames = FALSE
                      , selection = "single"
                      , options = list(
                        dom = 't',
                        #scrollY = "400px",
                        scrollX = TRUE,
                        paging = FALSE))
        })
      
      # Switch to datatable page
      nav_select(id = "preview_navpill", selected = "DataTable")
      }

    })
  
  # Update Response choices when Time  changes
  observeEvent(input$fld_time, {
    current_response <- input$fld_response
    available_response <- setdiff(fld_choices(), input$fld_time)
    
    updateSelectInput(session, "fld_response",
                      choices = c("", available_response),
                      selected = current_response)
  })
  
  # Update Time  when Response changes
  observeEvent(input$fld_response, {
    current_time <- input$fld_time
    available_time <- setdiff(fld_choices(), input$fld_response)
    
    updateSelectInput(session, "fld_time",
                      choices = c("", available_time),
                      selected = current_time)
  })
  
  # Render IMPORT STATUS TABLE
  output$status <- DT::renderDT({
    
    tab <- spc()$LOG|> dplyr::select(FILE_NAME, SOURCE, LOADED)
    
    if(input$filter == "BAD"){

      tab <- tab|> dplyr::filter(LOADED == FALSE)
    }

    
    DT::datatable(data = tab
                  , filter = "none"
                  , rownames = FALSE
                  , selection = "single"
                  , options = list(
                    dom = 't',
                    scrollY = "400px",
                    scrollX = TRUE,
                    paging = FALSE))|>
      
      DT::formatStyle("LOADED",
                      color =DT::styleEqual(c(TRUE, FALSE), c('green', 'red')
                                            ))
    
  })
  
  # DEBUGGing
  output$deb <- renderPrint({input$hdr})
  
}

# Run App
shinyApp(ui, server)




