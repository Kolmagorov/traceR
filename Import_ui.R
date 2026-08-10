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
    card_body( DT::DTOutput("status")|> 
                 shinycssloaders::withSpinner(type = 6, color = "#0d6efd") 
               )
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
      layout_columns(
        col_widths = c(6,6,12,12),
        row_heights = c(1,8,1),
        # Retention Time Column
        shiny::selectInput(inputId = "fld_time",
                           label = "Time: ", 
                           choices = ""),
        # Response column
        shiny::selectInput(inputId = "fld_response",
                           label = "Response: ", 
                           choices = ""),
        # Data Table View
        DT::DTOutput("data_tab"),
        actionButton(inputId = "btn_accpt"
                     , label = "Accept"
                     , icon = icon("check")
                     , disabled = TRUE)
        )
      )
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
    htmltools::h5("Parser controls:"),
    
    # Select decimal point
    shiny::selectInput(inputId = "dec",
                label = "Decimal:", 
                choices = list("Comma" = ",",
                               "Period" = "."), 
                selected = ".",
                width = "130px"),
    
    # Select a Delimiter
    shiny::selectInput(inputId = "sep",
                label = "Delim:", 
                choices = list("Tab" = "\t", 
                               "Comma" = ",",
                               "Semicolon" = ";",
                               "Colon" = ":",
                               "Newline" = "\n"), 
                selected = "\t",
                width = "130px"),
   
     # Set number of ros to skip
    shiny::numericInput(inputId = "skip",
              label = "Skip rows:", 
              value = 1,
              min = 0,
              width = "70px"),
    
    # Set number of rows to show
    shiny::numericInput(inputId = "nrow",
              label = "Rows to show:", 
              value = 6,
              min = 1,
              max = 99,
              width = "70px"),
    
    # Include Header
    shiny::checkboxInput(inputId = "hdr", 
                         label = "Include Header",
                         value = FALSE),
    # Reload btn
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
  #includeCSS("www/style.css"),
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
  
  # Import STATUS Columns to Show
  import_status_col <- rlang::syms(c("FILE_NAME", "SOURCE", "LOADED"))
  
  # Get the loaded data reactive
  spc <- reactiveVal(NULL)
  
  # get LOG reactive
  tab_log <- reactiveVal(NULL)
  
  # Container to keep path to the file
  bfl <- reactiveVal(NULL)
  
  # Container to keep Time and  Response columns
  fld_choices <- reactiveVal("")
  
  dt_reload <- reactiveVal(NULL)
  
  
    #if(input$filter == "BAD"){ spc()$LOG |> dplyr::filter(LOADED == FALSE) }
    #else{spc()$LOG }


  
  observeEvent(input$upload,{
    
    obj <- traceR::load_trace(fls = input$upload$datapath)
    obj$LOG$FILE_NAME <- input$upload$name
    
    spc(obj)
    tab_log(obj$LOG)
    
    
  })
  
  
  
  # Update Preview btn state and Select Input 
  observe({
    
    if(grepl(x = input$fld_time, pattern = "numeric")){
      lab_time <- tags$span(icon("check"), " Time:", style = "color: green;")
    }else{lab_time <- tags$span(icon("face-frown"), " Time:", style = "color: red;")}
    
    
    
    if(grepl(x = input$fld_response, pattern = "numeric")){
      lab_response <- tags$span(icon("check"), " Response:", style = "color: green;")
    }else{lab_response <- tags$span(icon("face-frown"), " Response:", style = "color: red;")}
    
    updateSelectInput(session, "fld_time", label = lab_time)
    updateSelectInput(session, "fld_response", label = lab_response)
    
  
    acpt_disabled = TRUE
    
    if(grepl(x = input$fld_response, pattern = "numeric") && 
       grepl(x = input$fld_time, pattern = "numeric")){
      acpt_disabled = FALSE
    }
    
    updateActionButton(session = session
                       , inputId = "btn_accpt"
                       , disabled = acpt_disabled)
    
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
      
      # Get a Filtered row 
      tab_log()[["FILE"]][input$status_rows_selected] |> 
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
      
      tmp_dt <- read.csv(file = bfl()
               , header = input$hdr
               , sep = input$sep
               , dec = input$dec
               , skip = input$skip
               , nrows = input$nrow)
      
      # Append data type
      types <- sapply(tmp_dt, class)
      colnames(tmp_dt) <- paste0( names(tmp_dt), " (", types, ")")
      
      # Update Reactive choices for selectInpit controls
      fld_choices(names( tmp_dt ))
      
      updateSelectInput(session, "fld_time",
                        choices = c("", fld_choices()),
                        selected = "")
      
      updateSelectInput(session, "fld_response",
                        choices = c("", fld_choices()),
                        selected = "")
      
      # Render DataTable Reloaded
      output$data_tab <- DT::renderDT({

        DT::datatable(data = tmp_dt
                      , filter = "none"
                      , rownames = FALSE
                      , selection = "single"
                      , extensions = "Scroller"
                      , options = list(
                        deferRender = TRUE,
                        dom = 't',
                        scrollY = 500,
                        sroller = TRUE,
                        scrollX = TRUE,
                        paging = FALSE))
        })
      
      # Update data table
      dt_reload(tmp_dt)
      
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
    
    if(!is.null(tab_log())){
      
      DT::datatable(data = tab_log()|> dplyr::select(!!!import_status_col)
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
      }
  })
  
  # on Accept btn clicked
  observeEvent(input$btn_accpt,{
    
    # Create tmp dt_reload with new Column Names 
    tmp_dt <- dt_reload() |> 
      dplyr::rename(RT = input$fld_time
                    , Response = input$fld_response)|>
      dplyr::select(RT, Response)
    
    # Rename Selected columns, How to add meta - launch parser?
    new_spc <- traceR::new_trace(x = tmp_dt
                                 , use_columns = c("RT", 'Response')
                                 )
    # DEBUGGing
    output$deb <- renderPrint({class(new_spc)})
    
    #spc
    # update pool spc, update LOG table....
    
  })
  
  
}

# Run App
shinyApp(ui, server)




