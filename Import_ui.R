library(shiny)
library(bslib)



plot_card <- function(header, ...) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(header, class = "bg-blue"),
    bslib::card_body(..., min_height = 150)
  )
}


st_log <- bslib::card(
  full_screen = TRUE,
  
  bslib::card_header(
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
  
  bslib::card_body( DT::DTOutput("status") ),
  
  bslib::card_footer(
    div(style ="display: flex; gap: 10px; justify-content: flex-end; align-items: center;",
        shiny::actionButton(inputId = "btn_preview"
                 , label = "Preview"
                 , icon = icon("binoculars")
                 , disabled = TRUE),
        shiny::actionButton(inputId = "btn_reload"
                 , label = "Reload"
                 , icon = icon("refresh")
                 , disabled = TRUE)
        ))
)


# IMPORT PAGE ==================================================================
import_page <- bslib::layout_columns(
  col_widths = c(6,6),
  st_log,
  
  navset_card_pill(
    full_screen = TRUE,
    #title = "PREVIEW : ",
    nav_panel(
      "Raw",
      shiny::verbatimTextOutput("preview")),
    
    nav_panel(
      "DataTable",
      DT::DTOutput("data_tab")),
  ),
)


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
    
    textInput(inputId = "dec",
              label = "Delim:", 
              value = ".", 
              width = "60px"),
    
    textInput(inputId = "sep",
              label = "Sep:", 
              value = ",", 
              width = "70px"),
    
    numericInput(inputId = "skip",
              label = "Skip rows:", 
              value = 1,
              min = 0,
              width = "70px"),
    
    numericInput(inputId = "nrow",
              label = "Rows to show:", 
              value = 25,
              min = 1,
              max = 99,
              width = "70px"),
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
  
  # Update Preview btn state 
  observeEvent(input$upload,{
    
    updateActionButton(session = session
                       , inputId = "btn_preview"
                       , disabled = FALSE)
    })
  
  observeEvent(input$nrow, {
    # Check if the input is not empty and exceeds 100
    if (!is.na(input$nrow) && input$nrow > 100 && input$nrow < 1) {
      
      # Force the input back to the maximum allowed value
      shiny::updateNumericInput(session, "nrow", value = 100)
      
      # Optional: Notify the user with a toast or alert
      shiny::showNotification("Maximum number of rows to preview is 100", type = "warning")}
      
      })
  
  
  # Activate Preview
  observeEvent(input$btn_preview,{
    
    if(!is.null(input$status_rows_selected)){
      bfl <- spc()$LOG[["FILE"]][input$status_rows_selected]
      prv_tab <- readLines(con = bfl, n = input$nrow)
      }else{prv_tab <- "No Data Selected"}
    
    #prv_tab <- read.csv(file = bfl
     #                   , header = FALSE
     #                   , sep = input$sep
     #                   , dec = input$dec
      #                  , skip = input$skip
     #                   , nrows = input$nrow)
    
    
    
    
    
    
    # Render Preview 
    output$preview <- shiny::renderPrint({
      
      prv_tab
      
    })
    
    
    
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
                    paging = FALSE))
    
  })
  
}

# Run App
shinyApp(ui, server)




