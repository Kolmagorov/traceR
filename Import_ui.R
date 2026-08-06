library(shiny)
library(bslib)



plot_card <- function(header, ...) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(header, class = "bg-blue"),
    bslib::card_body(..., min_height = 150)
  )
}


st_log <- card(
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
  
  card_body( DT::DTOutput("status") ),
  
  card_footer(
    div(style ="display: flex; gap: 10px; justify-content: flex-end; align-items: center;",
        actionButton(inputId = "btn_preview"
                 , label = "Preview"
                 , icon = icon("binoculars")
                 , disabled = TRUE),
        actionButton(inputId = "btn_reload"
                 , label = "Reload"
                 , icon = icon("refresh")
                 , disabled = TRUE)
        ))
)


# IMPORT PAGE ==================================================================
import_page <- bslib::layout_columns(
  col_widths = c(8,4),
  st_log,
  plot_card("PREVIEW", shiny::textOutput("preview"))
  
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
    
    textInput(inputId = "del",
              label = "Delim:", 
              value = ",", 
              width = "60px"),
    
    textInput(inputId = "sep",
              label = "Sep:", 
              value = ".", 
              width = "60px"),
    
    textInput(inputId = "skip",
              label = "Skip rows:", 
              value = 1, 
              width = "60px"),
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




server <- function(input, output, session){
  
  # Get loaded data reactive
  spc <- reactive({
    
    req(input$upload)
    obj <- traceR::load_trace(fls = input$upload$datapath)
    obj$LOG$FILE_NAME <- input$upload$name
    obj
    
  })
  
  
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
  
  output$preview <- shiny::renderText({
    
    input$filter
    
  })
  
  
}
shinyApp(ui, server)




