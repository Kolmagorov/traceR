library(shiny)
library(bslib)


# IMPORT PAGE ==================================================================
import_page <- bslib::layout_columns(
  DT::DTOutput("status"),
  DT::DTOutput("preview")
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
    
    htmltools::hr(),
    
    radioButtons( 
      inputId = "rbt_imput_sb",
      inline = TRUE,
      selected = 2,
      label = "Show Status", 
      choices = list("All" = 2, "Good" = 1, "BAD" = 0)),
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
  
  output$status <- DT::renderDT({
    
    DT::datatable(data = mtcars
                  , filter = "none"
                  , rownames = FALSE
                  , selection = "single"
                  , options = list(
                    scrollY = "400px",
                    scrollX = TRUE,
                    paging = FALSE))
    
  })
}
shinyApp(ui, server)




