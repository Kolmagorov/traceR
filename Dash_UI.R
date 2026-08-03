library(shiny)
library(bslib)


acc <- accordion(
  accordion_panel("Dropdowns", icon = bsicons::bs_icon("menu-app")),
  accordion_panel("Numerical", icon = bsicons::bs_icon("sliders"))
)

ui <- card(
  card_header("ACC"),
  layout_sidebar(sidebar = acc, 
                 hist(x = rnorm(500, mean = 0,sd = 1))
                 )
  )

# Run the application 
shinyApp(ui = ui, server = function(input, output, session){})
