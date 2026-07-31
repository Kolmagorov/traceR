library(shiny)
library(bslib)

accordion_filters <- accordion(
  accordion_panel(
    "Dropdowns", icon = bsicons::bs_icon("menu-app"),
    !!!filters
  ),
  accordion_panel(
    "Numerical", icon = bsicons::bs_icon("sliders"),
    filter_slider("depth", "Depth", dat, ~depth),
    filter_slider("table", "Table", dat, ~table)
  )
)

card(
  card_header("Groups of diamond filters"),
  layout_sidebar(
    sidebar = accordion_filters,
    plots[[1]]
  )
)
