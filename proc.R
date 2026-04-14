
# Rbuildignore
usethis::use_build_ignore(c("proc.R"))

# licensing
usethis::use_mit_license()

# READ ME
usethis::use_readme_rmd()

devtools::build_readme()


# Imports, dependencies, etc.

IMPORTS <- c("utils"
             , "methods"
             , "dplyr"
             , "lubridate"
             , "prospectr"
             , "ptw"
             , "purrr"
             , "stringr"
             , "tibble"
             , "rlang"
             , "ggplot2"
             , "scales"
             , "stats")


purrr::walk(IMPORTS, usethis::use_package)

roxygen2::roxygenise(clean = TRUE)

devtools::document()

devtools::load_all()

# TEST AREA
df <- load_trace(path_dir = "I:/Angular_Sim_Exp/TEST_AREA")

df <- load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/GNR_045_COSIM/TRACES")

df$LOG


devtools::load_all()
zad <- tr_crop(df, crop_to = c(5, 120), new_obj = T) |>
  tr_resample(pts = 2000, new_obj = T) |> 
  tr_rescale(type = "minmax", new_obj = T)|>
  del_trace(what = c(1,3,59))
trace_info(zad)
zad$LOG

devtools::load_all()
zad <- del_trace(x = df, what = c(1,3,6))

trace_info(zad)

