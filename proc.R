
# Rbuildignore
usethis::use_build_ignore(c("proc.R"))

# licensing
usethis::use_mit_license()

usethis::use_readme_rmd()

devtools::build_readme()

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

str(df$META)

expand_meta_data(lst = df$DATA$RAW)

a <- tr_align(x = df, new_obj = T, rm_na = F)
str(a)

devtools::load_all()
tr_crop(df, crop_to = c(5, 120), new_obj = T) |>
  tr_resample(pts = 2000, new_obj = T) |> 
  tr_rescale(type = "minmax", new_obj = T)|>
  plt_tracer(what = c(1,4,8))

devtools::load_all()
set_field_value(x = df,  
                what = c("w7y55nd",  "df", "foo"),
                record = c("ZAD", "ZAD2","XZ"),
                field = "SRC") |> 
  trace_info()

zad <-add_field(x = df, new_field = "ZAD5")


