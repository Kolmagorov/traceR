
# Rbuildignore
usethis::use_build_ignore(c("proc.R"))

# licensing
usethis::use_mit_license()

usethis::use_readme_rmd()

devtools::build_readme()

IMPORTS <- c("dplyr"
             ,"lubridate"
             , "prospectr"
             , "ptw"
             , "purrr"
             , "stringr"
             , "tibble")


purrr::walk(IMPORTS, usethis::use_package)

roxygen2::roxygenise(clean = TRUE)

devtools::document()

devtools::load_all()

# TEST AREA
df <- load_trace(path_dir = "I:/Angular_Sim_Exp/TEST_AREA")

df$LOG

str(df$META)

expand_meta_data(lst = df$DATA$RAW)

a <- tr_align(x = df, new_obj = T, rm_na = F)
str(a)
