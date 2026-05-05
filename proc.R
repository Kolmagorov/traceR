
# Rbuildignore
usethis::use_build_ignore(c("proc.R"
                            , "meta_default.R"
                            , "methods.R"))

# licensing
usethis::use_mit_license()

# READ ME
usethis::use_readme_rmd()

devtools::build_readme()

# INTERNAL DATA 
field_desc <- read.csv("//dc3/OAM/Сотрудники/Podkolzin_IV/PROJECTS_ALL/2026/field_description.csv" ,
         sep = ";" , header = T , fileEncoding = "CP1251")

tab_tmplate <- list(
  META_tmpl = data.frame(ID = NA,
                         SampleName = NA,
                         dateAcquired = as.POSIXlt(NA),
                         SOURCE = "Undefined",
                         FILE = NA,
                         Comments = NA),
  
  LOG_tmpl = data.frame(ID = NA,
                        SOURCE = "Undefined",
                        LOADED = FALSE,
                        FILE = NA,
                        FILE_NAME = NA) )

usethis::use_data(field_desc, tab_tmplate
                  , internal = TRUE
                  , overwrite = TRUE)


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
             , "stats"
             , "glue")


purrr::walk(IMPORTS, usethis::use_package)

roxygen2::roxygenise(clean = TRUE)

devtools::document()

devtools::load_all()


# TEST AREA ==========================================
devtools::load_all()
df <-load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/TEST_AREA/Merging/A")|>
  tr_crop(crop_to = c(3, 110) ,new_obj = T) |> 
  tr_resample(pts = 2000, new_obj = T) |> 
  tr_rescale(type = "minmax", new_obj = T)

df_b <- load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/TEST_AREA/Merging/B")|>
  tr_rescale(type = "minmax", new_obj = T)

devtools::load_all()
d <- merge_trace(df_a, df_a, keep_history = T)


plot(df
     , ylim = NULL
     , stacked = T
     , facet_lab = "SampleName + Channel.Description"
     , gr_col = "ID"
     , what = 1:2)

dt <- data.frame(A = 1:50, B = cos(rnorm(50, mean = 9, sd = 2)))
plot(dt, type = 'l')

devtools::load_all()
a <- init_log(tmpl = "META")
b <- init_log(tmpl = "LOG")

a()
b()

a(list(SSA=99))
a(list(SSD="LOL"))
a(list(SSD="D",FOO=59))


dt <- data.frame(A = 1:15, B = (1:15)*115)

devtools::load_all()
foo <- new_trace(x = dt)

raw_test <- df$DATA$RAW

foo <- new_trace(x = df$DATA$PROCESSED, 
                 meta = df$META)

trace_info(foo)
foo$LOG

devtools::load_all()
df <-load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/TEST_AREA/Merging/A") #|> 
  tr_crop(crop_to = c(3, 110) ,new_obj = T) #|> 
  #tr_resample(pts = 2000, new_obj = T) |> 
  tr_rescale(type = "minmax", new_obj = T)


foo <- load_trace(path_dir ="C:/RWD/GNR-127/Chromeleon")

devtools::load_all()
df <- load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/TEST_AREA/Merging/A")
trace_info(foo)


devtools::load_all()
df <- load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/TEST_AREA/HETERO")|>
  tr_crop(crop_to = c(2, 120))|>
  tr_rescale(type = "minmax")#|>
  plt_gg(facet_lab = "SampleName")

devtools::load_all()
zad <- tr_compar(df, metric = "angulardist", lab = "FILE", pw = 0)
zad$SIM

foo <- readRDS(file = "C:/RWD/zad.RDS")


df <- load_trace(path_dir = "D:/2026/GNR-122/ARC/GNR-122/TRACES")

df <- tr_crop(x = df, crop_to = c(2, 32))
df <- tr_rescale(x = df, type = "minmax")




df <- add_field(x = df, new_field = "EQP")

df$META <- lapply(df$META, function(dt){
  
  dt|>
    dplyr::mutate(EQP = dplyr::case_when(
      grepl(FILE, pattern = "^_GNR060") ~ "Arc Premier",
      .default = "Alliance")
      )
})


plt_gg(x = df
       , facet_lab = "EQP + SampleName"
       , ylim = c(-0.05, 0.7)
       , stacked = F
       , what = c(3,4))











