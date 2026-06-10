
# Rbuildignore
usethis::use_build_ignore(c("proc.R"
                            , "meta_default.R"
                            , "methods.R"
                            , "angular_proc.R"))

# licensing
usethis::use_mit_license()

# READ ME
usethis::use_readme_rmd()

devtools::build_readme()

usethis::use_news_md()




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
             , "glue"
             , "pracma")


purrr::walk(IMPORTS, usethis::use_package)

roxygen2::roxygenise(clean = TRUE)

devtools::document()

devtools::load_all()

usethis::use_author(
  given = "Ivan",
  family = "Podkolzin",
  email = "nizlokdop@gmail.com",
  role = c("aut", "cre"),
  comment = c(ORCID = "0000-0003-0851-747X")
)



# TEST AREA ==========================================
"C:/RWD/Angular_Sim_Exp/TEST_AREA/Merging/A"
"C:/RWD/Angular_Sim_Exp/TEST_AREA/Merging/B"
"C:/RWD/Angular_Sim_Exp/TEST_AREA/HETERO"
"C:/RWD/Angular_Sim_Exp/GNR_060/TRACES"

devtools::load_all()
dff <- load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/GNR_060/TRACES")|>
  tr_crop(crop_to = c(5, 110))|>
  tr_resample(pts = 2000)|>
  tr_rescale(type = "minmax")

df$META <- lapply(df$META, function(dt){
  
  dt|> dplyr::mutate(Group = dplyr::if_else(
    grepl(SampleName, pattern = "060"), "Tenekteplase", "Alteplase"))
})


idx <- trace_info(df)|>
  dplyr::filter(grepl(Channel.Description, pattern = "215"))|>
  dplyr::pull(ID)


df <- copy_trace(df, what = idx)|>
  tr_align(ref = 3)
  
tr_compar(x = df
          , metric = "cosdist"
          , pw = 0.55
          , lab = "SampleName")[[1]]|>
  as.dist()|>
  hclust(method = "ward.D2")|>
  plot()


plt_gg(df
       , facet_lab = "SampleName"
       , force_raw = F
       , gr_col = "Group"
       )

FLS <- "D:/2026/GNR-122/Traces"

df <- load_trace(path_dir = FLS)|>
  tr_crop(crop_to = c(1, 15))|>
  #tr_resample(pts = 800)|>
  tr_rescale(type = "maxnorm")


idx <-trace_info(df)|> dplyr::filter(score > 700)|> dplyr::pull(ID)




plt_gg(df
       , facet_lab = "SampleName"
       , force_raw = F
       , gr_col = "SampleName"
       #, what = idx
      # , ylim = c(-0.1, 0.4)
       , stacked = T
      , what = NULL
)


prc <- tr_workflow(flow = obj|> 
                     tr_crop(crop_to = c(5, 110))|>
                     tr_resample(pts = 8000)|>
                     tr_rescale(type = "minmax")|>
                     plt_gg(what = list(chan = "280"))
                   )

devtools::load_all()
set_field_value(x = dff
                , what = 1
                , global = F
                , field = list(gro = 8))|>
  trace_info()
