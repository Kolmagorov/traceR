
devtools::load_all()

df <- load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/GNR_060/TRACES")|>
  tr_crop(crop_to = c(5, 110))|>
  #tr_resample(pts = 800)|>
  tr_rescale(type = "minmax")|>
  add_field(new_field = list(Group = "Alteplase"))

df$META <- lapply(df$META, function(dt){
  
  dt|> dplyr::mutate(Group = dplyr::if_else(
    grepl(SampleName, pattern = "060"), "Tenekteplase", "Alteplase"))
})


idx <- trace_info(df)|>
  dplyr::filter(grepl(Channel.Description, pattern = "280"))|>
  dplyr::pull(ID)


df <- copy_trace(df, what = idx)|>
  tr_align(ref = 3)


plt_gg(df
       , facet_lab = "SampleName"
       , force_raw = F
       , gr_col = "Group"
)


compar <- tr_compar(x = b
                    , metric = "cosdist"
                    , pw = 0.55
                    , lab = "SampleName")
hc <- compar$SIM |>
  as.dist()|>
  hclust(method = "ward.D2")

par(mar=c(2,2,3,6))

COLS <- c("red", "blue","purple", "magenta")

dend <- hc |>
  as.dendrogram() |>
  dendextend::set("labels_cex", value = 0.8) |>
  dendextend::set("labels_col", value = COLS, k = 4) |>
dendextend::set("branches_k_color", value = COLS, k = 4) |>
plot(horiz = TRUE, axes = F)


compar$SIM |> corrplot::corrplot(method = c("square")
                                 , type = "lower"
                                 , diag = F
                                 , addCoef.col = "black"
                                 , is.corr = F
                                 , order = "hclust"
                                 , hclust.method = "ward.D2"
                                 , col = corrplot::COL1('Oranges', 100)
                                 #, col.lim = c(0.4, 1)
                                 )



# ============================= GNR - 045 =============================

zd <- "C:/RWD/Angular_Sim_Exp/GNR_045_COSIM/TRACES"
zd <- "I:/Angular_Sim_Exp/GNR_045_COSIM/TRACES"

devtools::load_all()

df <- load_trace(path_dir = zd)|>
  tr_crop(crop_to = c(5, 110))|>
  tr_resample(pts = 8000)|>
  tr_rescale(type = "minmax")|>
  add_field(new_field = list(Pyro = "Base"))

trace_info(df)

idx <- trace_info(df)|>
  dplyr::filter(grepl(Channel.Description, pattern = "215"))|>
  dplyr::filter(!grepl(SampleName, pattern = "_r.*$"))|>
  dplyr::pull(ID)


df <- copy_trace(df, what = idx)#|>
 #tr_align(ref = 3)

trace_info(df)

df$META <- lapply(df$META, function(dt){
  
  dt|> dplyr::mutate(Group = dplyr::case_when(
    grepl(SampleName, pattern = "_r\\d$") ~ "Base",
    grepl(SampleName, pattern = "_.*$") ~ "Oxidized",)
    
    )
})


trace_info(df)|>
  dplyr::arrange(dateAcquired)|>
  dplyr::select(dateAcquired, SampleName)



plt_gg(b
       , facet_lab = "SampleName"
       , stacked = T
       , force_raw = F
       #, gr_col = "Group"
)



a <- df

b <- merge_trace(a, df)

b <- merge_trace(b, df)

trace_info(b)

b$META <- lapply(b$META, function(dt){
  
  dt$SampleName <- gsub(dt$SampleName, pattern = "/", replacement = ".")|>
    gsub(pattern = "_(\\d.*)", replacement = "_[\\1%]")
  dt
  
})

saveRDS(object = b, file = "I:/Angular_Sim_Exp/GNR_045_COSIM/GNR_045vs118.Rds")







