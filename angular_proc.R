
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


compar <- tr_compar(x = df
                    , metric = "cosdist"
                    , pw = 0.55
                    , lab = "SampleName")
hc <- compar$SIM |>
  as.dist()|>
  hclust(method = "complete")

par(mar=c(6,3,3,2))

COLS <- c("red", "blue","purple")

dend <- hc |>
  as.dendrogram() |>
  dendextend::set("labels_cex", value = 0.8) |>
  dendextend::set("labels_col", value = COLS, k = 3) |>
dendextend::set("branches_k_color", value = COLS, k = 3) |>
plot(horiz = FALSE, axes = T, main = "До выравнивания")




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



plt_gg(df
       , facet_lab = "SampleName"
       , stacked = T
       , force_raw = F
       #, gr_col = "Group"
)



a<- df

b <- merge_trace(a, df)

b <- merge_trace(b, df)

trace_info(b)

