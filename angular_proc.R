
devtools::load_all()

df <- load_trace(path_dir = "C:/RWD/Angular_Sim_Exp/GNR_060/TRACES")|>
  tr_crop(crop_to = c(5, 110))|>
  tr_resample(pts = 5000)|>
  tr_rescale(type = "minmax")


df$META <- lapply(df$META, function(dt){
  
  dt$SampleName <- gsub(dt$SampleName, pattern = "GNR_060", replacement =  "Tenecteplase")
  dt$SampleName <- gsub(dt$SampleName, pattern = "GNR_004", replacement =  "Alteplase")
  
  dt <- dt|> dplyr::mutate(Group = dplyr::if_else(
    grepl(SampleName, pattern = "Tenecteplase"), "Tenecteplase", "Alteplase"))

  dt
})


idx <- trace_info(df)|>
  dplyr::filter(grepl(Channel.Description, pattern = "215"))|>
  dplyr::pull(ID)


df <- copy_trace(df, what = idx)|>
  tr_align(ref = 3)


plt_gg(df
       , facet_lab = "SampleName"
       , force_raw = F
       , gr_col = "Group"
       , what = c(1,4)
)


compar <- tr_compar(x = df
                    , use_diff = T
                    , metric = "eucli"
                    , pw = 1
                    , lab = "SampleName")
hc <- compar$SIM |>
  as.dist()|>
  hclust(method = "ward.D2")

par(mar=c(8,2,2,2))

COLS <- c("red", "blue","purple", "magenta")
K <- 2

dend <- hc |>
  as.dendrogram() |>
  dendextend::set("labels_cex", value = 0.8) |>
  dendextend::set("labels_col", value = COLS[1:K], k = K) |>
dendextend::set("branches_k_color", value = COLS[1:K], k = K) |>
plot(horiz = F, axes = F)


compar$SIM |> corrplot::corrplot(method = c("square")
                                 , type = "lower"
                                 , diag = F
                                 , is.corr = F
                                 , order = "hclust"
                                 , hclust.method = "ward.D2"
                                 , col = rev(hcl.colors(80, "viridis"))
                                 , addCoef.col = "grey45"
                                 #, bg = "lightblue"
                                 )


# ============================= GNR - 045 =============================

zd <- "C:/RWD/Angular_Sim_Exp/GNR_045_COSIM/TRACES"
zd <- "I:/Angular_Sim_Exp/GNR_060/TRACES"
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


df$META <- lapply(df$META, function(dt){
  
  dt$SampleName <- gsub(dt$SampleName, pattern = "GNR-045", replacement =  "Eculizumab")
  dt$SampleName <- gsub(dt$SampleName, pattern = "GNR-118", replacement =  "Ravulizumab")
  
  dt <- dt|> dplyr::mutate(Group = dplyr::case_when(
    grepl(SampleName, pattern = "Ecul") ~ "Eculizumab",
    .default = "Ravulizumab"))|> 
    
    dplyr::mutate(Group = dplyr::case_when(
      grepl(SampleName, pattern = "]$") ~ "Oxidized",
      .default = Group))
  dt
  })




trace_info(df)|>
  dplyr::arrange(dateAcquired)|>
  dplyr::select(dateAcquired, SampleName)



plt_gg(df
       , facet_lab = "SampleName"
       , stacked = T
       , force_raw = F
       , gr_col = "Group"
       , what = c(1,4,7,8,9,10,11)
)



a <- df

b <- merge_trace(a, df)

b <- merge_trace(b, df)

trace_info(df)

b$META <- lapply(b$META, function(dt){
  
  dt$SampleName <- gsub(dt$SampleName, pattern = "/", replacement = ".")|>
    gsub(pattern = "_(\\d.*)", replacement = "_[\\1%]")
  dt
  
})

saveRDS(object = b, file = "I:/Angular_Sim_Exp/GNR_045_COSIM/GNR_045vs118.Rds")

df <- readRDS(file = "F:/Angular_Sim_Exp/GNR_045_COSIM/GNR_045vs118.Rds")

dat <- compar$WM |>
  dplyr::filter(Pair == "Tenecteplase_2_vs_Alteplase_2")


cols <- dat|>
  dplyr::filter(W > 0.1)

vec <- hcl.colors(100, "reds", alpha = 0.08, rev = T)
zd <- vec[cut(cols$W, breaks = 200)]

ggplot2::ggplot(data = dat, ggplot2::aes(x=RT, y = Response))+
  
  ggplot2::geom_line(col = "blue")+
  ggplot2::geom_path(ggplot2::aes(x = RT, y = W)
                      #, size = 0.3
                      , col = "red"
                      , alpha = 0.5)+
  #ggplot2::geom_vline(ggplot2::aes(xintercept = RT, color = W)
   #                   , data = cols
    #                  , alpha = 0.15
    #                  , linewidth = 0.3) +
  #ggplot2::scale_colour_continuous(palette = c("#599614", "#f0da6e", "#DE2D26")
  #                        , name = "Importance")+
  #ggplot2::scale_x_continuous(breaks = seq(0, 130, 10)
  #                            , name = "Retention time [min]"
  #                            , minor_breaks = scales::breaks_width(2)) +
  #ggplot2::guides(x = ggplot2::guide_axis(minor.ticks = TRUE, cap = "both")) +
  ggplot2::theme_bw()+
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "grey95"),
                 panel.border = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(colour = "grey60"),
                 axis.ticks.length = ggplot2::unit(5, "pt"),
                 axis.minor.ticks.length = ggplot2::rel(0.5))


head(dat)
plot(dat$RT, -dat$W, type = 'l')





