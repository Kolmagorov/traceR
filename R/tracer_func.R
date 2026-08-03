
#' Gets trace info
#' @description Provides a compact table with descriptive information
#' about traces
#' @param x an object of class tracer
#' @param common logical. If TRUE returns only common field across all samples
#' @param extra logical. If TRUE computes and returns additional metrics 
#' based on meta data
#' @param force_raw logical if TRUE returns info for 
#' the Unprocessed data
#' @param ... an argument `frac` to pass to `expand_meta_data` function
#' @returns a list with descriptive parameters of traces
#' @export
trace_info <- function(x, common = FALSE, extra = TRUE, force_raw = FALSE, ...){

  if(methods::is(x) != "tracer"){ 
    stop("\n x must be an object of type tracer", call. = FALSE)}

  data_ <- "PROCESSED"

  if(isFALSE(force_raw)){
    if(is.null(x$PROCESSED)){ data_ <- "RAW"}
  }else{ data_ <- "RAW" }
  
  message("Returning info for ", data_, " data")
  
  if(common){
    
    hdr <- lapply(x$META, names) |> purrr::reduce(intersect)
    tab <- lapply(x$META, function(dt) dt[hdr] )|>
      do.call("rbind", args =_)
    
  }else{ tab <- dplyr::bind_rows(x$META) }
  
  if(extra){
    
    tab <- expand_meta_data(lst = x[[data_]],...)|>
      merge(y = tab, by = "ID")
    
  }

  return(tab)
}


#' Re-scale Response of a trace
#' @description Transforms Response to a new designated scale.
#' @param x an object of class tracer
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data
#' @param type a string defining re-scaling algorithm. Can be 
#' either minmax with arbitrary limits, or maxnorm that normalizes data to the 
#' greatest value.
#' @param ... an additional argument to specify range for minmax_scale rescale.
#' @export
tr_rescale <- function(x, type = "minmax", new_obj = TRUE, ...){

  if(methods::is(x) != "tracer"){ stop("\n x must be an object of type tracer")}

  if(!grepl(type, pattern = "minmax|maxnorm")){
    stop("Scaling type must be either minmax or maxnorm")}
  

  if(is.null(x$PROCESSED)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}

  out <- switch (type,
                 minmax = lapply(x[[data_]], function(x) minmax_scale(x, ...)),
                 maxnorm = lapply(x[[data_]], function(x){ maxnorm_scale(x, ...) })
                 )
  
  if(new_obj){

    x$PROCESSED <- out
    x <- history_upd(x = x, event = "re-scaled")
    x <- workflow_upd(x = x, m_call = match.call())
    return(x)

  }else{ return(out) }

}

#' Cropping a trace
#' @description Trims the profile to the set RT range 
#' @param x an object of class tracer
#' @param crop_to sets the limits defining a cropping segment, 
#' if it is a single positive number,  will be treated as an upper limit 
#' starting from zero.
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data
#' @export
tr_crop <- function(x, crop_to, new_obj = TRUE){
  

  if(methods::is(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(!is.numeric(crop_to)){stop("The cropping range must be positive numeric\n")}

  if(length(crop_to) == 1 & all(crop_to > 0) ){ crop_to <- c(0, crop_to) }

  if(length(crop_to) == 2 & crop_to[1] > crop_to[2]){
    stop("The lower bound of the crooping segment is greater than th Upper one\n")
  }
  
  
  if(is.null(x$PROCESSED)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}
  
  out <- lapply(x[[data_]], function(dt){

    if(crop_to[1] <= min(dt$RT)){ crop_to[1] <- min(dt$RT) }
    if(crop_to[2] >= max(dt$RT)){ crop_to[2] <- max(dt$RT) }

    dplyr::filter(.data = dt, .data$RT >= crop_to[1] & .data$RT <= crop_to[2])
  })

  if(new_obj){

    x$PROCESSED <- out
    x <- history_upd(x = x, event = "cropped")
    x <- workflow_upd(x = x, m_call = match.call())
    return(x)
    

  }else{ return(out) }

}

#' Trace re-sampling
#' @description Re-samples data to new retention time points 
#'  using spline or linear interpolation, applying 'prospectr::resample'
#' @param x an object of class tracer
#' @param pts an integer setting the number of points to re-sample
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data
#' @export
tr_resample <- function(x, pts, new_obj = TRUE){

  if(methods::is(x) != "tracer"){ stop("\n x must be an object of type tracer")}

  if(is.null(x$PROCESSED)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}
  
  blw_pts <- suppressMessages(trace_info(x = x), classes = "message")|>

    dplyr::select(.data$FILE, .data$pts, .data$SOURCE)|>
    dplyr::filter(.data$pts < pts)

  if(nrow(blw_pts) > 0){

    msg <- paste(nrow(blw_pts),  "out of", sum(x$LOG$LOADED == TRUE)
          , "samples have less data points than the pts")

    message(msg)

    ans <- readline(prompt = "Would you like to proceed (y/n): ")
    if(!grepl(ans, pattern = "[Y,y]")){

      print(blw_pts)

      stop("Resampling has been stopped by user", call. = F)
    }
  }

  out <- lapply(x[[data_]], function(dt){

    new_RT <- seq(min(dt[["RT"]]), max(dt[["RT"]]), length.out = pts)

    data.frame(
      RT = new_RT,
      Response = prospectr::resample(X = dt[["Response"]]
                          , wav = dt[["RT"]]
                          , new.wav = new_RT), 
      row.names = NULL
    )
  })


  if(new_obj){
    
    x$PROCESSED <- out
    x <- history_upd(x = x, event = "re-sampled")
    x <- workflow_upd(x = x, m_call = match.call())
    return(x) }
  
  else{return(out) }

}

#' a Trace Aligner
#' @description Aligns traces to a reference provided
#' @param x an object of class tracer
#' @param ref an index of a trace that will be used as a reference
#' @param rm_na logical, if TRUE will replace all NA's that may be introduced after
#' ptw alignment algorithm, with the lowest value in the traces.
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data. Also overrides 'return_mat'.
#' @param return_mat if TRUE, instead of a list returns a matrix of aligned data.
#' @param ... an additional argument to to be passed 'ptw::ptw'.
#' @returns either an object of class tracer, or a matrix, or a list of processed data, 
#' depending on the arguments `return_mat` and `new_obj`, the later overrides `return_mat`.
#' @export
tr_align <- function(x
                     , new_obj = TRUE
                     , ref = 1L
                     , rm_na = TRUE
                     , return_mat = FALSE
                     , ...){

  if(methods::is(x) != "tracer"){ stop("\n x must be an object of type tracer")}

  if(is.null(x$PROCESSED)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}
  
  # validate number of point in each samples of the input object x
  x <- data_point_validator(x)
  
 
  if(ref > length(x$RAW) | ref <= 0){
    stop("Argument ref must be within the range 1 - ", length(x$RAW))
  }
  
  reference <- x$META[[ref]]|>
    dplyr::select(.data$ID, .data$SampleName)
  
  message("Aligning against \n")
  print(reference)


  ref <- x[[data_]][[ref]][["Response"]]

  # Aligning and updating PROCESSED DATA
  x[[data_]] <- lapply(x[[data_]], function(dt){

    algn <- ptw::ptw(ref = ref
                     , samp = dt[["Response"]]
                     , ...)

    if(rm_na){
      algn$warped.sample[is.na(algn$warped.sample)] <- min(algn$warped.sample, na.rm = T)
    }

    dt[["Response"]] <- algn$warped.sample |> c()
    dt
  })

  
  if(new_obj){
    
    x <- history_upd(x = x, event = "alignment")
    x <- workflow_upd(x = x, m_call = match.call())
    return(x) }

  else if(return_mat){

    mat <- x$PROCESSED |>

      lapply(function(r){
        r$Response
    }) |>
      do.call("rbind", args=_)

    return(list(Response = mat, RT = x$PROCESSED[[1]][["RT"]]))
  }
  else{ return(x$PROCESSED) }
}


#' Plot traces with ggplot2
#' @description plots traces using ggplot2 graphics
#' @param x object of class tracer
#' @param what either numeric indexes or string of UID. If some Numeric indexes are 
#' out of range, an error will be thrown. Unmatched UID's will be ignored with warning.
#' @param stacked a logical, sets plotting mode to stacked or overlaid
#' @param x_lab a string passing x-axis name to the plot
#' @param force_raw If TRUE will plot RAW data
#' @param facet_lab trace grouping factors, default available options are names of
#' a table returning by trace_info.
#' @param xlim,ylim numeric vectors defining limits for x and y axis respectively
#' @param gr_col name of a veritable to be used to colorize groups
#' @param breaks an integer controlling major breaks on the x-axis
#' @param minor_breaks an integer controlling minor breaks on the x-axis
#' @param expd expands plot beyond limits set on x-axis
#' @export
#' @importFrom rlang .data
#' @import glue
plt_gg <- function(x
                   , what = NULL
                   , stacked = TRUE
                   , force_raw = FALSE
                   , x_lab = "Retention Time [min]"
                   , facet_lab = "ID"
                   , xlim = NULL
                   , ylim = NULL
                   , gr_col = "ID"
                   , breaks = 5
                   , minor_breaks = 1
                   , expd = 2){
  
  
  if(methods::is(x) != "tracer"){stop("The argument `x` must be a type of tracer")}
  
  data_ <- "PROCESSED"
  
  if(isFALSE(force_raw)){
    if(is.null(x$PROCESSED)){ data_ <- "RAW"}
  }else{ data_ <- "RAW" }
  
  # VALIDATOR
  what <- what_validator(x, what = what)
  
  # Create a table to plot the WHAT is MISSING
  df <- x[[data_]][what] |> #[what]
    do.call("rbind", args=_)
  
  df$ID <- row.names(df)|> gsub(pattern = "\\..*", replacement ="")
  row.names(df) <- NULL
  
  df <- suppressMessages(trace_info(x, force_raw = force_raw)) |>
    merge(x = df, y=_, by = "ID")
  
  if(is.null(xlim)){
    
    tr_lims <- expand_meta_data(lst = x[[data_]][what])|> # [what]
      dplyr::select(.data$minSig, .data$maxSig, .data$minRT, .data$maxRT)
    
    # SET RT limits
    xlim = c(min(tr_lims$minRT), max(tr_lims$maxRT))
  }
  
  if(is.null(ylim)){
    
    if(!exists("tr_lims")){
      tr_lims <- expand_meta_data(lst = x[[data_]][what])|> # [what]
        dplyr::select(.data$minSig, .data$maxSig)
    }
    
    ylim = c(min(tr_lims$minSig), max(tr_lims$maxSig))
  }
  
  # Construct plot
  plt <- 
    ggplot2::ggplot(data = df) +
    ggplot2::geom_line(ggplot2::aes(x = .data$RT, y = .data$Response, colour = .data[[gr_col]])) +
    #ggplot2::ylim(ylim) + 
    ggplot2::guides(x = ggplot2::guide_axis(minor.ticks = TRUE
                                            , cap = "both")) +
    ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = ylim,
      expand = TRUE,
      default = FALSE,
      clip = "on",
      reverse = "none",
      ratio = NULL
    ) +
    ggplot2::scale_x_continuous(limits = xlim
                                , expand = ggplot2::expansion(add = expd)
                                , breaks = seq(xlim[1], xlim[2], breaks)
                                , name = x_lab
                                , minor_breaks = scales::breaks_width(minor_breaks)
    ) +
    ggplot2::theme_bw() +
    
    ggplot2::theme(legend.position = "right" 
                   , panel.border = ggplot2::element_blank()
                   , panel.grid = ggplot2::element_blank()
                   , axis.text.y = ggplot2::element_blank()
                   , axis.title.y = ggplot2::element_blank()
                   , axis.ticks.y = ggplot2::element_blank()
                   , strip.background = ggplot2::element_rect(fill = "grey88", linetype = 0)
                   , axis.line.x = ggplot2::element_line()
                   , axis.ticks.length = ggplot2::unit(5, "pt")
                   , axis.minor.ticks.length = ggplot2::rel(0.5))
  
  # Plot config
  if(stacked){
    
    fmla <- stats::as.formula(paste(".", facet_lab, sep = " ~ "))
    
    plt <- plt + ggplot2::facet_wrap(fmla
                                     , ncol = 1
                                     , scales = "free_y"
                                     , strip.position = "left") +
      ggplot2::theme(legend.position = "none")
  }
  
  suppressWarnings(print(plt), classes = "warning") # or use coord_cartesian
  
}

#' Cosine based similarity and distance metrics
#' @description computing pair-wise distances and similarity 
#' @param x object of class tracer
#' @param lab name of a meta data column whose values would be used as labels for the output distance 
#' or similarity matrix.
#' @param use_diff logical, whether use peak difference or absolute intensity to get a vector of weights.
#' @param pw a numeric, a weighing coefficient for signal intensities default is 0.
#' @param align logical. If TRUE a pairwise alignment will be applied.
#' @param neg logical, if TRUE allows negative peaks, default is FALSE.
#' @param force_raw if TRUE, RAW data will be compared regardless of the previous processing steps taken.
#' @param gamma a numeric that defines scale spanning for weights, default 1.
#' @param lamb a positive number, to get similarity score based Euclidean distance, on see details.
#' @param simple  if TRUE computes unweighted Euclidean distance;
#' @param metric a string indicating which similarity or distance metric to compute, see details.
#' @param fun a string defining a function that will be used in getting re-weighting vector.
#' @details
#' Available metrics:
#' \itemize{
#' \item cosim - cosine similarity: \eqn{S_c(a,b)=\frac{\sum{a_ib_iw_i^2}}{\sqrt{\sum{(a_iw_i)^2}}\sqrt{\sum{(b_iw_i)^2}}} };
#' \item cosdist - cosine distance: \eqn{D_c=1-S_c(a,b)};\cr
#' \item euclidean - Euclidean distance:
#' \eqn{ D_E=\sqrt{2\cdot{D_c}} };\cr
#' If simple = TRUE, provides unweighted distance between vectors:
#' \eqn{D_E(a,b)=\sqrt{\sum_{i=1}^{n}{(a_i-b_i)^2}}};
#' \item angulardist - angular distance, takes on values from 0 to 1:
#' \eqn{ D_{\theta }={\frac {2\cdot\arccos(S_c(a,b))}{\pi }}={\frac {2\cdot\theta }{\pi }} };
#' \item angularsim - angular similarity, takes on values from 0 to 1:
#' \eqn{ S_{\theta }= 1-D_{\theta }};
#' \item simex - similarity score which is an exponential function of Euclidean distance \eqn{S_E=e^{-\gamma\cdot\|x\|^2}}.
#' }
#' RE-WEIGHING VECTOR\cr\cr
#' If we need to emphasize which variations in traces are more important then others,\cr
#' here comes the re-weighing vector. The weights take on values in the range \eqn{w_i \in [1;\infty)}. \cr
#' Observe, if all weights are equal, there will be no weighting. Thus, in order to disable\cr
#' weighting set `pw` argument to 0, it will convert all weights to 1 or to \emph{e} depending on mode selected.\cr\cr
#' 
#' There are two modes to compute the re-weighing vector:\cr\cr
#' \enumerate{
#' \item When the weights are proportional to the absolute difference between corresponding points,\cr
#' i.e., use_dif = TRUE, then the expression takes the form:\cr
#' \deqn{ w_i=e^{\gamma\cdot|a_i-b_i|^{pw}} }
#' where \eqn{\gamma} - gamma argument, and `pw` determines how much intensity variation\cr 
#' influence the corresponding weight.\cr
#' \item Or the weights are proportional to the value of 
#' \deqn{fun(a_i,b_i)^{pw}}
#' where `fun` is one of  the base functions: "max", "mean" or "min".
#' }
#' @export
tr_compar <- function(x
                  , lab = NULL
                  , use_diff = FALSE
                  , pw = 0
                  , align = FALSE
                  , neg = FALSE
                  , force_raw = FALSE
                  , gamma = 1
                  , lamb = 1
                  , simple = FALSE
                  , metric = c("cosim", "cosdist", "angularsim", "angulardist", "euclidean","simex")
                  , fun = c("max", "mean", "min")){
  
  if(methods::is(x) != "tracer"){ 
    stop("\n Argument x must be an object of type tracer", call. = FALSE)}
  
  # Validate number of data points
  x <- data_point_validator(x)
  
  data_ <- "PROCESSED"
  
  # Select data for the comparison
  if(isFALSE(force_raw)){
    if(is.null(x$PROCESSED)){ data_ <- "RAW"}
  }else{ data_ <- "RAW" }
  
  
  fun <- match.arg(fun)
  metric <- match.arg(metric)
  
  # Select a metric to compute
  metric <- switch(metric,
                   cosim = bquote(tr_cosine_sim(a, b, w)),
                   cosdist = bquote(tr_cosine_dist(a, b, w)),
                   angularsim = bquote(tr_angular_sim(a, b, w)),
                   angulardist = bquote(tr_angular_dist(a, b, w, neg)),
                   euclidian = bquote(tr_cos2euc(a, b, w, simple)),
                   simex = bquote(tr_euc2sim(a, b, w, lamb, simple))
                   )
  
  # Define labels for the similarity/distance matrix
  if(is.null(lab) ){ 
    item <- names(x[[data_]])
    lab <- item }
  else if(lab == "ID"){
    item <- names(x[[data_]])
    lab <- item }
  else{
    dt <- trace_info(x)|> dplyr::select(.data$ID, .data[[lab]])
    item <- dt$ID
    lab <- dt[[lab]]
    rm(dt)
    }
  
  if(any(duplicated(item))){
    stop("Argument lab has non-unique items", call. = FALSE)
    }
  
  if(dif_base <= 0){
    stop("Argument dif_base must be greater than 0", call. = FALSE)
    }
  
  if(pw < 0){
    stop("Argument pw must not be less than 0", call. = FALSE)
  }
  
  # init containers for output and weights, i.e. importance
  out <- NULL
  wgt <- NULL
  
  # iterate over samples
  for(i in seq_along(item)){
    
    a <- x[[ data_ ]][[ item[i] ]][["Response"]]
    
    for(j in seq_along(item)){
      
      b <- x[[ data_ ]][[ item[j] ]][["Response"]]
      
      # Pair-wise alignment
      if(align){ 
        b <- ptw::ptw(ref = a, samp = b) 
        b$warped.sample[is.na(b$warped.sample)] <- min(b$warped.sample, na.rm = T)
        b <- c(b$warped.sample)
        }

      # Select a re-weighting algorithm
      if(use_diff){
        
        w <- exp( gamma*abs(a - b)^pw ) # FIX ISOLATE as a helper!!!
        
        }else{
          w <- cbind(a, b) |> apply(2, function(x) x - min(x)) |> 
          apply(1, get(fun))
          w <- w**pw
      }
      
      out <- c(out, round(eval(metric), digits = 6))

      if((j-i) > 0){
        
        wgt <- data.frame(W = w/max(w) #tr_score_vec(a,b, win = 11, f = 3, d = 1) 
                          , Pair = paste(lab[i],"vs", lab[j], sep = "_")
                          , x[[data_]][[ item[i] ]]
                          ) |>
          rbind(wgt)
      }
      rm(b)
    }
  }
  # OUTPUT
  list(SIM = matrix(data = out
                    , nrow = length(item)
                    , dimnames = list(lab, lab))
       , WM = wgt)
}


#' Baseline correction
#' @description flattens baseline 
#' @param x an object of class tracer
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data
#' @export
tr_baseline <- function(x, new_obj = TRUE){
  
  if(methods::is(x) != "tracer"){ stop("\n x must be an object of type tracer")}
  
  if(is.null(x$PROCESSED)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}
  
  out <- lapply(x[[data_]], function(dt){

    dt$Response <- prospectr::baseline(X = matrix(dt[["Response"]], nrow = 1)
                        , wav = dt[["RT"]])
    dt

  })

  if(new_obj){
    
    x$PROCESSED <- out
    x <- history_upd(x = x, event = "baselined")
    x <- workflow_upd(x = x, m_call = match.call())
    return(x)
    
    
  }else{ return(out) }
  
}

#' Creates a workflow object
#' @description with workflows you can automate routines  applying the same processing chain 
#' of operations to a different objects
#' @param flow an expression representing a chain of sequential processing steps 
#' @export

tr_workflow <- function(flow){
  
  flow <- rlang::enexpr(flow)
  print(flow)
  
  foo <- function(obj){
    
    eval(expr = flow, envir = rlang::env(obj = obj))
    
  }
  return(foo)
}

#' Set object to default state
#' @description erases Processed data, History and Workflow
#' @param x an object of class tracer 
#' @export

tr_default <- function(x){
  
  if(methods::is(x) != "tracer"){ stop("\n x must be an object of type tracer")}
  
  x["PROCESSED"] <- list(NULL)
  x$HISTORY <- x$HISTORY[1,]
  x["Workflow"] <- list(NULL)
  
  return(x)
}


#' Reprocessing data
#' @description allows to reprocess Raw data using an existing object as a reference or on its own workflow 
#' @param x an object of class tracer to reprocess
#' @param ref an object of class tracer whose workflow will be used to reprocess data. 
#' If omitted, a workflow of the object x is going to be used.
#' @param level an integer or a vector of integers to specify on which steps 
#' to perform reprocessing. If a single value is supplied the reprocessing 
#' will be done for whole chain from the very beginning up to this step included.
#' @param skip logical. If TRUE every step will be treated as a separate processing step.
#' @export
tr_reprocess <- function(x, ref = NULL, level = NULL, skip = FALSE){
  
  # Check object
  if(methods::is(x) != "tracer"){ stop("\n x must be an object of class tracer")}
  
  # Check reference object
  if(!is.null(ref)){
    
    if(methods::is(ref) != "tracer"){ stop("\n x must be an object of class tracer")}
    if(is.null(ref$Workflow)){stop("\nObject ref contains no workflow records")}
    
    wflw <- ref$Workflow
    
  }else{ 
    if(is.null(x$Workflow)){stop("\nObject x contains no workflow records")}
    wflw <- x$Workflow }
  
  # Check levels
  if(is.null(level)){level <- 1:length(wflw)}
  else if(length(level) > 1){
    
    if(any(level > length(wflw)) || any(level < 1 )){
    stop("\nLevels must be in consistance with number of workflow steps")}}
  
  else{ level <- 1:level }
  
  obj <- tr_default(x)
  
  # Iterate over steps
  for(stp in level){
    print(stp)
    obj <- eval(expr = wflw[[stp]], envir = rlang::env(obj = obj))
    
  }
  return(obj)
}



