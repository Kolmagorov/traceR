# Class Tracer print method
#' @export
print.tracer <- function(x,...){

  n_trace_raw <- length(x$DATA$RAW)
  n_trace_proc <- length(x$DATA$PROCESSED)

  sys <- names(x$META)

  raw_qty <- table(x$LOG$SOURCE)|> data.frame()
  names(raw_qty) <- c("SOURCE", "QTY")


  cat("Object of type tracer\n",
      "\nTotal number of traces: ", n_trace_raw, "\n",
      "Number of traces processed: ", n_trace_proc, "\n",
      "\nTrace Distribution by Group:\n", fill = T)

  print(raw_qty, row.names = F, right = F)

  cat("\nLook up into the LOG table to see the load status of files")

}

#' Gets trace info
#' @description Provides a compact table with descriptive information
#' about traces
#' @param x an object of class tracer
#' @param force_raw logical if TRUE returns info for 
#' the Unprocessed data
#' @param ... an argument to pass to 'expand_meta_data' function
#' @returns a list with descriptive parameters of traces
#' @export
trace_info <- function(x, force_raw = FALSE, ...){

  if(methods::is(x) != "tracer"){ 
    stop("\n x must be a an object of type tracer", call. = FALSE)}

  d_type <- "PROCESSED"

  if(isFALSE(force_raw)){
    if(is.null(x$DATA$PROCESSED)){ d_type <- "RAW"}
  }else{ d_type <- "RAW" }
  
  hdr <- lapply(x$META, names) |> purrr::reduce(intersect)

  message("Returning info for ", d_type, " data")

  common <- lapply(x$META, function(dt) dt[hdr] )|>
    do.call("rbind", args =_)
  
  common <- expand_meta_data(lst = x$DATA[[d_type]],...)|>
    merge(y = common, by = "ID")
  
  return(common)
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

  if(methods::is(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(!grepl(type, pattern = "minmax|maxnorm")){
    stop("Scaling type must be either minmax or maxnorm")}

  if(is.null(x$HISTORY)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}

  out <- switch (type,
                 minmax = lapply(x$DATA[[data_]], function(x) minmax_scale(x, ...)),
                 maxnorm = lapply(x$DATA[[data_]], function(x){ maxnorm_scale(x, ...) })
                 )

  if(new_obj){

    x$DATA$PROCESSED <- out
    x <- history_upd(x = x, event = "re-scaled")
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

  out <- lapply(x$DATA[[data_]], function(dt){

    if(crop_to[1] <= min(dt$RT)){ crop_to[1] <- min(dt$RT) }
    if(crop_to[2] >= max(dt$RT)){ crop_to[2] <- max(dt$RT) }

    dplyr::filter(.data = dt, .data$RT >= crop_to[1] & .data$RT <= crop_to[2])
  })

  if(new_obj){

    x$DATA$PROCESSED <- out
    x <- history_upd(x = x, event = "cropped")
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

  if(methods::is(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(is.null(x$DATA$PROCESSED)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}
  
  blw_pts <- suppressMessages(trace_info(x = x), classes = "message")|>

    dplyr::select(.data$FILE, .data$dataPoints, .data$SOURCE)|>
    dplyr::filter(.data$dataPoints < pts)

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

  out <- lapply(x$DATA[[data_]], function(dt){

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
    
    x$DATA$PROCESSED <- out
    x <- history_upd(x = x, event = "re-sampled")
    return(x) }
  
  else{ return(out) }

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
#' @export
tr_align <- function(x
                     , new_obj = TRUE
                     , ref = 1L
                     , rm_na = TRUE
                     , return_mat = FALSE
                     , ...){

  if(methods::is(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(is.null(x$PROCESSED)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}

  blw_pts <- suppressMessages(trace_info(x = x), classes = "message")|>
    dplyr::select(.data$FILE, .data$dataPoints, .data$SOURCE)

  incomp <- unique(blw_pts$dataPoints)

  if(ref > nrow(blw_pts) | ref <= 0){
    stop("Argument ref must be within the range 1 -", nrow(blw_pts))
  }

  if(length(incomp) != 1){

    cat("\f")
    msg <- paste(length(incomp),  "out of", sum(x$LOG$LOADED == TRUE)
                 , "samples have different data points")

    message(msg)

    ans <- readline(prompt = "Would you like resampling gets done (y/n)? ")

    if(!grepl(ans, pattern = "[Y,y]")){

      print(blw_pts)

      stop("alignment has been stopped by user", call. = F)
    }
    pts <- readline(prompt = "Enter a disired number of resampling points (pts): ")

    pts <- stringr::str_extract_all(pts,  pattern = "\\d") |>
      unlist()|>
      paste0(collapse = "")|>
      as.numeric()

    if(is.na(pts)){
      stop("Number of resampling points must be a number not "
                              , pts
                              , call. = FALSE)}

    x <- tr_resample(x = x, pts = pts, new_obj = TRUE)
    data_ <- "PROCESSED"
  }

  ref <- x$DATA[[data_]][[ref]][["Response"]]

  # Aligning and updating PROCESSED DATA
  x$DATA[[data_]] <- lapply(x$DATA[[data_]], function(dt){

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
    return(x) }

  else if(return_mat){

    mat <- x$DATA$PROCESSED |>

      lapply(function(r){
        r$Response
    }) |>
      do.call("rbind", args=_)

    return(list(Response = mat, RT = x$DATA$PROCESSED[[1]][["RT"]]))
  }
  else{ return(x$DATA$PROCESSED) }
}


# Base Line correction

#' Plot traces
#' @description this function is - under development -
#' currently 'ggplot2' is only supported graphic system. 'base' and 'plotly' are coming next.
#' @param x an object of class tracer
#' @param ... additional arguments to pass to plt_gg, see ?plt_gg
#' @export
#' @importFrom rlang .data
plot.tracer <- function(x, ...){
  
  plt_gg(x = x, ...)

}


#' Operator `+` overloading
#' @param a,b objects of class tracer
#' @details a wrapper for `merge_trace`, where keep_history and re_active are TRUE.
#' @export
`+.tracer` <- function(a, b){
  merge_trace(a = a, b = b, keep_history = T)
}


# PERSPECTIVE -- `trace_info` change to `get_meta` to show output 
# either by group or all, or selected items

# add_trace and object trace constructor


#' Plot traces with ggplot2
#' @description plots traces using ggplot2 graphics
#' @param lst a list of data to plot, assumed to originate from a trace object
#' @param x_lab a string passing x-axis name to the plot
#' @param force_raw If TRUE will plot RAW data
#' @param facet_lab trace grouping factors, default available options are names of
#' a table returning by trace_info.
#' @param xlim,ylim numeric vectors defining limits for x and y axis respectively
#' @param breaks an integer controlling major breaks on the x-axis
#' @param minor_breaks an integer controlling minor breaks on the x-axis
#' @param expd expands plot beyond limits set on x-axis
#' @param what either numeric indexes or string of UID. If some Numeric indexes are 
#' out of range, an error will be thrown. Unmatched UID's will be ignored with warning.
#' @export
#' @importFrom rlang .data
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
    if(is.null(x$DATA$PROCESSED)){ data_ <- "RAW"}
  }else{ data_ <- "RAW" }
  
  
  # VALIDATOR
  what <- what_validator(lst = x$DATA[[data_]], what = what)
  
  # Create a table to plot the WHAT is MISSING
  df <- x$DATA[[data_]][what] |> #[what]
    do.call("rbind", args=_)
  
  df$ID <- row.names(df)|> gsub(pattern = "\\..*", replacement ="")
  row.names(df) <- NULL
  
  df <- trace_info(x, force_raw = force_raw) |>
    merge(x = df, y=_, by = "ID")
  
  if(is.null(xlim)){
    
    tr_lims <- expand_meta_data(lst = x$DATA[[data_]][what])|> # [what]
      dplyr::select(minSig, maxSig, minRT, maxRT)
    
    # SET RT limits
    xlim = c(min(tr_lims$minRT), max(tr_lims$maxRT))
  }
  
  if(is.null(ylim)){
    
    if(!exists("tr_lims")){
      tr_lims <- expand_meta_data(lst = x$DATA[[data_]][what])|> # [what]
        dplyr::select(minSig, maxSig)
    }
    
    ylim = c(min(tr_lims$minSig), max(tr_lims$maxSig))
  }
  
  # Construct plot
  plt <- 
    ggplot2::ggplot(data = df) +
    ggplot2::geom_line(ggplot2::aes(x = .data$RT, y = .data$Response, colour = .data[[gr_col]])) +
    ggplot2::ylim(ylim) + 
    ggplot2::guides(x = ggplot2::guide_axis(minor.ticks = TRUE
                                            , cap = "both")) +
    ggplot2::scale_x_continuous(limits = xlim
                                , expand = ggplot2::expansion(add = expd)
                                , breaks = seq(xlim[1], xlim[2], breaks)
                                , name = x_lab
                                , minor_breaks = scales::breaks_width(minor_breaks)
    ) +
    ggplot2::theme_bw() +
    
    ggplot2::theme(legend.position = "none"
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
                                     , strip.position = "left")
  }
  
  suppressWarnings(print(plt), classes = "warning") # or use coord_cartesian
  
}


