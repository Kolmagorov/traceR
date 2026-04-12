# Class Tracer print method
#' @export
print.tracer <- function(x,...){

  n_trace_raw <- length(x$DATA$RAW)
  n_trace_proc <- length(x$DATA$PROCESSED)

  sys <- names(x$META)

  raw_qty <- table(df$LOG$SOURCE)|> data.frame()
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
#' @param x object of class tracer
#' @param force_raw logical if TRUE returns info for 
#' the Unprocessed data
#' @param ... an argument to pass to 'expand_meta_data' function
#' @returns a list with descriptive parameters of traces
#' @export
trace_info <- function(x, force_raw = FALSE, ...){

  if(class(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  d_type <- "PROCESSED"

  if(isFALSE(force_raw)){
    if(is.null(x$DATA$PROCESSED)){ d_type <- "RAW"}
  }else{ d_type <- "RAW" }

  hdr <- lapply(df$META, names) |> purrr::reduce(intersect)

  message("Returning info for ", d_type, " data")

  common <- lapply(x$META, function(dt) dt[hdr] )|>
    do.call("rbind", args =_)

  common <- expand_meta_data(lst = x$DATA[[d_type]],...)|>
    merge(y = common, by = "ID")

  return(common)
}

#' Re-scale Helper
#' @description re-scales response variable
#' @keywords internal
minmax_scale <- function(x, bound = c(0, 1), rt_range = NULL){

  stopifnot("Input must be a data.frame with at least 2 columns" = is.data.frame(x) )

  if(!is.null(rt_range)){

    rt_filter <- x$RT >= rt_range[1] & x$RT <= rt_range[2]
    lims <- range(x[rt_filter, "Response"])

  }else{ lims <- range(x$Response) }

  x$Response <- bound[1] + (x$Response - lims[1])*(bound[2]-bound[1])/(diff(lims))
  return(x)
}

#' @keywords internal
maxnorm_scale <- function(x, rt_range = NULL){

  stopifnot("Input must be a data.frame with at least 2 columns" = is.data.frame(x) )

  if(!is.null(rt_range)){

    rt_filter <- x$RT >= rt_range[1] & x$RT <= rt_range[2]
    max_val <- max(x[rt_filter, "Response"])

  }else{ max_val <- max(x$Response) }

  x$Response <- x$Response/max_val
  return(x)
}

#' Re-scale Response of a trace
#' @description Transforms Response to a new designated scale.
#' @param x object of class tracer
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data
#' @param type a string defining re-scaling algorithm. Can be 
#' either minmax with arbitrary limits, or maxnorm that normalizes data to the 
#' greatest value.
#' @param ... an additional argument to specify range for minmax rescale.
#' @export
tr_rescale <- function(x, type = "minmax", new_obj = FALSE, ...){

  if(class(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(!grepl(type, pattern = "minmax|maxnorm")){
    stop("Scaling type must be either minmax or maxnorm")}

  if(is.null(x$HISTORY)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}

  out <- switch (type,
                 minmax = lapply(x$DATA[[data_]], function(x) minmax_scale(x, ...)),
                 maxnorm = lapply(x$DATA[[data_]], function(x){ maxnorm_scale(x, ...) })
                 )

  if(new_obj){

    his_proc <- data.frame(type = type
                           , proc_time = format(Sys.time(), "%d-%b-%Y %H:%M:%OS3"))

    x$DATA$PROCESSED <- out
    x$HISTORY <- rbind(x$HISTORY, his_proc)
    return(x)

  }else{ return(out) }

}

#' Cropping a trace
#' @description Trims the profile to the set RT range 
#' @param x object of class tracer
#' @param crop_to sets the limits defining a cropping segment, 
#' if it is a single positive number,  will be treated as an upper limit 
#' starting from zero.
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data
#' @export
tr_crop <- function(x, crop_to, new_obj = FALSE){

  if(class(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(!is.numeric(crop_to)){stop("The cropping range must be positive numeric\n")}

  if(length(crop_to) == 1 & all(crop_to > 0) ){ crop_to <- c(0, crop_to) }

  if(length(crop_to) == 2 & crop_to[1] > crop_to[2]){
    stop("The lower bound of the crooping segment is greater than th Upper one\n")
  }

  # FIX the X is an object of trace not a data frame!!!!

  # Maybe it is wise checking RT against the meta data


  if(is.null(x$HISTORY)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}

  out <- lapply(x$DATA[[data_]], function(dt){

    if(crop_to[1] <= min(dt$RT)){ crop_to[1] <- min(dt$RT) }
    if(crop_to[2] >= max(dt$RT)){ crop_to[2] <- max(dt$RT) }

    dplyr::filter(.data = dt, RT >= crop_to[1] & RT <= crop_to[2])
  })

  if(new_obj){

    his_proc <- data.frame(type = "cropping"
                           , proc_time = format(Sys.time(), "%d-%b-%Y %H:%M:%OS3"))

    x$DATA$PROCESSED <- out
    x$HISTORY <- rbind(x$HISTORY, his_proc)
    return(x)

  }else{ return(out) }

}

#' Resamples traces
#' @description Re-sample data to new retention times 
#'  using spline or linear interpolation, using 'prospectr::resample()'
#' @param x object of class tracer
#' @param pts an integer setting the number of points to re-sample
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data
#' @export
tr_resample <- function(x, pts, new_obj = FALSE){

  if(class(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(is.null(x$HISTORY)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}

  blw_pts <- suppressMessages(trace_info(x = x), classes = "message")|>

    dplyr::select(file, dataPoints, SRC)|>
    dplyr::filter(dataPoints < pts)

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
                          , new.wav = new_RT)
    )


  })


  if(new_obj){

    his_proc <- data.frame(type = "resampling"
                           , proc_time = format(Sys.time(), "%d-%b-%Y %H:%M:%OS3"))

    x$DATA$PROCESSED <- out
    x$HISTORY <- rbind(x$HISTORY, his_proc)
    return(x)

  }else{ return(out) }


}

#' a Trace Aligner
#' @description Aligns traces to a reference provided
#' @param x object of class tracer
#' @param ref an index of a trace that will be used as a reference
#' @param rm_na logical, if TRUE will replace all NA's that may be introduced after
#' ptw alignment algorithm, with the lowest value in the traces.
#' @param new_obj logical, if TRUE returns a modified object, 
#' otherwise - processed data. Also overides 'return_mat'.
#' @param return_mat if TRUE, instead of a list returns a matrix of aligned data.
#' @export 
tr_align <- function(x
                     , new_obj = FALSE
                     , ref = 1L
                     , rm_na = TRUE
                     , return_mat = FALSE
                     , ...){

  if(class(x) != "tracer"){ stop("\n x must be a an object of type tracer")}

  if(is.null(x$HISTORY)){data_ <- "RAW"}
  else{data_ <- "PROCESSED"}

  blw_pts <- suppressMessages(trace_info(x = x), classes = "message")|>
    dplyr::select(file, dataPoints, SRC)

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

    his_proc <- data.frame(type = "alignment"
                           , proc_time = format(Sys.time(), "%d-%b-%Y %H:%M:%OS3"))
    x$HISTORY <- rbind(x$HISTORY, his_proc)
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

# Base Line correction
# plot.tracer <- function(x,...){}









