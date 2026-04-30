
#' Index validator
#' @keywords internal
what_validator <- function(lst, what){
  
  n_smp <- length(lst)
  
 if(any(duplicated(what))){
    stop("Supplied indexes must be unique", call. = FALSE)
  }
  
  if(is.null(what)){ return( names(lst) )}
  else if(is.numeric(what)){
    
    if(max(what) > n_smp | any(what <= 0)){
      stop("Supplied indexes are out of range: \nmax(ID) is ", max(what)
           , " but number of samples is ", n_smp
           , call. = FALSE)}
    else{ return(names(lst)[what]) }
    
    
  }else if(is.character(what)){ 
    
    gacha <- what %in% names(lst)
    
    if(!any(gacha)){
      stop("The supplied indexes are missing in the data", call. = FALSE)}
    else if(all(gacha)){ return(what) }
    else{
      
      warning("The following item are missing:\n"
              , paste0(what[!gacha], collapse = ", ")
              , call. = FALSE)
      what <- what[gacha]
    }
  }
  
  return(what)
}

#' Update history records of trace objects
#' @keywords internal
history_upd <- function(x, event){
  
  if(!is.character(event)){stop("History event must be a type of character", call. = FALSE)}
  
  his_proc <- data.frame(type = event
                         , proc_time = format(Sys.time(), "%d-%b-%Y %H:%M:%OS3"))
  
  x$HISTORY <- rbind(x$HISTORY, his_proc)
  return(x)
}

#' a Re-scaling Helper
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

#' Maximum normalization
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


#' a Parser helper
#' @description
#' Tries different delimiters and determines a file Template to pass to
#' the parser selector
#' @keywords internal
file_scan <- function(fls, delim = NULL){
  
  REF <- data.frame(FIELD = c("SampleName", "Date Acquired", "URL")
                    , SYS = c("EMPOWER", "EMPOWER", "CHROMELEON"))
  
  
  if(is.null(delim)){ delim <- c(",", ";", "\t")}
  
  sep <- ""
  sys <- NULL
  skip <- 2
  
  
  for(item in delim){
    
    tmp <- utils::read.table(file = fls, header = F, sep = item, nrows = 1) |>
      unlist()
    
    if (any(REF$FIELD %in% tmp)){
      
      sep <- item
      sys <- REF[REF$FIELD %in% tmp, "SYS"] |>
        unique()
      break
      
    }else if(is.numeric(tmp)){
      
      sep <- item
      sys <- "Undefined"
      skip <- 0
      break
    }
  }
  
  if(is.null(sys)){
    warning("Failed in parsing file: ", fls, "\n", call. = FALSE)
    return(FALSE)}
  else{return( list(SEP = sep, SYS = sys, SKIP = skip))  }
  
}

#' Date-time converter
#' @description
#' Automatically selects a pattern to convert text to date-time
#' @keywords internal
time_scan <- function(dt, tz = Sys.timezone()){
  
  DT_FORM <- c("dmy HMS Op z",
               "mdy HMS Op z",
               "ymd HMS Op z",
               "mdy HMS Op",
               "dmy HMS z",
               "mdy HMS z",
               "dmy HMS")
  dt <- gsub(x = dt, pattern = "GMT-3|\\+03", replacement = "UTC+03")
  out <- lubridate::parse_date_time(x = dt
                                    , orders = DT_FORM
                                    , tz = tz
                                    , quiet = T)
  out
}

#' Computes some descriptive parameters of a trace
#' @keywords internal
#' @importFrom rlang .data
expand_meta_data <- function(lst, fac = 1e5){
  
  if(!is.list(lst)){ stop("Expand_meta_data accepts list as an input\n")}
  
  exp_meta <- lapply(lst, function(dt){
    
    energy <- fac*sum((dt[["Response"]])**2)**0.5

    data.frame(
      minSig = min(dt[["Response"]]),
      maxSig = max(dt[["Response"]]),
      dataPoints = nrow(dt),
      apexRT = dplyr::filter(.data$Response == max(.data$Response), .data = dt)|>
        dplyr::pull(.data$RT)|> mean(),
      minRT = min(dt[["RT"]]),
      maxRT = max(dt[["RT"]])
    )|>
      dplyr::mutate(samplingRate = round(.data$dataPoints/(.data$maxRT - .data$minRT)/60, digits = 1),
                    score = round(energy/.data$dataPoints, digits = 1))
    
  })|>
    do.call("rbind", args=_)|>
    dplyr::mutate(ID = names(lst))
  
  row.names(exp_meta) <- NULL
  return(exp_meta)
}

#' Keeps track of processing steps taken and records them into a data.frame
#' @param what either NULL or a list with named fields and values
#' @keywords internal
init_log <- function(what = NULL, tmpl = c("LOG", "META")){
  
  tmpl <- match.arg(tmpl)
  
  # Initialize table with an internal template
  what <- switch(tmpl,
                META = tab_tmplate$META_tmpl,
                LOG = tab_tmplate$LOG_tmpl)
  
  function(lst = NULL){

    if(is.null(lst)){ dt_row <- what[1,] }
    
    else if(is.list(lst)){
      
      
      nms <- names(lst)
      
      if(is.null(nms)){stop("In helper: init_log() argument `lst` mast have named items")}
      
      if(any(duplicated(nms))){stop("In helper: init_log() argument `lst` must have unique item names")}
      
      up_fld <- setdiff(nms, names(what))
      
      # Adding columns
      for(itm in up_fld){ what <<- what|> dplyr::mutate("{itm}" := NA)}
      
      dt_row <- what[1,]
      
      # Writing records
      for(itm in nms){
        dt_row <- dt_row |> 
          dplyr::mutate("{itm}" := lst[[itm]])
      }
      }else{ stop("In helper: init_log() argument `lst` isn't a type of list") }
    
    what <<- what |> dplyr::select(names(dt_row)) |> 
      rbind(dt_row)
    
    out <- what[-1,]
    row.names(out) <- NULL
    return(out)
  }
}


#' Rehashing indexes - a helper for merging objects
#' @keywords internal
rehash_id <- function(a, b){
  
  new_a <- gen_uid_pool(n = nrow(a$LOG), len = 6)
  new_b <- gen_uid_pool(n = nrow(b$LOG), len = 6, pool = new_a)

  out <- purrr::map2(list(a = a, b = b), list(new_a, new_b), function(obj, x_name){
    
    names(obj$META) <- x_name
    names(obj$DATA$RAW) <- x_name
    
    if(!is.null(obj$DATA$PROCESSED)){ names(obj$DATA$PROCESSED) <- x_name } 
    
    obj$LOG <- obj$LOG |> 
      dplyr::rename(ID_old = .data$ID) |> 
      dplyr::mutate(ID = x_name)
    
    obj$META <- purrr::map2(obj$META, names(obj$META), function(dt, idx){
      
      dt$ID <- idx
      dt
    })
    
    obj
    
  })
  
  return(out)
}

#' Computes cosine similarity between two vectors
#' @keywords internal
tr_cosine_sim <- function(a, b, w = 1){
  
  sum(a*b*w)/sqrt( sum(w*a*a)*sum(w*b*b) )
  
}

#' Computes cosine distance between two vectors
#' @keywords internal
tr_cosine_dist <- function(a, b, w = 1){
  
  1 - tr_cosine_sim(a, b, w)
}

#' Computes angular distance between two vectors
#' @keywords internal
tr_angular_dist <- function(a, b, w = 1, neg = FALSE){
  
  foc <- 2
  if(neg){foc <- 1}

  ad <- tr_cosine_sim(a, b, w) |> acos()
  
  return(foc*ad/pi)
}

#' Computes angular similarity between two vectors
#'@keywords internal
tr_angular_sim <- function(a, b, w = 1){
  
  1 - tr_angular_dist(a, b, w)
}


#' Prompt for re-sampling
#' depends on `trace_info()`, `tr_resample()`
#' @keywords internal
data_point_validator <- function(x){
  
  # get meta data info about data points
  blw_pts <- suppressMessages(trace_info(x = x), classes = "message")|>
    dplyr::select(.data$FILE, .data$dataPoints, .data$SOURCE)
  
  # checks if all samples have the same number of data points
  incomp <- unique(blw_pts$dataPoints)
  
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
  return(x)
}












