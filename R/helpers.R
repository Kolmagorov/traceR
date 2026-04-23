
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
#' @keywords internal
record_log <- function(what, on_disc = FALSE, f_names = NULL){
  
  if(!is.data.frame(what)){ what <- data.frame(what)}
  if(!is.null(f_names)){ names(what) <- f_names}
  
  
  function(tmp){
    
    if(!is.data.frame(tmp)){ tmp <- data.frame(tmp)}
    names(tmp) <- names(what)
    
    what <<- rbind(what, tmp)
    return(what[-1,])
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

#' Construct Default field for meta data
#' @keywords internal
 meta_default <- function(x = NULL){
   
   out <- data.frame(ID = NA,
              SampleName = NA,
              dateAcquired = NA,
              SRC = "Undefined",
              file = NA,
              Comments = NA)
   
   if(is.null(x)){return(out)}
   if(!is.list(x)){stop("In helper: meta_default() x must be a list")}
   
   fld <- names(x)
   
   for(i in fld){ 
     
     if(i %in% fld){out[[i]] <- x[[i]]}
     else{
         out <- out|>
           dplyr::mutate("{i}" := x[[i]])
       }
   }
   return(out)
 }






