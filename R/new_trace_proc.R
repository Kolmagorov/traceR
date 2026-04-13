#' LOREM IPSUM
#' @description reading in trace files exported from Empower or Chromeleon software.
#' @param path_dir a string specifying path to a directory with trace files to load
#' @param fls a vector of strings as an alternative to provide file path, overrides
#' 'path_dir'
#' @param pattern allows to select files by custom regex pattern. NOTE it overrides
#' selection by file extension pattern
#' @param uid_len an integer that specifies the length of unique ID that will
#' indexing rows.
#' @returns an object of class tracer which is a list containing:
#'  * DATA - a list of two list: RAW and PROCESSED data
#'  * LOG - a data.frame that contains info about the loading status of files
#'  * HISTORY - a data.frame with ordered records of processing steps.
#'  May absent if data is unprocessed.
#'  * META - a list of meta-data stored separately for each file loaded.
#' @export
load_trace <- function(path_dir = NULL
                       , fls = NULL
                       , pattern = NULL
                       , uid_len = 6L){

  # Check input args
  if(is.null(path_dir) & is.null(fls)){
    stop("Either path_dir or fls must be privided\n")
  }

  # Setting pattern
  if(is.null(pattern)){

    pattern = ".*(\\.arw)$|.*(\\.csv)$|.*(\\.txt)$"
  }

  # Check fls
  if(is.null(fls)){

    fls <- dir(path_dir
               , full.names = TRUE
               , pattern = pattern)
  }

  # Allocate a list
  raw_data <- list()
  meta <- list()
  id_pool <- NULL
  
  # Initialize a log record
  log_tmp <- record_log(what = list(ID = NA, SOURCE = NA, LOADED = NA, FILE = NA))

  # Scanning and loading files
  for(i in seq_along(fls)){

    # tmp can be either FALSE or a list with Meta and TRACE data
    tmp <- parser_selector(fls = fls[i])

     if(isFALSE(tmp)){
       # Update log
       dt_log <- log_tmp(list(ID = NA, SOURCE = NA, LOADED = FALSE, FILE = fls[i]))

      next}
    
    # if tmp not empty generate new ID
    idx <- gen_uid_pool(n = 1, len = uid_len, pool = id_pool)

    # Adding ID to the meta table
    tmp$META$ID <- idx

    # Update log
    dt_log <- log_tmp(list(ID = idx
                           , SOURCE = tmp$META$SRC
                           , LOADED = TRUE
                           , FILE = fls[i]))


    # Place data into RAW list
    raw_data[[idx]] <- tmp$TRACE

    # Append Meta list
    meta[[idx]] <- tmp$META

    # Add ID to a pool
    id_pool <- c(id_pool, idx)

  }

  rm(list = c("log_tmp", "tmp"))

  dt_log$FILE_NAME <- basename(dt_log$FILE)

  obj <- structure(

    list( DATA = list(RAW = raw_data, PROCESSED = NULL),
          LOG = tibble::as_tibble(dt_log),
          HISTORY = NULL,
          META = meta),
    class = "tracer"
    )

  return(obj)
}


#' Tries different separators and determines a file Template
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

      maxSig = max(dt[["Response"]]),
      dataPoints = nrow(dt),
      apexRT = dplyr::filter(.data$Response == max(.data$Response), .data = dt)|>
        dplyr::pull(.data$RT)|> mean()

      )|>
      dplyr::mutate(samplingRate = round(.data$dataPoints/(max(dt[["RT"]] - min(dt[["RT"]])))/60, digits = 1),
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

#' Generates unique ID
#' @description An ID generator that produces a single ID code composed of
#' lower case letters and 10 digits with prefix 'w'
#' @param len the length or the number of symbols in UID
#' @export
gen_uid <- function(len){

  idx <- paste0(
    sample(c(letters, 0:9)
           , size = len
           , replace = T)
    , collapse = "")

  idx <- paste0("w",idx)

  return(idx)
}

#' ID pool generator
#' @description Generates a pool of unique ID's
#' @param n a number of UID to generate
#' @param len the length or the number of symbols in UID
#' @param pool a vector filed with unique ID's - optional.
#' @export
gen_uid_pool <- function(n, len = 6, pool = NULL){

  code <- NULL
  pool <-c(pool, code)

  #if(!is.null(pool)){code <- pool}

  for(i in seq_len(n)){

    idx <- gen_uid(len = len)

    while(idx %in% pool){ idx <- gen_uid(len = len)}

    code <- c(code, idx)
  }

  return(code)
}









