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
  
  # NOTE an isolated Class constructor is needed
  # NOTE meta helper function as well
  obj <- structure(

    list( DATA = list(RAW = raw_data, PROCESSED = NULL),
          LOG = tibble::as_tibble(dt_log),
          
          HISTORY = data.frame(type = "created"
                               , proc_time = format(Sys.time(), "%d-%b-%Y %H:%M:%OS3")),
          META = meta),
    class = "tracer"
    )

  return(obj)
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









