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
  #reg <- NULL

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

    tmp <- read.table(file = fls, header = F, sep = item, nrows = 1) |>
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

#' Selects parser
#' @keywords internal
parser_selector <- function(fls){

  read_par <- file_scan(fls)

  if(isFALSE(read_par)){ return(read_par) }
  else{
    out <- switch(EXPR = read_par$SYS,
           EMPOWER = parse_empower(fls = fls, sep = read_par$SEP , skip = read_par$SKIP),
           CHROMELEON = parse_chromeleon(fls = fls, sep = read_par$SEP),
           Undefined = parse_empower(fls = fls, sep = read_par$SEP , skip = read_par$SKIP))
    out$META$SRC <- read_par$SYS } # Assigning source info to the meta data row

  return(out)
}

#' Empower parser, imports .csv, .txt, .arw files
#' @keywords internal
#' @importFrom rlang .data
parse_empower <- function(fls, skip, sep){

  # Getting trace data
  trace_data <- read.csv(file = fls
                           , header = FALSE
                           , sep = sep
                           , skip = skip
                           , col.names = c("RT", "Response"))

  # Initialize Meta
  if(skip == 0){ meta <- data.frame(ID = NA, SampleName = NA, dateAcquired = NA)}

  else{ meta <- read.csv(file = fls
                          , header = TRUE
                          , sep = sep
                          , nrow = 1)|>
    dplyr::mutate(ID = NA
                  , dateAcquired = time_scan(.data$Date.Acquired))|>
    dplyr::select(!Date.Acquired)}

  # Add file name
  meta <- meta |> dplyr::mutate(file = basename(fls))

  return(list(TRACE = trace_data, META = meta))
}

#' Chromeleon parser, imports .csv and .txt files
#' @keywords internal
#' @importFrom rlang .data
parse_chromeleon <- function(fls, sep){

  # Getting trace data
  trace_data <- read.csv(file = fls
                           , header = FALSE
                           , sep = sep
                           , skip = 43
                           , blank.lines.skip = FALSE
                           , col.names = c("RT", "Step", "Response")
                           , colClasses = c("character", "NULL", "character"))|>
    # Fixing ugly numbers
    lapply(X =_, function(x){

      gsub(x, pattern = ",", replacement ="")|>
      as.numeric()
      }) |>
    do.call("cbind", args=_)|>
    data.frame()

  meta <- read.csv(file = fls
                   , header = FALSE
                   , sep = sep
                   , nrow = 37
                   , blank.lines.skip = TRUE
                   , col.names = c("Attribute", "Value")
                   )
  # Removing redudant rows
  meta <- meta[-c(3, 18, 30, 36),]
  meta <- meta|>
    dplyr::mutate(Attribute = gsub(.data$Attribute, pattern = "\\s|\\(.*\\)|\\.", replacement = "")
                  , Value = gsub(.data$Value, pattern = ",", replacement = ""))


  tmp <- meta$Value |> t()
  colnames(tmp) <- meta$Attribute

  # Expand Meta data
   meta <- data.frame(tmp)|>
    dplyr::mutate(ID = NA
                  , InjectTime = time_scan(.data$InjectTime)
                  , file = basename(fls) )|>
     dplyr::rename(SampleName = Name, dateAcquired = InjectTime)

  rm(tmp)
  return(list(TRACE = trace_data, META = meta))
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
      apexRT = dplyr::filter(Response == max(.data$Response), .data = dt)|>
        dplyr::pull(.data$RT)|> mean()

      )|>
      dplyr::mutate(samplingRate = round(dataPoints/(max(dt[["RT"]] - min(dt[["RT"]])))/60, digits = 1),
                    score = round(energy/dataPoints, digits = 1))

  })|>
    do.call("rbind", args=_)|>
    dplyr::mutate(ID = names(lst))

  row.names(exp_meta) <- NULL

  return(exp_meta)
}

#' Keeps track of processing steps records them into a data.frame
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

#' An ID generator that produces a single ID code composed of
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

#' ID pool generator. Generates a pool of unique ID's
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









