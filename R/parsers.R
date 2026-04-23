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
  trace_data <- utils::read.csv(file = fls
                         , header = FALSE
                         , sep = sep
                         , skip = skip
                         , col.names = c("RT", "Response"))
  
  # Initialize Meta
  if(skip == 0){ meta <- meta_default()}
  
  else{ meta <- utils::read.csv(file = fls
                         , header = TRUE
                         , sep = sep
                         , nrow = 1)|>
    dplyr::mutate(ID = NA
                  , dateAcquired = time_scan(.data$Date.Acquired))|>
    dplyr::select(!.data$Date.Acquired)}
  
  # Add file name
  meta <- meta |> dplyr::mutate(file = basename(fls))
  
  return(list(TRACE = trace_data, META = meta))
}

#' Chromeleon parser, imports .csv and .txt files
#' @keywords internal
#' @importFrom rlang .data
parse_chromeleon <- function(fls, sep){
  
  # Getting trace data
  trace_data <- utils::read.csv(file = fls
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
  
  meta <- utils::read.csv(file = fls
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
    dplyr::rename(SampleName = .data$Name
                  , dateAcquired = .data$InjectTime
                  , Comments = .data$Comment)
  
  rm(tmp)
  return(list(TRACE = trace_data, META = meta))
}

# netcdf parser ....
# parse_cdf <- function(){}