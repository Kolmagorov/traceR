#' Set a new records to a selected field of the meta data
#' @description setting new sample name to a trace
#' @param x an object of class tracer
#' @param what either numeric indexes or vector string of UID. 
#' If some Numeric indexes out of range an error will be thrown. 
#' All Unmatched UID's will be ignored with warning.
#' @param record a vector of entities
#' @param field a string representing the name of the field into which 
#' the record should be placed
#' @returns object of type tracer with field records updated 
#' @export
set_field_value <- function(x
                            , what
                            , record
                            , field = c("SampleName", "SRC", "Comments")){
  
  field <- match.arg(field)
  
  if(methods::is(x) != "tracer"){ 
    stop("\n x must be a an object of type tracer", call. = FALSE)
    }
  
  what <- what_validator(x$META, what = what)
  
  if(length(what) != length(record)){
    stop("Length of arguments must be equal", call. = FALSE) }
  
  x$META[what]
  
  purrr::walk2(.x = what, .y = record, .f = function(idx, rec){
    
    x$META[[idx]][[field]] <<- rec
    
    })
  x
}



#' Add a new custom field
#' @description adding a field to the meta data
#' @param x an object of class tracer
#' @param field a string, a name of the new field
#' @param init a value to fill a new field with
#' @returns object of type tracer with a new field added into the meta data
#' @export
add_field <- function(x, new_field, init = NA){
  
  if(methods::is(x) != "tracer"){ 
    stop("\n x must be a an object of type tracer", call. = FALSE)
  }
  
  if(grepl(new_field, pattern = "^[^A-Za-z]")){
    stop("Field name must start with a letter", call. = FALSE)
  }
  
  hdr <- lapply(x$META, names) |> purrr::reduce(union)
  in_tab <- any(grepl(hdr, pattern = new_field, ignore.case = TRUE))
  if(in_tab){stop("Filed name", new_field, " is alredy taken", call. = FALSE)}
  
  x$META <- lapply(x$META, function(dt){
    dt[new_field] <- init
    dt })
  
  # Updating history records
   x <- history_upd(x = x, event = "field added")
  
  return(x)
}

#' Delete an item field
#' @description delete selected traces from the trace object
#' @param x an object of class tracer
#' @param what either numeric indexes or vector string of UID. 
#' If some Numeric indexes out of range an error will be thrown. 
#' All Unmatched UID's will be ignored with warning.
#' @returns object of type tracer with selected traces removed.
#' @export
del_trace <- function(x, what, keep_history = FALSE){
  
  if(methods::is(x) != "tracer"){ 
    stop("\n x must be a an object of type tracer", call. = FALSE)
  }
  
  what <- what_validator(x$META, what = what)
  
  if(is.null(x$HISTORY)){ x$DATA$RAW[what] <- NULL }
  else{
    x$DATA$RAW[what] <- NULL
    x$DATA$PROCESSED[what] <- NULL
    }

  x$META[what] <- NULL
  
  # Cleaning LOG
  if(is.character(what)){
    x$LOG <- x$LOG |>
      dplyr::filter(!(ID %in% what))
  }else{ x$LOG <- x$LOG[-what,] }
  
  x <- history_upd(x = x, event = "item('s) deleted")

  return(x)
}

#' Merge objects
#' @description merge two trace objects into one
#' @param a,b an objects of class tracer
#' @param what an optional argument either numeric indexes or vector string of UID, 
#' that allows selecting traces from object 'b' to be merged with object 'a'.
#' @details
#' If both arguments 'what'  and 'rehash' are NULL than all identical UIDs found 
#' in objects 'a' and 'b' will be ignored and not copied into object a. If
#' argument 'rehash' is TRUE all data will be merged, but all UID's will be recomputed
#' to ensure uniqueness.
#' @returns object of type tracer.
#' @export
merge_trace <- function(a, b, what = NULL){
  
  if(methods::is(a) != "tracer" | methods::is(b) != "tracer"){ 
    stop("\n The merging objects must be a type of tracer", call. = FALSE)
  }
  
  what <- what_validator(b$META, what = what)
  
  # Combining lists

  a$META <- append(a$META, b$META[what])
  a$DATA$RAW <- append(a$DATA$RAW, b$RAW[what])
  a$DATA$PROCESSED <- append(a$DATA$PROCESSED, b$PROCESSED[what])
  a$LOG <- rbind(a$LOG, b$LOG) # A new field is needed + record
  return(a)
}



