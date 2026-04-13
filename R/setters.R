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
    dt
    })
  return(x)
}
