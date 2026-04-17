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
    stop("Length of arguments must be equal", call. = FALSE)
    }
  
  #x$META[what]
  
  purrr::walk2(.x = what, .y = record, .f = function(idx, rec){
    
    x$META[[idx]][[field]] <<- rec
    
    })
  x
}



#' Add a new custom field
#' @description adding a field to the meta data
#' @param x an object of class tracer
#' @param new_field a string, i.e. a name of the new field
#' @param init a value to fill a new field with
#' @returns object of type tracer with a new field added into the meta data
#' @export
add_field <- function(x, new_field){
  
  if(methods::is(x) != "tracer"){ 
    stop("\n x must be a an object of type tracer", call. = FALSE)
  }
  
  hdr <- lapply(x$META, names) |> purrr::reduce(union)
  
  
  if(is.list(new_field)){
    
    init <- new_field
    new_field <- names(new_field)
    
  }else{
    
    init <- replicate(length(new_field), NA, simplify = FALSE)
    names(init) <- new_field

    }
  
  chk_names <- any(grepl(new_field, pattern = "^[^A-Za-z]"))
  
  if(chk_names){
    
    stop("A field name must start with a letter", call. = FALSE)
  }
  
  in_tab <- hdr %in% new_field
  
  if(any(in_tab)){stop("Fieled name", hdr[in_tab], " is alredy taken", call. = FALSE)}
  
    x$META <- lapply(x$META, function(dt){
      
      for(item in new_field){ dt[item] <- init[[item]] }
      
      dt})
    
  # Updating history records
   x <- history_upd(x = x, event = "field added")
  
  return(x)
}

#' Delete an item field
#' @description delete selected traces from the trace object
#' @param x an object of class tracer
#' @param what either numeric indexes or a vector string of UID. 
#' If some numeric indexes out of range an error will be thrown. 
#' All Unmatched UID's will be ignored with warning.
#' @returns object of type tracer with selected traces removed.
#' @export
del_trace <- function(x, what){
  
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
  x$LOG <- x$LOG |>
    dplyr::filter(!(.data$ID %in% what))
  
  x <- history_upd(x = x, event = "item('s) deleted")

  return(x)
}

#' Merge objects
#' @description merges two trace objects into one
#' @param a,b objects of class tracer
#' @param what an optional argument either numeric indexes or vector string of UID, 
#' that allows selecting traces from object `b` to be merged into object `a`. Default is NULL.
#' @param keep_history logical, whether processing history of object `b` should be kept 
#' in the new object or not.
#' @param active_re logical. If TRUE recomputes UID's to avoid duplicates.
#' @details
#' If  argument `active_re` is FALSE than all identical UIDs found 
#' in objects `a` and `b` will be ignored and not merged into object `a`.
#' @returns object of type tracer.
#' @export
merge_trace <- function(a, b, what = NULL, active_re = TRUE, keep_history = FALSE){
  
  a$LOG$Object <- deparse(substitute(a))
  b$LOG$Object <- deparse(substitute(b))
  
  if(methods::is(a) != "tracer" | methods::is(b) != "tracer"){ 
    stop("\n The merging objects must be a type of tracer", call. = FALSE)
  }
  
  # Update HISTORY
  if(keep_history){
    
    a$HISTORY$Object <-  deparse(substitute(a))
    b$HISTORY$Object <-  deparse(substitute(b))
    
    a$HISTORY <- rbind(a$HISTORY, b$HISTORY)
    
  }else{a <- history_upd(x = a, event = "merged")}
  
  # Checkin on UID's
  clashed_id <- intersect(a$LOG$ID, b$LOG$ID)
  what <- what_validator(b$META, what = what)
  
  # Checking whether the rehash option is ENABLED
  if(!active_re){
    
    id_filed <- "ID"
    
    
    if(length(clashed_id) != 0){
      # Update what and ignore duplicates
      what <- what[!(what %in% clashed_id)]
      
      
      warning("\nDUPLICATED traces have been detected and not merged: \n"
              , call. = FALSE
              , immediate. = TRUE)
      b$LOG |> 
        dplyr::filter(ID %in% clashed_id) |> 
        dplyr::select(.data$ID, .data$FILE_NAME)|>
        print()
      
    }
    
  }else{
    id_filed <- "ID_old"
    rhs <- rehash_id(a = a, b = b)
    a <- rhs$a
    b <- rhs$b
    rm(rhs)
  }
  
  
  what <- b$LOG|> dplyr::filter(.data[[id_filed]] %in% what)|> dplyr::pull(.data$ID)
  
  # APPEND content of the object b to object a
  a$META <- append(a$META, b$META[what])
  a$DATA$RAW <- append(a$DATA$RAW, b$DATA$RAW[what])
  
  if(any(!is.null(a$DATA$PROCESSED), !is.null(b$DATA$PROCESSED))){
    
    a$DATA$PROCESSED <- append(a$DATA$PROCESSED, b$DATA$PROCESSED[what])
    
  }
  # Combine LOG data
  a$LOG <- b$LOG|> 
    dplyr::filter(.data$ID %in% what)|>
    rbind(a$LOG, make.row.names = FALSE)|>
    dplyr::arrange(.data$Object)
  
  
  return(a)
}


#' Get meta data field description
#' @description returns fields of mete data across all traces
#' @param x objects of class tracer, optional
#' @export
get_meta_fields <- function(x = NULL){
  
  if(is.null(x)){ return(fiel_desc) }
  
  else if(methods::is(x) != "tracer"){ 
    stop("\n x must be a an object of type tracer", call. = FALSE)
  }else{
    
    hdr <- lapply(x$META, names)|>
      purrr::reduce(union)|>
      data.frame(FIELD =_)|>
      merge(y=_, x= fiel_desc, by = "FIELD", all.x = TRUE, all.y=TRUE)
    
    return(hdr)
  }

}


















