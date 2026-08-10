#' Set a new records to a selected field of the meta data
#' @description adding new meta data to the selected samples
#' @param x an object of class tracer
#' @param what a smart row selector see `plt_gg` for details.
#' @param field a named list of fields and corresponding records
#' @param full_match logical, if set TRUE a field name if exists will be searched 
#' for exact match.
#' @details depending on what is passed to `what`, meta data update can be 
#' done to a single sample or group of samples. If `what` is set to NULL then 
#' to all samples the same fields and records will be added.
#' 
#' @returns an object of type tracer with new fields and records updated 
#' @export
set_field <- function(x
                      , what
                      , field
                      , full_match = FALSE){
  
  if(methods::is(x) != "tracer"){ 
    stop("\n x must be an object of type tracer", call. = FALSE)
    }
  
  if(!is.list(field)){
    stop("The argument field must be a type of list", call. = FALSE)
    }
  
  if(full_match){ ptrn <- rlang::enexpr(paste0("\\b", f,"\\b")) }
  else{ ptrn <- rlang::expr(paste0("^", f)) }
  
  what <- what_validator(x, what = what)
  fld <- names(field)
  
  if(length(fld) != length(unique(fld))){
    stop("Field names must be unique\n")
  }
  
  x$META <- lapply(x$META, function(dt){
    
    for(f in fld){
      
      tmp <- grep(x = names(dt)
                  , pattern = eval(expr = ptrn)
                  , ignore.case = !full_match
                  , value = T)
      
      if(length(tmp) > 1){
        warning("The field ", f, " is not unique so it was skipped\n")
        next}
      if(length(tmp) == 0){ tmp <- f }
      
      
      if(dt$ID %in% what){ dt[tmp] <- field[f] }
      else{next}
      }
    dt
    })
  
  x <- history_upd(x = x, event = "set_field")

  return(x)
}


#' Deletes selected traces
#' @description removes all selected traces
#' @param x an object of class tracer
#' @param what a smart row selector, see `plt_gg` for details.
#' @returns object of type tracer with a new field added into the meta data
#' @export
del_trace <- function(x, what){
  
  if(methods::is(x) != "tracer"){ 
    stop("\n x must be an object of type tracer", call. = FALSE)
  }
  
  what <- what_validator(x, what = what)
  
  # Deletion
  x$RAW[what] <- NULL
  x$META[what] <- NULL
  x$LOG <- x$LOG |>
    dplyr::filter(!(.data$ID %in% what))
  if(!is.null(x$PROCESSED)){ x$PROCESSED[what] <- NULL }
  
  x <- history_upd(x = x, event = "delete")

  return(x)
}

#' Copy an item of tracer object
#' @description copy selected traces from the tracer object
#' @param x an object of class tracer
#' @param what see `plt_gg` for details
#' @returns object of type tracer with selected traces.
#' @export
copy_trace <- function(x, what = NULL){
  
  if(methods::is(x) != "tracer"){ 
    stop("\n x must be an object of type tracer", call. = FALSE)
  }
  
  if(is.null(what)){
    return(x)
  }else{
    
    what <- setdiff(names(x$META), what)
    x <- del_trace(x, what)
    x <- history_upd(x = x, event = "copied")
  }
  
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
#' In case `a` and `b` are identical an object `a` will be returned.
#' @returns object of type tracer.
#' @export
merge_trace <- function(a, b, what = NULL, active_re = TRUE, keep_history = FALSE){
  
  if(identical(a,b)){
    return(a)
  }
  
  a$LOG$Object <- deparse(substitute(a))
  b$LOG$Object <- deparse(substitute(b))
  
  if(methods::is(a) != "tracer" | methods::is(b) != "tracer"){ 
    stop("\n The merging objects must be a type of tracer", call. = FALSE)
  }
  
  # Update HISTORY
  if(keep_history){

    a$HISTORY$Object <-  unique(a$LOG$Object)
    b$HISTORY$Object <-  unique(b$LOG$Object)
    
    a$HISTORY <- rbind(a$HISTORY, b$HISTORY)
    
  }else{a <- history_upd(x = a, event = "merged")}
  
  # Checking on UID's
  clashed_id <- intersect(a$LOG$ID, b$LOG$ID)
  what <- what_validator(b, what = what)
  
  # Checking whether the rehash option is ENABLED
  if(!active_re){
    
    id_fld <- "ID"
    
    
    if(length(clashed_id) != 0){
      # Update what and ignore duplicates
      what <- what[!(what %in% clashed_id)]
      
      
      warning("\nDUPLICATED traces have been detected and not merged: \n"
              , call. = FALSE
              , immediate. = TRUE)
      b$LOG |> 
        dplyr::filter(.data$ID %in% clashed_id) |> 
        dplyr::select(.data$ID, .data$FILE_NAME)|>
        print()
      
    }
    
  }else{
    id_fld <- "ID_old"
    rhs <- rehash_id(a = a, b = b)
    a <- rhs$a
    b <- rhs$b
    rm(rhs)
  }
  
  
  what <- b$LOG|> dplyr::filter(.data[[id_fld]] %in% what)|> dplyr::pull(.data$ID)
  
  # APPEND content of the object b to the object a
  a$META <- append(a$META, b$META[what])
  a$RAW <- append(a$RAW, b$RAW[what])
  
  if(any(!is.null(a$PROCESSED), !is.null(b$PROCESSED))){
    
    a$PROCESSED <- append(a$PROCESSED, b$PROCESSED[what])
    
  }
  # Combine LOG data
  
  a$LOG <- b$LOG|> 
    dplyr::filter(.data$ID %in% what)|>
    rbind(a$LOG, make.row.names = FALSE)|>
    dplyr::arrange(.data$Object)|>
    dplyr::select(!.data[[id_fld]])
  
  
  return(a)
}


#' Get meta data field description
#' @description returns fields of mete data across all traces
#' @param x objects of class tracer. If an object of class tracer is not supplied, 
#' a default list of fields and description will be returned.
#' @export
get_meta_fields <- function(x = NULL){
  
  if(is.null(x)){ return(field_desc) }
  
  else if(methods::is(x) != "tracer"){ 
    stop("\n x must be an object of type tracer", call. = FALSE)
  }else{
    
    hdr <- lapply(x$META, names)|>
      purrr::reduce(union)|>
      data.frame(FIELD =_)|>
      merge(y=_, x= field_desc, by = "FIELD", all.x = TRUE, all.y = TRUE)
    
    return(hdr)
  }

}


#' object constructor
#' @description create an obect of type tracer
#' @param x a data.frame or a list of data.frames
#' @param len a numeric that defines UID length
#' @param use_columns a vector of length 2, defining wich columns will be used 
#' as time and response variables. By default first is Time, second - Response.
#' @param meta meta data, can be a data.frame or a list
#' @param use_names if TRUE than the names of  input list x will be used as ID's
#' @details an input data.frame must contain two columns of type numeric. To construct
#' an object with multiple traces, a list of data.frames must be provided.
#' 
#' @export
new_trace <- function(x
                      , meta = NULL
                      , use_columns = NULL
                      , use_names = FALSE
                      , len = 6L){
  
  # Validate use_columns
  if(is.null(use_columns)){
    use_columns <- 1:2
  }else if(length(use_columns) != 2){
    stop("The argument use_columns must be either numeric or character vector of length 2")
    }
  
  # check input x and convert to list
  if(is.data.frame(x)){
    x <- list(x)
  }
  
  
  if(is.list(x)){
    
    # initialize LOG
    log_rec <- init_log(tmpl = "LOG")
    
    # create a list for META data
    meta_d <- vector(mode = "list", length = length(x))
    
    # Generate UID
    if(use_names){idx <- names(x)}
    else{
      idx <- gen_uid_pool(n = length(x), len = len, pool = NULL)
    }
    
    
    # Transform input x to RAW
    names(x) <- idx
    
    # Assign UIDs to items in the META container
    names(meta_d) <- idx
    
    # Checking type of the list items of input data
    if(!all(sapply(x, is.data.frame, simplify = TRUE))){
      
      stop("new_trace: All items of argument x must be of class data.frame", call. = FALSE)
    }
    
    # Iterate over the input list
    for(i in seq_along(x)){
      
      # Overrides input x with RAW data
      x[[i]] <- x[[i]][use_columns]
      
      if(!all( use_columns %in% c("RT","Response"))){
        names(x[[i]]) <- c("RT", "Response")
      }
      
      # Checking type of columns in input data items
      if(!all(sapply(x[[i]], is.numeric, simplify = TRUE))){
        stop("All columns must be of type numeric", call. = FALSE)
      }
      
      # Select a way to treat meta
      if(is.data.frame(meta[[i]])){
        
        meta_tmp <- as.list(meta[[i]])
        
      }else(meta_tmp <- meta)
      
      # meta_tmp contains POSIX
      
      # Update LOG with a new record
      dt_log <- log_rec(list(ID = idx[i], FILE_NAME = NA, SOURCE = "manual"))
      
      # Assign UID to meta_tmp
      meta_tmp[["ID"]] <- idx[i]
      
      # Init meta table records
      meta_rec <- init_log(tmpl = "META")
      
      # Update META with a new record
      meta_d[[i]] <- meta_rec(meta_tmp)
   }
  }
  
  
  # Compile and create an object 
  obj <- structure(
    
    list( RAW = x,
          PROCESSED = NULL,
          LOG = tibble::as_tibble(dt_log),
          HISTORY = data.frame(type = "created"
                               , proc_time = format(Sys.time(), "%d-%b-%Y %H:%M:%OS3")),
          META = meta_d,
          Workflow = NULL),
    class = "tracer"
  )
  
  return(obj)
  
}
















