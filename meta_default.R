#' Construct Default field for meta data
#' @param tmpl a string argument to select a template
#' @param lst a list of fields and values to pass to the selected template, if NULL
#' all fields will be filed with default values. 
#' @details if `lst`  contains the same field as in template its default value 
#' is going to be overwritten.
#' @keywords internal
meta_default <- function(tmpl = c("LOG", "META"), lst = NULL){
  
  tmpl <- match.arg(tmpl)
  
  out <- switch(tmpl,
                META = tab_tmplate$META_tmpl,
                LOG = tab_tmplate$LOG_tmpl)
  
  if(is.null(lst)){return(out)}
  if(!is.list(lst)){stop("In helper: meta_default() lst must be a list")}
  
  fld <- names(lst)
  
  for(nms in fld){ 
    
    if(nms %in% names(out)){out[[nms]] <- lst[[nms]]}
    else{
      out <- out|>
        dplyr::mutate("{nms}" := lst[[nms]])
    }
  }
  return(out)
}