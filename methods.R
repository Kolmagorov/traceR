
# Class Tracer print method
#' @export
print.tracer <- function(x,...){
  
  n_trace_raw <- length(x$DATA$RAW)
  n_trace_proc <- length(x$DATA$PROCESSED)
  
  sys <- names(x$META)
  
  raw_qty <- x$LOG|> 
    dplyr::filter(.data$LOADED == TRUE)|>
    dplyr::select(.data$SOURCE)|>
    dplyr::summarise(QTY = dplyr::n(), .by = .data$SOURCE)
  
  cat("Object of type tracer\n",
      "\nTotal number of traces: ", n_trace_raw, "\n",
      "Number of traces processed: ", n_trace_proc, "\n",
      "\nTrace Distribution by Group:\n", fill = T)
  
  print(raw_qty,...)
  
  cat("\nLook up into the LOG table to see file load status")
  
}


# Class Tracer length method
#' @export
length.tracer <- function(x){
  len <- length(x$DATA$PROCESSED)
  if(is.null(len)|is.na(len)){
    return(length(x$DATA$RAW))
  }
  
  return(len)
}

#' Plot traces
#' @description this function is - under development -
#' @currently 'ggplot2' is only supported graphic system. 'base' and 'plotly' are coming next.
#' @param x an object of class tracer
#' @param ... additional arguments to pass to plt_gg, see ?plt_gg
#' @export
#' @importFrom rlang .data
plot.tracer <- function(x, ...){
  
  plt_gg(x = x, ...)
  
}

#' Operator `+` overloading
#' @param a,b objects of class tracer
#' @details a wrapper for `merge_trace`, where keep_history and re_active are TRUE.
#' @export
`+.tracer` <- function(a, b){
  merge_trace(a = a, b = b, keep_history = T)
}
















