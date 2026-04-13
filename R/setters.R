#' Set name
#' @description setting new sample name to a trace
#' @param x an object of class tracer
set_sample_name <- function(x, what, entries){
  
  if(methods::is(x) != "tracer"){ stop("\n x must be a an object of type tracer")}
  
  what <- what_validator(x, what = what)
  

}





#' Set SOURCE
#' @description assigning a source  to a trace
#' @param x an object of class tracer
set_source <- function(x, what, entries){
  
  if(methods::is(x) != "tracer"){ stop("\n x must be a an object of type tracer")}
  
  what <- what_validator(x, what = what)
  
}