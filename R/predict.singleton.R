#' Make predictions from a ``singleton'' object
#' 
#' Returns the value stored in the singleton. Intended for internal use only.
#' @param object an object of class \code{singleton}
#' @param type type of prediction to be returned, "response" or "class"
#' @param ... ignored
#' 
#' @return The value of the singleton
#' 
#' @export
#' 
predict.singleton <-
function(object, type = c('response', 'class', 'nonzero'), ...)
{
  type = match.arg(type)
  if (type == 'nonzero') return(NULL)
  return(object[[type]])
}
