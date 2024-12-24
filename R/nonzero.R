#' Return selected variables
#' 
#' \code{nonzero} is a generic function for returning the set of variables selected by a model
#' 
#' @param object a model object for which the set of selected variables is desired
#' @param ... additional arguments, e.g. tuning parameters
#' 
#' @return The form of the value returned by \code{nonzero} depends on the class of its
#' argument. See the documentation of the particular methods for details of what
#' is produced by that method.
#' 
#' @export
#' 
nonzero <-
function(object, ...) UseMethod("nonzero")
