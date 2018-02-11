predict.customizedGlmnet <-
function(object, lambda, type = c('response', 'class'), ...)
{
    type = match.arg(type)
    groups = as.character(sort(unique(object$groupid)))
    prediction = matrix(NA, nrow(object$x$test), length(lambda))

    if (object$family == 'multinomial' & type == 'response') {
      K = length(unique(object$y))
      prediction = array(NA, c(nrow(object$x$test), length(lambda), K),
        dimnames = list(NULL, NULL, sort(unique(object$y))))
    }

    for (group in groups) {

        x = object$x$test[object$groupid == group, ]

        if (sum(object$groupid == group) == 1) {
            x = t(x)
        }

      if (object$family == 'multinomial' & type == 'response') {
        prediction[object$groupid == group, , ] = 0
        prediction[object$groupid == group, , object$fit[[group]]$classnames] =
          predict(object$fit[[group]], x, s = lambda/object$fit[[group]]$nobs,
          type = type, ...)[, , 1]
      } else {
        prediction[object$groupid == group, ] = predict(object$fit[[group]],
            x, s = lambda/object$fit[[group]]$nobs, type = type, ...)
      }
    }

    prediction
}
