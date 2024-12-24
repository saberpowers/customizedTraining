#' Make predictions from a \code{customizedGlmnet} object
#' 
#' Returns predictions for the test set provided at the time of fitting the \code{customizedGlmnet} object.
#' 
#' @param object a fitted \code{customizedGlmnet} object
#' @param lambda regularization parameter
#' @param type Type of prediction, currently only "response" and "class" are supported. Type
#'   "response" returns fitted values for "gaussian" family and fitted probabilities
#'   for "binomial" and "multinomial" families. Type "class" applies only to
#'   "binomial" and "multinomial" families and returns the class with the highest
#'   fitted probability.
#' @param ... ignored
#' 
#' @return a vector of predictions corresponding to the test data input to the model at
#'   the time of fitting
#' 
#' @export
#' 
#' @examples
#' require(glmnet)
#' 
#' # Simulate synthetic data
#' n = m = 150
#' p = 50
#' q = 5
#' K = 3
#' sigmaC = 10
#' sigmaX = sigmaY = 1
#' set.seed(5914)
#' 
#' beta = matrix(0, nrow = p, ncol = K)
#' for (k in 1:K) beta[sample(1:p, q), k] = 1
#' c = matrix(rnorm(K*p, 0, sigmaC), K, p)
#' eta = rnorm(K)
#' pi = (exp(eta)+1)/sum(exp(eta)+1)
#' z = t(rmultinom(m + n, 1, pi))
#' x = crossprod(t(z), c) + matrix(rnorm((m + n)*p, 0, sigmaX), m + n, p)
#' y = rowSums(z*(crossprod(t(x), beta))) + rnorm(m + n, 0, sigmaY)
#' 
#' x.train = x[1:n, ]
#' y.train = y[1:n]
#' x.test = x[n + 1:m, ]
#' y.test = y[n + 1:m]
#' 
#' # Example 1: Use clustering to fit the customized training model to training
#' # and test data with no predefined test-set blocks
#' 
#' fit1 = customizedGlmnet(x.train, y.train, x.test, G = 3,
#'     family = "gaussian")
#' 
#' # Compute test error using the predict function:
#' mean((y.test - predict(fit1, lambda = 10))^2)
#' 
#' # Example 2: If the test set has predefined blocks, use these blocks to define
#' # the customized training sets, instead of using clustering.
#' group.id = apply(z == 1, 1, which)[n + 1:m]
#' 
#' fit2 = customizedGlmnet(x.train, y.train, x.test, group.id)
#' 
#' # Compute test error using the predict function:
#' mean((y.test - predict(fit2, lambda = 10))^2)
#' 
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
          stats::predict(object$fit[[group]], x, s = lambda/object$fit[[group]]$nobs,
          type = type, ...)[, , 1]
      } else {
        prediction[object$groupid == group, ] = stats::predict(object$fit[[group]],
            x, s = lambda/object$fit[[group]]$nobs, type = type, ...)
      }
    }

    prediction
}
