#' Return selected variables from a \code{customizedGlmnet} object
#' 
#' Returns a list of vectors of selected variables for each separate glmnet model
#' fit by a \code{customizedGlmnet} object
#' 
#' @param object fitted \code{customizedGlmnet} model object
#' @param lambda value of regularization parameter to use. Must be specified
#' @param ... ignored
#' 
#' @return  a list of vectors, each vectors representing one of the \code{glmnet} models
#'   fit by the \code{customizedGlmnet} model. Each vector gives the indices of the
#'   variables selected by the model
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
#' # Extract nonzero regression coefficients for each group:
#' nonzero(fit1, lambda = 10)
#' 
#' # Example 2: If the test set has predefined blocks, use these blocks to define
#' # the customized training sets, instead of using clustering.
#' group.id = apply(z == 1, 1, which)[n + 1:m]
#' 
#' fit2 = customizedGlmnet(x.train, y.train, x.test, group.id)
#' 
#' # Extract nonzero regression coefficients for each group:
#' nonzero(fit2, lambda = 10)
#' 
nonzero.customizedGlmnet <-
function(object, lambda = NULL, ...)
{
    groups = as.character(sort(unique(object$groupid)))
    selected = list()

    for (group in groups) {

        selected[[group]] = stats::predict(object$fit[[group]],
            s = lambda/object$fit[[group]]$nobs, type = "nonzero")
    }

    selected
}
