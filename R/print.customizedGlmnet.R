#' Print the summary of a fitted \code{customizedGlmnet} object
#' 
#' Print the numbers of training observations and test observations in each
#' submodel of the \code{customizedGlmnet} fit
#' 
#' @param x fitted \code{customizedGlmnet} object
#' @param ... ignored
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
#' # Print the customized training model fit:
#' fit1
#' 
#' # Example 2: If the test set has predefined blocks, use these blocks to define
#' # the customized training sets, instead of using clustering.
#' group.id = apply(z == 1, 1, which)[n + 1:m]
#' 
#' fit2 = customizedGlmnet(x.train, y.train, x.test, group.id)
#' 
#' # Print the customized training model fit:
#' fit2
#' 
print.customizedGlmnet <-
function(x, ...)
{
    cat("\nCall: \n")
    print(x$call)

    cat("\nn =", nrow(x$x$train), "training observations\n")
    cat("m =", nrow(x$x$test), "test observations\n")
    cat("p =", ncol(x$x$train), "predictor variables\n\n")

    for (group in sort(unique(x$groupid))) {
        cat("Model ", group, ": ", sep = "")
        cat(length(x$CTset[[group]]), "train obs. and ")
        cat(sum(x$groupid == group), "test obs.\n")
    }
    cat("\n")
}
