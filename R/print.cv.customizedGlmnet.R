#' Print a ``cv.customizedGlmnet'' object
#' 
#' Print the number of customized training subsets chosen by cross-validation and
#' the number of variables selected in each training subset.
#' 
#' @param x a fitted \code{cv.customizedGlmnet} object
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
#' foldid = sample(rep(1:10, length = nrow(x.train)))
#' 
#' # Example 1: Use clustering to fit the customized training model to training
#' # and test data with no predefined test-set blocks
#' 
#' fit1 = cv.customizedGlmnet(x.train, y.train, x.test, Gs = c(1, 2, 3, 5),
#'     family = "gaussian", foldid = foldid)
#' 
#' # Print the optimal number of groups and value of lambda:
#' fit1$G.min
#' fit1$lambda.min
#' 
#' # Print the customized training model fit:
#' fit1
#' 
#' # Compute test error using the predict function:
#' mean((y[n + 1:m] - predict(fit1))^2)
#' 
#' # Plot nonzero coefficients by group:
#' plot(fit1)
#' 
#' # Example 2: If the test set has predefined blocks, use these blocks to define
#' # the customized training sets, instead of using clustering.
#' foldid = apply(z == 1, 1, which)[1:n]
#' group.id = apply(z == 1, 1, which)[n + 1:m]
#' 
#' fit2 = cv.customizedGlmnet(x.train, y.train, x.test, group.id, foldid = foldid)
#' 
#' # Print the optimal value of lambda:
#' fit2$lambda.min
#' 
#' # Print the customized training model fit:
#' fit2
#' 
#' # Compute test error using the predict function:
#' mean((y[n + 1:m] - predict(fit2))^2)
#' 
#' # Plot nonzero coefficients by group:
#' plot(fit2)
#' 
#' # Example 3: If there is no test set, but the training set is organized into
#' # blocks, you can do cross validation with these blocks as the basis for the
#' # customized training sets.
#' 
#' fit3 = cv.customizedGlmnet(x.train, y.train, foldid = foldid)
#' 
#' # Print the optimal value of lambda:
#' fit3$lambda.min
#' 
#' # Print the customized training model fit:
#' fit3
#' 
#' # Compute test error using the predict function:
#' mean((y[n + 1:m] - predict(fit3))^2)
#' 
#' # Plot nonzero coefficients by group:
#' plot(fit3)
#' 
print.cv.customizedGlmnet <-
function(x, ...)
{
    cat("\nCall: \n")
    print(x$call)

    cat("\nn =", nrow(x$fit$x$train), "training observations\n")
    cat("m =", nrow(x$fit$x$test), "test observations\n")
    cat("p =", ncol(x$fit$x$train), "predictor variables\n")

    cat("\nNumber of groups chosen:", x$G.min, "\n\n")

    for (group in sort(unique(x$fit$groupid))) {
        cat("Model ", group, ": ", sep = "")
        cat(length(x$fit$CTset[[group]]), "train obs. and ")
        cat(sum(x$fit$groupid == group), "test obs.\n")

        cat(" No. of variables selected: ")
        if (is.element(x$fit$family, c("gaussian", "binomial"))) {
            cat(length(x$selected[[group]][[1]]))
        } else if (x$fit$family == "multinomial") {
            for (level in levels(x$fit$y)) {
                cat("Class ", level, ": ", sep = "")
                cat(length(x$selected[[group]][[level]][[1]]))
                cat("   ")
            }
        }
        cat("\n\n")
    }
}
