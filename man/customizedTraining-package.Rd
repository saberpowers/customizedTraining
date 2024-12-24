\name{customizedTraining-package}
\alias{customizedTraining-package}
\alias{customizedTraining}
\docType{package}
\title{
customizedTraining
}
\description{
Fit a regularized lasso model using customized training
}
\details{
Customized training is a simple strategy for making predictions on test data
when the features of the test data are available at the time of model fitting.
The method clusters the data to find training points close to each test point
and then fits a GLM elastic net model separately in each training cluster. In
this way, customized training is a localized method for transductive learning.
In contrast with local regression, however, instead of fitting a separate
regression model for each test point, customized training fits only one model
for each of a handful of clusters.

Use \code{customizedGlmnet()} to fit a \code{glmnet()} model with customized
training. Use \code{cv.customizedGlmnet()} to do the same while choosing the
regularization parameter and potentially the number of groups using
cross-validation. The \code{plot()} and \code{predict()} methods are
implemented for both \code{customizedGlmnet()} and
\code{cv.customizedGlmnet()}.
}
\author{Scott Powers, Trevor Hastie, Robert Tibshirani

Maintainer: Scott Powers <saberpowers@gmail.com>
}
\references{
Scott Powers, Trevor Hastie and Robert Tibshirani (2015) "Customized training
with an application to mass specrometric imaging of gastric cancer data."
Annals of Applied Statistics 9, 4:1709-1725.
}
\keyword{ package }
\seealso{
\code{\link[glmnet:glmnet-package]{glmnet}}
}
\examples{
require(glmnet)


#   Simulate three groups, each with a different sparse linear model
#   producing the response, and fit customized training model (with CV) to the
#   synthetic data

# Simulation parameters:
n = m = 300
p = 100
q = 10
K = 3
sigmaC = 10
sigmaX = sigmaY = 1
set.seed(5914)

# Produce the synthetic data:
beta = matrix(0, nrow = p, ncol = K)
for (k in 1:K) beta[sample(1:p, q), k] = 1
c = matrix(rnorm(K*p, 0, sigmaC), K, p)
eta = rnorm(K)
pi = (exp(eta)+1)/sum(exp(eta)+1)
z = t(rmultinom(m + n, 1, pi))
x = crossprod(t(z), c) + matrix(rnorm((m + n)*p, 0, sigmaX), m + n, p)
y = rowSums(z*(crossprod(t(x), beta))) + rnorm(m + n, 0, sigmaY)

x.train = x[1:n, ]
y.train = y[1:n]
x.test = x[n + 1:m, ]
foldid = sample(rep(1:10, length = nrow(x.train)))

# Fit the customized training model with CV:
fit2 = cv.customizedGlmnet(x.train, y.train, x.test, Gs = 1:3,
    family = "gaussian", foldid = foldid)

# Print the optimal number of groups and value of lambda:
fit2$G.min
fit2$lambda.min

# Print the customized training model fit:
fit2

# Compute test error using the predict function:
mean((y[n + 1:m] - predict(fit2))^2)

# Plot nonzero coefficients by group:
plot(fit2)
}
