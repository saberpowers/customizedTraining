\name{predict.customizedGlmnet}
\alias{predict.customizedGlmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make predictions from a \code{customizedGlmnet} object
}
\description{
Returns predictions for the test set provided at the time of fitting the
\code{customizedGlmnet} object.
}
\usage{
\method{predict}{customizedGlmnet}(object, lambda,
  type = c('response', 'class'), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
a fitted \code{customizedGlmnet} object
}
  \item{lambda}{
regularization parameter
}
  \item{type}{
Type of prediction, currently only "response" and "class" are supported. Type
"response" returns fitted values for "gaussian" family and fitted probabilities
for "binomial" and "multinomial" families. Type "class" applies only to
"binomial" and "multinomial" families and returns the class with the highest
fitted probability.
}
    \item{\dots}{
        ignored
    }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a vector of predictions corresponding to the test data input to the model at
the time of fitting
}
\references{
Scott Powers, Trevor Hastie and Robert Tibshirani (2015) "Customized training
with an application to mass specrometric imaging of gastric cancer data."
Annals of Applied Statistics 9, 4:1709-1725.
}
\author{
Scott Powers, Trevor Hastie, Robert Tibshirani
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{predict}}, \code{\link{customizedGlmnet}}
}
\examples{
require(glmnet)

# Simulate synthetic data

n = m = 150
p = 50
q = 5
K = 3
sigmaC = 10
sigmaX = sigmaY = 1
set.seed(5914)

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
y.test = y[n + 1:m]


# Example 1: Use clustering to fit the customized training model to training
# and test data with no predefined test-set blocks

fit1 = customizedGlmnet(x.train, y.train, x.test, G = 3,
    family = "gaussian")

# Compute test error using the predict function:
mean((y.test - predict(fit1, lambda = 10))^2)


# Example 2: If the test set has predefined blocks, use these blocks to define
# the customized training sets, instead of using clustering.
group.id = apply(z == 1, 1, which)[n + 1:m]

fit2 = customizedGlmnet(x.train, y.train, x.test, group.id)

# Compute test error using the predict function:
mean((y.test - predict(fit2, lambda = 10))^2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
