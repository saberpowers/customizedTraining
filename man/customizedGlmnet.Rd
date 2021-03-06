\name{customizedGlmnet}
\alias{customizedGlmnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fit glmnet using customized training
}
\description{
Fit a regularized lasso model using customized training
}
\usage{
customizedGlmnet(xTrain, yTrain, xTest, groupid = NULL, G = NULL,
    family = c("gaussian", "binomial", "multinomial"), dendrogram = NULL,
    dendrogramTestIndices = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xTrain}{
an n-by-p matrix of training covariates
}
  \item{yTrain}{
a length-n vector of training responses. Numeric for family = \code{"gaussian"}.
Factor or character for \code{family = "binomial"} or
\code{family = "multinomial"}
}
  \item{xTest}{
an m-by-p matrix of test covariates
}
  \item{groupid}{
an optional length-m vector of group memberships for the test set. If
specified, customized training subsets are identified using the union of
nearest neighbor sets for each test group. Either \code{groupid} or \code{G}
must be specified
}
  \item{G}{
a positive integer indicating the number of clusters for the joint clustering
of the test and training data. Ignored if \code{groupid} is specified. Either
\code{groupid} or \code{G} must be specified
}
  \item{family}{
response type
}
  \item{dendrogram}{
optional output from \code{hclust} on the joint covariate data. Used by
\code{cv.customizedGlmnet} so that clustering is not computed redundantly
}
  \item{dendrogramTestIndices}{
optional set of indices (corresponding to dendrogram) held out in
cross-validation. Used by \code{cv.customizedGlmnet}
}
}
\details{
Identify customized training subsets of the training data through one of two
methods: (1) If groupid is specified, grouping the test data, then for each
test group find the 10 nearest neighbors of each observation in the group and
use the union of these nearest neighbor sets as the customized training set or
(2) If G is specified, jointly cluster the test and training data using
hierarchical clustering with complete linkage. Within each cluster, the
training data are used as the customized training subset for the test data.
Once the customized training subsets have been identified, use glmnet to fit an
l1-regularized regression model to each.
}
\value{
an object with class \code{customizedGlmnet}
  \item{call}{
the call that produced this object
}
  \item{CTset}{
a list containing the customized training subsets for each test group
}
  \item{fit}{
a list containing the glmnet fit for each test group
}
  \item{groupid}{
a length-m vector containing the group memberships of the test data
}
  \item{x}{
a list containing \code{train} (which is the input \code{xTrain}) and
\code{test} (which is the input \code{xTest}). Specified in function call
}
  \item{y}{
training response vector (specified in function call)
}
  \item{family}{
response type (specified in function call)
}
  \item{standard}{
the fit of \code{glmnet} to the entire training set using standard training
}
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
\code{\link{print.customizedGlmnet}}, \code{\link{predict.customizedGlmnet}},
\code{\link{plot.customizedGlmnet}}, \code{\link{cv.customizedGlmnet}}
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

# Print the customized training model fit:
fit1

# Extract nonzero regression coefficients for each group:
nonzero(fit1, lambda = 10)

# Compute test error using the predict function:
mean((y.test - predict(fit1, lambda = 10))^2)

# Plot nonzero coefficients by group:
plot(fit1, lambda = 10)


# Example 2: If the test set has predefined blocks, use these blocks to define
# the customized training sets, instead of using clustering.
group.id = apply(z == 1, 1, which)[n + 1:m]

fit2 = customizedGlmnet(x.train, y.train, x.test, group.id)

# Print the customized training model fit:
fit2

# Extract nonzero regression coefficients for each group:
nonzero(fit2, lambda = 10)

# Compute test error using the predict function:
mean((y.test - predict(fit2, lambda = 10))^2)

# Plot nonzero coefficients by group:
plot(fit2, lambda = 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
