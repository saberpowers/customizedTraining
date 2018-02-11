\name{nonzero}
\alias{nonzero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
return selected variables
}
\description{
\code{nonzero} is a generic function for returning the set of variables
selected by a model
}
\usage{
nonzero(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
a model object for which the set of selected variables is desired
}
  \item{\dots}{
additional arguments, e.g. tuning parameters
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The form of the value returned by \code{nonzero} depends on the class of its
argument. See the documentation of the particular methods for details of what
is produced by that method.
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{nonzero.customizedGlmnet}}, \code{\link{nonzero.singleton}}
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (object, ...) 
%UseMethod("nonzero")
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
