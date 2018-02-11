\name{predict.singleton}
\alias{predict.singleton}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make predictions from a ``singleton'' object
}
\description{
Returns the value stored in the singleton. Intended for internal use only.
}
\usage{
\method{predict}{singleton}(object, type = c('response', 'class', 'nonzero'),
  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{singleton}
}
  \item{type}{
Type of prediction to be returned, "response" or "class"
}
  \item{\dots}{
ignored
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The value of the singleton
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Scott Powers, Trevor Hastie, Robert Tibshirani
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{predict}}
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (y, ...) 
%{
%    if (!is.null(levels(y))) {
%        return(levels(y))
%    }
%    else return(y)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
