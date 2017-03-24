\name{extspace_dat}
\alias{extspace_dat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
space extension for training data
}
\description{
space extension for training data
}
\usage{
extspace_dat(xdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdata}{
space extension for training data
}
}
\details{
space extension for training data
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Random Survival Forest with Space Extensions for Censored Data, submitted to Artificial Intelligence in Medicine
}
\author{
Hong Wang
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (xdata) 
{
    xdata = model.matrix(~0 + ., as.data.frame(xdata))
    p = ncol(xdata)
    d = p
    ncomb <- combn(p, 2)
    ncombsub <- ncomb[, sample(choose(p, 2), d)]
    newx <- vector(mode = "list", length = d)
    newx <- NULL
    new_names <- paste("newvar", 1:d, sep = "_")
    for (i in 1:p) {
        newx = cbind(newx, xdata[, ncombsub[2, i]] - xdata[, 
            ncombsub[1, i]])
    }
    colnames(newx) <- new_names
    newxdata = cbind(xdata, newx)
    return(list(newxdata = newxdata, ncombsub = ncombsub))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
