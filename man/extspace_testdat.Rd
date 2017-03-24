\name{extspace_testdat}
\alias{extspace_testdat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 space extension for test data
}
\description{
space extension for test data
}
\usage{
extspace_testdat(testdata, ncombsub)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{testdata}{
 test data
}
  \item{ncombsub}{
candidate variable for genereation
}
}
\details{
space extension for test data
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
function (testdata, ncombsub) 
{
    testdata = model.matrix(~0 + ., as.data.frame(testdata))
    d = dim(ncombsub)[2]
    newx <- vector(mode = "list", length = d)
    newx <- NULL
    new_names <- paste("newvar", 1:d, sep = "_")
    for (i in 1:d) {
        newx = cbind(newx, testdata[, ncombsub[2, i]] - testdata[, 
            ncombsub[1, i]])
    }
    colnames(newx) <- new_names
    newxdata = cbind(testdata, newx)
    return(newxdata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
