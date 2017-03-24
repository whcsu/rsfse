\name{getsurvprob}
\alias{getsurvprob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get survprob from pec packages
}
\description{
Get survprob from pec packages
}
\usage{
getsurvprob(obj, times)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
survfit object
}
  \item{times}{
%%     ~~Describe \code{times} here~~
}
}
\details{
Get survprob from pec packages
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
 from pec packages
}
\author{
pec
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
function (obj, times) 
{
    if (!inherits(obj, "survfit")) 
        stop("obj is not of class survfit")
    class(obj) <- NULL
    lt <- length(times)
    nsurv <- times
    if (length(times) == length(obj$time)) {
        if (all(times == obj$time)) 
            return(obj$surv)
    }
    inside <- times \%in\% obj$time
    for (i in (1:lt)) {
        if (inside[i]) 
            nsurv[i] <- obj$surv[obj$time == times[i]]
        else {
            less <- obj$time[obj$time < times[i]]
            if (length(less) == 0) 
                nsurv[i] <- 1
            else nsurv[i] <- obj$surv[obj$time == max(less)]
        }
    }
    nsurv
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
