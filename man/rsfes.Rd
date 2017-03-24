\name{rsfes}
\alias{rsfes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grow a random survival forest with space extensions
}
\description{
Grow a random survival forest with space extensions
}
\usage{
rsfes(x, y, trlength = 500, mtry = floor(sqrt(ncol(x))), control = control, na.action = na.omit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{trlength}{
%%     ~~Describe \code{trlength} here~~
}
  \item{mtry}{
%%     ~~Describe \code{mtry} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{na.action}{
%%     ~~Describe \code{na.action} here~~
}
}
\details{
Grow a random survival forest with space extensions
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
function (x, y, trlength = 500, mtry = floor(sqrt(ncol(x))), 
    control = control, na.action = na.omit) 
{
    Call <- match.call()
    if (!inherits(y, "Surv")) 
        stop("Response must be a 'survival' object - use the 'Surv()' function")
    ny <- ncol(y)
    n <- nrow(y)
    status <- y[, ny]
    survtime = y[, 1L]
    if (any(survtime <= 0)) 
        stop("Observation time must be > 0")
    if (all(status == 0)) 
        stop("No deaths in training data set")
    if (!missing(control)) 
        controls[names(control)] <- control
    pectrees <- vector(mode = "list", length = trlength)
    colindexes <- vector(mode = "list", length = trlength)
    newindexes <- vector(mode = "list", length = trlength)
    varimp <- NULL
    for (i in 1:trlength) {
        if (ncol(x) <= 100) {
            newx = extspace_dat(x)
            newindexes[[i]] = newx$ncombsub
            colindex = sample(ncol(newx$newxdata), size = mtry * 
                2)
            colindexes[[i]] = colindex
            newxdata = newx$newxdata[, colindex]
        }
        else {
            colindex = sample(ncol(x), size = mtry)
            colindexes[[i]] = colindex
            newx = extspace_dat(x[, colindex])
            newindexes[[i]] = newx$ncombsub
            newxdata = newx$newxdata
        }
        mf = data.frame(y[, 1], y[, 2], newxdata)
        colnames(mf)[c(1, 2)] = c("time", "status")
        trainindex = sample(nrow(mf), replace = T)
        trset = mf[trainindex, ]
        pectrees[[i]] = pecRpart(Surv(time, status) ~ ., data = trset)
    }
    fit = pectrees
    class(fit) <- "rsfes"
    return(list(pectrees = pectrees, colindexes = colindexes, 
        newindexes = newindexes))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
