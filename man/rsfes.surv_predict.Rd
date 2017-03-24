\name{rsfes.surv_predict}
\alias{rsfes.surv_predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prediction with new data and return a saved forest with mean surv prob at each time points 
}
\description{
Prediction with new data and return a saved forest with mean surv prob at each time points 
}
\usage{
rsfes.surv_predict(rsfesfit, newdata, uniquetimes, trlength = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rsfesfit}{
%%     ~~Describe \code{rsfesfit} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{uniquetimes}{
%%     ~~Describe \code{uniquetimes} here~~
}
  \item{trlength}{
%%     ~~Describe \code{trlength} here~~
}
}
\details{
Prediction with new data and return a saved forest with mean surv prob at each time points 
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
function (rsfesfit, newdata, uniquetimes, trlength = 500) 
{
    trees = rsfesfit$pectrees
    colindexes = rsfesfit$colindexes
    newindexes = rsfesfit$newindexes
    if (trlength > length(rsfesfit$pectrees)) 
        stop("Number of Trees for prediction should not be more than Number of Trees Fitted")
    testpre <- matrix(0, nrow = dim(newdata)[1], ncol = length(uniquetimes))
    colnames(testpre) = paste0(uniquetimes)
    for (i in 1:trlength) {
        {
            if (ncol(newdata) <= 100) {
                testdata = extspace_testdat(newdata, newindexes[[i]])
                testdata = testdata[, colindexes[[i]]]
            }
            else {
                testdata = newdata[, colindexes[[i]]]
                testdata = extspace_testdat(testdata, newindexes[[i]])
            }
            testdata = as.data.frame(testdata)
            predicts = predictSurvProb(trees[[i]], testdata, 
                uniquetimes)
            predicts[is.na(predicts)] = 0
            colnames(predicts) = paste0(uniquetimes)
            testpre <- testpre + predicts
        }
    }
    return(testpre/trlength)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
