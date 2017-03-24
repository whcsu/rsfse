\name{rsfes.bs_predict}
\alias{rsfes.bs_predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prediction with new data and return a saved forest brier score function 
}
\description{
Prediction with new data and return a saved forest brier score function 
}
\usage{
rsfes.bs_predict(rsfesfit, testdat, rii, trlength = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rsfesfit}{
%%     ~~Describe \code{rsfesfit} here~~
}
  \item{testdat}{
%%     ~~Describe \code{testdat} here~~
}
  \item{rii}{
%%     ~~Describe \code{rii} here~~
}
  \item{trlength}{
%%     ~~Describe \code{trlength} here~~
}
}
\details{
Prediction with new data and return a saved forest brier score function 
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
HONG WANG
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
function (rsfesfit, testdat, rii, trlength = 500) 
{
    trees = rsfesfit$pectrees
    colindexes = rsfesfit$colindexes
    newindexes = rsfesfit$newindexes
    newdata = testdat[, -c(rii)]
    if (trlength > length(rsfesfit$pectrees)) 
        stop("Number of Trees for prediction should not be more than Number of Trees Fitted")
    testpre <- NULL
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
            newtestdat = cbind.data.frame(testdat[, c(rii)], 
                testdata)
            pecerror <- pec(list(rsfse = trees[[i]]), formula = Surv(time, 
                status) ~ ., data = newtestdat, cens.model = "marginal", 
                reference = FALSE)
            print((pecerror))
            pecerror$AppErr$rsfse[is.na(pecerror$AppErr$rsfse)] = 0
            predicts = crps(pecerror)[1]
            print(crps(pecerror))
            testpre <- cbind(predicts, testpre)
        }
    }
    ensemble_predictions <- rowMeans(testpre)
    return(ensemble_predictions)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
