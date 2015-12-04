\name{doMultiCore}
\alias{doMultiCore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
doMultiCore
}
\description{
Separate a BionavigatoR "per cell"process over the number of avaialble CPU cores on the local machine. Firewall permission may be required.
}
\usage{
doMultiCore(rhsFormula, data, operatorFunction, .export = NULL, .packages = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rhsFormula}{
  "plyr-style" right hand sided formula splitting the input data frame, e.g. ~rowSeq+ colSeq
}
  \item{data}{
  Data frame, containing the data to be processed in part. The terms of rhsFormula must appear as column names.
}
  \item{operatorFunction}{
  Function to apply to the data parts
}
  \item{.export}{
  array of strings listing user defined functions or other symbols that are used by the operatorFunction AND the            operatorFunction itself! 
}
  \item{.packages}{
  array of strings listing any packages that are used by operatorFunctions
}
  \item{\dots}{
  other arguments to be passed to operator function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data frame with results
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(pgMulticore)
data(BionavigatoR)

quadreg = function(aFrame){
    aLm = lm(value ~ Cycle + I(Cycle^2), data = aFrame)
    result = c(aFrame$rowSeq[1], aFrame$colSeq[1], coef(aLm))
    names(result) = c("rowSeq", "colSeq", "p0", "p1", "p2")
    return(result)
}

aResult = doMultiCore(~rowSeq + colSeq, aFlatFile, quadreg, .export = c("quadreg"))

## The function is currently defined as
function (rhsFormula, data, operatorFunction, .export = NULL, 
    .packages = NULL, ...) 
{
    aFormTerms = terms(rhsFormula)
    if (attr(aFormTerms, "response")) 
        stop("Please use a right hand sided formula only")
    term.labels = attr(aFormTerms, "term.labels")
    bFound = term.labels \%in\% colnames(data)
    if (any(!bFound)) {
        notFound = term.labels[!bFound]
        msg = paste("Could not find the following columns in the data frame", 
            notFound)
        stop(msg)
    }
    cell.split = drop(interaction(data[term.labels]))
    nCores = detectCores()
    cpu.split = subdivCells(cell.split, nCores)
    aSplitList = split(data, cpu.split)
    cl = makeCluster(nCores)
    registerDoParallel(cl)
    print(paste("Please wait, processing on ", nCores, " CPU cores ..."))
    anExportList = c(.export, "doSingleCore")
    aPackagesList = c(.packages, "plyr")
    aResult = foreach(i = 1:nCores, .combine = rbind, .packages = aPackagesList, 
        .export = anExportList) \%dopar\% doSingleCore(aSplitList[[i]], 
        rhsFormula, operatorFunction, aProgress = "none", ...)
    stopCluster(cl)
    return(aResult)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
