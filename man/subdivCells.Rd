\name{subdivCells}
\alias{subdivCells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
subdivCells
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
subdivCells(cellID, numDiv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cellID}{
%%     ~~Describe \code{cellID} here~~
}
  \item{numDiv}{
%%     ~~Describe \code{numDiv} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (cellID, numDiv) 
{
    cellID = as.factor(cellID)
    nCells = length(levels(cellID))
    idx = subdiv(nCells, numDiv)
    dvIdx = vector(length = length(cellID))
    for (i in 1:numDiv) {
        dvIdx[cellID \%in\% levels(cellID)[idx == i]] = i
    }
    return(dvIdx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
