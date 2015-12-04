library(pgMulticore)
data(BionavigatoR)


quadreg = function(aFrame){
    aLm = lm(value ~ Cycle + I(Cycle^2), data = aFrame)
    result = c(aFrame$rowSeq[1], aFrame$colSeq[1], coef(aLm))
    names(result) = c("rowSeq", "colSeq", "p0", "p1", "p2")
    return(result)
}

aResult = doMultiCore(~rowSeq + colSeq, aFlatFile, quadreg, .export = c("quadreg"))
