\name{makeFlow-package}
\alias{makeFlow-package}
\docType{package}
\title{
\packageTitle{makeFlow}
}
\description{
\packageDescription{makeFlow}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{makeFlow}
\packageIndices{makeFlow}
Users should ensure all classFields (columns) are explicitly defined in the same dataset.
colorCount(), FlowSummaries(), GateSummaries(), and makeFlow() can all operate with the same two basic inputs: data and classFields. Graphical parameters can be defined with additional makeFlow() arguments.
}
\author{
\packageAuthor{makeFlow}

Maintainer: \packageMaintainer{makeFlow}
}

\keyword{ package }
\keyword{ hplot }

\examples{
##  Data:
##  carData <- mtcars
##  carData$car <- "All Cars"
##  carData$speedclass <- ifelse(carData$qsec < 15, "Fast",
##                          ifelse(carData$qsec < 18, "Mid-Speed","Slow"))
##  carData$speedclass <- factor(x = carData$speedclass, levels = c("Slow","Mid-Speed","Fast"))
##  
##  Create Diagram:
##  makeFlow(data = carData, classFields = c("car","cyl","speedclass"),
##    gateWidth = 20, minVerticalBtwnGates = .15, distanceBtwnGates = 70,
##    fieldLabels = c("", "Cylinders","Speed"), gateBorder = "black")

##  Generate underlying tables using GateSummaries() and FlowSummaries()
}
