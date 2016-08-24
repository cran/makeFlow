\name{GateSummaries}
\alias{GateSummaries}
\title{
GateSummaries()
}
\description{
Outputs a list of objects of class tbl_df containing the values in a makeFlow() diagram utilizing the same data and classFields arguments.
}
\usage{
GateSummaries(data, classFields)
}
\arguments{
  \item{data}{
An object of class data.frame in which all specified classFields (column names) can be found.
}
  \item{classFields}{
A vector of the column names intended to be represented in a makeFlow() diagram. Each element must be a string.
}
}
\details{
Outputs tables with standard naming convention of Gate_Summary_x. For example, Gate_Summary_1 provides the counts and frequencies of each category within the first column specified in the classFields argument.
}

\author{
Alex J. Krebs
}
\note{
Relies on Hadley Wickham's dplyr package.
}

\examples{
## myGates <- GateSummaries(data = shelters, classFields = c("loc","Jan","Feb","Mar"))
## Gate_Summary_2 will show the count and percentage of observations
##  within each category of the column "Jan"
}