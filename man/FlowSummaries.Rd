\name{FlowSummaries}
\alias{FlowSummaries}
\title{
FlowSummaries()
}
\description{
Outputs a list of objects of class tbl_df containing values presented in a makeFlow() diagram utilizing the same data and classFields arguments.
}
\usage{
FlowSummaries(data, classFields)
}
\arguments{
  \item{data}{
An object of class data.frame in which all specified classFields (column names) can be found.
}
  \item{classFields}{
A vector of the column names intended to be represented in the makeFlow() diagram. Each element must be a string.
}
}
\details{
Outputs tables with standard naming convention of Flow_Summary_x. For example, Flow_Summary_1 provides the counts and frequencies of each category (gate) from the first column specified in classFields moving to each of the categories (gates) in the second column listed in classFields.
}

\author{
Alex J. Krebs
}
\note{
Relies on Hadley Wickham's dplyr package.
}


\examples{
## myFlows <- FlowSummaries(data= shelters, classFields= c("loc","Jan","Feb","Mar"))
## Flow_Summary_2 will show the counts and percentages from
## "Jan" categories that move to the categories in "Feb"
}
