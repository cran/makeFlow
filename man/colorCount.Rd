\name{colorCount}
\alias{colorCount}

\title{
colorCount()
}
\description{
Returns an integer representing the number of unique categories from all specified fields. This value should serve as a guide for users specifying colors in the makeFlow() function.
}
\usage{
colorCount(data, classFields)
}
\arguments{
  \item{data}{
An object of class data.frame in which all specified classFields (or column names) can be found.
}
  \item{classFields}{
A vector of the column names intended to be represented in a makeFlow() diagram. Each element must be a string.
}
}

\author{
Alex J. Krebs
}
\note{
Relies on Hadley Wickham's dplyr package.
}
