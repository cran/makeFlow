\name{addAlpha}
\alias{addAlpha}
\title{
addAlpha()
}
\description{
Adds a specified opacity (between 0 and 1) to any color(s) listed.
}
\usage{
addAlpha(col, alpha = 1)
}
\arguments{
  \item{col}{
A vector of one or many colors.
}
  \item{alpha}{
A value between 0 and 1. 0 indicates complete transparency. 1 indicates complete opacity.
}
}
\value{
Returns the Hexadecimal representation of the modified color(s).
}

\examples{

## The function is currently defined as
function (col, alpha = 1) 
{
    if (missing(col)) 
        stop("Please provide a vector of colors.")
    apply(sapply(col, col2rgb)/255, 2, function(x) {
        rgb(x[1], x[2], x[3], alpha = alpha)
    })
  }
}
