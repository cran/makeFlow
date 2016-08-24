\name{makeFlow}
\alias{makeFlow}
\title{
makeFlow()
}
\description{
Uses the selected dataset and specified order of columns to produce a left-to-right flow diagram. This function assumes the use of a single dataset and categorical variables resembling observations' movement from one state to another, such that at every state, every observation can be mapped. Colors bridging states assume the color of the gate from which they originate.
}
\usage{
makeFlow(data, classFields, rotate = F, gatecolors = NA, minVerticalBtwnGates = 0.1, 
connectingAlpha = 0.5, bg = "white", plotTitle = "", titleAdj = 0.5, txtColor = "black", 
distanceBtwnGates = 50, gateWidth = 7, gateBorder = NA, labels = T, fieldLabels = NA, 
showPercentages = T, showConnectPercentages = F, percentTextColor = "black", 
showCounts = T, countTextColor = "black")
}
\arguments{
  \item{data}{
An object of class data.frame in which all specified classFields (column names) can be found.
}
  \item{classFields}{
A vector of the column names intended to be represented in a makeFlow() diagram. Each element in this vector must be a string.
}
  \item{rotate}{
Set to TRUE to rotate all text. This is a work-around to allow for a vertical (top-down) flow. Manual manipulation after exporting a diagram is needed to adjust the orientation of the output.
}
  \item{gatecolors}{
A vector of colors to apply to gates. For greater control, the alphabetized order of categories from all selected classFields will match the corresponding color in this list. If the length of this vector is shorter than the number of unique categories in the full diagram, a predefined palette "Set3" is substituted in its place, and a warning message will alert the user of how many colors are required for manual input. If more colors are supplied than necessary, only the required number will be taken from the beginning of the vector. (default is NA, which will assign colors from palette "Set3")
}
  \item{minVerticalBtwnGates}{
A single value (likely between 0 and 1) used to determine the minumum plotted gap between categories (gates) within the same classField. For reference, the default plot height is 1. (default is 0.1)
}
  \item{connectingAlpha}{
A single decimal value [0,1] setting the opacity of the "flows" connecting gates. (default is 0.5)
}
  \item{bg}{
A single color from colors() specifying the diagram's background color. (default is "white")
}
  \item{plotTitle}{
The title of the diagram. (default is "")
}
  \item{titleAdj}{
A single value [0,1] adjusting the title's horizontal placement. 0 implies left-align; 1 impllies right-align. (default is 0.5)
}
  \item{txtColor}{
A single color from colors() specifying the text color in the diagram. (default is "black")
}
  \item{distanceBtwnGates}{
A single numeric value representing the horizontal distance between classFields. (default is 50)
}
  \item{gateWidth}{
A single numeric value representing the horizontal width of gates. (default is 7)
}
  \item{gateBorder}{
A single color from colors() specifying the color of all gates' borders. (default is NA)
}
  \item{labels}{
Set to FALSE to not see the category labels above each gate.
}
  \item{fieldLabels}{
A character vector of names to identify each classField in the diagram. These labels will be placed below each set of gates. If labels is FALSE, fieldLabels will not be displayed. (default is NA)
}
  \item{showPercentages}{
Set to FALSE to hide the percentage values within gates.
}
  \item{showConnectPercentages}{
Set to FALSE to hide the percentage values within flows.
}
  \item{percentTextColor}{
A single color from colors() for the gate percentage and flow percentage texts. (default is "black")
}
  \item{showCounts}{
Set to FALSE to hide the counts of observations within each gate.
}
  \item{countTextColor}{
A single color from colors() for the gate count text. (default is "black")
}
}

\author{
Alex J. Krebs
}
\note{
Relies on Hadley Wickham's dplyr package to generate summaries. Gates within each field are ordered alphabetically, numerically, or (if applicable) in the order of a factor's levels.
}
\examples{
## makeFlow(data = shelters, classFields = c("loc","Jan","Feb","Mar"),
##          fieldLabels = c("","Jan","Feb","Mar"), gateWidth = 20)
}

\keyword{ hplot }
