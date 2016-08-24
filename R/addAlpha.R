requireNamespace("grDevices")
addAlpha <-
function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colors.")
  apply(sapply(col, grDevices::col2rgb)/255, 2, function(x) {grDevices::rgb(x[1], x[2], x[3], alpha=alpha)} )  
}
