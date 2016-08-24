requireNamespace("dplyr")
requireNamespace("magrittr")
utils::suppressForeignCheck(names = "n")
utils::globalVariables(c("n","%>%","select_","group_by_","summarise","mutate"))
GateSummaries <- function(data, classFields){ 
  Gates <- list(); n = NULL
  for(field in 1:(length(classFields))){
    Gates[[field]] <- data %>% select_(classFields[field]) %>% group_by_(classFields[field])  %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
    names(Gates)[[field]] <- paste0("Gate_Summary_",field)
  }
  return(Gates)
}