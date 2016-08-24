requireNamespace("dplyr")
requireNamespace("magrittr")
utils::suppressForeignCheck(names = "n")
utils::globalVariables(c("n","%>%","select_","group_by_","summarise","mutate"))
FlowSummaries <- function(data,classFields){
  Flows <- list(); n=NULL
  for(field in 1:(length(classFields)-1)){
    Flows[[field]] <- data %>% select_(classFields[field],classFields[field+1]) %>% group_by_(classFields[field],classFields[field+1]) %>% summarise(n=n()) %>% mutate(freq=n/sum(n))
    names(Flows)[[field]] <- paste0("Flow_Summary_",field)
  }
  return(Flows)
}