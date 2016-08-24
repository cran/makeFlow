requireNamespace("dplyr")
requireNamespace("magrittr")
colorCount <-
function(data, classFields){
  FieldCats <- c()
  unique_cats <- c()
  for(i in 1:length(classFields)){
    temp <- dplyr::select_(.data = data,classFields[i])
    temp <- dplyr::group_by_(.data = temp,classFields[i])
    temp <- dplyr::summarise(.data = temp)
    FieldCats <- c(FieldCats,nrow(temp))
    unique_cats <- c(unique_cats,temp[,1])
  }
  
  unique_cats <- unique(as.character(unlist(unique_cats)))
  return(length(unique_cats))
}
