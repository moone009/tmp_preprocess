#' @title ReplaceNA
#' @description
#' Process to replace NA values within a dataframe, typlically this would be used for categorical and ordinal variables
#' 
#' @param df is a datafame
#' @param replacement is the value that will replace NA
#' @return dataframe with replaced NA 
#' @export

replaceNA <- function(df,replacement){
  
  li <- names(which(colSums(is.na(df)) > 0))
  if(length(li)> 0){
    for(x in 1:length(li)){
      
      df[,li[x]] <- as.character(df[,li[x]])
      df[,li[x]][is.na(df[,li[x]])] <- replacement   
      
    }
  }
  return(df)
}
