#' Draw Bootstraps
#' 
#' @param df A dataframe
#' @param sample.percent percent of the dataframe to sample
#' @param bootstraps number of bootstraps to return
#' @return Vector containing N bootstraps
#' @examples
#' bootstraps(iris,.05,10)



bootstraps <- function(df,sample.percent,bootstraps){
  
  dataframes <- vector(mode = "list", length = bootstraps)
  require(data.table)
  df <- data.table(df)
  for(x in 1:length(dataframes)){
  dataframes[[x]] <- df[sample(.N,  as.integer(nrow(df)* sample.percent))]
  }
  return(dataframes)
  
}