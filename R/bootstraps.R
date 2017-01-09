#' @title bootstraps
#' 
#' @param df A dataframe
#' @param sample.percent percent of the dataframe to sample
#' @param samples number of bootstraps to return
#' @return Vector containing N bootstraps
#' @import data.table
#' @export
#' @examples
#' bootstraps(iris,.05,10)

bootstraps <- function(df,sample.percent,samples){
  requireNamespace("data.table", quietly = TRUE)
  dataframes <- vector(mode = "list", length = samples)
  df <- data.table(df)
  for(x in 1:length(dataframes)){
    dataframes[[x]] <- df[sample(.N,  as.integer(nrow(df)* sample.percent))]
  }
  return(dataframes)
  
}
