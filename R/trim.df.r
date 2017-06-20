#' @title Calculates Regression Metrics
#' @description
#' Function to calculate regression metrics
#' 
#' @param FileDir is your regression model LM or MLR
#' @param Seperator is the data used to train your model
#' @param IsHeader is the value your model is attempting to predict 
#' @param quote number of features used to in the model
#' @export
#' 

trim.df <- function(df){
  
  for(x in 1:length(df)){
    
    if(class(df[,x]) == 'factor' | class(df[,x]) == 'character' ){df[,x] <- trim(df[,x])}
  }
  return(df)
}
