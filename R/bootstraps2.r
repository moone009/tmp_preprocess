#' @title Calculates Regression Metrics
#' @description
#' Function to calculate regression metrics
#' 
#' @param df is your regression model LM or MLR
#' @param samples is the data used to train your model
#' @param records is the value your model is attempting to predict 
#' @param randomVars number of features used to in the model
#' @param Target number of features used to in the model
#' @export
#' 



library(dplyr)
bootstraps <- function(df,samples,records,randomVars = F,Target=F){
  
  li <- list()
  for(x in 1:samples){
    if(randomVars==T){
      li[[x]] <- sample_n(df[,c(sample(1:length(df), round(length(df)*.7,0), replace=F))], records)
    }else{
      li[[x]] <- sample_n(df, records)
    }
  }
  
  return(li)
  
}