#' @title Mean absolute Error
#' @description
#' Function to calculate the mean absolute error
#' 
#' @param y is the value of your actual
#' @param y_pred is the value that your model predicted
#' @return the MAE
#' @export
#' 
MAE <- function(y,y_pred){mean(y-y_pred)}
