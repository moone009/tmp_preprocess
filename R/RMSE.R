#' @title Calculate Root Mean Square Error
#' @description
#' Function to calculate the root mean square
#' 
#' @param y is the value of your actual
#' @param y_pred is the value that your model predicted
#' @return the RMSE
#' @export
#' 
RMSE <- function(y,y_pred){sqrt(mean((y-y_pred)^2))}
