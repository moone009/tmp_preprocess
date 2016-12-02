#' @title Mean Square Error
#' @description
#' Function to calculate the mean square error
#' 
#' @param y is the value of your actual
#' @param y_pred is the value that your model predicted
#' @return the MSE
#' @export
#' 
MSE <- function(y,y_pred){mean((y-y_pred)^2)}
