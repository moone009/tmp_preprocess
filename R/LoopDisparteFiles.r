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

LoopDisparteFiles <- function (FileDir,Seperator,IsHeader,quote=F) {
  
  # find the files that you want
  list.of.files <- list.files(FileDir)
  list.of.files <- list.of.files[grep('.csv',list.of.files)]
  
  # Create empty frame
  
  for( i in 1:length(list.of.files)){
    
    FileToRead = paste(FileDir,list.of.files[i],sep="")
    if(quote == T){
      print(FileToRead)
      
      assign(gsub('.csv','',list.of.files[i]),  read.csv(FileToRead,sep=Seperator,quote = "",header = IsHeader), envir = .GlobalEnv)
    }
    if(quote == F){
      print(FileToRead)
      
      assign(gsub('.csv','',list.of.files[i]),  read.csv(FileToRead,sep=Seperator,header = IsHeader), envir = .GlobalEnv)
    }
  }
  
}

LoopDisparteFiles('Y:/R_Packages/Preprocess/data/',",",T,T)