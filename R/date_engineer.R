#' @title Feature Engineering Dates
#' 
#' @param df A dfframe
#' @param col is numeric index value of date column to be engineered
#' @param holiday True or False if ouput should return date values
#' @return dataframe with date values engineered
#' @importFrom lubridate month year day quarter week
#' @export

date_engineer <- function(df,col,holiday){
  #require(lubridate)
  df[paste(col,'_mon',sep='')] = month(df[[col]])
  df[paste(col,'_year',sep='')] = year(df[[col]])
  df[paste(col,'_day',sep='')] = day(df[[col]])
  df[paste(col,'_quarter',sep='')] = quarter(df[[col]])
  df[paste(col,'_week',sep='')] = week(df[[col]])
  
  # Major Holidays
  # within 7 days of Christmas, Easter, 4th July, Memorial Day, Labor Day, New Years Eve, Thanks Giving
  
  if(holiday == T){
    Holiday <- function(x,month = 12,day = 25){ 
      Y = as.numeric(x - as.Date(paste(year(df[[col]]),'-',month,'-',day,sep='')))   
      if(Y <= 0 & Y >= -1){1}else{0}
    }
    thanksgiving <- c('23-Nov-2000','22-Nov-2001','28-Nov-2002','27-Nov-2003','25-Nov-2004','24-Nov-2005','23-Nov-2006',
                      '22-Nov-2007','27-Nov-2008','26-Nov-2009','25-Nov-2010','24-Nov-2011','22-Nov-2012','28-Nov-2013',
                      '27-Nov-2014','26-Nov-2015','24-Nov-2016','23-Nov-2017','22-Nov-2018','28-Nov-2019','26-Nov-2020')
    
    df[paste(col,'_Christmas',sep='')] =  sapply(df[[col]],Holiday,month=12,day=25)  
    df[paste(col,'_ChristmasEve',sep='')] =  sapply(df[[col]],Holiday,month=12,day=24)       
    df[paste(col,'_Independence',sep='')] =  sapply(df[[col]],Holiday,month=07,day=04)  
    
    #df[paste(col,'_Easter',sep='')] =  sapply(df[[col]],Holiday,month=05,day=04)       
    #df[paste(col,'_Memorial',sep='')] =  sapply(df[[col]],Holiday,month=07,day=04)       
    #df[paste(col,'_NYE',sep='')] =  sapply(df[[col]],Holiday,month=12,day=31{})       
    #df[paste(col,'_ThanksGiving',sep='')] =  sapply(df[[col]],Holiday,month=07,day=04)       
  }
  return(df)
}
