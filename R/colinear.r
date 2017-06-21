







library(reshape2)


colinear <- function(df){

  corTbl <- round(abs(cor(df[,unlist(lapply(df, is.numeric))])),2)
  corTbl[corTbl < .80] = 0
  corTbl[corTbl == 1] = 0
  corTbl <- as.data.frame(corTbl)
  corTbl$tmp <- rowSums(corTbl)
  corTbl <- subset(corTbl,tmp > 0)
  corTbl$tmp <- NULL
  return(corTbl)
  
  
  corTbl$var <- rownames(corTbl)
  corTbl <- melt(corTbl,id = 'var')
  corTbl <-subset(corTbl,value > 0)[,c(2,1,3)]
  corTbl$Keep = ''
  corTbl$variable <- as.character(corTbl$variable)
  corTbl$Keep[1] = 'Yes'
  for(x in 1:nrow(corTbl)){
    
    item <- corTbl[x,]
    if(item$Keep == ''){
    corTbl[corTbl$variable == as.character(item[1]) & corTbl$var == as.character(item[2]) & corTbl$value == as.numeric(item[3]) ,which(colnames(corTbl)=="Keep")] = 'Yes'
    
    corTbl[corTbl$variable == as.character(item[2]) & corTbl$var == as.character(item[1]) & corTbl$value == as.numeric(item[3]) ,which(colnames(corTbl)=="Keep")] = 'No'
    }

  }
  
}









 