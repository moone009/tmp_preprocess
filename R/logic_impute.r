
RMSE <- function(y,y_pred){sqrt(mean((y-y_pred)^2))}
MSE <- function(y,y_pred){mean((y-y_pred)^2)}
MAE <- function(y,y_pred){mean(y-y_pred)}

impute <-function(df,col,mean,median,linear){
  
  if(class(df[,col]) %in% c('factor','character')){
    stop('Value is not numeric or integer, stopping operation')
    
  }
  
  test <- df[complete.cases(df),]
  testCases <- test[sample(nrow(test), nrow(test)*.1), ]
  trainCases <- test[-c(as.numeric(rownames(testCases))),]
  
  MeanVal <- mean(trainCases[,col])
  MedianVal <- median(trainCases[,col])
  form <- paste(colnames(df)[col],'~.')
  mod <- lm(as.formula(form),data=trainCases)
  Predictions <- predict(mod,testCases)
  
  blended <- (Predictions*.33+(MeanVal*.33)+(MedianVal*.33))
  
  MeanVal <- data.frame(Method = 'Mean',MSE = MSE(testCases[,col],MeanVal), MASE = MAE(testCases[,col],MeanVal),RMSE = RMSE(testCases[,col],MeanVal))
  MedianVal <- data.frame(Method = 'Median',MSE = MSE(testCases[,col],MedianVal), MASE = MAE(testCases[,col],MedianVal),RMSE = RMSE(testCases[,col],MedianVal))
  LinearModel <- data.frame(Method = 'LinearModel',MSE = MSE(testCases[,col],Predictions), MASE = MAE(testCases[,col],Predictions),RMSE = RMSE(testCases[,col],Predictions))
  BlendedModel <- data.frame(Method = 'BlendedModel',MSE = MSE(testCases[,col],blended), MASE = MAE(testCases[,col],blended),RMSE = RMSE(testCases[,col],blended))
  
  output <- rbind(MeanVal,MedianVal,LinearModel,BlendedModel)
  bestOption <- as.character(output[which(output$RMSE ==  min(output$RMSE)),1])
  print(paste('Based upon using a sample of 10% of the complete cases the imputer ran all three methods and believes',bestOption, 'is the best method'))
  print(output)
  
  if(mean==T){
    print('imputing mean')
    df[which(is.na(df[,col])==T),][col] <-  mean(df[which(is.na(df[,col])==F),][,col])
  } else if(median==T){
    print('imputing median')
    df[which(is.na(df[,col])==T),][col] <-  median(df[which(is.na(df[,col])==F),][,col])
  } else{
    print('imputing regression')
    form <- paste(colnames(df)[col],'~.')
    mod <- lm(as.formula(form),data=df[which(is.na(df[,col])==F),])
    df[which(is.na(df[,col])==T),][col] <- predict(mod,df[which(is.na(df[,col])==T),][-col])
  }
  return(df)
  
}

#iris$Petal.Length[c(1,5,7,3)] <- NA
#impute(iris,3,F,F,T)