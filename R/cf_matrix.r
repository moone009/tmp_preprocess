
# Required Package for multinomial logit
library(nnet)
library(caret)

F1 score considers both recall and precision to measure accuracy. It can be interpreted as a weighted average of the precision and recall 

Recall:  the fraction of relevant instances that are retrieved. Recall can be interpreted as the number of correct keywords assigned, divided by all keywords deemed relevant to the object.

Precision:  the fraction of retrieved instances that are relevant. Precision can be interpreted as the number of keywords judged as correct divided by the total number of keywords assigned
https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/
  
  https://github.com/moone009/AnalyticsProcess/blob/master/R/Custom_Functions/PairingCols.r
https://msdn.microsoft.com/library/azure/927d65ac-3b50-4694-9903-20f6c1672089/#bkmk_classification.
  
  https://github.com/moone009/Metrics/blob/master/R/R/metrics.r

##_____________________________________________________________________________________________________________________________
# init function

cf_matrix <- function(tbl){
  
  # Set vars
  recall <- c()
  precision <- c()
  
  for(i in 1:floor(sqrt(length(tbl)))){
    
    recall <- c(recall,tbl[,i][i]/colSums(tbl)[i])
    
    precision <- c(precision,tbl[,i][i]/rowSums(tbl)[i])
  }
  
  accuracy <- sum(diag(tbl))/sum(tbl)
  ka <- sum(colSums(tbl)*rowSums(tbl)/sum(tbl))/sum(tbl)
  Kappa <- (accuracy-ka)/(1-ka)

  # Wrap suppress warning because accuracy will repeat which is what I want
  Results <-  suppressWarnings(cbind(tbl,Precision = round(precision,2),Recall =  round(recall,2),Overall_Accuracy =round(accuracy,2),Overall_Kappa = Kappa))
  return(Results)
  
}

##_____________________________________________________________________________________________________________________________
# Test and Train
trainIndex <- createDataPartition(iris$Species, p = .5,
                                  list = FALSE,
                                  times = 1)
Train <- iris[ trainIndex,]
Test  <- iris[-trainIndex,]

##_____________________________________________________________________________________________________________________________
# Model
model <- multinom(Species ~ ., data = Train)

predictions <- predict(model,Test)

xtab <- table(predictions, Test[['Species']])

##_____________________________________________________________________________________________________________________________
# Model Analysis
cf_matrix(xtab)

confusionMatrix(xtab)

((27*25)/75)+((24*25)/75)
((71/75)-.22)/(1-.22)


ka <- sum(colSums(xtab)*rowSums(xtab)/sum(xtab))/sum(xtab)
acc <- sum(diag(xtab))/sum(xtab)
(acc-ka)/(1-ka)





