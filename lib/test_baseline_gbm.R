########################################
### Classification with testing data ###
########################################

test <- function(gbm_model, dat_test){
  ### Input: 
  ###  - the fitted classification model using training data
  ###  - processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  
  ### make predictions
  pred <- predict.gbm(gbm_model$model, newdata = dat_test, n.trees = gbm_model$k, type = "response")
  pred_class <- round(pred)
  return(list(pred, pred_class))
}
