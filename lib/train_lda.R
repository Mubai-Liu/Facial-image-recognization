###########################################################
### Train a classification model with training features ###
###########################################################
train_lda <- function(feature_df = dat_train, par = NULL){
  ### Train an SVM model using processed features from training images
  
  ### Input:
  ### - a data frame containing features and labels
  ### - a parameter list
  ### Output: trained model
  
  ### load libraries
  library(MASS)
  
  ### set seed
  set.seed(2020)
  
  model <- lda(label~., data = feature_df)
  
  return(model)
}
