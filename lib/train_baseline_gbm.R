###########################################################
### Train a classification model with training features ###
###########################################################

# Base model training method
# train <- function(features, labels, w = NULL, l = 1){
#   model <- glmnet(features, labels, weights = w, alpha = 1, family = "binomial", lambda = l)
#   return(model)
# }


train <- function(feature_df, weights = NULL, par = NULL){
  ### Train an GBM model using processed features from training images
  
  ### Input:
  ### - a data frame containing features and labels
  ### - a parameter list
  ### Output: trained model
  
  ### Train with GBM
  if(is.null(par)){
    k = 150
  } else {
    k = par$k
  }
  feature_df <- feature_df %>%
    mutate(label = as.character(label))
  model <- gbm(label ~., data = feature_df, weights = weights,
               distribution = "bernoulli", n.trees = k) 
  
  gbm_model <- list(model = model, k = k)
  
  return(gbm_model)
}
