###########################################################
### Train a classification model with training features ###
###########################################################
feature_improve <- function(){
  if(run.improved){
    tm_feature_train <- NA
    if(run.feature.train){
      tm_feature_train <- system.time(dat_train <- feature(fiducial_pt_list, train_idx))
      save(dat_train, file="../output/feature_train.RData")
      save(tm_feature_train, file="../output/tm_feature_train.RData")
    }
    
    tm_feature_test <- NA
    if(run.feature.test){
      tm_feature_test <- system.time(dat_test <- feature(fiducial_pt_list, test_idx))
      save(dat_test, file="../output/feature_test.RData")
      save(tm_feature_test, file="../output/tm_feature_test.RData")
    }
  }
}

train_improved <- function(feature_df, w = NULL, par = NULL){
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
  model <- gbm(label ~., data = feature_df, weights = w,
               distribution = "bernoulli", n.trees = k) 
  
  gbm_model <- list(model = model, k = k)
  
  return(gbm_model)
}

