library(gbm)
library(R.matlab)
library(readxl)
library(dplyr)
library(EBImage)
library(ggplot2)
library(caret)
library(glmnet)
library(WeightedROC)
library(geometry)
library(xgboost)
library(e1071)
library(ROCR)
# ---------------# Baseline GBM
# ---------------# IMPORTANT NOTE
# ---------------# PLEASE TRAIN THE MODEL before this test!!!
setwd("../lib")
source("feature.R")
source("feature_default.R")
source("img_process.R")
source("test_baseline_gbm.R")
source("xgb_test.R")
run.test.real <- TRUE # Change this to TRUE when actually running!!!

test_dir <- "../data/test_set_predict/" # For the presentation test set
test_image_dir <- paste(test_dir, "images/", sep="")
test_pt_dir <- paste(test_dir,  "points/", sep="")
test_label_path <- paste(test_dir, "label_prediction.csv", sep="") 
if (run.test.real == TRUE){
  info <- read.csv(test_label_path)
  info$label <- -1 # assign the label to avoid problem not having it
}
if (run.test.real == TRUE){
  n_files <- length(list.files(test_image_dir))
  image_list <- list()
}
if (run.test.real == TRUE){
  readMat.matrix <- function(index){
    return(round(readMat(paste0(test_pt_dir, sprintf("%04d", index), ".mat"))[[1]],0))
  }
  #load fiducial points
  fiducial_pt_list <- lapply(1:n_files, readMat.matrix)
}
# 1 mins for above code


test_index <- info$Index
real_test <- feature_default(fiducial_pt_list, test_index)
# Loading GBM
load("../output/fit_train.RData")
real_test <- real_test %>% select(-label)
pred_gbm <- test(fit_train, real_test)[[1]]

# 1 mins for above code
# ---------------# XGBoost
# ---------------# IMPORTANT NOTE
# ---------------# PLEASE TRAIN THE MODEL before this test!!!

load("../output/xgb_model.RData")
fiducial_pt_list_processed <- list()
for (i in 1:n){
  fiducial_pt_list_processed[[i]] <- img_process(fiducial_pt_list[[i]])
}
# 16m45s to run

real_test_xgb <- feature(fiducial_pt_list_processed, test_index)
real_test_xgb <- real_test_xgb %>% select(-label)
pred_xgb <- xgb_test(xgb_model, real_test_xgb)[[1]] %>% round()

# Safe csv file

info$label <- pred_gbm
info$Advanced <- pred_xgb
info <- info %>% rename(Baseline = label)
write.csv(info, "../output/Realtest.csv", row.names = FALSE)

# 1 mins