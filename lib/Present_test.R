library(tidyverse)
# ---------------# Baseline GBM
# ---------------# IMPORTANT NOTE
# ---------------# PLEASE TRAIN THE MODEL before this test!!!
setwd("../lib")
source("feature.R")
source("feature_default.R")
source("img_process.R")
source("test_baseline_gbm.R")
source("xgb_test.R")
run.test.real <- FALSE # Change this to TRUE when actually running!!!

test_dir <- "../data/test_set_predict/" # For the presentation test set
# test_image_dir <- paste(test_dir, "images/", sep="")
test_pt_dir <- paste(test_dir,  "points/", sep="")
test_label_path <- paste(test_dir, "label_prediction.csv", sep="") 
if (run.test.real == TRUE){
  info <- read.csv(test_label_path)
  info$label <- -1 # assign the label to avoid problem not having it
}
if (run.test.real == TRUE){
  readMat.matrix <- function(index){
    return(round(readMat(paste0(test_pt_dir, sprintf("%04d", index), ".mat"))[[1]],0))
  }
  #load fiducial points
  fiducial_pt_list <- lapply(1:n_files, readMat.matrix)
}
test_index <- info$Index
real_test <- feature_default(fiducial_pt_list, test_index)
# Loading GBM
load("../output/fit_train.RData")
pred_gbm <- test(fit_train, real_test)


# ---------------# XGBoost
# ---------------# IMPORTANT NOTE
# ---------------# PLEASE TRAIN THE MODEL before this test!!!

load("../output/xgb_model.RData")
fiducial_pt_list_processed <- list()
for (i in 1:n){
  fiducial_pt_list_processed[[i]] <- img_process(fiducial_pt_list[[i]])
}
real_test_xgb <- feature(fiducial_pt_list_processed, test_index)
pred_xgb <- test(xgb_model, real_test_xgb)

# Safe csv file

info$label <- pred_gbm
info$Advanced <- pred_xgb
info <- info %>% rename(Baseline = label)
write.csv(info, "../output/Realtest.csv", row.names = FALSE)
