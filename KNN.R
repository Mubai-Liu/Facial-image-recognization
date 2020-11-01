#fiducial_pt_list[1]
#set.seed(2020)
library(tidyverse)
library(R.matlab)

############################################################################ train ##################################################################3
#ath <- getwd()
#pathname<-file.path(paste0(path,"/data/images"),'0001.jpg')
######for (i in 1:30){
#readImage <-readJPEG(pathname)



if(!require("EBImage")){
  install.packages("BiocManager")
  BiocManager::install("EBImage")
}
if(!require("R.matlab")){
  install.packages("R.matlab")
}
if(!require("readxl")){
  install.packages("readxl")
}

if(!require("dplyr")){
  install.packages("dplyr")
}
if(!require("readxl")){
  install.packages("readxl")
}

if(!require("ggplot2")){
  install.packages("ggplot2")
}

if(!require("caret")){
  install.packages("caret")
}

if(!require("glmnet")){
  install.packages("glmnet")
}

if(!require("WeightedROC")){
  install.packages("WeightedROC")
}

# if(!require("tensorflow")){
#   install.packages("tensorflow")
# }

if(!require("geometry")){
  install.packages("geometry")
}
if(!require("gbm")){
  install.packages("gbm")
}
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


set.seed(2020)

train_dir <- "./data/" # This will be modified for different data sets.
train_image_dir <- paste(train_dir, "images/", sep="")
train_pt_dir <- paste(train_dir,  "points/", sep="")
train_label_path <- paste(train_dir, "label.csv", sep="") 

run.default <- TRUE # run default
run.improved <- TRUE # feature improved
run.cv <- TRUE # run cross-validation on the training set
sample.reweight <- FALSE # run sample reweighting in model training
K <- 5  # number of CV folds
run.feature.train <- TRUE # process features for training set
run.test <- TRUE # run evaluation on an independent test set
run.feature.test <- TRUE # process features for test set

# number of trees
k = c(50,100,150,200,250,300)
model_labels = paste("Boosted Decision Machine with number of trees K =", k)

info <- read.csv(train_label_path)
n <- nrow(info)
n_train <- round(n*(4/5), 0)
train_idx <- sample(info$Index, n_train, replace = F)
test_idx <- setdiff(info$Index, train_idx)

n_files <- length(list.files(train_image_dir))

image_list <- list()
for(i in 1:100){
  image_list[[i]] <- readImage(paste0(train_image_dir, sprintf("%04d", i), ".jpg"))
}

#function to read fiducial points
#input: index
#output: matrix of fiducial points corresponding to the index
readMat.matrix <- function(index){
  return(round(readMat(paste0(train_pt_dir, sprintf("%04d", index), ".mat"))[[1]],0))
}

#load fiducial points
fiducial_pt_list <- lapply(1:n_files, readMat.matrix)
save(fiducial_pt_list, file="./output/fiducial_pt_list.RData")

source("./lib/feature_default.R")
if(run.default){
  tm_feature_train_default <- NA
  if(run.feature.train){
    tm_feature_train_default <- system.time(dat_train_default <- feature_default(fiducial_pt_list, train_idx))
    save(dat_train_default, file="./output/feature_train_default.RData")
    save(tm_feature_train_default, file="./output/tm_feature_train_default.RData")
  }
  
  tm_feature_test_default <- NA
  if(run.feature.test){
    tm_feature_test_default <- system.time(dat_test_default <- feature_default(fiducial_pt_list, test_idx))
    save(dat_test_default, file="./output/feature_test_default.RData")
    save(tm_feature_test_default, file="./output/tm_feature_test_default.RData")
  }
}

data <- data.frame((dat_train_default)[[1]][1:1200],dat_train_default[["label"]][1:1200],(dat_train_default)[[1]][1201:2400],dat_train_default[["label"]][1201:2400])


for (i in 2:6006){
  
  data1 <- data.frame((dat_train_default)[[i]][1:1200],dat_train_default[["label"]][1:1200],(dat_train_default)[[i]][1201:2400],dat_train_default[["label"]][1201:2400])
  names(data) <- names(data1) 
  data <- rbind(data,data1)
}

data_index <- sample(length(data[,1]), length(data[,1])*.75)
Smarket1 <- data[c(data_index), ][,c(1,3,4)]
Smarket2 <- data[-c(data_index),][,c(1,3,4)]
colnames(Smarket1) <- c("eye", "lip", "direction")
colnames(Smarket2) <- c("eye", "lip", "direction")


KNN.decision <- function(Lag1.new = Smarket2$eye, Lag2.new=Smarket2$lip, K=5, Lag1 = Smarket1$eye, Lag2 = Smarket1$lip) {
  
  n <- length(Lag1)
  choice <- 1:length(Smarket2$eye)
  for(i in 1:length(Smarket2$eye)){
    stopifnot(length(Lag2) == n, length(Lag1.new) != 1, length(Lag2.new) != 1, K <= n)
    
    dists <- sqrt((Lag1[i]-Lag1.new)^2 + (Lag2[i]-Lag2.new)^2)
    neighbors <- order(dists)[1:K]
    neighb.dir <- Smarket1$direction[neighbors]
    choice[i] <- names(which.max(table(neighb.dir)))
  }
  return(choice)
}
head(KNN.decision()[1])



sum(as.numeric(KNN.decision()))/(length(as.numeric(KNN.decision())))

