library(keras)
library(tensorflow)
#library(ROSE)
#library(DMwR)
tensorflow::install_tensorflow()
tensorflow::tf_config()
install_tensorflow(gpu=TRUE)
keras::install_keras(tensorflow="gpu")

x_train <- array(unlist(dat_train[, -6007]), dim = c(2400,152,3))
y_train <- array(as.numeric(dat_train$label)) - 1
x_test <- array(unlist(dat_test[, -6007]), dim = c(600, 152, 3))

model <- keras_model_sequential() 
model %>% 
  #first layer is Convolution Layer
  layer_conv_1d(filters = 100, kernel_size = 3, activation = "relu",input_shape = c(152,3)) %>%
  #Second layer is Pooling Layer
  #layer_max_pooling_1d(pool_size = 3) %>%
  #Third layer is Convolution Layer
  #layer_conv_1d(filters = 200, kernel_size = 3, activation = "relu") %>% 
  #Fourth layer is Flatten Layer
  layer_flatten() %>% 
  #Fifth layer is Dense Layer
  layer_dense(units = 256, activation = 'relu', input_shape = c(2400,152)) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 16, activation = 'relu') %>%
  #Sixth layer is Dense Layer
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  loss='binary_crossentropy',
  optimizer = "adam",
  metrics = "accuracy"
)
summary(model)
#dat_train_improved <- dat_train[,-6007]
#dat_train_improved <- feature_improved(dat_train_improved)
model %>% fit(
  x_train, y_train, 
  epochs = 10, batch_size = 100, 
  validation_split = 0.2,
  verbose = 1
)
pred <- predict_classes(model, x_test)
1-sum(pred != dat_test$label)/length(pred)

# SMOTE method
# 
# trainSplit <- SMOTE(label ~ ., dat_train, perc.over = 100, perc.under = 100, k = 3)
# x_train <- array(unlist(trainSplit[, -6007]), dim = c(1416, 200, 3))
# x_train <- array(unlist(trainSplit[,-6007]), dim = c(1416, 6006))
# y_train <- array(as.numeric(trainSplit$label)) - 1
