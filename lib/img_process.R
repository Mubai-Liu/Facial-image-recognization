library(R.matlab)
library(jpeg)
library(EBImage)

distance <- function(x,y){
  return(sqrt(sum((x-y)^2)))
}

img_process <- function(mat){
  
  img <- readImage(paste0(train_image_dir, sprintf("%04d", 4), ".jpg"))
  points_mat <- data.frame(mat)
  names(points_mat) <- c("X","Y")
  x_array <- points_mat$X
  y_array <- points_mat$Y
  
  central_index = c(35,36,37,38,44,52,56,59,62)
  left_index = c(1,2,3,4,5,6,7,8,9,19,20,21,22,23,24,25,26,39,40,41,42,43,50,51,57,58,63,64,65,66,67,68,69,70)
  right_index = c(10,15,14,13,12,11,18,17,16,31,30,29,28,27,34,33,32,49,48,47,46,45,54,53,55,60,61,78,77,76,75,74,73,72)
  
  x_lm <- c(x_array[central_index],(x_array[left_index]+x_array[right_index])/2)
  y_lm <- c(y_array[central_index],(y_array[left_index]+y_array[right_index])/2)
  
  model <- lm(x_lm~y_lm)
  summary(model)
  lm_intercept <- model$coefficients[1]
  lm_k <- model$coefficients[2]
  
  y0 = -250:1000
  x0 = lm_intercept + lm_k * y0
  
  ## rotation
  theta_rotate <- atan(lm_k)
  theta_rotate_degree <- atan(lm_k) * 180 / pi ## in degree
  
  img_rotate <- rotate(img,theta_rotate_degree)
  
  if (theta_rotate>0){
    ## formula: x' = x cos\theta - y sin\theta; y' = y cos\theta + x sin\theta 
    
    x0_rotate <- x0 * cos(theta_rotate) - y0 * sin(theta_rotate) + 750 * sin(theta_rotate)
    y0_rotate <- y0 * cos(theta_rotate) + x0 * sin(theta_rotate)
    
    ##find new fiducial points
    
    x_array_rotate <- x_array * cos(theta_rotate) - y_array * sin(theta_rotate) + 750 * sin(theta_rotate)
    y_array_rotate <- y_array * cos(theta_rotate) + x_array * sin(theta_rotate)
    
    
    
    
    
  }else{
    ## formula: x' = x cos\theta - y sin\theta; y' = y cos\theta + x sin\theta 
    
    x0_rotate <- x0 * cos(theta_rotate) - y0 * sin(theta_rotate)
    y0_rotate <- y0 * cos(theta_rotate) + x0 * sin(theta_rotate) - 1000 * sin(theta_rotate)
    
    
    ##find new fiducial points
    
    x_array_rotate <- x_array * cos(theta_rotate) - y_array * sin(theta_rotate)
    y_array_rotate <- y_array * cos(theta_rotate) + x_array * sin(theta_rotate) - 1000 * sin(theta_rotate)
    
  }
  
  eyebrow_mat <- points_mat[c(19:26,27:34),]
  jaw_mat <- points_mat[71,]
  
  center_eyebrow <- apply(eyebrow_mat,2,mean)
  center_jaw <- apply(jaw_mat,2,mean)
  
  expected_dist <- 500
  rate_zoom <- expected_dist / distance(center_eyebrow,center_jaw)
  img_zoom <- resize(img_rotate,nrow(img_rotate)*rate_zoom)
  
  x0_zoom <- x0_rotate * rate_zoom
  y0_zoom <- y0_rotate * rate_zoom
  x_array_zoom <- x_array_rotate * rate_zoom
  y_array_zoom <- y_array_rotate * rate_zoom
  
  jaw_y <- jaw_mat[2]
  jaw_y_index <- which(y0 > as.numeric(jaw_y))[1]
  bottom_point_y <- y0_zoom[jaw_y_index]
  bottom_point_x <- x0_zoom[jaw_y_index]


  x_range_done <- (bottom_point_x-499):(bottom_point_x+500)
  x_range_done <- x_range_done + (bottom_point_x-499) * ifelse(bottom_point_x-499 >= 0,0,-1) + (nrow(img_zoom) - bottom_point_x - 500) * ifelse(bottom_point_x+500 < nrow(img_zoom),0,1) 
  y_range_done <- (bottom_point_y-699):(bottom_point_y+50)
  y_range_done <- y_range_done + (bottom_point_y-699) * ifelse(bottom_point_y-699 >= 0,0,-1) + (ncol(img_zoom) - bottom_point_y - 50) * ifelse(bottom_point_y+50 < ncol(img_zoom),0,1) 
  
  x_array_done <- x_array_zoom + 500 - bottom_point_x
  y_array_done <- y_array_zoom + 700 - bottom_point_y
  
  img_done <- img_zoom[x_range_done,y_range_done,]
  points_mat_done <- cbind(as.numeric(x_array_done),as.numeric(y_array_done))
  
  return(points_mat_done)
}

