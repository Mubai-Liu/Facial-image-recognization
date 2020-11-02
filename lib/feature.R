#############################################################
### Construct features and responses for training images  ###
#############################################################

feature <- function(input_list = fiducial_pt_list, index, inclass = FALSE){
  # input_list: 
  #   Default: fiducial_pt_list
  #   A list Which indicates the original points position
  # index:
  #   A vector indicates the rows that need feature extraction
  
  # Compute the distance between each two points
  get_distance <- function(mat){
    colnames(mat) <- c("x", "y")
    dif <- as.data.frame(diff(mat))
    dif$dist <- sqrt(dif$x^2 + dif$y^2)
    return (dif$dist)
  }
  
  # Compute the distance between each point in a matrix to a designated point
  get_des_dist <- function(mat, pt){
    colnames(mat) <- c("x", "y")
    n = nrow(mat)
    a = pt[1]
    b = pt[2]
    center <- matrix(c(rep(a, n), rep(b,n)), ncol = 2)
    dif <- as.data.frame(mat- center)
    dif$dist <- sqrt(dif$x^2+dif$y^2)
    return (dif$dist)
  }
  
  # Compute the angle between each two points
  get_angle <- function(mat){
    colnames(mat) <- c("x", "y")
    dif <- as.data.frame(diff(mat))
    dif$angle <- atan(dif$y/dif$x)
    return (dif$angle)
  }
  
  # Get fitted quadratic coefficient of all points
  get_quad <- function(mat){
    x <- mat[ ,1]
    y <- mat[ ,2]
    model <- lm(y~x+I(x^2))
    return (model$coefficients[3])
  }
  
  # Get the position difference of the symmetrical position points
  get_symmetric <- function(mat1, mat2){
    colnames(mat1) <- c("x", "y")
    colnames(mat2) <- c("x", "y")
    mat1[,1] <- 1000-mat1[,1]
    mat <- mat2-mat1
    return (sqrt(mat[,1]^2 + mat[,2]^2))
  }

  get_result <- function(mat,index){
    ## face
    points.face <- mat[64:78, ]
    points.face.left <- mat[64:71, ]
    points.face.right <- mat[71:78, ]
    
    face_dist <- get_distance(points.face)
    face_angle <- get_angle(points.face)
    face_quad_left <- get_quad(points.face.left)
    face_quad_right <- get_quad(points.face.right)
    
    ## left eye
    center.eye.left <- mat[1, ]
    points.eye.left <- mat[2:9, ]
    left_eye_dist <- get_des_dist(points.eye.left, center.eye.left)
    left_eye_angle1 <- get_angle(mat[c(4,2), ]) - get_angle(mat[c(2,8), ])
    left_eye_angle2 <- get_angle(mat[c(2,4), ]) - get_angle(mat[c(4,6), ])
    left_eye_height1 <- get_distance(mat[c(4, 8), ])
    left_eye_height2 <- get_distance(mat[c(3, 9), ])
    left_eye_height3 <- get_distance(mat[c(5, 7), ])
    left_eye_width <- get_distance(mat[c(2, 6), ])
    
    ## right eye
    center.eye.right <- mat[10, ]
    points.eye.right <- mat[11:18, ]
    right_eye_dist <- get_des_dist(points.eye.right, center.eye.right)
    right_eye_angle1 <- get_angle(mat[c(13, 11), ]) - get_angle(mat[c(11, 17), ])
    right_eye_angle2 <- get_angle(mat[c(11, 17), ]) - get_angle(mat[c(17, 15), ])
    right_eye_height1 <- get_distance(mat[c(13, 17), ])
    right_eye_height2 <- get_distance(mat[c(14, 16), ])
    right_eye_height3 <- get_distance(mat[c(12, 18), ])
    right_eye_width <- get_distance(mat[c(11, 15), ])
    
    ## between eyes
    eye_centers_dist <- get_distance(mat[c(1, 10), ])
    eye_inner_dist <- get_distance(mat[c(6, 11), ])
    eye_outer_dist <- get_distance(mat[c(2, 15), ])

    ## left eyebrow
    points.brow.left <- mat[19:23, ]
    left_brow_dist <- get_distance(points.brow.left)
    left_brow_angle <- get_angle(points.brow.left)
    left_brow_eye_dist <- get_des_dist(points.brow.left, center.eye.left)
    left_brow_quad <- get_quad(points.brow.left)
    
    ## right eyebrow
    points.brow.right <- mat[27:31, ]
    right_brow_dist <- get_distance(points.brow.right)
    right_brow_angle <- get_angle(points.brow.right)
    right_brow_eye_dist <- get_des_dist(points.brow.right, center.eye.right)
    right_brow_quad <- get_quad(points.brow.right)
    
    ## between eyebrows
    brow_inner_dist <- get_distance(mat[c(23, 27), ])
    brow_outer_dist <- get_distance(mat[c(19, 31), ])
    
    ## nose
    points.nose <- mat[40:48, ]
    nose_dist <- get_distance(points.nose)
    nose_angle <- get_angle(points.nose)
    nose_quad <- get_quad(mat[42:46, ])
    nose_length <- get_distance(mat[c(35, 44), ])
    
    pt1 <- mat[40, ]
    pt2 <- mat[42, ]
    pt3 <- mat[46, ]
    pt4 <- mat[48, ]
    a1 <- (pt1[2] - pt2[2])/(pt1[1] - pt2[1])
    b1 <- pt1[2] - (pt1[2] - pt2[2])/(pt1[1] - pt2[1])*pt1[1]
    a2 <- (pt3[2] - pt4[2])/(pt3[1] - pt4[1])
    b2 <- pt3[2] - (pt3[2] - pt4[2])/(pt3[1] - pt4[1])*pt3[1]
    x <- (b2 - b1)/(a1 - a2)
    y <- a1*x + b1
    pt5 <- c(x, y)
    mat.nose <- rbind(pt2, pt3, pt5, pt2)
    len <- get_distance(mat.nose)
    a <- len[1]
    b <- len[2]
    c <- len[3]
    p=(a + b + c)/2
    nose_area <- sqrt(p*(p - a)*(p - b)*(p - c))
    
    ## lips
    lips_area <- polyarea(x = c(mat[50, 1], mat[51, 1], mat[52, 1], mat[53, 1],
                                 mat[54, 1], mat[55, 1], mat[56, 1], mat[57, 1]),
                           y = c(mat[50, 2], mat[51, 2], mat[52, 2], mat[53, 2],
                                 mat[54, 2], mat[55, 2], mat[56, 2], mat[57, 2]))

    points.lips.up <- mat[50:54, ]
    lips_dist_up <- get_distance(points.lips.up)
    lips_angle_up <- get_angle(points.lips.up)
    lips_quad_up <- get_quad(points.lips.up)
    
    points.lips.down <- mat[c(50, 54:57), ]
    lips_dist_down <- get_distance(points.lips.down)
    lips_angle_down <- get_angle(points.lips.down)
    lips_quad_down <- get_quad(points.lips.down)
    
    ## lips - chin
    point.chin <- mat[71, ]
    lips_chin_dist_up <- get_des_dist(points.lips.up, point.chin)
    lips_chin_dist_down <- get_des_dist(points.lips.down, point.chin)
    
    ## mouth
    mouth_area <- polyarea(x = c(mat[58, 1], mat[59, 1], mat[60, 1], 
                                 mat[61, 1], mat[62, 1], mat[63, 1]),
                           y = c(mat[58, 2], mat[59, 2], mat[60, 2],
                                 mat[61, 2], mat[62, 2], mat[63, 2]))
    
    points.mouth.up <- mat[58:60, ]
    mouth_dist_up <- get_distance(points.mouth.up)
    mouth_angle_up <- get_angle(points.mouth.up)
    mouth_quad_up <- get_quad(points.mouth.up)
    
    points.mouth.down <- mat[61:63, ]
    mouth_dist_down <- get_distance(points.mouth.down)
    mouth_angle_down <- get_angle(points.mouth.down)
    mouth_quad_down <- get_quad(points.mouth.down)
    
    ##result
    result <- t(matrix(c(face_dist, face_angle, face_quad_left,
                         face_quad_left, face_quad_right, left_eye_dist,
                         left_eye_angle1, left_eye_angle2, left_eye_height1,
                         left_eye_height2, left_eye_height3, left_eye_width,
                         right_eye_dist, right_eye_angle1, right_eye_angle2,
                         right_eye_height1, right_eye_height2, right_eye_height3,
                         right_eye_width, eye_centers_dist, eye_inner_dist,
                         eye_outer_dist, left_brow_dist, left_brow_angle,
                         left_brow_eye_dist, left_brow_quad, right_brow_dist,
                         right_brow_angle, right_brow_eye_dist, right_brow_quad,
                         brow_inner_dist, brow_outer_dist, nose_dist, nose_angle,
                         nose_quad, nose_length, nose_area, lips_area, lips_dist_up,
                         lips_angle_up, lips_quad_up, lips_dist_down, lips_angle_down,
                         lips_quad_down, lips_chin_dist_up, lips_chin_dist_down,
                         mouth_area, mouth_dist_up, mouth_angle_up, mouth_quad_up,
                         mouth_dist_down, mouth_angle_down, mouth_quad_down
                         )))
    
    return (result)
  }

  constructed_feature <- t(sapply(input_list[index], get_result))
  

  feature_label_data <- cbind(constructed_feature, 
                                info$label[index])
  
  colnames(feature_label_data) <- c(paste("feature", 
                                          1:(ncol(feature_label_data)-1), 
                                          sep = ""), "label")
  feature_label_data <- as.data.frame(feature_label_data)
  feature_label_data$label <- as.factor(feature_label_data$label)


  # Return features
  return(feature_df = feature_label_data)
}
