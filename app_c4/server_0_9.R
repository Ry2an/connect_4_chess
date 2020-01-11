####fake server####
library(ggplot2)

draw_map <- function(
  current_map_temp = matrix(c(
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9
  ), nrow = 12, ncol = 13, byrow = T)
){
  list_temp <- data.frame("x_ax" = 1:42, "y_ax" = 1:42, "value_temp" = 1:42)
  counter <- 1
  for(i in 1:7){
    for(j in 1:6){
      list_temp$x_ax[counter] <- j
      list_temp$y_ax[counter] <- i
      list_temp$value_temp[counter] <- as.character(current_map_temp[j + 3, i + 3])
      counter <- counter + 1
    }
  }
  output_img <- ggplot(list_temp, aes(x = y_ax, y = x_ax, color = value_temp, alpha = value_temp)) + geom_point(shape = 19, size = 10) +
    scale_color_manual(values = c("-1" = "#1E9F94","1" = "#E91773","-5" ="#0B0838")) +
    scale_alpha_manual(values = c("-1" = 1,"1" = 1,"-5" = 0)) +
    theme(legend.position = "none") +
    theme(axis.title = element_blank())
  return(output_img)
}


win_test <- function(vec_temp = c(0,0,0,0,0,0,0)){
  counter <- 1
  for(i in 2:length(vec_temp)){
    if(vec_temp[i] == vec_temp[i - 1]){
      counter <- counter + 1
      if(counter >= 4){
        break()
      }
    }else{
      counter <- 1
    }
  }
  if(counter >= 4){
    return(1)
  }else{
    return(0)
  }
}

find_array <- function(
  x = 1, y = 1,
  current_map_temp = matrix(c(
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9
  ), nrow = 12, ncol = 13, byrow = T)
){
  #vec 1: shuiping
  #vec 2: shuzhi
  #vec 3: zuoshang youxia
  #vec 4: youshang zuo xia
  vec_1 <- c(current_map_temp[x + 3, y],
             current_map_temp[x + 3, y + 1],
             current_map_temp[x + 3, y + 2],
             current_map_temp[x + 3, y + 3],
             current_map_temp[x + 3, y + 4],
             current_map_temp[x + 3, y + 5],
             current_map_temp[x + 3, y + 6])
  vec_2 <- c(current_map_temp[x, y + 3],
             current_map_temp[x + 1, y + 3],
             current_map_temp[x + 2, y + 3],
             current_map_temp[x + 3, y + 3],
             current_map_temp[x + 4, y + 3],
             current_map_temp[x + 5, y + 3],
             current_map_temp[x + 6, y + 3])
  vec_3 <- c(current_map_temp[x + 6, y],
             current_map_temp[x + 5, y + 1],
             current_map_temp[x + 4, y + 2],
             current_map_temp[x + 3, y + 3],
             current_map_temp[x + 2, y + 4],
             current_map_temp[x + 1, y + 5],
             current_map_temp[x, y + 6])
  vec_4 <- c(current_map_temp[x, y],
             current_map_temp[x + 1, y + 1],
             current_map_temp[x + 2, y + 2],
             current_map_temp[x + 3, y + 3],
             current_map_temp[x + 4, y + 4],
             current_map_temp[x + 5, y + 5],
             current_map_temp[x + 6, y + 6])
  win_1 <- win_test(vec_1)
  if(win_test(vec_1) == 1 || win_test(vec_2) == 1 || win_test(vec_3) == 1 || win_test(vec_4) == 1){
    return(1)
  }else{
    return(0)
  }
}



add_dot <- function(col_num_temp = 1,
                    color_temp = -1,
                    current_map_temp = matrix(c(
                      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
                      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
                      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
                      -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
                      -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
                      -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
                      -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
                      -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
                      -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
                      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
                      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
                      -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9
                    ), nrow = 12, ncol = 13, byrow = T)
){
  add_success <- 0
  win_temp <- 0
  for(i in 1:6){
    if(current_map_temp[i + 3, col_num_temp + 3] == -5){
      current_map_temp[i + 3, col_num_temp + 3] <- color_temp
      add_success <- 1
      win_temp <- find_array(x = i, y = col_num_temp, current_map_temp = current_map_temp)
      #next player
      current_map_temp[12, 13] <- color_temp * (-1)
      #drop success
      current_map_temp[11, 13] <- 1
      #win
      current_map_temp[10, 13] <- win_temp
      break()
    }else{
      current_map_temp[12, 13] <- color_temp
      #drop success
      current_map_temp[11, 13] <- 0
      #win
      current_map_temp[10, 13] <- win_temp
    }
  }
  #return(list(current_map_temp, add_success, win_temp))
  save_map(current_map_temp)
  return(1)
}

restart <- function(){
  current_map_temp <- matrix(c(
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,0,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,1,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-1
  ), nrow = 12, ncol = 13, byrow = T)
  write.csv(current_map_temp, file = "main_map.csv", row.names = F)
  return(1)
}

load_map <- function(){
  return(read.csv(file = "main_map.csv", header = T))
}


save_map <- function(
  current_map_temp = matrix(c(
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-5,-5,-5,-5,-5,-5,-5,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,
    -9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9,-9
  ), nrow = 12, ncol = 13, byrow = T)
){
  write.csv(current_map_temp, file = "main_map.csv", row.names = F)
  return(1)
}
