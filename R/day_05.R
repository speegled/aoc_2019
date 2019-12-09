library(tidyverse)
dd <- read.csv("data/data_05", header = FALSE) %>% t() %>% as.vector()
dd
cur_vec <- dd
output <- numeric(0)
my_ifelse <- function(test, x, y, debug = T) {
  if(debug) browser()
  sapply(1:length(x), function(i) {
    if(test[i]) {x[i]} else {y[i]}
  })
}
update_vec <- function(long_vec, position, my_input = 1, debug = FALSE) {
  if(debug) {
    browser()
  }
  parameter_mode <- outer(X = long_vec[position], Y = 10^c(5:0), FUN = function(a, b) a %/% b %% 10)
  
  parameter_mode <- as.vector(parameter_mode)
  code <- parameter_mode[6] + 10 * parameter_mode[5]
  if(code == 1) {
    new_dat <- long_vec[position:(position + 3)]
    positions <- new_dat[2:4] + 1
    long_vec[positions[3]] <- sum(ifelse(parameter_mode[4:3] == 0, 
                                         long_vec[abs(positions[1:2])],
                                         new_dat[2:3]))
    new_position <- position + 4
  } else if(code == 2) {
    new_dat <- long_vec[position:(position + 3)]
    positions <- new_dat[2:4] + 1
    long_vec[positions[3]] <- prod(ifelse(parameter_mode[4:3] == 0, 
                                         long_vec[abs(positions[1:2])],
                                         new_dat[2:3]))
    new_position <- position + 4
  } else if(code == 3) {
    new_dat <- long_vec[position:(position + 1)]
    positions <- new_dat[2] + 1
    long_vec[positions] <- my_input
    new_position <- position + 2
  } else if(code == 4) {
    new_dat <- long_vec[position:(position + 1)]
    positions <- new_dat[2] + 1
    output <<- ifelse(parameter_mode[3] == 0, 
                      cur_vec[positions],
                      new_dat[2])
    new_position <- position + 2
  }
  list(long_vec = long_vec,
       new_position = new_position)
}

position <- 1
pp <- numeric(0)
cur_vec <- dd
while(cur_vec[position] != 99) {
  cur_val <- update_vec(cur_vec, position, debug = F)
  
  cur_vec <- cur_val$long_vec
  position <- cur_val$new_position
  pp <- c(pp, position)
}
output #first star



update_vec <- function(long_vec, position, my_input = 1, debug = FALSE) {
  if(debug) {
    browser()
  }
  parameter_mode <- outer(X = long_vec[position], Y = 10^c(5:0), FUN = function(a, b) a %/% b %% 10)
  
  parameter_mode <- as.vector(parameter_mode)
  code <- parameter_mode[6] + 10 * parameter_mode[5]
  if(code == 1) {
    new_dat <- long_vec[position:(position + 3)]
    positions <- new_dat[2:4] + 1
    long_vec[positions[3]] <- sum(ifelse(parameter_mode[4:3] == 0, 
                                         long_vec[abs(positions[1:2])],
                                         new_dat[2:3]))
    new_position <- position + 4
  } else if(code == 2) {
    new_dat <- long_vec[position:(position + 3)]
    positions <- new_dat[2:4] + 1
    long_vec[positions[3]] <- prod(ifelse(parameter_mode[4:3] == 0, 
                                          long_vec[abs(positions[1:2])],
                                          new_dat[2:3]))
    new_position <- position + 4
  } else if(code == 3) {
    new_dat <- long_vec[position:(position + 1)]
    positions <- new_dat[2] + 1
    long_vec[positions] <- my_input
    new_position <- position + 2
  } else if(code == 4) {
    new_dat <- long_vec[position:(position + 1)]
    positions <- new_dat[2] + 1
    output <<- ifelse(parameter_mode[4] == 0, 
                      cur_vec[positions],
                      new_dat[2])
    new_position <- position + 2
  } else if (code == 5) {
    new_dat <- long_vec[position:(position + 2)]
    positions <- new_dat[2:3] + 1
    if(parameter_mode[4] == 0) {
      if(long_vec[positions[1]] != 0) {
        new_position <- ifelse(parameter_mode[3] == 0, long_vec[positions[2]] + 1, positions[2])
      } else{
        new_position <- position + 3
      }
    } else{
      if(new_dat[2] != 0) {
        new_position <- ifelse(parameter_mode[3] == 0, long_vec[positions[2]] + 1, positions[2])
      } else {
        new_position <- position + 3
      }
    }
  } else if (code == 6) {
    new_dat <- long_vec[position:(position + 2)]
    positions <- new_dat[2:3] + 1
    if(parameter_mode[4] == 0) {
      if(long_vec[positions[1]] == 0) {
        new_position <- ifelse(parameter_mode[3] == 0, long_vec[positions[2]] + 1, positions[2])
      } else{
        new_position <- position + 3
      }
    } else{
      if(new_dat[2] == 0) {
        new_position <- ifelse(parameter_mode[3] == 0, long_vec[positions[2]] + 1, positions[2])
      } else {
        new_position <- position + 3
      }
    }
  } else if (code == 7) {
     new_dat <- long_vec[position:(position + 3)]
     positions <- new_dat[1:4] + 1
     first_val <- ifelse(parameter_mode[4] == 0, long_vec[positions[2]], new_dat[2])
     second_val <- ifelse(parameter_mode[3] == 0, long_vec[positions[3]], new_dat[3])
     if(first_val < second_val) {
       long_vec[positions[4]] <- 1
     } else {
       long_vec[positions[4]] <- 0
     }
     new_position <- position + 4
  } else if (code == 8) {
    #browser()
    new_dat <- long_vec[position:(position + 3)]
    positions <- new_dat[1:4] + 1
    first_val <- ifelse(parameter_mode[4] == 0, long_vec[positions[2]], new_dat[2])
    second_val <- ifelse(parameter_mode[3] == 0, long_vec[positions[3]], new_dat[3])
    if(first_val == second_val) {
      long_vec[positions[4]] <- 1
    } else {
      long_vec[positions[4]] <- 0
    }
    new_position <- position + 4
    
  }
  list(long_vec = long_vec,
       new_position = new_position)
}


pp <- numeric(0)
output <- numeric(0)
cur_vec <- dd
position <- 1
while(cur_vec[position] != 99) {
  cur_val <- update_vec(cur_vec, position, my_input = 5, debug = F)
  
  cur_vec <- cur_val$long_vec
  position <- cur_val$new_position
  pp <- c(pp, position)
}
output #second star
