#'
#' New this round --- multiple inputs. That doesn't sound too hard.
#'


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
    long_vec[positions] <- my_input[input_location]
    input_location <<- input_location + 1
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


library(tidyverse )
dd <- read.csv("data/day_07", header = FALSE) %>% t() %>% as.vector()

# cur_vec <- c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
#              1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)

cur_vec <- dd


max <- 0
inputs <- combinat::permn(0:4)
output <- numeric(0)

for(my_input in inputs) {
  add_on <- 0
  for(j in my_input) {
    input_location <- 1
    position <- 1
    while(cur_vec[position] != 99) {
      cur_val <- update_vec(cur_vec, 
                            position, 
                            my_input = c(j, 
                                         add_on), 
                            debug = F)
      cur_vec <- cur_val$long_vec
      position <- cur_val$new_position
      pp <- c(pp, output)
    }
    add_on <- output
  }
  if(output > max) {
    max <- output
    best_input <- my_input
  }
}
best_input
max #first star

