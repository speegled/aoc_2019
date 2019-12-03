library(tidyverse)
dd <- read.csv("data/day_2", header = FALSE) %>% t() %>% as.vector()

cur_vec <- dd
update_vec <- function(long_vec, position, debug = FALSE) {
  if(debug) {
    browser()
  }
  new_dat <- long_vec[position:(position + 3)]
  if(new_dat[1] == 1) {
    positions <- new_dat[2:4] + 1
    long_vec[positions[3]] <- sum(long_vec[positions[1:2]])
  } else if(new_dat[1] == 2) {
    positions <- new_dat[2:4] + 1
    long_vec[positions[3]] <- prod(long_vec[positions[1:2]])
  }
  long_vec
}

position <- 1
cur_vec[2] <- 12 #I didn't read this the first time. Sigh.
cur_vec[3] <- 2  #Nor this.
cur_vec
#cur_vec <- c(2,4,4,5,99,0)
while(cur_vec[position] != 99) {
  cur_vec <- update_vec(cur_vec, position)
  position <- position + 4
}
cur_vec[1] #first star


for(noun in 0:99) {
  for(verb in 0:99) {
    position <- 1
    cur_vec <- dd
    cur_vec[2] <- noun
    cur_vec[3] <- verb
    while(cur_vec[position] != 99) {
      cur_vec <- update_vec(cur_vec, position)
      position <- position + 4
    }
    if(cur_vec[1] == 19690720) {
      soultion_noun <<- noun
      solution_verb <<- verb
      break
    }
  }
}
100 * soultion_noun + solution_verb #second star