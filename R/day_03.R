library(tidyverse)

dd <- read.csv("data/day_3", header = FALSE, stringsAsFactors = FALSE) %>% t() %>% as.data.frame()
names(dd) <- c("wire_1", "wire_2")
dd
dd2 <- transmute_all(dd, .funs = list(dist  = function(x) as.integer(str_extract(x, "[0-9]+")),
                            direction = function(x) str_extract(x, "[A-Z]")))
dd2$wire_1_vertical <- case_when(dd2$wire_1_direction == "U" ~  1,
          dd2$wire_1_direction == "D" ~ -1,
          TRUE ~ 0)
dd2$wire_1_horizontal <- case_when(dd2$wire_1_direction == "L" ~  1,
                                  dd2$wire_1_direction == "R" ~ -1,
                                  TRUE ~ 0)
dd2$wire_2_vertical <- case_when(dd2$wire_2_direction == "U" ~  1,
                                dd2$wire_2_direction == "D" ~ -1,
                                TRUE ~ 0)
dd2$wire_2_horizontal <- case_when(dd2$wire_2_direction == "L" ~  1,
                                  dd2$wire_2_direction == "R" ~ -1,
                                  TRUE ~ 0)

dd2 <- dd2 %>% select(-wire_1_direction, -wire_2_direction)

cur_pos <- data.frame(cur_x_2 = cumsum(dd2$wire_2_dist * dd2$wire_2_horizontal),
               cur_y_2 = cumsum(dd2$wire_2_dist * dd2$wire_2_vertical),
               cur_x_1 = cumsum(dd2$wire_1_dist * dd2$wire_1_horizontal),
               cur_y_1 = cumsum(dd2$wire_1_dist * dd2$wire_1_vertical))

do_intersect <- function(x, y) {
  if(x[1] == x[2] && x[3] == x[4]) {
    return(Inf)
  }
  if(y[1] == y[2] && y[3] == y[4]) {
    return(Inf)
  }
  if(x[1] == x[2]) {
    val <- min(x[3:4]) < x[1] && max(x[3:4]) > x[1] && min(y[1:2]) < y[3] && max(y[1:2]) > y[3]
    if(val) {
      return(abs(x[1]) + abs(y[3]))
    } else {
      return(Inf)
    }
  }
  if(y[1] == y[2]) {
    t <- x
    x <- y
    y <- t
    val <- min(x[3:4]) < x[1] && max(x[3:4]) > x[1] && min(y[1:2]) < y[3] && max(y[1:2]) > y[3]
    if(val) {
      return(abs(x[1]) + abs(y[3]))
    } else {
      return(Inf)
    }
  }
}
cur_pos
i <- 1
j <- 2
cur_pos <- rbind(c(0, 0, 0, 0), cur_pos)
min(sapply(1:(sum(!is.na(cur_pos$cur_x_2)) - 1), function(i) {min(sapply(1:(sum(!is.na(cur_pos$cur_x_1)) - 1), function(j) 
  do_intersect(x = c(cur_pos$cur_x_2[i],
                   cur_pos$cur_x_2[i + 1],
                   cur_pos$cur_x_1[j],
                   cur_pos$cur_x_1[j + 1]),
             y = c(cur_pos$cur_y_2[i],
                   cur_pos$cur_y_2[i + 1],
                   cur_pos$cur_y_1[j],
                   cur_pos$cur_y_1[j + 1]))))})) #first star


#'
#' Oh, crud, I did the first part in not only a bad way, but also a bad
#' way. It doesn't seem to generalize easily to the second star. 
#' 
#' Story of my life.
#'


do_intersect <- function(x, y) {
  if(x[1] == x[2] && x[3] == x[4]) {
    return(list(dist = Inf,
                location = c(0,0)))
  }
  if(y[1] == y[2] && y[3] == y[4]) {
    return(list(dist = Inf,
                location = c(0,0)))
  }
  if(x[1] == x[2]) {
    val <- min(x[3:4]) < x[1] && max(x[3:4]) > x[1] && min(y[1:2]) < y[3] && max(y[1:2]) > y[3]
    if(val) {
      return(list(dist = abs(x[1]) + abs(y[3]),
                  location = c(x[1], y[3])))
    } else {
      return(list(dist = Inf,
                  location = c(0,0)))
    }
  }
  if(y[1] == y[2]) {
    t <- x
    x <- y
    y <- t
    val <- min(x[3:4]) < x[1] && max(x[3:4]) > x[1] && min(y[1:2]) < y[3] && max(y[1:2]) > y[3]
    if(val) {
      return(list(dist = abs(x[1]) + abs(y[3]),
                  location = c(y[3], x[1])))
    } else {
      return(list(dist = Inf,
                  location = c(0,0)))
    }
  }
}


i <- 1
dd2 <- rbind(0, dd2)
# min(sapply(1:301, function(i) {
#   min(sapply(1:301, function(j) {
    
min(sapply(1:(sum(!is.na(cur_pos$cur_x_2)) - 1), 
           function(i) {min(sapply(1:(sum(!is.na(cur_pos$cur_x_1)) - 1), function(j) { 
      vv <- do_intersect(x = c(cur_pos$cur_x_2[i],
                     cur_pos$cur_x_2[i + 1],
                     cur_pos$cur_x_1[j],
                     cur_pos$cur_x_1[j + 1]),
               y = c(cur_pos$cur_y_2[i],
                     cur_pos$cur_y_2[i + 1],
                     cur_pos$cur_y_1[j],
                     cur_pos$cur_y_1[j + 1]))
  if(vv$dist < Inf) {
    ll <- sum(dd2$wire_2_dist[1:i]) + sum(dd2$wire_1_dist[1:j]) +
      abs(cur_pos$cur_x_2[i] - vv$location[1]) +
      abs(cur_pos$cur_y_2[i] - vv$location[2]) + 
      abs(cur_pos$cur_x_1[j] - vv$location[1]) +
      abs(cur_pos$cur_y_1[j] - vv$location[2])
  } else {
    ll <- Inf
  }
  ll
}))
}))


