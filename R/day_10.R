library(tidyverse)
library(phonTools)
dd <- read.csv("data/day_10", header= F, stringsAsFactors = F)
dd <- sapply(1:nrow(dd), 
             function(x) substring(dd$V1[x], 
                                   first = 1:str_length(dd$V1[1]), 
                                   last = 1:str_length(dd$V1[1])))
nrow <- length(dd[1,])
dd <- str_replace_all(dd, "\\.", "0") %>% 
  str_replace_all("\\#", "1") %>% 
  as.integer() %>% 
  matrix(nrow = nrow, byrow = T)
vals <- data.frame(which(dd == 1, arr.ind = T))
vals <- lapply(1:nrow(vals), function(x) vals[x,])
get_slope <- function(a, b) {
  # browser()
  if(a[2] - b[2] == 0) {
    slope <- c(Inf, Inf)
    dir <- as.integer(sign(b[1] - a[1]))
  } else {
    num <- as.integer(b[1] - a[1])
    den <- -1 * as.integer(b[2] - a[2])
    slope <- reduce.fraction(ratio = c(num, den))
    dir <- -1 * as.integer(sign(b[2] - a[2]))
  }
  return(c(dir = dir, slope1 = slope[1], slope2 = slope[2]))
}

get_slope_vec <- function(a, b, j) {
  sapply(1:length(a), function(x) get_slope(a[[x]], b[[j]]))
}

dat <- lapply(1:length(vals), function(j) get_slope_vec(vals, vals, j)) #slower than I thought it would be

max(sapply(1:length(vals), function(i) {
  a <- data.frame(t(dat[[i]]))
  nrow(distinct(a)) - 1
})) #first star

which(sapply(1:length(vals), function(i) {
  a <- data.frame(t(dat[[i]]))
  nrow(distinct(a)) - 1 == 263
}))

ll <- 227
# save(dat, file = "data/for_day_10") in case I get tired of working on this and want to come back to it later!
# load("data/for_day_10")
vals[[ll]]
ff <- dat[[ll]]
ff <- data.frame(t(ff))

get_angle <- function(x, y, dx) {
  if(x == Inf) {
    if(dx == 1) 
      return(0)
    if(dx == -1) {
      return(pi)
    }
  }
  angle <- atan2(x, y) + ifelse(dx == -1, pi, 0) + ifelse(sign(x) == -1, pi, 0)
  return(angle)
}

get_angle_vec <- function(x, y, dx) {
  sapply(1:length(x), function(i) get_angle(x[i], y[i], dx[i]))
}

ff <- mutate(ff, angle = get_angle_vec(slope2, slope1, dir)) %>% 
  mutate(angle = round(angle/pi * 180, 4))

coords <- unlist(vals) %>% matrix(byrow = T, ncol = 2)
ff$distance <- as.matrix(dist(coords))[ll,]

ff2 <- filter(ff, dir != 0)
arrange(ff2, angle, distance) %>% 
  mutate(temp = 1) %>% 
  group_by(angle) %>% 
  mutate(which = cumsum(temp)) %>% 
  arrange(which, angle, distance) %>%
  ungroup() %>% 
  rownames_to_column() %>% 
  filter(rowname == 200)

which.min(abs(ff$distance - 22.5) + abs(ff$angle - 328))
ff[109,]
vals[[109]]


