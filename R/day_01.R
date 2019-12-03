dd <- read.csv("data/day_1", header = FALSE)
sum(floor(dd$V1/3) - 2) #first star


calc_fuel <- function(x) {
  tot <- 0
  new <- floor(x/3) - 2 
  while(new > 0) {
    tot <- tot + new
    new <- floor(new/3) - 2
  }
  tot
}
sum(sapply(dd$V1, function(x) calc_fuel(x))) #second star

