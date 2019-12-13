moon <- matrix(c(1, 4, 4, -4, -1, 19, -15, -14, 12, -17, 1, 10), 
               ncol = 3,
               byrow = T) #My data
vel <- matrix(0, ncol = 3, nrow = 4)
N <- 2772
find_velocity_delta <- function(moon, which) {
  apply(t(sapply(1:4,
                 function(x) sign(-moon[which,] + moon[x,]))), 
        2, sum)
}


for(i in 1:N) {
  vel <- t(sapply(1:4, function(x) find_velocity_delta(moon, x))) + vel
  moon <- moon + vel
}
#First star
sum(apply(moon, 1, function(x) sum(abs(x))) * apply(vel, 1, function(x) sum(abs(x))))


vel <- matrix(0, ncol = 3, nrow = 4)
full_moon <- moon
N <- 1000000
find_velocity_delta <- function(moon, which) {
  sum(sapply(1:4, function(x) sign(-moon[which] + moon[x])))
}


vals <- numeric(0)

for(j in 1:3) {
  moon <- full_moon[,j]
  moon_x = moon
  vel <- rep(0, 4)
  for(i in 1:N) {
    vel <- sapply(1:4, function(x) find_velocity_delta(moon, x)) + vel
    moon <- moon + vel
    if(sum(moon_x == moon) == 4) {
      break
    }
  }
  vals <- c(vals, i + 1)
}
gmp::lcm.bigz(gmp::lcm.bigz(vals[1], vals[2]), vals[3]) #second star
