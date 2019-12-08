dd <- readChar("data/day_08", "15000") #This is not ideal
dd <- substring(dd, seq(1, 14999, 1), seq(1, 15000, 1)) #This seems like a hack

15000/25/6 #still hacking
dd <- data.frame(dat = dd, group = rep(1:100, each = 25 * 6))

dd %>% group_by(group) %>% 
  summarize(zero = sum(dat == "0"),
            one  = sum(dat == "1"),
            two = sum(dat == "2")) %>% 
  top_n(1, -zero) %>% 
  summarize(zero, one * two) #first star

dd$position <- rep(1:(25 * 6), 100)

my_value <- function(x) {
  x[min(which(x != 2))]
}
a <- dd %>% 
  group_by(position) %>% 
  summarize(p = my_value(dat)) %>% 
  pull(p) %>%
  as.integer() %>% 
  matrix(byrow = T, nrow = 6)
image(a) #second star; but this is rotated

flip <- function(x, nrow = 6, ncol = 25) {
  for(i in 1:ncol) {
    x[,i] <- x[nrow:1, i]
  }
  x
}

image(t(flip(a))) #That's better
 