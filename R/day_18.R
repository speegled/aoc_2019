library(tidyverse)
library(igraph)

dd <- read_lines("data/day_18")
N <- str_length(dd[1])
dd2 <- matrix(sapply(1:length(dd), function(i) str_sub(dd[i], 1:N, 1:N)),
       ncol = N, byrow = T)




ff <- matrix(sapply(1:length(dd), function(i) str_sub(dd[i], 1:N, 1:N)) %>% 
               str_replace_all("[^\\#]", "."),
       ncol = N, byrow = T)

a <- which(ff == ".", arr.ind = T) 
a_ind <- which(ff == ".")
# str(combinat::combn(1:sum(ff == "."), 2, fun = function(x, y) pmin(abs(a[x][1] - a[y][1]),
#           

d <- dim(ff)

get_neighbors <- function(ind, a) {
  nn <- which((abs(ind[1] - a[,1]) == 1 & ind[2] == a[,2]) |
          (abs(ind[2] - a[,2]) == 1 & ind[1] == a[,1]) )
  aa <- which(a[,1] == ind[1] & a[,2] == ind[2])
  sapply(1:length(nn), function(x) c(aa, nn[x]))
}

M <- nrow(a)
nbs <- lapply(1:M, function(x) get_neighbors(a[x,], a) ) %>% unlist()
nbs <- a_ind[nbs]
maze <- make_graph(nbs)

keys <- sapply(letters, function(x) (which(dd2 == x)))
locks <- sapply(LETTERS, function(x) which(dd2 == x))

distances(graph = maze, 
          v = as.character(start), 
          to = as.character(keys[1]))


reprex::reprex({}, venue = "so")


