library(tidyverse)
library(igraph)

dd <- read.csv("data/day_06", stringsAsFactors = FALSE, header = F) %>% pull(V1)

dd <- str_split_fixed(dd, "\\)", 2)
edges <- as.vector(t(dd[,2:1]))

my_graph <- graph(edges)
sum(distances(my_graph, v = V(my_graph), to = "COM")) #first star!


distances(my_graph, v = "YOU", to = "SAN") - 2 #second star!