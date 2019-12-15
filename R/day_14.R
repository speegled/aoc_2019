oo <- readLines("data/day_14")
oo
num_elements <- str_extract_all(oo, "[A-Z]+") %>% 
  unlist() %>% 
  unique() %>% 
  length()
df <- as.data.frame(matrix(0, nrow = num_elements, ncol = num_elements + 2))
names(df) <- c(str_extract_all(oo, "[A-Z]+") %>% 
                 unlist() %>% 
                 unique(), 
               "available", "needed")
rownames(df) <- str_extract_all(oo, "[A-Z]+") %>% 
  unlist() %>% 
  unique()









df
i <- "A"
for(i in rownames(df)) {
  j <- which(word(oo, -1) == i)
  row_to_change <- str_extract_all(oo[j], "[A-Z]+") %>% unlist()
  vals <- str_extract_all(oo[j], "[0-9]+") %>% unlist() %>% as.integer()
  df[i, row_to_change] <- vals
}
df["FUEL", "needed"] <- 1
df

how_many <- function(av, ne, source, outp) {
  diff_ne <- ne - av
  mult_factor <- ceiling(diff_ne/outp)
  needed <- t(source * mult_factor) %>% as.vector()
  available <- outp * mult_factor
  return(list(needed = needed,
              available = available))
}
N <- nrow(df)

df["ORE", "available"] <- Inf
old_df <- df
df
while(any(df[,"needed"] > df[,"available"])) {
  mar <- min(which(df[,"needed"] > df[,"available"]))
  hm <- how_many(df[mar,"available"], 
                           df[mar,"needed"], 
                           df[mar, 1:N], 
                           df[mar, mar])
  hm$needed[mar] <- 0
  df$needed <- df$needed + hm$needed
  df$available[mar] <- hm$available + df$available[mar]
}

df["ORE", "needed"] #first star

1000000000000/485720 #gives rough idea how much
ORE <- 1
FUEL <- 1000000L #Get upper bound
while(ORE < 1000000000000) {
  FUEL <- FUEL * 2L
  df <- old_df
  df["FUEL", "needed"] <- FUEL
  while(any(df[,"needed"] > df[,"available"])) {
    mar <- min(which(df[,"needed"] > df[,"available"]))
    hm <- how_many(df[mar,"available"], 
                   df[mar,"needed"], 
                   df[mar, 1:N], 
                   df[mar, mar])
    hm$needed[mar] <- 0
    df$needed <- df$needed + hm$needed
    df$available[mar] <- hm$available + df$available[mar]
  }
  ORE <- df["ORE", "needed"]
  if(ORE < 1000000000000) {
    FUEL_MIN <- FUEL
  } else {
    FUEL_MAX <- FUEL
  }
  print(FUEL)
  print(ORE)
}
FUEL #It's between 2000000 and 4000000.

FUEL_MIN <- 2000000
FUEL_MAX <- 4000000
while(abs(FUEL_MIN - FUEL_MAX) > 1) {
  FUEL <- sample(c(floor(mean(c(FUEL_MIN, FUEL_MAX))),
                   ceiling(mean(c(FUEL_MIN, FUEL_MAX)))), 1)
  # FUEL <- FUEL * 2
  df <- old_df
  df["FUEL", "needed"] <- FUEL
  while(any(df[,"needed"] > df[,"available"])) {
    mar <- min(which(df[,"needed"] > df[,"available"]))
    hm <- how_many(df[mar,"available"], 
                   df[mar,"needed"], 
                   df[mar, 1:N], 
                   df[mar, mar])
    hm$needed[mar] <- 0
    df$needed <- df$needed + hm$needed
    df$available[mar] <- hm$available + df$available[mar]
  }
  ORE <- df["ORE", "needed"]
  if(ORE < 1000000000000) {
    FUEL_MIN <- FUEL
  } else {
    FUEL_MAX <- FUEL
  }
  print(FUEL)
  print(ORE)
}
FUEL #Second star

