
is_triple <- function(vec, pos) {
  if(pos <= length(vec) - 2) {
    vec[pos] == vec[pos + 1] && vec[pos + 1] == vec[pos + 2]
  }
}

test_number <- function(vec) {
  is_double <- any(sapply(1:(length(vec) - 1), 
                          function(x) vec[x] == vec[x + 1]))
  is_increasing <- all(sapply(1:(length(vec) - 1), 
                       function(x) vec[x] <= vec[x + 1]))
  is_double && is_increasing 
}

sum(sapply(284639:748759, function(x) {
  vec <- unlist(strsplit(as.character(x), split = "")) #Is there a better way to split an integer into a vector of digits? Probably.
  vec <- as.numeric(vec)
  test_number(vec)
})) #Hmmm, didn't expect this to take so long. 



test_number_2 <- function(vec) {
  vec <- c(-10, vec, 11)
  is_double <- any(sapply(2:(length(vec) - 2), 
                          function(x) {
                            vec[x] == vec[x + 1] && 
                               !(vec[x - 1] == vec[x] && vec[x] == vec[x + 1]) &&
                              !(vec[x] == vec[x + 1] && vec[x + 1] == vec[x + 2])
                          }))
  is_increasing <- all(sapply(2:(length(vec) - 2), 
                              function(x) vec[x] <= vec[x + 1]))
  is_double && is_increasing 
}

sum(sapply(284639:748759, function(x) {
  vec <- unlist(strsplit(as.character(x), split = ""))
  vec <- as.numeric(vec)
  test_number_2(vec)
})) #second star

#'
#' After I did this, I looked up how I should really be splitting an integer into 
#' a vector of its digits. There doesn't seem to be consensus, but this is pretty cool.
#'

outer(X = c(284639, 123), Y = 10^c(5:0), FUN = function(a, b) a %/% b %% 10)
