#'
#' Oooh, FFT.
#'
library(tidyverse)
phase_generator <- function(i, length) {
  ph <- rep_len(rep.int(c(0L, 1L, 0L, -1L), c(i, i, i, i)), length + 1)
  ph[-1]
}

tens_digit <- function(x) {
  abs(x) %% 10
}

conv <- function(sig, filter) {
  sig * filter
}

sig <- read_lines("data/day_16")
sig_char <- sig
N <- str_length(sig_char)
sig <- as.integer(str_sub(sig, 1:N, 1:N))
sapply(1:N, function(i) tens_digit(sum(round(conv(sig, phase_generator(i, N))))))

sig_new <- sig
for(i in 1:100) {
  sig_new <- sapply(1:N, function(i) tens_digit(sum(round(conv(sig_new, phase_generator(i, N))))))
  print(i)
}
sig_new[1:8] #first star

sig <- rep(sig, 10000)
start <- as.integer(paste(sig[1:7], collapse = ""))
sig2 <- sig[start: length(sig)]
sig2 <- rev(sig2)

for(i in 1:100) {
  sig2 <- cumsum(sig2) %% 10
}
rev(sig2)[2:9] #second star!
