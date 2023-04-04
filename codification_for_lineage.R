library(tidyverse)

d <- read.table('subset.txt', header = T)

d <- as_tibble(t(d))

sub_gen <- function(x) {
  x |>
    plyr::count() -> freq_table
  
  order_freq_table  <- freq_table[order(freq_table$freq, decreasing = T), ]
  
  gen_major_freq <- order_freq_table[1, 1]
  
  ifelse(x == gen_major_freq, 1, 0)
}

coded_d1 <- apply(d, 2, sub_gen)


coded_d2 <- map_df(d, sub_gen)


