d <- read.table('subset.txt', header = T)


sub_gen <- function(x) {
  x |>
    plyr::count() -> freq_table
  
  order_freq_table  <- freq_table[order(freq_table$freq, decreasing = T), ]
  
  gen_major_freq <- order_freq_table[1, 1]
  
  ifelse(x == gen_major_freq, 1, 0)
}

coded_d <- apply(d, 1, sub_gen)

View(coded_d)
