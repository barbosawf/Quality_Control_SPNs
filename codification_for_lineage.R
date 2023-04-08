
# Pacotes necessários -----------------------------------------------------

library(tidyverse)

# Banco de dados ----------------------------------------------------------

d <- read.table('subset.txt', header = T)

d <- as_tibble(t(d))


# Frequência dos allelos em cada SNP --------------------------------------

freq_alle <- function(x) {
  
  if (sum(is.na(x)) == length(x)) {
    out <- c(Alleles = "NA",
             Frequency = '1.00')
    
  } else {
    
    x |>
      na.omit() |>
      strsplit(NULL) |>
      unlist() |>
      plyr::count() -> freq_table
    
    r <- freq_table$freq / sum(freq_table$freq)
    
    freq_table$rate <- format(round(r, 2), nsmall = 2)
    
    order_freq_table  <-
      freq_table[order(freq_table$freq, decreasing = T),]
    
    a_max <- order_freq_table[1, 1]
    a_min <- order_freq_table[2, 1]
    
    r_max <- order_freq_table[1, 3]
    r_min <- order_freq_table[2, 3]
    
    if (r_max == '1.00')  {
      out <- c(Alleles = a_max,
               Frequency = r_max)
    } else {
      out <- c(
        Alleles = paste0(a_max, '/', a_min),
        Frequency = paste0(r_max, '/', r_min)
      )
    }
    
  }
  
  out
  
}


freq_alle_df <- map_dfr(d, freq_alle, .id = 'SNPs')

freq_alle_df |> view()


# Transformação dos genótipos (Heterozigoto recebe NA) --------------------

sub_gen <- function(x) {
  x |>
    plyr::count() -> freq_table
  
  order_freq_table  <- freq_table[order(freq_table$freq, decreasing = T), ]
  
  gen_major_freq <- order_freq_table[1, 1]
  gen_minor_freq <- order_freq_table[2, 1]
  
  ifelse(x == gen_major_freq, 2, ifelse(x == gen_minor_freq, 0, NA))
  
}

coded_d1 <- apply(d, 2, sub_gen)

coded_d1 # generated using apply()

coded_d2 <- map_df(d, sub_gen)

coded_d2 # generated using map_df()

saveRDS(coded_d2, file = 'coded_d2.rds')

# MAF and Missing Values --------------------------------------------------

maf_missing_values_freq <-  function(df) {
  
  f_maf <-
    function (x) {
      min(c(
        mean(x == 0, na.rm = T) + mean(x == 1, na.rm = T) / 2,
        mean(x == 2, na.rm = T) + mean(x == 1, na.rm = T) / 2
      ))
    }
  
  maf <- apply(df, 2, f_maf)
  
  mv <- colMeans(is.na(df))
  
  out <- data.frame(MAF = maf, Missing_Values = mv) |>
    tibble::rownames_to_column(var = 'Markers') |>
    as_tibble()
  
  out
  
}

maf_missing_values_freq(coded_d2)
