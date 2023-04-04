maf_and_call_rate <-
  
  function(df,
           maf_threshold = 0.05,
           cr_threshold = 0.90,
           do_maf_call_rate = c(TRUE, TRUE)) {
    
    if (!is.logical(do_maf_call_rate) |
        !(length(do_maf_call_rate) == 2)) {
      stop('ATENTION! "do_maf_call_rate" must be a logical vector with two elements!')
    }
    
    if (do_maf_call_rate[1]) {
      f_maf <-
        function (x) {
          min(c(
            mean(x == 0, na.rm = T) + mean(x == 1, na.rm = T) / 2,
            mean(x == 2, na.rm = T) + mean(x == 1, na.rm = T) / 2
          ))
        }
      
      maf <- apply(df, 2, f_maf)
      
      df <- df[, maf > maf_threshold]
      
    }
    
    if (do_maf_call_rate[2]) {
      cr <- as.logical(colMeans(!is.na(df)))
      
      df <- df[, cr > cr_threshold]
    }
    
    df
    
  }

df <- read.table('dados_rep_1_cen_1.txt', header = T)
df <- df[, -c(1:2)]
dim(df)
head(df[, 1:10])
new_df <- maf_and_call_rate(df)
dim(new_df)
