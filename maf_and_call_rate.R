maf_and_call_rate <-
  
  function(df,
           maf_threshold = 0.05,
           call_rate_threshold = 0.90,
           do_maf_call_rate = c(TRUE, TRUE)) {
    
    if (!is.logical(do_maf_call_rate) |
        !(length(do_maf_call_rate) == 2)) {
      stop('ATENTION! "do_maf_call_rate" must be a logical vector with two elements!')
    }
    
    if (do_maf_call_rate[1]) {
      f_maf <-
        function (x) {
          min(c(
            mean(x == 0, na.rm = T),
            mean(x == 1, na.rm = T),
            mean(x == 2, na.rm = T)
          ))
          
          maf <- apply(df, 2, f_maf)
          
          df <- df[, maf > maf_threshold]
        }
    }
    
    if (do_maf_call_rate[2]) {
      
      cr <- as.logical(colMeans(!is.na(df)))
      
      df <- df[, cr > maf_threshold]
    }
    
    df
    
  }

 maf_and_call_rate(df, do_maf_call_rate = c(TRUE, TRUE))

