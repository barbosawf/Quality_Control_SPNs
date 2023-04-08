
# MAF and Call Rate -------------------------------------------------------

maf_and_call_rate <-
  
  function(df,
           maf_threshold = 0.05,
           cr_threshold = 0.90,
           do_call_rate_maf = c(TRUE, TRUE)) {
    
    if (!is.logical(do_maf_call_rate) |
        !(length(do_maf_call_rate) == 2)) {
      stop('ATENTION! "do_maf_call_rate" must be a logical vector with two elements!')
    }
    
    
    test1 <- colMeans(is.na(df)) == 1
    test2 <- sum(test1 == TRUE)
    
    
    if (test2 > 0) {
      if (test2 == 1) {
        ini_f <- 'There is'
        med_f <- 'marker'
      } else {
        ini_f <- 'There are'
        med_f <- 'markers'
      }
      cat(
        paste(
          ini_f,
          test2,
          med_f,
          'filled entirely with NA.
Removing marks under this condition is done regardless of the call rate.'
        )
      )
      df <- df[,!test1]
    }
    
    
    if (do_maf_call_rate[1]) {
      cr <- colMeans(!is.na(df))
      
      df <- df[, cr > cr_threshold]
    }
    
    
    if (do_maf_call_rate[2]) {
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
    
    df
    
  }

coded_d2 <- readRDS(file = 'coded_d2.rds')

dim(coded_d2)
head(coded_d2[, 1:15])
new_df <- maf_and_call_rate(coded_d2)
new_df
dim(new_df)
view(new_df)




