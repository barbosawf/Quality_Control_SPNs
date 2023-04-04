

# Redundancy function -----------------------------------------------------

redundancy <- function(df, corr = 0.99) {
  cor_d <- cor(df, use = "na.or.complete")
  
  p <- dim(cor_d)[1]
  
  vec_cor <- cor_d[!upper.tri(cor_d, diag = T)]
  
  combs <- t(combn(1:p, 2))
  colnames(combs) <- c('v1', 'v2')
  
  m_cor <- as.data.frame(cbind(combs, vec_cor))
  
  condition <- abs(m_cor[, 3]) > corr
  
  if (sum(condition) > 0) {
    m_fil <- m_cor[condition,]
    
    vars_fil <- unique(m_fil[, 2])
    
    df[,-vars_fil]
  } else {
    df
  }
  
}

df <- read.table('dados_rep_1_cen_1.txt', header = T)
df <- df[, -c(1:2)]
dim(df)
head(df[, 1:10])

clean_df <- redundancy(df, corr = 0.99)
dim(clean_df)
head(clean_df)


df <- read.csv('genotype.csv', header = F)
head(df[, 1:10])
dim(df)

clean_df <- redundancy(df, corr = 0.99)
dim(clean_df)
head(clean_df[, 1:10])


