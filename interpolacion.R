RicksInterpolation <- function(col)
{
  col2 <- vector()
  col2[1] <- col[1]
  #i = 2
  for(i in 2:length(col))
  {
    col2[i] <-  ifelse( !is.na(col[i]) , col[i], 
                        ifelse(is.na(col2[i-1]), NA, 
                               ifelse(!is.na(col[i+1]), col2[i-1] + (col[i+1]-col2[i-1])/2,
                                      ifelse(!is.na(col[i+2]) ,col2[i-1] + (col[i+2]-col2[i-1])/3, 
                                             ifelse(!is.na(col[i+3]) ,col2[i-1] + (col[i+3]-col2[i-1])/4,
                                                    ifelse(!is.na(col[i+4]),col2[i-1] + (col[i+4]-col2[i-1])/5,
                                                           ifelse(!is.na(col[i+5]),col2[i-1] + (col[i+5]-col2[i-1])/6,NA)))))))
    
    
  }
  col2
}

a <- c(1,NA,3,4,5,6,7,8,9,10)
a <- c(1,NA,NA,NA,NA,6,7,8,9,10)
a <- c(1,NA,NA,NA,NA,NA,7,8,9,10)
a <- c(1,NA,NA,NA,NA,NA,NA,8,9,10)
RicksInterpolation(a)
