prepseries <- function(stock, len, insamp){
  df <- get(stock)
  df <- as.data.frame(df)
  
  select <- paste(stock, ".Adjusted", sep = "")
  series <- as.numeric(df[,select])
  
  series.1 <- series[-1]
  series.2 <- series[-length(series)]
  returns  <- (log(series.1) - log(series.2))*100
  
  outsamp <- floor((1 - insamp)*len)
  total   <- len + outsamp
  
  returns  <- returns[(length(returns) - total + 1):(length(returns))]
  returns.in <- returns[1:(length(returns) - outsamp)]
  returns.out <- returns[(length(returns) - outsamp + 1):(length(returns))]
  print(total)
  return(list(insamp = returns.in, outsamp = returns.out))
}
