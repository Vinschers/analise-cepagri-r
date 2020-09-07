#Analise da crrelacao entre umidade e sensacao termica

library(ggplot2)

get_mean_dataframe <- function(df){
  sum_sens <- 0
  sum_umid <- 0
  sum_vento <- 0
  sum_temp <- 0
  
  counter <- 0
  current_day <- 1
  
  mean_sens <- c()
  mean_umid <- c()
  mean_vento <- c()
  mean_temp <- c()
  days <- c()
  
  for (i in 1:nrow(df)) {
    curr_day <- as.POSIXlt(df[i, 1])$mday
    if(curr_day != current_day){
      
      days <- c(days, as.numeric(df[i-1, 1]))
      
      mean_sens <- c(mean_sens, as.numeric(sum_sens/counter))
      mean_umid <- c(mean_umid, as.numeric(sum_umid/counter))
      mean_vento <- c(mean_vento, as.numeric(sum_vento/counter))
      mean_temp <- c(mean_temp, as.numeric(sum_temp/counter))
      
      sum_sens <- 0
      sum_umid <- 0
      sum_vento <- 0
      sum_temp <- 0
      counter <- 0
      current_day <- curr_day
    }
    
    sum_temp <- sum_temp + df[i, 2]
    sum_vento <- sum_vento + df[i, 3]
    sum_umid <- sum_umid + df[i, 4]
    sum_sens <- sum_sens + df[i, 5]
    
    counter <- counter + 1
  }
  dfRet <- data.frame(dia=as.POSIXct(days, format="%d/%m/%Y", origin="01/01/1970", tz=Sys.timezone()), temp=mean_temp, vento=mean_vento, umid=mean_umid, sensa=mean_sens)
  
  rm(counter)
  rm(curr_day)
  rm(current_day)
  rm(sum_sens)
  rm(sum_umid)
  rm(days)
  rm(mean_sens)
  rm(mean_umid)
  rm(i)
  
  return(dfRet)
}

plotTimeXUmidSens <- function(df, start="2015-01-01", end="2020-01-01") {
  df$umid <- df$umid/max(df$umid)
  df$sensa <- df$sensa/max(df$sensa)
  
  ggplot(data=df[df$dia >= start & df$dia < end,], aes(dia)) +
    geom_line(aes(y=umid, colour="umid"), size=1) +
    geom_line(aes(y=sensa, colour="sensa"), size=1)
}

plotUmidXSens <- function(df) {
  umids <- sort(unique(x=floor(df$umid)))
  sens <- c()
  for (i in 1:length(umids)) {
    sens <- c(sens, mean(df[floor(df$umid) == umids[i], 5]))
  }
  
  df2 = data.frame(umid=umids, sensa=sens)
  
  ggplot(df2, aes(umid, sensa)) +
    geom_smooth(method='lm', formula= y~x) +
    geom_point()
}

#dfMean = get_mean_dataframe(df)
plotUmidXSens(dfMean)
