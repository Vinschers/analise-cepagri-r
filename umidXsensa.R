#Analise da correlacao entre umidade e sensacao termica

library(ggplot2)

#retorna um dataframe que contém a média dos valores de cada dia
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
  
  #percorre-se todo o dataframe
  for (i in 1:nrow(df)) {
    curr_day <- as.POSIXlt(df[i, 1])$mday
    
    #se o contador chegar à um novo dia
    if(curr_day != current_day){
      
      #salvar o dia anterior no vetor de dias
      days <- c(days, as.numeric(df[i-1, 1]))
      
      #calcula a média dos parâmetros do dia anterior e salva nos seus respectivos vetores
      mean_sens <- c(mean_sens, as.numeric(sum_sens/counter))
      mean_umid <- c(mean_umid, as.numeric(sum_umid/counter))
      mean_vento <- c(mean_vento, as.numeric(sum_vento/counter))
      mean_temp <- c(mean_temp, as.numeric(sum_temp/counter))
      
      #soma dos parâmetros resetada
      sum_sens <- 0
      sum_umid <- 0
      sum_vento <- 0
      sum_temp <- 0
      counter <- 0
      current_day <- curr_day
    }
    
    #soma-se os valores da hora atual
    sum_temp <- sum_temp + df[i, 2]
    sum_vento <- sum_vento + df[i, 3]
    sum_umid <- sum_umid + df[i, 4]
    sum_sens <- sum_sens + df[i, 5]
    
    counter <- counter + 1
  }
  
  #dataframe de retorno é feito à partir dos vetores obtidos anteriormente
  dfRet <- data.frame(dia=as.POSIXct(days, format="%d/%m/%Y", origin="01/01/1970", tz=Sys.timezone()), temp=mean_temp, vento=mean_vento, umid=mean_umid, sensa=mean_sens)
  
  #remoção de variáveis desnecessárias
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

#Cria o plot que relaciona a Umidade e a Sensação térmica conforme o tempo passa
#start e end são parâmetros que indicam o intervalo que deve ser plotado
plotTimeXUmidSens <- function(df, start="2015-01-01", end="2020-01-01") {
  #normalização dos dados
  df$umid <- (df$umid - min(df$umid))/(max(df$umid) - min(df$sensa))
  df$sensa <- (df$sensa - min(df$sensa))/(max(df$sensa) - min(df$sensa))
  
  #plot em si
  ggplot(data=df[df$dia >= start & df$dia < end,], aes(dia)) +
    geom_line(aes(y=umid, colour="umid"), size=1) +
    geom_line(aes(y=sensa, colour="sensa"), size=1) +
    xlab("Data") +
    ylab("Umidade & Sensação térmica")
}

#Cria um plot que dá a sensação térmica média em função da umidade
plotUmidXSens <- function(df) {
  umids <- sort(unique(x=floor(df$umid)))
  sens <- c()
  #cálculo da sensação térmica por umidade
  for (i in 1:length(umids)) {
    sens <- c(sens, mean(df[floor(df$umid) == umids[i], 5]))
  }
  
  #criação de um dataframe auxiliar
  df2 = data.frame(umid=umids, sensa=sens)
  corr = cor(df2)
  
  ggplot(df2, aes(umid, sensa)) +
    geom_smooth(method='lm', formula= y~x) +
    geom_point() +
    xlab("Umidade (%)") +
    ylab("Sensação térmica (°C)")
  
  return(corr)
}

#dfMean = get_mean_dataframe(df)
plotTimeXUmidSens(dfMean, "2015-01-01", "2015-03-30")