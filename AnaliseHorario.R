#--Analise dos dados para cada hora--#
library(stringr)
library(ggplot2)

#Ajustando as datas para suas devidas horas do dia
dfAgrupaHor <- df
tempoIni <- as.numeric(dfAgrupaHor[1, 1])
tempoDia <- 86400 #Segundos num dia

fAjustaHor <- function(h){
  ret <- (as.numeric(h) - tempoIni) %% tempoDia #Calculando quanto tempo se passou desdo inicio do dia
  #Descobrindo as horas e minutos e transformando em string
  hor <- ret %/% 3600
  min <- (ret %% 3600) %/% 60
  
  ret <- paste(str_pad(as.character(hor), 2, "left", "0"), ":")
  ret <- paste(ret, str_pad(as.character(min), 2, "left", "0"))
  
  return (ret)
}

dfAgrupaHor$horario <- fAjustaHor(dfAgrupaHor$horario)

#Ajustando a ordem do DF
ajustaOrdem <- order(dfAgrupaHor$horario)
dfAgrupaHor$horario <- dfAgrupaHor$horario[ajustaOrdem]
dfAgrupaHor$temp    <- dfAgrupaHor$temp[ajustaOrdem]
dfAgrupaHor$vento   <- dfAgrupaHor$vento[ajustaOrdem]
dfAgrupaHor$umid    <- dfAgrupaHor$umid[ajustaOrdem]
dfAgrupaHor$sensa   <- dfAgrupaHor$sensa[ajustaOrdem]

#Agrupando e mostrando os graficos
dfAgrupaHor <- aggregate(dfAgrupaHor[, 2:5], by=list(dfAgrupaHor$horario), FUN=mean)
colnames(dfAgrupaHor)[1] <- "horario"

#Funcao para plotar
fPlotHor <- function(tipo){
  dfPlot <- dfAgrupaHor
  #O eixo X fica mais legivel se for usado variaveis do tipo "POSIXct"
  dfPlot$horario <- as.POSIXct(dfPlot$horario,format="%H : %M")
  
  #Para cada uma das opcoes, printaremos o plot devido
  if(tipo == "temp")
  {
    plot <- ggplot(dfPlot, aes(x = horario, y = temp))
    plot <- plot + geom_point() + scale_x_datetime(date_label = "%H:%M")
    plot <- plot + xlab("Horários")
    plot <- plot + ylab("Temperatura (°C)")
    print(plot)
  }
  else if(tipo == "vento")
  {
    plot <- ggplot(dfPlot, aes(x = horario, y = vento))
    plot <- plot + geom_point() + scale_x_datetime(date_label = "%H:%M")
    plot <- plot + xlab("Horários")
    plot <- plot + ylab("Vento (km/h)")
    print(plot)
  }
  else if(tipo == "umid")
  {
    plot <- ggplot(dfPlot, aes(x = horario, y = umid))
    plot <- plot + geom_point() + scale_x_datetime(date_label = "%H:%M")
    plot <- plot + xlab("Horários")
    plot <- plot + ylab("Humidade (%)")
    print(plot)
  }
  else if(tipo == "sensa")
  {
    plot <- ggplot(dfPlot, aes(x = horario, y = sensa))
    plot <- plot + geom_point() + scale_x_datetime(date_label = "%H:%M")
    plot <- plot + xlab("Horários")
    plot <- plot + ylab("Sensa (°C)")
    print(plot)
  }
}

#Funcao para mostrar a tabela
fTabelaHor <- function(){
  #Tirando os minutos dos horarios: queremos ver as medias das horas
  dfTabelaHor <- dfAgrupaHor
  dfTabelaHor$horario <- paste(substr(dfTabelaHor$horario, 1, 2), "h")
  
  #Agrupando os horarios e descobrindo as medias
  dfTabelaHor <- aggregate(dfTabelaHor[, 2:5], by=list(dfTabelaHor$horario), FUN=mean)
  colnames(dfTabelaHor)[1] <- "horario"
  
  return (dfTabelaHor)
}

#Deletando variaveis de suporte
rm(tempoDia, tempoIni, fAjustaHor, ajustaOrdem)