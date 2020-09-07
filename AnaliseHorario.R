#--Analise dos dados para cada hora--#
library(stringr)
library(ggplot2)

#Funcoes auxiliares
fAjustaHor <- function(h){
  ret <- (as.numeric(h) - tempoIni) %% tempoDia
  hor <- ret %/% 3600
  min <- (ret %% 3600) %/% 60
  
  ret <- paste(str_pad(as.character(hor), 2, "left", "0"), ":")
  ret <- paste(ret, str_pad(as.character(min), 2, "left", "0"))
  
  ret <- as.POSIXct(ret,format="%H : %M")
  
  return (ret)
}

#Ajustando as datas para suas devidas horas do dia
dfAgrupa <- df
tempoIni <- as.numeric(dfAgrupa[1, 1])
tempoDia <- 86400 #Segundos num dia

dfAgrupa$horario <- fAjustaHor(dfAgrupa$horario)

#Ajustando a ordem do DF
ajustaOrdem <- order(dfAgrupa$horario)
dfAgrupa$horario <- dfAgrupa$horario[ajustaOrdem]
dfAgrupa$temp    <- dfAgrupa$temp[ajustaOrdem]
dfAgrupa$vento   <- dfAgrupa$vento[ajustaOrdem]
dfAgrupa$umid    <- dfAgrupa$umid[ajustaOrdem]
dfAgrupa$sensa   <- dfAgrupa$sensa[ajustaOrdem]

#Agrupando e mostrando os graficos
dfAgrupa <- aggregate(dfAgrupa[, 2:5], by=list(dfAgrupa$horario), FUN=mean)
colnames(dfAgrupa)[1] <- "horario"

#Limpando as abicissas
plot <- ggplot(dfAgrupa, aes(x = horario, y = temp))
plot <- plot + geom_point() + scale_x_datetime(date_label = "%H:%M")
print(plot)