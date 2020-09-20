library(stringr)
library(ggplot2)

myView <- function(x, title) #Funcao para printar as tabelas
  get("View", envir = as.environment("package:utils"))(x, title)

#====================================================================================
#Analisando os dados brutos

leitura_de_dados <- function(caminho) {
  #Lendo os dados do csv ja baixado
  names <- c("horario", "temp", "vento", "umid", "sensa")
  df <- read.csv(caminho, header = FALSE, sep = ";", col.names = names)

  #Ajustando os tipos das colunas
  df$horario <- as.POSIXct(df$horario,format="%d/%m/%Y-%H:%M",tz=Sys.timezone())

  #Deletando variaveis que nao serao mais usadas
  rm(names)
  return(df)
}

print_temperatura_2018_2019 <- function(df) {
  dfMenor <- df
  dfMenor$dataT <- format(strptime(df$horario,"%Y-%m-%d %H:%M:%S"), '%Y-%m')


  dfMenor <- dfMenor[dfMenor['dataT'] > '2018-08' & dfMenor['dataT'] < '2019-02',]
  plot <- ggplot(dfMenor, aes(x = horario, y = temp, alpha = 1))
  plot <- plot + geom_point()
  plot <- plot + xlab("Mês (2018-2019)")
  plot <- plot + ylab("Temperatura(°C)")

  print(plot)
}

print_intervalos_entre_registros <- function(df) {
  inters = 10
  for (i in 2:nrow(df)) {
    inters <- c(inters, df[i, 1] - df[i - 1, 1])
  }

  dfInters <- data.frame(Intervalos = as.factor(round(inters[inters != 10])), 1)

  plot <- ggplot(dfInters, aes(x = Intervalos))
  plot <- plot + geom_bar()
  print(plot)

  #Deletando variaveis que nao serao mais usadas
  rm(i)
}

#====================================================================================
#Analise dos erros do DataFrame original

anos <- c("2014","2015","2016","2017","2018","2019","2020") # Coluna com os anos
get_tabelaErro <- function(df) {
  #grafico com frequencia de [ERRO] na temperatura em cada ano
  dfErros <- df[df$temp == " [ERRO]", ]
  
  summary(dfErros)
  er2014 <- nrow(dfErros[dfErros$horario >= "2014-01-01" & dfErros$horario < "2015-01-01", ])#erros em 2014
  er2015 <- nrow(dfErros[dfErros$horario >= "2015-01-01" & dfErros$horario < "2016-01-01", ])#erros em 2015
  er2016 <- nrow(dfErros[dfErros$horario >= "2016-01-01" & dfErros$horario < "2017-01-01", ])#erros em 2016
  er2017 <- nrow(dfErros[dfErros$horario >= "2017-01-01" & dfErros$horario < "2018-01-01", ])#erros em 2017
  er2018 <- nrow(dfErros[dfErros$horario >= "2018-01-01" & dfErros$horario < "2019-01-01", ])#erros em 2018
  er2019 <- nrow(dfErros[dfErros$horario >= "2019-01-01" & dfErros$horario < "2020-01-01", ])#erros em 2019
  er2020 <- nrow(dfErros[dfErros$horario >= "2020-01-01", ])#erros em 2020
  
  erros <- c(er2014,er2015,er2016,er2017,er2018,er2019,er2020)
  
  tabelaErro <- data.frame(Ano=anos, QtdErros=erros,stringsAsFactors=TRUE); # Data Frame com os anos e qtd erros equivalentes
  return(tabelaErro)
}

#Gráfico de [ERRO]:
print_graficoERRO <- function(df){
  tabelaErro <- get_tabelaErro(df)
  plot <- ggplot(tabelaErro, aes(x = anos, y = QtdErros, group= 1))
  plot <- plot + geom_point() + geom_line()
  print(plot)
}
#====================================================================================
#Removendo os erros do DataFrame

retira_ERRO <- function(df) {
  #Tratamento de dados:
  #Removendo linhas que possuam valor nulo
  df <- df[df$temp != " [ERRO]", ]
  return(df)
}

#====================================================================================
#Grafico Sensacao Termica Elevada(após padronizar as datas):
print_graficoSensaTermElevada <- function(df){
  tabelaErro <- get_tabelaErro(df)
  s2014 <- nrow(df[df$horario >= "2014-01-01" & df$horario < "2015-01-01" & df$sensa > 90, ])#sensacoes em 2014
  s2015 <- nrow(df[df$horario >= "2015-01-01" & df$horario < "2016-01-01" & df$sensa > 90, ])#sensacoes em 2015
  s2016 <- nrow(df[df$horario >= "2016-01-01" & df$horario < "2017-01-01" & df$sensa > 90, ])#sensacoes em 2016
  s2017 <- nrow(df[df$horario >= "2017-01-01" & df$horario < "2018-01-01" & df$sensa > 90, ])#sensacoes em 2017
  s2018 <- nrow(df[df$horario >= "2018-01-01" & df$horario < "2019-01-01" & df$sensa > 90, ])#sensacoes em 2018
  s2019 <- nrow(df[df$horario >= "2019-01-01" & df$horario < "2020-01-01" & df$sensa > 90, ])#sensacoes em 2019
  s2020 <- nrow(df[df$horario >= "2020-01-01" & df$sensa > 90, ])#sensacoes em 2020
  
  
  sensacoesElevadas <- c(s2014,s2015,s2016,s2017,s2018,s2019,s2020)
  
  anos <- c("2014","2015","2016","2017","2018","2019","2020") # Coluna com os anos
  
  tabelaSensacoes <- data.frame(Ano=anos, QtdSensacoesElevadas=sensacoesElevadas,stringsAsFactors=TRUE); # Data Frame com os anos e temperaturas médias equivalentes
  plot <- ggplot(tabelaErro, aes(x = anos, y = sensacoesElevadas, group= 1))
  plot <- plot + geom_point() + geom_line()
  print(plot)
}

#====================================================================================
#Gráfico de umidade zero(após padronizar as datas):
print_graficoUmidZero <- function(df){
  u2014 <- nrow(df[df$horario >= "2014-01-01" & df$horario < "2015-01-01" & df$umid == 0, ])#umidade em 2014
  u2015 <- nrow(df[df$horario >= "2015-01-01" & df$horario < "2016-01-01" & df$umid == 0, ])#umidade em 2015
  u2016 <- nrow(df[df$horario >= "2016-01-01" & df$horario < "2017-01-01" & df$umid == 0, ])#umidade em 2016
  u2017 <- nrow(df[df$horario >= "2017-01-01" & df$horario < "2018-01-01" & df$umid == 0, ])#umidade em 2017
  u2018 <- nrow(df[df$horario >= "2018-01-01" & df$horario < "2019-01-01" & df$umid == 0, ])#umidade em 2018
  u2019 <- nrow(df[df$horario >= "2019-01-01" & df$horario < "2020-01-01" & df$umid == 0, ])#umidade em 2019
  u2020 <- nrow(df[df$horario >= "2020-01-01" & df$umid == 0, ])#umidade em 2020
  
  
  umidadesZero <- c(u2014,u2015,u2016,u2017,u2018,u2019,u2020)
  
  tabelaErro <- get_tabelaErro(df)
  
  tabelaUmidade <- data.frame(Ano=anos, QtdUmidadesZero=umidadesZero,stringsAsFactors=TRUE); # Data Frame com os anos e qtd umidades zero
  plot <- ggplot(tabelaErro, aes(x = anos, y = umidadesZero, group= 1))
  plot <- plot + geom_point() + geom_line()
  print(plot)
}

#====================================================================================
#Processamento inicial

processa_dataframe <- function(df) {
  df$horario <- as.POSIXct(df$horario,format="%d/%m/%Y-%H:%M",tz=Sys.timezone())
  #Ajustando os tipos das colunas
  df$temp <- as.numeric(df$temp)
  #Removendo as linhas que nao serao analisadas
  df <- df[df$horario >= "2015-01-01" & df$horario < "2020-01-01", ]

  #Ajustando os nomes das linhas
  rownames(df) <- NULL

  #Ajustando valores
  for(i in 2:nrow(df))
  {
    if(df$umid[i] == 0) #Umidade
      df$umid[i] <- df$umid[i - 1]
    
    if(df$sensa[i] > 90) #Sensacao termica
      df$sensa[i] <- df$sensa[i - 1]
  }

  #Removendo horarios repetidos
  df <- df[!duplicated(df$horario), ]

  #Ajustando os nomes das linhas
  rownames(df) <- NULL

  #Deletando variaveis que nao serao mais usadas
  rm(i)
  
  return(df)
}


#====================================================================================
#Gráfico de Temperatura média anual:
print_graficoOuTabelaTempMediaAnual <- function(df, mostra_tabela){
  tempMediaAnuais <- c(mean(df$temp[df$horario>="2015-01-01" & df$horario<"2016-01-01"]),
                       mean(df$temp[df$horario>="2016-01-01" & df$horario<"2017-01-01"]),
                       mean(df$temp[df$horario>="2017-01-01" & df$horario<"2018-01-01"]),
                       mean(df$temp[df$horario>="2018-01-01" & df$horario<"2019-01-01"]),
                       mean(df$temp[df$horario>="2019-01-01"])) # Coluna com as médias de temperatura equivalentes aos anos
  
  tabela1 <- data.frame(Ano=anos[2:6], Media=tempMediaAnuais,
                        stringsAsFactors=TRUE); # Data Frame com os anos e temperaturas médias equivalentes
  
  if (!mostra_tabela) {
    plot <- ggplot(tabela1, aes(x = anos[2:6], y = tempMediaAnuais, group= 1))
    plot <- plot + geom_point() + geom_line() + xlab("Anos") + ylab("Temperatura (oC)")
  
    print(plot)
  }
  else {
    myView(tabela1)
  }
}

#====================================================================================
#retorna um dataframe que contém a média dos valores de cada dia
fCalcula_media_diaria <- function(df){
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
    
    #se o contador chegar, é um novo dia
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
  
  #dataframe de retorno é feito a partir dos vetores obtidos anteriormente
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
  plot <- ggplot(data=df[df$dia >= start & df$dia < end,], aes(dia)) +
    geom_line(aes(y=umid, colour="umid"), size=1) +
    geom_line(aes(y=sensa, colour="sensa"), size=1) +
    xlab("Data") +
    ylab("Umidade & Sensação térmica")
  
  print(plot)
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
  
  plot <- ggplot(df2, aes(umid, sensa)) +
    geom_smooth(method='lm', formula= y~x) +
    geom_point() +
    xlab("Umidade (%)") +
    ylab("Sensação térmica (°C)")
  
  print(plot)
  
  return(corr)
}

#====================================================================================
#Analise das variaveis por horario do dia

tempoIni <- 0 #Guarda o valor o primeiro horario disponivel no DataFrame

fAgrupa_horario <- function(df) {
  #Ajustando as datas para suas devidas horas do dia
  dfAgrupaHor <- df
  tempoIni <- as.numeric(dfAgrupaHor[1, 1])
  return(dfAgrupaHor)
}

fAjustaHor <- function(h){
  ret <- (as.numeric(h) - tempoIni) %% 86400 #Calculando quanto tempo se passou desdo inicio do dia
  #Descobrindo as horas e minutos e transformando em string
  hor <- ret %/% 3600
  min <- (ret %% 3600) %/% 60
  ret <- paste(str_pad(as.character(hor), 2, "left", "0"), ":")
  ret <- paste(ret, str_pad(as.character(min), 2, "left", "0"))
  
  return (ret)
}

fAjusta_dfHor <- function(df) {
  dfAgrupaHor <- fAgrupa_horario(df)
  dfAgrupaHor$horario <- fAjustaHor(dfAgrupaHor$horario) #Salvando as alteracoes

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

  return(dfAgrupaHor)
}

#Funcao para plotar
print_PlotHor <- function(dfPlot, tipo){
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
    plot <- plot + ylab("Umidade (%)")
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
fTabelaHor <- function(dfTabelaHor){
  #Tirando os minutos dos horarios: queremos ver as medias das horas
  dfTabelaHor$horario <- paste(substr(dfTabelaHor$horario, 1, 2), "h")
  
  #Agrupando os horarios e descobrindo as medias
  dfTabelaHor <- aggregate(dfTabelaHor[, 2:5], by=list(dfTabelaHor$horario), FUN=mean)
  colnames(dfTabelaHor)[1] <- "horario"
  
  return (dfTabelaHor)
}

#====================================================================================
#Analise dos dados de acordo com as estacoes

fColunasEstacoes = function (df) {
  df["estacao"] = 0
  df["diaEstacao"] = 0
  
  #Cria as colunas estacao e diaEstacao
  for (x in 1:nrow(df)) {
    horario <- as.POSIXlt(df[x, "horario"])
    dia <- horario$yday;
    if (horario$year == 116 && dia > 58) { # ano bissexto
      dia = dia - 1
    } 
    
    if (dia >= 78 && dia < 170) { # entre 20 de março e 19 de junho
      df[x, "estacao"] = "outono"
      df[x, "diaEstacao"] = dia - 77
    } else if (dia >= 170 && dia < 264) { # entre 20 de junho e 21 de setembro
      df[x, "estacao"] = "inverno"
      df[x, "diaEstacao"] = dia - 169
    } else if (dia >= 264 && dia < 354) { # entre 22 de setembro e 20 de dezembro
      df[x, "estacao"] = "primavera"
      df[x, "diaEstacao"] = dia - 263
    } else { # entre 21 de dezembro e 19 de março
      df[x, "estacao"] = "verao"
      if (dia <= 78) {
        df[x, "diaEstacao"] = dia + 12
      } else {
        df[x, "diaEstacao"] = dia - 353
      }
    }
  }
  return(df)
}

fAgregarColunas = function(df, colunaAPlotar) {
  # Separando em dataframes pra cada estação, pra poder agregar pelo dia em seguida
  dfVerao = df[df['estacao'] == 'verao',]
  dfInverno = df[df['estacao'] == 'inverno',]
  dfPrimavera = df[df['estacao'] == 'primavera',]
  dfOutono = df[df['estacao'] == 'outono',]
  
  # Média da coluna por dia da estação
  dfVerao = aggregate(dfVerao[colunaAPlotar], by = list(DiaEstacao=dfVerao$diaEstacao), FUN=mean)
  dfVerao['estacao'] = 'verao'
  dfInverno = aggregate(dfInverno[colunaAPlotar], by =  list(DiaEstacao=dfInverno$diaEstacao), FUN=mean)
  dfInverno['estacao'] = 'inverno'
  dfPrimavera = aggregate(dfPrimavera[colunaAPlotar], by = list(DiaEstacao=dfPrimavera$diaEstacao), FUN=mean)
  dfPrimavera['estacao'] = 'primavera'
  dfOutono = aggregate(dfOutono[colunaAPlotar], by = list(DiaEstacao=dfOutono$diaEstacao), FUN=mean)
  dfOutono['estacao'] = 'outono'
  
  # Unindo todos os dataframes novamente
  dfEstacoes = rbind(dfVerao, dfInverno, dfPrimavera, dfOutono)

  return(dfEstacoes)
}

fPlotarEstacoes = function(dfEstacoes, colunaAPlotar) {
  # Plotando o gráfico
  if (colunaAPlotar == 'umid') {
    p <- ggplot(dfEstacoes,aes(x = DiaEstacao, y=umid, colour=estacao))
    p <- p + ylab("Umidade do ar (%)")
  }
  else if (colunaAPlotar == 'temp') {
    p <- ggplot(dfEstacoes,aes(x = DiaEstacao, y=temp, colour=estacao))
    p <- p + ylab("Temperatura (°C)")
  }
  else if (colunaAPlotar == 'sensa') {
    p <- ggplot(dfEstacoes,aes(x = DiaEstacao, y=sensa, colour=estacao))
    p <- p + ylab("Sensação Térmica (°C)")
  }
  else if (colunaAPlotar == 'vento') {
    p <- ggplot(dfEstacoes,aes(x = DiaEstacao, y=vento, colour=estacao))
    p <- p + ylab("Vento (km/h)")
  }
  if (colunaAPlotar == 'temp' || colunaAPlotar == 'sensa') {
    p <- p + geom_point()
    p <- p + geom_line()
  }
  p <- p + geom_smooth(method='loess', formula= y~x)
  p <- p + xlab("Dias desde o início da estação")
  p <- p + labs(colour="Estação")
  print(p)
}

fTabelaEstacoes = function(df) {
  # Tabela
  tabelaEstacoes = aggregate(df[c("temp", "vento", "umid", "sensa")], by=list(Estacao=df$estacao), FUN=mean)
  colnames(tabelaEstacoes)[2] <- "Temperatura (°C)"
  colnames(tabelaEstacoes)[3] <- "Vento (km/h)"
  colnames(tabelaEstacoes)[4] <- "Umidade (%)"
  colnames(tabelaEstacoes)[5] <- "Sensação Térmica (°C)"
  return(tabelaEstacoes)
}

fCorEstacoes = function(df) {
  # Correlação de Pearson de temperatura e sensação térmica
  cor(df$temp, df$sensa, method = c("pearson"))
}

#====================================================================================
#Criando Menu interativo

caminho <- readline("Digite o caminho absoluto para o cepagri.csv: ")

print("Aviso: todo o processamento pode levar cerca de 10 minutos.")

if (!exists("df_bruto"))
  df_bruto <- leitura_de_dados(caminho)
print("Processamento bruto OK")

if (!exists("df_sem_erros"))
  df_sem_erros <- retira_ERRO(df_bruto)
print("Processamento sem erros OK")

if(!exists("df_processado"))
  df_processado <- processa_dataframe(df_sem_erros)
print("Processamento geral OK")

if(!exists("df_media_dia"))
  df_media_dia <- fCalcula_media_diaria(df_processado)
print("Processamento por dia OK")

if(!exists("df_media_hora"))
  df_media_hora <- fAjusta_dfHor(df_processado)
print("Processamento por horario OK")

if(!exists("df_tabela_estacoes"))
  df_tabela_estacoes <- fColunasEstacoes(df_processado)
if(!exists("df_estacoes"))
  df_estacoes <- fAgregarColunas(df_tabela_estacoes)
print("Processamento estacoes OK")

opcoes <- c("Gráfico de erros", "Gráfico de erros de sensação térmica", "Gráfico de erros de umidade",
            "Gráfico de intervalos != 10", "Gráfico final de 2018 e começo de 2019", "Gráfico de temperatura média anual",
            "Tabela da temperatura média anual", "Gráfico de umidade do ar e sensação térmica pelo tempo",
            "Gráfico de correlação de umidade do ar e sensação térmica com regressão linear", "Tabela da correlação entre umidade do ar e sensação térmica",
            "Gráfico de temperatura por hora do dia", "Gráfico de umidade por hora do dia", "Gráfico de vento por hora do dia",
            "Gráfico de sensação térmica por hora do dia", "Tabela das variáveis pelo horário do dia", "Tabela das variáveis pelas estações",
            "Gráfico de temperatura por estação", "Gráfico de umidade por estação", "Gráfico de vento por estação", "Gráfico de sensação térmica por estação")

while (T) {
  for (i in 1:length(opcoes)) {
    print(paste(as.character(i), ". ", opcoes[i]))
  }
  
  opcao <- as.numeric(readline("Selecione a opção (0 para finalizar): "))
  
  if (opcao < 1 || opcao > 20) {
    stop("Finalizando programa.")
  }
  
  if (opcao == 1) {
    print_graficoERRO(df_bruto)
  } else if (opcao == 2) {
    print_graficoSensaTermElevada(df_sem_erros)
  } else if (opcao == 3) {
    print_graficoUmidZero(df_sem_erros)
  } else if (opcao == 4) {
    print("Aviso: grafico pode demorar cerca de 2 minutos para aparecer.")
    print_intervalos_entre_registros(df_sem_erros)
  } else if (opcao == 5) {
    print_temperatura_2018_2019(df_processado)
  } else if (opcao == 6) {
    print_graficoOuTabelaTempMediaAnual(df_processado, F)
  } else if (opcao == 7) {
    print_graficoOuTabelaTempMediaAnual(df_processado, T)
  } else if (opcao == 8) {
    plotTimeXUmidSens(df_media_dia, end="2015-03-30")
  } else if (opcao == 9) {
    plotUmidXSens(df_media_dia)
  } else if (opcao == 10) {
    myView(plotUmidXSens(df_media_dia))
  } else if (opcao == 11) {
    print_PlotHor(df_media_hora, "temp")
  } else if (opcao == 12) {
    print_PlotHor(df_media_hora, "umid")
  } else if (opcao == 13) {
    print_PlotHor(df_media_hora, "vento")
  } else if (opcao == 14) {
    print_PlotHor(df_media_hora, "sensa")
  } else if (opcao == 15) {
    myView(fTabelaHor(df_media_hora))
  } else if (opcao == 16) {
    myView(fTabelaEstacoes(df_tabela_estacoes))
  } else if (opcao == 17) {
    fPlotarEstacoes(df_estacoes, "temp")
  } else if (opcao == 18) {
    fPlotarEstacoes(df_estacoes, "umid")
  } else if (opcao == 19) {
    fPlotarEstacoes(df_estacoes, "vento")
  } else if (opcao == 20) {
    fPlotarEstacoes(df_estacoes, "sensa")
  }
}
