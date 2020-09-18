#Lendo os dados do csv ja baixado
names <- c("horario", "temp", "vento", "umid", "sensa")
df <- read.csv("D:\\gusta\\Documents\\Github\\analise-cepagri-r\\cepagri.csv", header = FALSE, sep = ";", col.names = names)

summary(df)

#Deletando variaveis que nao serao mais usadas
rm(names)


#Ajustando os tipos das colunas
df$horario <- as.POSIXct(df$horario,format="%d/%m/%Y-%H:%M",tz=Sys.timezone())
#====================================================================================
#Gráfico de [ERRO]:
graficoERRO <- function(df){
  
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
  
  anos <- c("2014","2015","2016","2017","2018","2019","2020") # Coluna com os anos
  
  tabelaErro <- data.frame(Ano=anos, QtdErros=erros,stringsAsFactors=TRUE); # Data Frame com os anos e qtd erros equivalentes
  plot <- ggplot(tabelaErro, aes(x = anos, y = QtdErros, group= 1))
  plot <- plot + geom_point() + geom_line()
  return(plot)
}

print(graficoERRO(df))
#====================================================================================

#Tratamento de dados:
#Removendo linhas que possuam valor nulo
df <- df[df$temp != " [ERRO]", ]


#====================================================================================
#Grafico Sensacao Termica Elevada(após padronizar as datas):
graficoSensaTermElevada <- function(df){
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
  return(plot)
}
print(graficoSensaTermElevada(df))
#====================================================================================
#Gráfico de umidade zero(após padronizar as datas):
graficoUmidZero <- function(df){
  u2014 <- nrow(df[df$horario >= "2014-01-01" & df$horario < "2015-01-01" & df$umid == 0, ])#umidade em 2014
  u2015 <- nrow(df[df$horario >= "2015-01-01" & df$horario < "2016-01-01" & df$umid == 0, ])#umidade em 2015
  u2016 <- nrow(df[df$horario >= "2016-01-01" & df$horario < "2017-01-01" & df$umid == 0, ])#umidade em 2016
  u2017 <- nrow(df[df$horario >= "2017-01-01" & df$horario < "2018-01-01" & df$umid == 0, ])#umidade em 2017
  u2018 <- nrow(df[df$horario >= "2018-01-01" & df$horario < "2019-01-01" & df$umid == 0, ])#umidade em 2018
  u2019 <- nrow(df[df$horario >= "2019-01-01" & df$horario < "2020-01-01" & df$umid == 0, ])#umidade em 2019
  u2020 <- nrow(df[df$horario >= "2020-01-01" & df$umid == 0, ])#umidade em 2020
  
  
  umidadesZero <- c(u2014,u2015,u2016,u2017,u2018,u2019,u2020)
  
  anos <- c("2014","2015","2016","2017","2018","2019","2020") # Coluna com os anos
  
  tabelaUmidade <- data.frame(Ano=anos, QtdUmidadesZero=umidadesZero,stringsAsFactors=TRUE); # Data Frame com os anos e qtd umidades zero
  plot <- ggplot(tabelaErro, aes(x = anos, y = umidadesZero, group= 1))
  plot <- plot + geom_point() + geom_line()
  return(plot)
}
print(graficoUmidZero(df))

#====================================================================================
#Ajustando os tipos das colunas
df$temp <- as.numeric(df$temp)
#Removendo as linhas que nao serao analisadas
df <- df[df$horario >= "2015-01-01" & df$horario < "2020-01-01", ]

#Ajustando os nomes das linhas
rownames(df) <- NULL

#Ajustando valores
for(i in 1:nrow(df))
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


#====================================================================================
#Gráfico de Temperatura média anual:
graficoTempMediaAnual <- function(df){
  tempMediaAnuais <- c(mean(df$temp[df$horario>="2015-01-01" & df$horario<"2016-01-01"]),
                       mean(df$temp[df$horario>="2016-01-01" & df$horario<"2017-01-01"]),
                       mean(df$temp[df$horario>="2017-01-01" & df$horario<"2018-01-01"]),
                       mean(df$temp[df$horario>="2018-01-01" & df$horario<"2019-01-01"]),
                       mean(df$temp[df$horario>="2019-01-01"])) # Coluna com as médias de temperatura equivalentes aos anos
  
  anos <- c("2015","2016","2017","2018","2019") # Coluna com os anos
  
  tabela1 <- data.frame(Ano=anos, Media=tempMediaAnuais,
                        stringsAsFactors=TRUE); # Data Frame com os anos e temperaturas médias equivalentes
  
  plot <- ggplot(tabela1, aes(x = anos, y = tempMediaAnuais, group= 1))
  plot <- plot + geom_point() + geom_line()
  
  return(plot)
}
print(graficoTempMediaAnual(df))
