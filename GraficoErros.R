#Lendo os dados do csv ja baixado
library(ggplot2)
names <- c("horario", "temp", "vento", "umid", "sensa")
df <- read.csv("D:\\gusta\\Documents\\Github\\analise-cepagri-r\\cepagri.csv", header = FALSE, sep = ";", col.names = names)

#Deletando variaveis que nao serao mais usadas
rm(names)

#Removendo linhas que possuam valor nulo
#df <- df[df$temp != " [ERRO]", ]

#Ajustando os tipos das colunas
df$horario <- as.POSIXct(df$horario,format="%d/%m/%Y-%H:%M",tz=Sys.timezone())

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
print(plot)



