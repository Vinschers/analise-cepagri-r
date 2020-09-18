#Lendo novamente os dados do csv original para analisar os dados desajustados
library(ggplot2)
names <- c("horario", "temp", "vento", "umid", "sensa")
df <- read.csv("SEU CAMINHO\\analise-cepagri-r\\cepagri.csv", header = FALSE, sep = ";", col.names = names)

#Deletando variaveis que nao serao mais usadas
rm(names)

#Removendo linhas que possuam valor nulo
df <- df[df$temp != " [ERRO]", ]

#Ajustando os tipos das colunas
df$horario <- as.POSIXct(df$horario,format="%d/%m/%Y-%H:%M",tz=Sys.timezone())

#grafico sensacao térmica elevada

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
