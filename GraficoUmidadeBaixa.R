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

#grafico umidade baixa

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
print(plot)

