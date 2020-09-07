#Lendo os dados do csv ja baixado
names <- c("horario", "temp", "vento", "umid", "sensa")
df <- read.csv("C:\\Users\\Pedro\\Desktop\\Projetos\\analise-cepagri-r\\cepagri.csv", header = FALSE, sep = ";", col.names = names)

summary(df)

#Deletando variaveis que nao serao mais usadas
rm(names)

#Removendo linhas que possuam valor nulo
df <- df[df$temp != " [ERRO]", ]

#Ajustando os tipos das colunas
df$horario <- as.POSIXct(df$horario,format="%d/%m/%Y-%H:%M",tz=Sys.timezone())
df$temp <- as.numeric(df$temp)

#Removendo as linhas que nao serao analisadas
df <- df[df$horario >= "2015-01-01" & df$horario < "2020-01-01", ]

#Ajustando os nomes das linhas
rownames(df) <- NULL