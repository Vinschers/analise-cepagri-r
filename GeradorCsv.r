#Lendo os dados do csv ja baixado
names <- c("horario", "temp", "vento", "umid", "sensa")
df <- read.csv("SEU CAMINHO\\analise-cepagri-r\\cepagri.csv", header = FALSE, sep = ";", col.names = names)

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

