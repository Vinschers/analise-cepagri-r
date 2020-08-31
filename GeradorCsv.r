#Lendo os dados da internet
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("http://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
df <- read.csv(con, header = FALSE, sep = ";", col.names = names)

summary(df)

#Deletando variaveis que nao serao mais usadas
rm(con)
rm(names)