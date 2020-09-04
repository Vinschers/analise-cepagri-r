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