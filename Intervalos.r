#Analise dos intervalos de tempo
library(ggplot2)

inters = 10
for (i in 2:nrow(df)) {
  inters <- c(inters, df[i, 1] - df[i - 1, 1])
}

dfInters <- data.frame(Intervalos = as.factor(round(inters[inters != 10])), 1)

plot <- ggplot(dfInters, aes(x = Intervalos))
plot <- plot + geom_bar()
print(plot)

#print(plot)
print("ALO")

#Deletando variaveis que nao serao mais usadas
rm(i)