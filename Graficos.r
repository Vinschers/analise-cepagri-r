#Fazendo o plot
library(ggplot2)

dfMenor <- df
dfMenor$dataT <- format(strptime(df$horario,"%Y-%m-%d %H:%M:%S"), '%Y-%m')


dfMenor <- dfMenor[dfMenor['dataT'] > '2018-08' & dfMenor['dataT'] < '2019-02',]
plot <- ggplot(dfMenor, aes(x = horario, y = temp, alpha = 1))
plot <- plot + geom_point()
plot <- plot + xlab("Mês (2018-2019)")
plot <- plot + ylab("Temperatura(°C)")

print(plot)