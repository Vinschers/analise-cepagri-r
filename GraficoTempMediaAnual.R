library(ggplot2)



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

print(plot)
