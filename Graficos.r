#Fazendo o plot da umidade
library(ggplot2)
plot <- ggplot(df, aes(x = horario, y = vento, alpha = 0.000001))
plot <- plot + geom_point()

#pdf("C:\\Users\\Pedro\\Desktop\\Projetos\\analise-cepagri-r\\grafico.pdf")
#print(plot)
#dev.off()

#ggsave("grafico.png", width = 4, height = 6)