#Fazendo o plot
library(ggplot2)
plot <- ggplot(df, aes(x = horario, y = vento, alpha = 1))
plot <- plot + geom_point()

#pdf("C:\\Users\\Pedro\\Desktop\\Projetos\\analise-cepagri-r\\grafico.pdf")
#print(plot)
#dev.off()

#ggsave("grafico.png", width = 4, height = 6)