
df["estacao"] = 0
df["diaEstacao"] = 0

#Cria as colunas estacao e diaEstacao
for (x in 1:nrow(df)) {
  horario <- as.POSIXlt(df[x, "horario"])
  dia <- horario$yday;
  if (horario$year == 116 && dia > 58) { # ano bissexto
    dia = dia - 1
  } 

  if (dia >= 78 && dia < 170) { # entre 20 de março e 19 de junho
    df[x, "estacao"] = "outono"
    df[x, "diaEstacao"] = dia - 77
  } else if (dia >= 170 && dia < 264) { # entre 20 de junho e 21 de setembro
    df[x, "estacao"] = "inverno"
    df[x, "diaEstacao"] = dia - 169
  } else if (dia >= 264 && dia < 354) { # entre 22 de setembro e 20 de dezembro
    df[x, "estacao"] = "primavera"
    df[x, "diaEstacao"] = dia - 263
  } else { # entre 21 de dezembro e 19 de março
    df[x, "estacao"] = "verao"
    if (dia <= 78) {
      df[x, "diaEstacao"] = dia + 12
    } else {
      df[x, "diaEstacao"] = dia - 353
    }
  }
}

colunaAPlotar <- 'sensa'

dfVerao = df[df['estacao'] == 'verao',]
dfInverno = df[df['estacao'] == 'inverno',]
dfPrimavera = df[df['estacao'] == 'primavera',]
dfOutono = df[df['estacao'] == 'outono',]

dfVerao = aggregate(dfVerao[colunaAPlotar], by = list(DiaEstacao=dfVerao$diaEstacao), FUN=mean)
dfVerao['estacao'] = 'verao'
dfInverno = aggregate(dfInverno[colunaAPlotar], by =  list(DiaEstacao=dfInverno$diaEstacao), FUN=mean)
dfInverno['estacao'] = 'inverno'
dfPrimavera = aggregate(dfPrimavera[colunaAPlotar], by = list(DiaEstacao=dfPrimavera$diaEstacao), FUN=mean)
dfPrimavera['estacao'] = 'primavera'
dfOutono = aggregate(dfOutono[colunaAPlotar], by = list(DiaEstacao=dfOutono$diaEstacao), FUN=mean)
dfOutono['estacao'] = 'outono'

dfEstacoes = rbind(dfVerao, dfInverno, dfPrimavera, dfOutono)

library(ggplot2)

p <- ggplot(dfEstacoes,aes(x = DiaEstacao, y=sensa, colour=estacao))
p <- p + geom_point()
p <- p + geom_line()
p <- p + geom_smooth()
print(p)

tabelaEstacoes = aggregate(df[c("temp", "vento", "umid", "sensa")], by=list(Estacao=df$estacao), FUN=mean)
View(tabelaEstacoes)
