# Estudio del campo texto:
df <- data.frame(topTerminos[1:10])
df$terminos <- rownames(df)
colnames(df)<-c("Frecuencia", "FrecuenciaRelativa", "Terminos")
rownames(df)<-seq(1:10)

# Diagrama de barras horizontales
plotTexto<-ggplot(data=b, aes(x=reorder(Terminos, FrecuenciaRelativa), y=FrecuenciaRelativa, fill=Terminos%in%palabrasClave, color=terminos)) + geom_bar(stat="identity") + scale_fill_brewer(palette="Blues") + scale_color_manual(values=rep("#00007F",10)) + xlab("Términos") + ylab("Frecuencia relativa en %") + guides(fill=guide_legend(title="Palabra clave"))+ coord_flip()+ ggtitle("Palabras comunes en el texto de los tweets")+geom_text(aes(label=paste(FrecuenciaRelativa, "%")), hjust=1.1, vjust=0.4, color="darkblue", size=3.5)+theme_minimal()+ theme(plot.title=element_text(color="darkblue", face="bold.italic", size=16), axis.title.x = element_text(color="darkblue", face="italic", size=14), axis.title.y = element_text(color="darkblue", face="italic", size=14))

# Estudio de la fecha de publicación del Tweet
dias <- dfOfertasRelevantes$diaCreacion
mes <- dfOfertasRelevantes$mesCreacion
hora <- dfOfertasRelevantes$horaCreacion
hora[which(hora==0)] <- 24
diaSemana <- dfOfertasRelevantes$diaSemana

# dias
dfDias <- as.data.frame(table(cut(dias, breaks=c(0,10,20,31), labels=c("Primeros de mes", "Mediados de mes", "Finales de mes"))))
colnames(dfDias) <- c("Dias", "Frecuencia", "FrecuenciaRelativa")

# mes
dfMes <- as.data.frame(table(mes))
colnames(dfMes) <- c("Meses", "Frecuencia", "FrecuenciaRelativa")
dfMes$Meses <- factor(dfMes$Meses, levels=c("7", "8", "9"), labels=c("Julio", "Agosto", "Septiembre"))

# horas
dfHoras <- as.data.frame(table(cut(hora, seq(0,24,4))))
colnames(dfHoras) <- c("Horas", "Frecuencia", "FrecuenciaRelativa")

# diaSemana
dfDiaSemana <- as.data.frame(table(diaSemana))
colnames(dfDiaSemana) <- c("DiaSemana", "Frecuencia", "FrecuenciaRelativa")
dfDiaSemana$DiaSemana <- factor(dfDiaSemana$DiaSemana, levels=c("1", "2", "3", "4", "5", "6", "7"), labels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))

# Representación gráfica
library(ggplot2)
library(cowplot)

blank_theme <- theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=16, face="bold.italic")
    ) 

pie <- ggplot(dfDias, aes(x="", y=FrecuenciaRelativa, fill=Dias))+geom_bar(width = 1, stat = "identity", color="darkgreen") + coord_polar("y", start=0)
pDias <- pie +scale_fill_manual(values=c("#66b266","#99cc99","#cce5cc"))+blank_theme+theme(axis.text.x=element_blank())+guides(fill=guide_legend(title="Época del mes"))+ggtitle("Días del mes en los que \nmás se publica")+theme(plot.title=element_text(color="darkgreen"))+geom_text(aes(y = FrecuenciaRelativa/3 + c(0, cumsum(FrecuenciaRelativa)[-length(FrecuenciaRelativa)]), label = paste(FrecuenciaRelativa, "%")), size=6, color="darkgreen")

pie <- ggplot(dfMes, aes(x="", y=FrecuenciaRelativa, fill=Meses))+geom_bar(width = 1, stat = "identity", color="#b26200") + coord_polar("y", start=0)
pMes <- pie +scale_fill_manual(values=c("#ffa332","#ffba66","#ffdcb2"))+blank_theme+theme(axis.text.x=element_blank())+guides(fill=guide_legend(title="Mes"))+ggtitle("Meses en los que \nmás se publica")+theme(plot.title=element_text(color="#b26200"))+geom_text(aes(y = FrecuenciaRelativa/3 + c(0, cumsum(FrecuenciaRelativa)[-length(FrecuenciaRelativa)]), label = paste(FrecuenciaRelativa, "%")), size=6, color="#b26200")

pDiaSemana<-ggplot(data=dfDiaSemana, aes(x=DiaSemana, y=FrecuenciaRelativa, fill=DiaSemana)) + geom_bar(stat="identity", fill="gold", color="#e5c100") + ggtitle("Días de la semana en los que más se publica") + xlab("Día de la semana") + ylab("Frecuencia relativa en %")+theme_minimal()+ theme(plot.title=element_text(color="#ccac00", face="bold.italic", size=16), axis.title.x = element_text(color="#ccac00", face="italic", size=14), axis.title.y = element_text(color="#ccac00", face="italic", size=14))

pHoras<-ggplot(data=dfHoras, aes(x=Horas, y=Frecuencia, group=1)) + geom_bar(stat="identity", fill="lightblue", color="#00007F") + geom_line(linetype = "dotted", color="darkblue", size=2) + geom_point(color="darkblue", size=3) + ggtitle("Horas") + scale_x_discrete(breaks=c("(0,4]", "(4,8]", "(8,12]", "(12,16]", "(16,20]","(20,24]"),labels=c("De 0 a 4", "De 4 a 8", "De 8 a 12", "De 12 a 16", "De 16 a 20", "De 20 a 24"))

# Diagrama de barras apiladas
source("D:\\Estudios\\DatosTFM\\provinciasEspanya.r", encoding = "UTF-8")
source("D:\\Estudios\\DatosTFM\\paisesAmerica.r", encoding = "UTF-8")
localizacion <- tweetsOfertasRelevantes$infoUsuario.localizacionUsuario
localizaciones <- c();
for(loc in localizacion){
	if (localizacion ==""){
		localizaciones <- append(localizaciones,"SIN"); 
	} else {
		localizacionSplit <- unlist(strsplit(loc, " "));
		cont <- 1;
		encontrado <- FALSE;
		while(!encontrado && cont <= length(localizacionSplit)){
			lugar <- localizacionSplit[cont];
			if (lugar %in% provinciasEspanya){localizaciones <- append(localizaciones,"ES"); encontrado = TRUE;} 
			else if (lugar %in% paisesAmerica){localizaciones <- append(localizaciones,"AM"); encontrado = TRUE;} 
			else {cont <- cont +1;}
		}
		if (!encontrado) {localizaciones <- append(localizaciones,"OTROS");}
	}
}
tweetsOfertasRelevantes$zona <- localizaciones
horasEspanya <- tweetsOfertasRelevantes[which((tweetsOfertasRelevantes$zona=="ES")==TRUE),"horaCreacion"]
horasEspanya <- tweetsOfertasRelevantes[which((tweetsOfertasRelevantes$zona=="ES")==TRUE),"horaCreacion"]
horasAmerica <- tweetsOfertasRelevantes[which((tweetsOfertasRelevantes$zona=="AM")==TRUE),"horaCreacion"]
horasOtros <- tweetsOfertasRelevantes[which((tweetsOfertasRelevantes$zona=="OTROS")==TRUE),"horaCreacion"]
horasSin <- tweetsOfertasRelevantes[which((tweetsOfertasRelevantes$zona=="SIN")==TRUE),"horaCreacion"]
dfHorasEspanya <- as.data.frame(table(cut(horasEspanya, seq(0,24,4))))
dfHorasAmerica <- as.data.frame(table(cut(horasAmerica, seq(0,24,4))))
dfHorasOtros <- as.data.frame(table(cut(horasOtros, seq(0,24,4))))
dfHorasSin <- as.data.frame(table(cut(horasSin, seq(0,24,4))))
dfHoras <- rbind(dfHorasEspanya, dfHorasAmerica)
dfHoras <- rbind(dfHoras, dfHorasOtros)
dfHoras <- rbind(dfHoras, dfHorasSin)
dfHoras$FrecuenciaRelativa <- (dfHoras$Frecuencia/length(tweetsOfertasRelevantes$id))*100

pHoras<-ggplot(data=dfHoras, aes(x=Horas, y=FrecuenciaRelativa, fill=region)) + geom_bar(stat="identity", color="#00007F") + ggtitle("Horas en las que más se publica") + scale_fill_brewer(palette="Blues") + scale_x_discrete(breaks=c("(0,4]", "(4,8]", "(8,12]", "(12,16]", "(16,20]","(20,24]"),labels=c("De 0 a 4", "De 4 a 8", "De 8 a 12", "De 12 a 16", "De 16 a 20", "De 20 a 24")) + ylab("Frecuencia relativa en %")+ guides(fill=guide_legend(title="Región"))+theme_minimal()+ theme(plot.title=element_text(color="darkblue", face="bold.italic", size=16), axis.title.x = element_text(color="darkblue", face="italic", size=14), axis.title.y = element_text(color="darkblue", face="italic", size=14))

# Diagrama de líneas
horasMes <- c("horaCreacion", "mesCreacion")
dfHorasMes <- tweetsOfertasRelevantes[,horasMes]
dfHorasMes[which(dfHorasMes$horaCreacion==0),1] <- 24
dfHorasMes <- as.data.frame(table(cut(hora, seq(0,24,4)), dfHorasMes$mesCreacion))
colnames(dfHorasMes) <- c("Horas", "Meses", "Frecuencia")
dfHorasMes$Meses <- factor(dfHorasMes$Meses, levels=c("7", "8", "9"), labels=c("Julio", "Agosto", "Septiembre"))

pHorasMes <- ggplot(dfHorasMes, aes(x=Horas, y=Frecuencia, colour=Meses, group=Meses)) + geom_line(size=1) + ggtitle("Intervalos de tiempo con mayor frecuencia \nde publicación de Tweets en distintos meses")+theme(plot.title=element_text(color="black", face="bold.italic", size=16), axis.title.x = element_text(color="black", face="italic", size=14), axis.title.y = element_text(color="black", face="italic", size=14))

# Agrupación de gráficos
plot1 <- plot_grid(pDias, pMes, ncol = 2, nrow = 1)
plot2 <- plot_grid(plot1, pHoras, ncol = 2, nrow = 1)
plot3 <- plot_grid(pDiaSemana, pHorasMes, ncol = 2, nrow = 1)
plot4 <- plot_grid(plot2, plot3, ncol = 1, nrow = 2)

#Estudio de los Hashtags
numHayHashtag <- length(which(!is.na(dfOfertasRelevantes$hashtags)))
numNoHayHashtag <- length(which(is.na(dfOfertasRelevantes$hashtags)))
dfAux <- as.data.frame(list(HayHashtag=c("Si","No"),Frecuencia=c(numHayHashtag, numNoHayHashtag)))

info <- paste("El", substr(length(which(!is.na(dfOfertasRelevantes$hashtags)))*100/length(dfOfertasRelevantes$hashtags),1,6), "% de los tweets \nllevan hashtags.", sep=" ")
bp <- ggplot(dfAux, aes(x="", y=Frecuencia, fill=HayHashtag))+geom_bar(width = 1, stat = "identity", color="#b20000")
pie <- bp + coord_polar("y", start=0)
pieHashtag <- pie + scale_fill_manual(values=c("#ffe5e5", "#ff9999"))+blank_theme+theme(axis.text.x=element_blank())+geom_text(aes(y = Frecuencia/2 + c(0, cumsum(Frecuencia)[-length(Frecuencia)]), label = ifelse(Frecuencia==0,"",percent(Frecuencia/sum(dfAux$Frecuencia)))), size=6, color="#b20000")+guides(fill=guide_legend(title="Hay hashtags"))+ggtitle("Tweets con Hashtags")+theme(plot.title=element_text(color="#b20000"))+geom_text(aes(y=0,label=info), color="#b20000")

hashtagSplit <- c()
for (i in dfOfertasRelevantes$hashtags){if(!is.na(i)){hashtagSplit <- append(hashtagSplit,unlist(strsplit(i, ",")))}};
corpusHashtags <- Corpus(VectorSource(hashtagSplit))
tdmHashTags <- TermDocumentMatrix(corpusHashtags)
terminosHashtags <- tdmHashTags$dimnames$Terms
hashtagOrder <- sort(row_sums(tdmHashTags, na.rm = T), decreasing = TRUE)
numHashtags <- length(hashtagOrder)
topHashtags <- hashtagOrder[1:numHashtags]
df <- data.frame(topHashtags[1: ifelse(numHashtags>5,5, numHashtags)])
df$terminos <- rownames(df)
colnames(df)<-c("Frecuencia", "Terminos")
rownames(df)<-seq(1: length(df$Frecuencia))
df$FrecuenciaRelativa <- (df$Frecuencia/dim(dfOfertasRelevantes)[1])*100

pHashtags<-ggplot(data=df, aes(x=reorder(Terminos, FrecuenciaRelativa), y=FrecuenciaRelativa, color=Terminos)) + geom_bar(stat="identity",  fill="#ff9999",color="#b20000") + xlab("Hashtags") + ylab("Frecuencia relativa en %") + coord_flip()+ggtitle("Hashtags frecuentes")+geom_text(aes(label=paste(substr(FrecuenciaRelativa,1,5), "%")), hjust=1.1, vjust=0.4, color="#b20000", size=3.5)+theme_minimal()+ theme(plot.title=element_text(color="#b20000", face="bold.italic", size=16), axis.title.x = element_text(color="#b20000", face="italic", size=14), axis.title.y = element_text(color="#b20000", face="italic", size=14))

plot_grid(pieHashtag, pHashtags, ncol = 2, nrow = 1)

# Urls y media
numHayMedia <- length(which(!is.na(dfOfertasRelevantes$hayMedia)))
numNoHayMedia <- length(which(is.na(dfOfertasRelevantes$hayMedia)))
dfAux <- as.data.frame(list(HayMedia=c("Si","No"),Frecuencia=c(numHayMedia, numNoHayMedia)))

infoMedia <- paste("El", substr((dfAux[dfAux$HayMedia=="Si",2])*100/length(dfOfertasRelevantes $hayMedia),1,6), "% de los tweets \nllevan media", sep=" ")

bpMedia <- ggplot(dfAux, aes(x="", y=Frecuencia, fill=HayMedia))+geom_bar(width = 1, stat = "identity", color="#5e7734")
pieMedia <- bpMedia + coord_polar("y", start=0)
pieMedia <- pieMedia + scale_fill_manual(values=c("#ddf6b3", "#a9d65d"))+blank_theme+theme(axis.text.x=element_blank())+geom_text(aes(y = Frecuencia/2 + c(0, cumsum(Frecuencia)[-length(Frecuencia)]), label = ifelse(Frecuencia==0,"",percent(Frecuencia/sum(dfAux$Frecuencia)))), size=6, color="#5e7734")+guides(fill=guide_legend(title="Hay media"))+ggtitle("Tweets con Media")+theme(plot.title=element_text(color="#5e7734"))+geom_text(aes(y=0,label=infoMedia), color="#5e7734")

numHayUrl <- length(which(dfOfertasRelevantes $hayUrls=="Si"))
numNoHayUrl <- length(which(dfOfertasRelevantes $hayUrls=="No"))
dfAux <- as.data.frame(list(HayUrl=c("Si","No"),Frecuencia=c(numHayUrl, numNoHayUrl)))

infoUrl <- paste("El", substr((dfAux[dfAux$HayUrl=="Si",2])*100/length(dfOfertasRelevantes $hayUrls),1,6), "% de los tweets \nllevan urls.", sep=" ")

bpUrl <- ggplot(dfAux, aes(x="", y=Frecuencia, fill=HayUrl))+geom_bar(width = 1, stat = "identity", color="#5e7734")
pieUrl <- bpUrl + coord_polar("y", start=0)
pieUrl <- pieUrl + scale_fill_manual(values=c("#ddf6b3", "#a9d65d"))+blank_theme+theme(axis.text.x=element_blank())+geom_text(aes(y = Frecuencia/2 + c(0, cumsum(Frecuencia)[-length(Frecuencia)]), label =  ifelse(Frecuencia==0,"",percent(Frecuencia/sum(dfAux$Frecuencia)))), size=6, color="#5e7734")+guides(fill=guide_legend(title="Hay urls"))+ggtitle("Tweets con Urls")+theme(plot.title=element_text(color="#5e7734"))+geom_text(aes(y=0,label=infoUrl), color="#5e7734",vjust = 4, hjust = "center")

plot_grid(pieMedia, pieUrl, ncol = 2, nrow = 1)

# Estudio relativo a la información de la cuenta
numHay <- length(which(dfOfertasRelevantes $infoUsuario.usuarioVerificado==TRUE))
numNoHay <- length(which(dfOfertasRelevantes $infoUsuario.usuarioVerificado==FALSE))
dfAux1 <- as.data.frame(list(Hay=c("Si","No"),Frecuencia=c(numHay, numNoHay)))
dfAux1
bp1 <- ggplot(dfAux1, aes(x="", y=Frecuencia, fill=Hay))+geom_bar(width = 1, stat = "identity", color="#125699")
pie1 <- bp1 + coord_polar("y", start=0)
pie1 <- pie1 + scale_fill_manual(values=c("#bbddff", "#61b1ff"))+blank_theme+theme(axis.text.x=element_blank())+geom_text(aes(y = Frecuencia/2 + c(0, cumsum(Frecuencia)[-length(Frecuencia)]), label = ifelse(Frecuencia==0,"",percent(Frecuencia/sum(dfAux1$Frecuencia)))), size=6, color="#125699")			+guides(fill=guide_legend(title="Cuentas verificadas"))+ggtitle("Verificación")+theme(plot.title=element_text(color="#125699"))

numHay <- length(which(dfOfertasRelevantes $infoUsuario.usuarioConURL=="Si"))
numNoHay <- length(which(dfOfertasRelevantes $infoUsuario.usuarioConURL=="No"))
dfAux2 <- as.data.frame(list(Hay=c("Si","No"),Frecuencia=c(numHay, numNoHay)))
dfAux2
bp2 <- ggplot(dfAux2, aes(x="", y=Frecuencia, fill=Hay))+geom_bar(width = 1, stat = "identity", color="#125699")
pie2 <- bp2 + coord_polar("y", start=0)
pie2 <- pie2 + scale_fill_manual(values=c("#bbddff", "#61b1ff"))+blank_theme+theme(axis.text.x=element_blank())+geom_text(aes(y = Frecuencia/2 + c(0, cumsum(Frecuencia)[-length(Frecuencia)]), label = ifelse(Frecuencia==0,"",percent(Frecuencia/sum(dfAux1$Frecuencia)))), size=6, color="#125699")+guides(fill=guide_legend(title="Cuentas con url"))+ggtitle("Cuentas con URL")+theme(plot.title=element_text(color="#125699"))

numHay <- length(which(dfOfertasRelevantes $infoUsuario.imagenPerfilUsuarioDefecto==TRUE))
numNoHay <- length(which(dfOfertasRelevantes $infoUsuario.imagenPerfilUsuarioDefecto==FALSE))
dfAux3 <- as.data.frame(list(Hay=c("Imagen por defecto", "Imagen de usuario"),Frecuencia=c(numHay, numNoHay)))
dfAux3
bp3 <- ggplot(dfAux3, aes(x="", y=Frecuencia, fill=Hay))+geom_bar(width = 1, stat = "identity", color="#125699")
pie3 <- bp3 + coord_polar("y", start=0)
pie3 <- pie3 + scale_fill_manual(values=c("#bbddff", "#61b1ff"))+blank_theme+theme(axis.text.x=element_blank())+geom_text(aes(y = Frecuencia/2 + c(0, cumsum(Frecuencia)[-length(Frecuencia)]), label = ifelse(Frecuencia==0,"",percent(Frecuencia/sum(dfAux1$Frecuencia)))), size=6, color="#125699")+guides(fill=guide_legend(title="Imagen", reverse=TRUE))+ggtitle("Imagen del perfil")+theme(plot.title=element_text(color="#125699"))

numHay <- length(which(dfOfertasRelevantes $infoUsuario.perfilUsuarioDefecto==TRUE))
numNoHay <- length(which(dfOfertasRelevantes $infoUsuario.perfilUsuarioDefecto==FALSE))
dfAux4 <- as.data.frame(list(Hay=c("Perfil por defecto", "Perfil de usuario"),Frecuencia=c(numHay, numNoHay)))
dfAux4

bp4 <- ggplot(dfAux4, aes(x="", y=Frecuencia, fill=Hay))+geom_bar(width = 1, stat = "identity", color="#125699")
pie4 <- bp4 + coord_polar("y", start=0)
pie4 <- pie4 + scale_fill_manual(values=c("#bbddff", "#61b1ff"))+blank_theme+theme(axis.text.x=element_blank())+geom_text(aes(y = Frecuencia/2 + c(0, cumsum(Frecuencia)[-length(Frecuencia)]), label = ifelse(Frecuencia==0,"",percent(Frecuencia/sum(dfAux1$Frecuencia)))), size=6,
color="#125699")+guides(fill=guide_legend(title="Perfil", reverse=TRUE))+ggtitle("Perfil de la cuenta")+theme(plot.title=element_text(color="#125699"))
gridPies <- plot_grid(pie1, pie2, pie3, pie4, ncol = 2, nrow = 2)



