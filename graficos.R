library(ggplot2)
head(mpg)


cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}


#Gr?fico de Colunas
#Univariado
ggplot(mpg, aes(x = class)) + geom_bar(fill="#A11D21") +
  labs(x="Classe do autom?vel", y="Frequ?ncia") +
  theme_estat() 
#ggsave("colunas_uni.png", width = 158, height = 93, units = "mm")

#Barras
ggplot(mpg, aes(x = class)) + geom_bar(fill="#A11D21") +
  labs(x="Classe do autom?vel", y="Frequ?ncia") +
  theme_estat() 
#ggsave("barras_uni.png", width = 158, height = 93, units = "mm")

#Com porcentagem

Fr<-table(mpg$class)
#Determinando as porcentagens de cada classe
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=Var1, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#A11D21") +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Classe do autom?vel", y="Frequ?ncia") +
  theme_estat()  
#ggsave("porc_uni.png", width = 158, height = 93, units = "mm")
#ou
library(scales)
ggplot(mpg, aes(x = class)) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(gsub("\\.",",",round(prop.table(..count..) * 100,2)), '%')), 
            stat = 'count', vjust=0, size = 4) +
  labs(x="Classe do autom?vel", y="Frequ?ncia") +
  theme_estat() 
#ggsave("porc_uni2.png", width = 158, height = 93, units = "mm")

#Bivariado
mpg$trans <- as.factor(mpg$trans)
levels(mpg$trans)<-c(rep("Automatico", 8), rep("Manual", 2))

ggplot(mpg, aes(x=class, fill=trans)) + geom_bar(position="dodge") +
  scale_fill_manual(name="Transmiss?o", values=c("#A11D21", "#003366"))+
  labs(x="Classe do autom?vel", y="Frequ?ncia") +
  theme_estat() 
#ggsave("colunas_biva.png", width = 158, height = 93, units = "mm")

#2
dados <- as.data.frame(table(mpg$class,mpg$trans))
ggplot(dados, aes(x=Var1, y=Freq,fill=Var2)) + geom_bar(stat = "identity",position="dodge") +
  scale_fill_manual(name="Transmiss?o", values=c("#A11D21", "#003366"))+
  labs(x="Classe do autom?vel", y="Frequ?ncia") +
  theme_estat() 
#ggsave("colunas_biva2.png", width = 158, height = 93, units = "mm")

#Setor
ggplot(mpg, aes(x=factor(""), fill=factor(trans))) + geom_bar(width=1)+
  coord_polar(theta="y") +
  scale_x_discrete() +
  scale_fill_manual(name="Transmiss?o", values=c("#A11D21", "#003366")) +
  theme_estat()  
#ggsave("setor.png", width = 158, height = 93, units = "mm")

#Boxplot
#Univariado
ggplot(mpg, aes(x=factor(""), y=cty)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Consumo em Cidade (milhas/gal?o)")+
  theme_estat()  
#ggsave("box_uni.png", width = 158, height = 93, units = "mm")

#Duas vari?veis 
ggplot(mpg, aes(x=trans, y=cty)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Transmiss?o", y="Consumo em Cidade (milhas/gal?o)") +
  theme_estat()  
#ggsave("box_categ.png", width = 158, height = 93, units = "mm")

#Histograma
#Univariado
ggplot(mpg, aes(x=cty)) + geom_histogram(colour="white", fill="#A11D21",binwidth=7)+
  labs(x="Consumo em Cidade (milhas/gal?o)", y="Frequ?ncia") +
  theme_estat()  
#ggsave("hist_uni.png", width = 158, height = 93, units = "mm")

#Univariado em porcentagem
library(scales)
ggplot(mpg, aes(x=cty)) + geom_histogram(aes(y = 100 * (..count..)/sum(..count..)),colour="white", fill="#A11D21",binwidth=7)+
  labs(x="Consumo em Cidade (milhas/gal?o)", y="Porcentagem") +
  theme_estat() 
#ggsave("hist_uni_porc.png", width = 158, height = 93, units = "mm")

#Bivariado
ggplot(mpg, aes(x=cty)) + geom_histogram(colour="white", fill="#A11D21", binwidth=7)+
  facet_grid(trans ~ .) +
  labs(x="Consumo em Cidade (milhas/gal?o)", y="Frequ?ncia") +
  theme_estat()
#ggsave("hist_pan.png", width = 158, height = 93, units = "mm")

#Dispers?o
#Univariado
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point(colour="#A11D21", size=3) +
  labs(x="Consumo em Cidade (milhas/gal?o)", y="Consumo em Rodovias (milhas/gal?o)") +
  theme_estat()
#ggsave("disp_uni.png", width = 158, height = 93, units = "mm")

#Bivariado
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point(aes(colour=trans)) +
  scale_colour_manual(name="Transmiss?o", values = c("#A11D21", "#003366"))+
  labs(x="Consumo em Cidade (milhas/gal?o)", y="Consumo em Rodovias (milhas/gal?o)")+
  theme_estat()
#ggsave("disp_bi.png", width = 158, height = 93, units = "mm")

#Gr?fico de Linhas
ano<-as.character(c("2006","2007","2008","2009","2010","2011","2012","2013",
                    "2014","2015","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"))
preco<-as.numeric(c(2.5,5.7,3.4,3.7,4.5,4.8,4.1,4.6,4.8,5, 4.5,6.5,3.5,4.6,4.7,
                    4.9,5,5.5,3.5,7.5))
grupo<-as.character(c("a","a","a","a","a","a","a","a","b","b","b","b","b","b",
                      "b","b","b","b","b","b"))
dados<-as.data.frame(cbind(ano,preco,grupo))

#Univariado
ggplot(dados, aes(x=ano, y=preco, group=1)) +
  geom_line(size=1, colour="#A11D21") + geom_point( colour = "#A11D21",size=2) +
  labs(x="Ano", y="Pre?o") +
  theme_estat()
#ggsave("series_uni.png", width = 158, height = 93, units = "mm")


#Bivariado
ggplot(dados, aes(x=ano, y=preco, group = grupo, colour=grupo )) +
  geom_line(size=1) + geom_point(size=2) +
  scale_colour_manual(name="Transmiss?o", values = c("#A11D21", "#003366"))+
  labs(x="Ano", y="Pre?o") +
  theme_estat() +
  theme(legend.position="top")
#ggsave("series_grupo.png", width = 158, height = 93, units = "mm")

#QQPlot

y <- quantile(mpg$cty, c(0.25, 0.75))
x <- qnorm(c(0.25,0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
d <- data.frame(resids = mpg$cty)

ggplot(d, aes(sample = resids)) + stat_qq(colour = "#A11D21") + 
  geom_abline(slope = slope, intercept = int, size = .8)+
  xlab("Quantis da Normal")+ylab("Consumo em Cidade (milhas/gal?o)") + 
  theme_estat()
#ggsave("qq_plot.png", width = 158, height = 93, units = "mm")


