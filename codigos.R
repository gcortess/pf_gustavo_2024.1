banco <- read.csv("banco_final.csv")
library(tidyverse)
library(lubridate)

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



# descobrindo quais os valores das variaveis

### formato(crossover, filme e serie)
banco$format <- as.factor(banco$format)
levels(banco$format)
banco$format <- as.character(banco$format)

# trocando os valores para o português

### Product Name
banco$format[banco$format == "Movie"] <- "Filme"
banco$format <- as.character(banco$format)
banco$format <- as.factor(banco$format)

# analise 1 (Número de lançamentos a cada década por formato de lançamento)

banco$date_aired <- ymd(banco$date_aired)
banco$ano <- year(banco$date_aired)
banco$ano <- as.numeric(banco$ano)

decadaformato <- banco %>% 
  filter(ano != "") %>% 
  mutate(decada = floor(ano / 10) * 10) %>%  
  filter(format != "") %>%  
  group_by(format, decada) %>%  
  summarise(lancamentos = n())

grafico_analise1 <- ggplot(decadaformato, aes(x = decada, y = lancamentos, group = format, colour = format)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(x = "Decadas", y = "Número de lançamentos") +
  scale_colour_manual(name = "Formato de lançamento", values = c("#A11D21", "#003366", "#CC9900"))+
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.text.y = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  theme(legend.position = "top")+
  scale_x_continuous(breaks = seq(min(decadaformato$decada), max(decadaformato$decada), by = 10))
grafico_analise1

### met 2 analise 1
shapiro.test(decadaformato$lancamentos)

kruskal.test(decadaformato$lancamentos, decadaformato$format)

# analise 2 (Variação da nota IMDB por temporada dos episódios)

banco$season <- as.factor(banco$season)

banco2 <- banco %>% 
  filter(season != "Crossover" , season !="Movie" , season != "Special")

ggplot(banco2, aes(desc(x=season), y=imdb)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Temporada", y="Nota IMDB") +
  scale_y_continuous(breaks = seq(min(banco2$imdb), max(banco2$imdb), by = 1)) +
  theme_estat()

banco2_est <- filter(banco2, season == "1")
sd(banco2_est$imdb, na.rm = T)
summary(banco2_est$imdb)

### met 2 analise 2

shapiro.test(banco2$imdb)

anova <- aov(imdb ~ season, data = banco2)

summary(anova)
