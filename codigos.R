banco <- read.csv("banco_final.csv")
library(tidyverse)
library(lubridate)
library(car)
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
ggsave("graficoanalise1.pdf",plot = grafico_analise1 ,width = 158, height = 93, units = "mm")


### met 2 analise 1
shapiro.test(decadaformato$lancamentos)

kruskal.test(decadaformato$lancamentos, decadaformato$format)

# analise 2 (Variação da nota IMDB por temporada dos episódios)

banco$season <- as.character(banco$season)

banco2 <- banco %>% 
  filter(season != "Crossover" , season !="Movie" , season != "Special")

### ordenado pela temporada

grafico_analise2 <- ggplot(banco2, aes(x=season, y=imdb)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Temporada", y="Nota IMDB")+
  theme_estat() +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 2))

grafico_analise2
ggsave("graficoanalise2.png",plot = grafico_analise2 ,width = 158, height = 93, units = "mm")

banco2_est <- filter(banco2, season == "1")
sd(banco2_est$imdb, na.rm = T)
summary(banco2_est$imdb)

### met 2 analise 2

shapiro.test(banco2$imdb)

banco2$season <- as.factor(banco2$season)
leveneTest(imdb ~ season, data = banco2)


anova <- aov(imdb ~ season, data = banco2)

summary(anova)


# analise 3

banco$setting_terrain <- as.factor(banco$setting_terrain)
summary(banco$setting_terrain)

banco3 <- banco %>%
  filter(setting_terrain == "Urban" |setting_terrain == "Rural" | setting_terrain == "Forest") %>% 
  filter(trap_work_first == "True" | trap_work_first == "False") %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

porcentagens <- str_c(banco3$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(banco3$freq, " (", porcentagens, ")"))

banco3$setting_terrain <- as.character(banco3$setting_terrain)

banco3$setting_terrain[banco3$setting_terrain == "Urban"] <- "Urbano"
banco3$setting_terrain[banco3$setting_terrain == "Forest"] <- "Floresta"
banco3$trap_work_first[banco3$trap_work_first == "True"] <- "Sim"
banco3$trap_work_first[banco3$trap_work_first == "False"] <- "Não"
banco3$setting_terrain <- as.factor(banco3$setting_terrain)

armadilha_ordenado <- factor(banco3$trap_work_first, levels = c("Sim", "Não"))

grafico_analise3 <- ggplot(banco3) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = armadilha_ordenado, label = legendas
  ) +
  scale_fill_manual(name="Funcionou de primeira", values=c("#A11D21", "#003366"))+
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terreno", y = "Frequência") +
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),  
    axis.text.y = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20))

grafico_analise3

ggsave("graficoanalise3.png",plot = grafico_analise3 ,width = 158, height = 93, units = "mm")

#tabela analise 3
summary(banco3$setting_terrain)

banco3$setting_terrain <- as.character(banco3$setting_terrain)

banco3_est <- banco3 %>% 
  filter(setting_terrain == "Floresta" & trap_work_first == "Sim" )

# correlação

banco3$trap_work_first <- as.factor(banco3$trap_work_first)

resultado_teste <- chisq.test(table(banco3$setting_terrain, banco3$trap_work_first))
resultado_teste


# analise 4

grafico_analise4 <- ggplot(banco, aes(x=engagement, y=imdb)) + geom_point(colour="#A11D21", size=3, alpha = 0.4) +
  labs(x="Engajamento", y="Nota IMDB") +
  theme_estat() +
  scale_y_continuous(limits = c(0, 8.3), breaks = seq(2, 8, by = 2))

grafico_analise4
ggsave("graficoanalise4.png",plot = grafico_analise4 ,width = 158, height = 93, units = "mm")


cor.test(banco$imdb, banco$engagement, method = "pearson")

sd(banco$engagement, na.rm = T)
summary(banco$engagement)


#analise 5 

banco5 <- banco %>%
  mutate(capturou = case_when(
    caught_fred == "True" ~ "Fred",
    caught_velma == "True" ~ "Velma",
    caught_shaggy == "True" ~ "Shaggy",
    caught_daphnie == "True" ~ "Daphnie",
    caught_scooby == "True" ~ "Scooby",
    caught_other == "True" ~ "Outro",
    TRUE ~ "Ninguém"
  ))

banco5$capturou <- as.factor(banco5$capturou)

grafico_analise5 <- ggplot(banco5) +
  aes(x = reorder(capturou, engagement,  FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Quem Capturou", y = "Engajamento") +
  theme_estat()
grafico_analise5

ggsave("graficoanalise5.png",plot = grafico_analise5 ,width = 158, height = 93, units = "mm")

banco5_est <- banco5 %>% filter(capturou == "Daphnie")
sd(banco5_est$engagement, na.rm = T)
summary(banco5_est$engagement)
  

#### met 2 analise 5


shapiro.test(banco5$engagement)

t.test(imdb ~ season, data = banco2)

leveneTest(engagement ~ capturou, data = banco5)


anova5 <- aov(engagement ~ capturou, data = banco5)

summary(anova5)
