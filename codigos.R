banco <- read.csv("banco_final.csv")
library(tidyverse)
library(lubridate)
library(car)
library(stringr)
library(dplyr)
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
  filter(season == "1" | season == "2" | season == "4" | season == "3" & imdb != " ") 


### ordenado pela temporada

grafico_analise2 <- ggplot(banco2, aes(x=season, y=imdb)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Temporada", y="Nota IMDB")+
  theme_estat() +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 2))

grafico_analise2
ggsave("graficoanalise2.png",plot = grafico_analise2 ,width = 158, height = 93, units = "mm")

## quadro analise 2

print_quadro_resumo <- function(data, var_name, title="Medidas resumo da nota imdb por temporada", label="quad:quadro_resumo2")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

banco2 %>% 
  group_by(season) %>%
  print_quadro_resumo(var_name = imdb)

#### met 2 analise 2

shapiro.test(banco2$imdb)
banco2$season <- factor(banco2$season)
leveneTest(engagement ~ season, data = banco2)
anova2 <- aov(imdb ~ season, data = banco2)

summary(anova2)

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

banco3 <- banco %>%
  select(setting_terrain, trap_work_first) %>% 
  filter(setting_terrain == "Urban"| setting_terrain == "Rural"| setting_terrain == "Forest") %>% 
  filter(trap_work_first == "True" | trap_work_first == "False")
banco3$setting_terrain <- factor(banco3$setting_terrain)  
banco3$trap_work_first <- factor(banco3$trap_work_first)
tabela_contingencia <- table(banco3$trap_work_first, banco3$setting_terrain)
chisq.test(tabela_contingencia)

# analise 4

grafico_analise4 <- ggplot(banco, aes(x=engagement, y=imdb)) + geom_point(colour="#A11D21", size=3, alpha = 0.4) +
  labs(x="Engajamento", y="Nota IMDB") +
  theme_estat() +
  scale_y_continuous(limits = c(0, 8.3), breaks = seq(2, 8, by = 2))

grafico_analise4
ggsave("graficoanalise4.png",plot = grafico_analise4 ,width = 158, height = 93, units = "mm")


cor.test(banco$imdb, banco$engagement, method = "pearson")

sd(banco$imdb, na.rm = T)
summary(banco$imdb)

grafico_engajamento <- ggplot(banco, aes(x=factor(""), y=engagement)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Engajamento")+
  theme_estat()

ggsave("graficoengajamento.png",plot = grafico_engajamento ,width = 158, height = 93, units = "mm")


grafico_imdb <- ggplot(banco, aes(x=factor(""), y=imdb)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Nota IMDB")+
  theme_estat()

ggsave("graficoimdb.png",plot = grafico_imdb ,width = 158, height = 93, units = "mm")

banco_est <- banco %>% 
  select(imdb, engagement) %>% 
  filter(imdb != " ") %>% 
  filter(engagement != " ")

shapiro.test(banco_est$imdb)
shapiro.test(banco_est$engagement)

#### quadro

print_quadro_resumo <- function(data, var_name, title="Medidas resumo do engajamento", label="quad:quadro_resumo42")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

print_quadro_resumo(banco, engagement)

#analise 5 


create_character_df <- function(df, column_name, character_name) {
  df %>%
    filter(!!sym(column_name) == "True") %>%
    select(all_of(column_name), engagement) %>%
    rename(caught = !!sym(column_name)) %>%
    mutate(caught = character_name)
}


banco_fred <- create_character_df(banco, "caught_fred", "Fred")
banco_velma <- create_character_df(banco, "caught_velma", "Velma")
banco_shaggy <- create_character_df(banco, "caught_shaggy", "Salsicha")
banco_daphne <- create_character_df(banco, "caught_daphnie", "Daphnie")
banco_scooby <- create_character_df(banco, "caught_scooby", "Scooby")
banco_outros <- create_character_df(banco, "caught_other", "Outros")
banco_ninguem <- create_character_df(banco, "caught_not", "Ninguém")

banco_completo <- bind_rows(banco_fred, banco_velma, banco_shaggy, banco_daphne, banco_scooby, banco_ninguem, banco_outros)


# grafico 

grafico_analise5 <- ggplot(banco_completo1) +
  aes(x = reorder(caught, engagement,  FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Quem Capturou", y = "Engajamento") +
  theme_estat() +
  scale_y_continuous(limits = c(100, 270), breaks = seq(100, 250, by = 50))

grafico_analise5

ggsave("graficoanalise5.png",plot = grafico_analise5 ,width = 158, height = 93, units = "mm")

# quadro

print_quadro_resumo <- function(data, var_name, title="Medidas resumo do engajamento", label="quad:quadro_resumo5")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

banco_completo %>% 
  group_by(caught) %>% 
  print_quadro_resumo(var_name = engagement)

#### met 2 analise 5

shapiro.test(banco_completo$engagement)
banco_completo$caught <- as.factor(banco_completo$caught)
leveneTest(engagement ~ caught, data = banco_completo)

anova5 <- aov(engagement ~ caught, data = banco_completo)

summary(anova5)

