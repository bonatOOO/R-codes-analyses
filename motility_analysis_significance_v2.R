# Carregar as bibliotecas necessárias
library(ggplot2)
library(ggsignif)
library(dplyr)
library(tidyr)  # Função pivot_longer
library(tibble) # Função rownames_to_column

# Selecionar arquivo via explorador
my_file <- file.choose()
raw_data <- read.csv(my_file, header = TRUE, fileEncoding = "UTF-8")

# Configurar como fatores para manter a ordem das cepas
raw_data$Cepas <- factor(raw_data$Cepas, levels = unique(raw_data$Cepas))

# Definir a motilidade e a cor
motility <- "Swarming" # Pode ser Swarming, Twitching, Swimming, Sliding, CMC
motility_colors <- c(
  "Swarming" = "dodgerblue4",
  "Twitching" = "darkmagenta",
  "Swimming" = "cyan4",
  "Sliding" = "goldenrod",
  "CMC" = "red"
)
color <- motility_colors[motility]

# Testes estatísticos: Comparar WT com as demais
WT <- "WT" # Nome da cepa controle
comparisons <- lapply(setdiff(unique(raw_data$Cepas), WT), function(x) c(WT, x))

t_tests <- pairwise.t.test(raw_data$Area, raw_data$Cepas, p.adjust.method = "bonferroni")
significance_levels <- as.data.frame(t_tests$p.value) %>%
  rownames_to_column("Group1") %>%
  pivot_longer(cols = -Group1, names_to = "Group2", values_to = "p_value") %>%
  filter(Group1 == WT | Group2 == WT) %>%
  mutate(Significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ "NS"  # Exibe "NS" quando não há significância
  ))

# Calcular média e erro padrão
graph_data <- raw_data %>%
  group_by(Cepas) %>%
  summarise(
    Mean = mean(Area),
    SEM = sd(Area) / sqrt(n())
  ) %>%
  mutate(Motility_Color = color)

# Ajustar a unidade para cm² se a motilidade for CMC
if (motility == "CMC") {
  graph_data <- graph_data %>%
    mutate(
      Mean = Mean / 100,  # Converter mm² para cm²
      SEM = SEM / 100
    )
}

# Criar o gráfico
plot_motility <- ggplot(graph_data, aes(x = Cepas, y = Mean, fill = Motility_Color)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(0.8)) + # Ajusta largura das barras
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2, position = position_dodge(0.8)) +
  geom_signif(
    comparisons = comparisons,
    map_signif_level = TRUE,
    step_increase = 0.5,  # Aumenta o espaçamento vertical entre linhas de significância
    textsize = 3,  # Ajusta o tamanho do texto
    annotations = significance_levels$Significance  # Exibe todos os valores
  ) +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(graph_data$Mean + graph_data$SEM) * 1.6)) + # Ajusta o limite do eixo Y
  ggtitle(paste(motility, "(em cm²)")) +
  ylab("Área (cm²)") +
  xlab("Cepas") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1) # Inclina os rótulos do eixo X para evitar sobreposição
  )

# Salvar o gráfico
date <- Sys.Date()
title <- paste(date, "Motility", motility, sep = "_")
ggsave(filename = paste(title, "png", sep = "."), plot = plot_motility)

# Exibir o gráfico
print(plot_motility)

# Ver onde salvou
getwd()

