# Este código analisa dados de motilidade bacteriana e gera gráficos e testes estatísticos.
# Inclui suporte para as motilidades "Swarming", "Twitching", "Swimming" e "Sliding".

# Carregar bibliotecas necessárias
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggsignif)

# Definir motilidade e data
motility <- "Sliding" # Escolha: "Swarming", "Twitching", "Swimming", "Sliding"
date <- Sys.Date()

# Escolher arquivo de dados interativamente
cat("Selecione o arquivo de dados (CSV):\n")
my_file <- file.choose()

# Ler dados do arquivo
raw_data <- read.csv(my_file, header = TRUE)
raw_data$Cepas <- factor(raw_data$Cepas, levels = unique(raw_data$Cepas))

# Verificar se as colunas necessárias estão presentes
if (!all(c("Cepas", "Area") %in% colnames(raw_data))) {
  stop("O arquivo deve conter as colunas 'Cepas' e 'Area'.")
}

# Teste t com e sem correção de Bonferroni
t_tests <- pairwise.t.test(raw_data$Area, raw_data$Cepas, p.adjust.method = "bonferroni")
t_test_nocorrect <- pairwise.t.test(raw_data$Area, raw_data$Cepas)
smp_size <- length(raw_data$Cepas) / length(unique(raw_data$Cepas))

# Criar dados para o gráfico
graph_data <- raw_data %>%
  group_by(Cepas) %>%
  summarise(
    Mean = mean(Area),
    SEM = sd(Area) / sqrt(smp_size)
  )

# Gerar relatório de resultados
title <- paste(date, "Motility", motility, sep = "_")
output_file <- paste0(title, ".txt")
sink(output_file)
cat("Este relatório foi gerado para o arquivo:\n")
cat(my_file, "\n\n")
cat("Data e hora da análise:\n")
cat(Sys.time(), "\n\n")
cat("> Teste de significância entre cepas (sem correção):\n")
print(t_test_nocorrect$p.value)
cat("\n> Teste de significância entre cepas (correção Bonferroni):\n")
print(t_tests$p.value)
sink()

# Definir cor com base na motilidade
color <- switch(
  motility,
  "Twitching" = "darkmagenta",
  "Swimming" = "cyan4",
  "Swarming" = "dodgerblue4",
  "Sliding" = "goldenrod",
  "black" # Cor padrão
)

# Criar o gráfico
plot_motility <- ggplot(data = graph_data, aes(x = Cepas, y = Mean, fill = Cepas)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2, position = position_dodge(0.9), colour = "black") +
  scale_fill_manual(values = rep(color, length(unique(graph_data$Cepas)))) +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle(paste("Motilidade -", motility)) +
  ylab("Área (mm²)") +
  xlab("Cepas") +
  theme_classic() +
  theme(legend.position = "none")

# Exibir o gráfico
print(plot_motility)

# Salvar o gráfico como PNG
graph_file <- paste0(title, ".png")
ggsave(filename = graph_file, plot = plot_motility, width = 8, height = 6)

cat("Análise concluída. Relatório salvo em", output_file, "e gráfico salvo em", graph_file, ".\n")
getwd()
