# Instalar e carregar pacotes necessários
install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "reshape2", "multcomp"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(multcomp)  # Para o teste de Bonferroni

# Selecionar o arquivo CSV
arquivo <- file.choose()

# Ler o arquivo CSV selecionado
dados <- read.csv(arquivo, header = TRUE, sep = ",", fileEncoding = "UTF-8")

# Renomear as colunas para remover o "X" adicionado pelo R
colnames(dados) <- gsub("^X", "", colnames(dados))

# Converter os dados para formato longo
dados_longos <- dados %>%
  gather(key = "Tempo", value = "DO600", -Cepas) %>%
  mutate(
    Tempo = as.numeric(sub("^(\\d{2})\\.(\\d{2})\\.(\\d{2})$", "\\1", Tempo)) +  # Horas
      as.numeric(sub("^(\\d{2})\\.(\\d{2})\\.(\\d{2})$", "\\2", Tempo)) / 60  # Minutos
  )

# Remover linhas com valores NA
dados_longos <- dados_longos %>% drop_na()

# Converter Cepas para fator e definir WT como referência
dados_longos$Cepas <- factor(dados_longos$Cepas, levels = c("WT", setdiff(unique(dados_longos$Cepas), "WT")))

# Criar o gráfico de curvas de crescimento
grafico <- ggplot(dados_longos, aes(x = Tempo, y = DO600, color = Cepas, group = Cepas)) +
  geom_point(size = 1.5, alpha = 0.8) +  # Pontos menores com transparência
  geom_line(linewidth = 0.8) +  # Linhas do gráfico um pouco mais finas
  scale_color_manual(values = c("black", "red", "blue", "green", "purple", "orange")) +  # Cores personalizadas
  labs(
    x = "Tempo (horas)",
    y = "Aborsorvância 600 nm",
    title = "Curvas de Crescimento das Cepas de Bactérias",
    subtitle = "Medições ao longo do tempo",
    caption = "Análise de crescimento bacteriano"
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalista com fonte maior
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),  # Remove as linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove as linhas de grade secundárias
    axis.line = element_line(color = "black"),  # Adiciona as linhas dos eixos
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(0, max(dados_longos$Tempo, na.rm = TRUE), by = 4),  # Marcação a cada 4 horas
    limits = c(0, max(dados_longos$Tempo, na.rm = TRUE))  # Garante que o eixo comece do 0
  )

# Exibir o gráfico
print(grafico)


# ANOVA para comparar as cepas ao longo do tempo
anova_resultado <- aov(DO600 ~ Cepas * Tempo, data = dados_longos)

# Resumo da ANOVA
anova_resumo <- summary(anova_resultado)
print(anova_resumo)

# Teste de comparações múltiplas (Bonferroni) comparando as cepas com a WT
contraste <- glht(anova_resultado, linfct = mcp(Cepas = "Tukey"))
resultado_bonferroni <- summary(contraste, test = adjusted("bonferroni"))
print(resultado_bonferroni)

# Criar um data frame para salvar os resultados
anova_df <- as.data.frame(anova_resumo[[1]])  # Transformar ANOVA em data frame
bonferroni_df <- as.data.frame(resultado_bonferroni$test$pvalues)  # P-valores do Bonferroni
colnames(bonferroni_df) <- "p_value_Bonferroni"

# Criar um caminho para salvar o arquivo
caminho_saida <- file.path(dirname(arquivo), "resultado_analise.csv")

# Salvar o gráfico com a recomendação de tamanho (10x7 polegadas) e resolução de 300 dpi
ggsave("grafico_crescimento_bacteriano.png", grafico, width = 10, height = 7, dpi = 300)

# Salvar os resultados em um arquivo CSV
write.csv(anova_df, caminho_saida, row.names = TRUE)
write.csv(bonferroni_df, caminho_saida, row.names = TRUE, append = TRUE)

# Mensagem com o local onde foi salvo
cat("Os resultados foram salvos em:", caminho_saida, "\n")

