library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate) # Para manipulação do tempo
library(stats)  # Para ANOVA

# Carregar os dados
growth_m63 <- read_csv(file.choose())

# Ajuste dos tempos
times <- colnames(growth_m63)[-1] # Remove a coluna 'Cepas'
times <- gsub("X", "", times)  # Remove o "X" do início dos nomes das colunas
times <- gsub("\\.", ":", times)  # Substitui pontos por dois-pontos para o formato de hora
times <- as.POSIXct(times, format = "%H:%M:%S")  # Converte para data-hora
times <- hour(times) + minute(times) / 60  # Extrai horas + fração de minutos

# Transformar os dados para formato longo (tidy data)
growth_m63_long <- growth_m63 %>%
  pivot_longer(-Cepas, names_to = "tempo", values_to = "absorbancia") %>%
  mutate(tempo = gsub("X", "", tempo),  # Remove "X"
         tempo = gsub("\\.", ":", tempo),  # Substitui "." por ":"
         tempo = as.POSIXct(tempo, format = "%H:%M:%S"),  # Converte para formato de hora
         tempo = hour(tempo) + minute(tempo) / 60)  # Converte para decimal

# Verificar se houve erro na conversão
print(growth_m63_long)

# Cálculo da média e erro padrão por tempo e cepa
stat_m63 <- growth_m63_long %>%
  group_by(Cepas, tempo) %>%
  summarise(
    mean = mean(absorbancia, na.rm = TRUE),
    se = sd(absorbancia, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ANOVA: Teste de diferenças significativas entre as cepas em cada tempo
anova_results <- growth_m63_long %>%
  group_by(tempo) %>%
  do(tidy(aov(absorbancia ~ Cepas, data = .)))  # Teste ANOVA por tempo

# Adicionando a correção de Bonferroni aos resultados
anova_results <- anova_results %>%
  mutate(
    p_value_adjusted = p.adjust(p.value, method = "bonferroni")  # Correção de Bonferroni
  )

# Exibir resultados da ANOVA com correção de Bonferroni
print(anova_results)

# Criando o gráfico com facetas por cepa e mantendo a mesma escala no eixo Y
plot_m63_facetas <- ggplot(stat_m63, aes(x = tempo, y = mean, group = Cepas, color = Cepas)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25) +
  scale_color_viridis_d(option = "plasma") +  # Paleta automática de cores
  theme_classic(base_size = 14) +
  xlab("Tempo de incubação (h)") +
  ylab("Absorbância a 600 nm") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 21)) +  # Ajustado para 21 horas
  scale_y_continuous(trans = "log10", expand = c(0, 0.6)) +
  labs(title = "Curva de Crescimento - M63", color = "Cepas") +
  facet_wrap(~ Cepas)  # Mantendo a mesma escala no eixo Y para todos os painéis

# Criando o gráfico único com todas as cepas
plot_m63_unico <- ggplot(stat_m63, aes(x = tempo, y = mean, group = Cepas, color = Cepas)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.25) +
  scale_color_viridis_d(option = "plasma") +  # Paleta automática de cores
  theme_classic(base_size = 14) +
  xlab("Tempo de incubação (h)") +
  ylab("Absorbância a 600 nm") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 21)) +  # Ajustado para 21 horas
  scale_y_continuous(trans = "log10", expand = c(0, 0.6)) +
  labs(title = "Curva de Crescimento - M63 (Todas as Cepas)", color = "Cepas")

# Exibir os gráficos
print(plot_m63_facetas)  # Facetas por cepa
print(plot_m63_unico)    # Gráfico único com todas as cepas

# Salvar os gráficos
ggsave(filename = "Curva_Crescimento_M63_21h_Facetas.png", plot = plot_m63_facetas, height = 90, width = 160, units = "mm")
ggsave(filename = "Curva_Crescimento_M63_21h_Unico.png", plot = plot_m63_unico, height = 90, width = 160, units = "mm")
