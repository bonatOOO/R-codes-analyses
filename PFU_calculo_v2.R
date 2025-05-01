# Carregar pacotes
library(readr)
library(ggplot2)
library(emmeans)
library(ggsignif)
library(multcomp)

# 1. Gerar tabela modelo
tabela_modelo <- data.frame(
  Cepa = c("WT", "Mutante1", "Mutante2"),
  Placas_lise = c(NA, NA, NA),
  Volume_uL = c(NA, NA, NA),
  Fator_diluicao = c(NA, NA, NA)
)
write_csv(tabela_modelo, "tabela_modelo_PFU.csv")
cat("✅ Tabela modelo criada: 'tabela_modelo_PFU.csv'\nPreencha com seus dados e salve.\n\n")

# 2. Selecionar arquivo preenchido
arquivo_csv <- file.choose()

# 3. Função para cálculo de PFU/mL
calcular_PFU <- function(placas_lise, volume_uL, diluicao) {
  PFU_mL <- (placas_lise * 1000) / volume_uL * diluicao
  return(PFU_mL)
}

# 4. Ler a tabela preenchida
dados <- read_csv(arquivo_csv, locale = locale(encoding = "UTF-8"))

# 5. Calcular PFU/mL
dados$PFU_mL <- calcular_PFU(dados$Placas_lise, dados$Volume_uL, dados$Fator_diluicao)

# 6. Salvar os resultados com a nova coluna
write_csv(dados, "resultado_PFU_calculado.csv")
cat("✅ PFU/mL calculado e salvo em: 'resultado_PFU_calculado.csv'\n")

# 7. Análise estatística
anova <- aov(PFU_mL ~ Cepa, data = dados)
resumo_anova <- summary(anova)

# Pós-teste com Bonferroni
posthoc <- emmeans(anova, pairwise ~ Cepa, adjust = "bonferroni")
comparacoes <- posthoc$contrasts
resultados <- summary(comparacoes)

# 8. Salvar análise estatística em .txt
sink("analise_estatistica_PFU.txt")
cat("==== RESULTADOS DA ANOVA ====\n\n")
print(resumo_anova)
cat("\n\n==== COMPARAÇÕES MÚLTIPLAS COM WT (Bonferroni) ====\n\n")
print(resultados)
sink()
cat("✅ Análise estatística salva em: 'analise_estatistica_PFU.txt'\n")

# 9. Reordenar para WT ser a primeira
dados$Cepa <- factor(dados$Cepa, levels = c("WT", setdiff(unique(dados$Cepa), "WT")))

# 10. Gráfico
grafico <- ggplot(dados, aes(x = Cepa, y = PFU_mL)) +
  geom_bar(stat = "summary", fun = mean, fill = "steelblue", width = 0.6) +
  geom_errorbar(
    stat = "summary",
    fun.data = mean_se,
    width = 0.2,
    linewidth = 0.8
  ) +
  geom_signif(
    comparisons = lapply(setdiff(unique(dados$Cepa), "WT"), function(x) c("WT", x)),
    map_signif_level = TRUE,
    step_increase = 0.1,
    test = "t.test"
  ) +
  labs(
    title = "Quantificação de infecção por fago (PFU/mL)",
    x = "Cepa",
    y = "PFU/mL"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(
    labels = scales::trans_format("log10", math_format(10^.x)),
    trans = "log10",
    expand = expansion(mult = c(0, 0.3))  # aumenta o topo para espaço extra
  )

# 11. Exibir gráfico na tela
print(grafico)

# 12. Salvar gráfico em PNG
ggsave("grafico_PFU.png", plot = grafico, width = 8, height = 6, dpi = 300)
cat("✅ Gráfico salvo como: 'grafico_PFU.png'\n")
