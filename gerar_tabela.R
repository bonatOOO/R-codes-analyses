# Gerar os tempos de medição
tempos <- seq(from = 0, to = 48, by = 0.5)  # 48 horas com intervalos de 30 minutos
tempos_formatados <- sprintf("%02d:%02d:00", floor(tempos), (tempos %% 1) * 60)  # Formatar como HH:MM:00

# Criar uma tabela de exemplo com cepas e medições (do valor de DO600)
cepas <- c("WT", "WT", "WT", "WT", "WT", "WT", "PB001", "PB001", "PB001", "PB001", "PB001", "PB001", "PB003", "PB003", "PB003", "PB003", "PB003", "PB003", "PB008", "PB008", "PB008", "PB008", "PB008", "PB008")
valores_do600 <- matrix(runif(length(cepas) * length(tempos), min = 0, max = 3), 
                        nrow = length(cepas), 
                        dimnames = list(cepas, tempos_formatados))

# Convertendo para um data.frame
tabela <- as.data.frame(valores_do600)
tabela$Cepas <- rownames(tabela)

# Reorganizar as colunas para que 'Cepas' esteja na primeira coluna
tabela <- tabela[, c(ncol(tabela), 1:(ncol(tabela) - 1))]

# Salvar como CSV
write.csv(tabela, "tabela_crescimento_bacteriano.csv", row.names = FALSE)

# Exibir as primeiras linhas da tabela gerada
head(tabela)

# Verificar o diretório de trabalho atual
getwd()

