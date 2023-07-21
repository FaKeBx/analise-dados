# Carregar os pacotes necessários
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(moments)
library(DT)

# Carregar banco de dados
dados <- read.table("/Users/felipekucharski/Desktop/atividade2/bancodedados.txt",header = TRUE)

# classificar todas as variáveis desse conjunto de dados:
# Criar uma tabela vazia
tabela <- matrix(nrow = 3, ncol = 2)
colnames(tabela) <- c("Variável", "Tipo de Variável")
rownames(tabela) <- NULL

# Preencher a tabela com os valores apropriados
tabela[1, "Variável"] <- "Grupo"
tabela[1, "Tipo de Variável"] <- "Qualitativa"
tabela[2, "Variável"] <- "Nota"
tabela[2, "Tipo de Variável"] <- "Quantitativa"
tabela[3, "Variável"] <- "Aluno"
tabela[3, "Tipo de Variável"] <- "Qualitativa"

# Exibir a tabela
tabela %>%
  as.data.frame() %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

###########################

# 1 ANÁLISE VARIAVEL GRUPO
# Calcular a frequência de ocorrência dos grupos
frequencia_grupos <- table(dados$Grupo)



# Exibir a soma de alunos de cada grupo
frequencia_grupos

# Exibe o grupo com maior número de alunos
maior_grupo <- names(which.max(frequencia_grupos))
cat("O maior grupo é:", maior_grupo, "\n")

# COMPARECIMENTO DE GRUPOS
# Em valores absolutos

# Exibe todos os alunos que compareceram em cada grupo
comparecimento <- dados$Nota != "NC"
tabela_comparecimento <- table(dados$Grupo[comparecimento])
tabela_comparecimento

# Em valores percentuais
proporcao <- prop.table(tabela_comparecimento)
porcentagem <- round(proporcao * 100, 2)
resultado <- paste0(names(tabela_comparecimento), ": ", porcentagem, "%")
resultado

# Número de valores ausentes (NA ou "NC") em cada grupo
# Transformar "NC" em NA (valor ausente)
dados$Nota[dados$Nota == "NC"] <- NA

# Contar o número de vezes que "NC" aparece em cada grupo
contagem_NC_por_grupo <- dados %>%
  group_by(Grupo) %>%
  summarise(Quantidade_NC = sum(is.na(Nota)))

# Exibir a contagem de valores "NC" em cada grupo
print("Contagem de valores 'NC' em cada grupo:")
contagem_NC_por_grupo

# Tabela grupo
tabelaGrupo <- aggregate(Aluno ~ Grupo, data = dados, FUN = length)
colnames(tabelaGrupo)[2] <- "Total de Alunos"

tabelaGrupo$Comparecimento <- aggregate(Aluno ~ Grupo, data = subset(dados, Nota != "NC"), FUN = length)$Aluno
tabelaGrupo$Nao_Comparecimento <- tabelaGrupo$`Total de Alunos` - tabelaGrupo$Comparecimento
colnames(tabelaGrupo)[4] <- gsub("Nao_Comparecimento", " Não Comparecimento", colnames(tabelaGrupo)[4], fixed = TRUE)

tabelaGrupo %>%
  as.data.frame() %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Converter a tabela de frequência em um data frame
df <- as.data.frame(frequencia_grupos)

# Renomear as colunas do data frame
colnames(df) <- c("Grupo", "Numero_de_Alunos")

# Calcular a frequência relativa dos grupos
df$FrequenciaRelativa <- df$frequencia_grupos / sum(df$frequencia_grupos)

# Criar o gráfico de barras
ggplot(df, aes(x = Grupo, y = frequencia_grupos)) +
  geom_bar(stat = "identity") +
  ggtitle("Gráfico de Barras da Variável Grupo")

# Criar o gráfico de setores
ggplot(df, aes(x = "", y = frequencia_grupos, fill = Grupo)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  ggtitle("Gráfico de Setores da Variável Grupo")

# Identificar a moda
moda <- names(which.max(frequencia_grupos))

# Exibir a moda
cat("A moda da variável Grupo é:", moda)





# 2 ANÁLISE VARIAVEL NOTA

# Remover valores não numéricos da variável Nota
dados$Nota <- as.numeric(as.character(dados$Nota))

# Criar a tabela de distribuição de frequência simples
tabela_freq_simples <- dados %>%
  group_by(Nota) %>%
  summarise(FrequenciaAbsoluta = n()) %>%
  mutate(Comparecimento_Percentual = FrequenciaAbsoluta / sum(FrequenciaAbsoluta) * 100,
         FrequenciaRelativa = FrequenciaAbsoluta / sum(FrequenciaAbsoluta))

# Criar a tabela de distribuição de frequência acumulada
tabela_freq_acumulada <- tabela_freq_simples %>%
  mutate(FrequenciaAcumuladaAbsoluta = cumsum(FrequenciaAbsoluta),
         FrequenciaAcumuladaPercentual = cumsum(Comparecimento_Percentual),
         FrequenciaAcumuladaRelativa = cumsum(FrequenciaRelativa))

# Ordenar as tabelas pelas notas
tabela_freq_simples <- tabela_freq_simples[order(tabela_freq_simples$Nota), ]
tabela_freq_acumulada <- tabela_freq_acumulada[order(tabela_freq_acumulada$Nota), ]

# Exibir as tabelas
cat("Tabela de Frequência Absoluta, Percentual e Relativa:\n")
tabela_freq_simples %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

cat("\nTabela de Frequência Acumulada Absoluta, Percentual e Relativa:\n")
tabela_freq_acumulada %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


# Remover valores "NC" (não calculável) para permitir cálculos numéricos
dados$Nota <- as.numeric(as.character(dados$Nota))

# Calcular a média da variável "Nota"
media_nota <- mean(dados$Nota, na.rm = TRUE)

# Calcular a mediana da variável "Nota"
mediana_nota <- median(dados$Nota, na.rm = TRUE)

# Calcular o valor mínimo da variável "Nota"
minimo_nota <- min(dados$Nota, na.rm = TRUE)

# Calcular o valor máximo da variável "Nota"
maximo_nota <- max(dados$Nota, na.rm = TRUE)

# Calcular o desvio padrão da variável "Nota"
desvio_padrao_nota <- sd(dados$Nota, na.rm = TRUE)

# Calcular a variância da variável Nota
variancia <- var(dados$Nota)

#Coef de Variação
coeficiente_variacao <- (desvio_padrao_nota / media_nota) * 100

# Exibir os resultados
cat("Média:", media_nota, "\n")
cat("Mediana:", mediana_nota, "\n")
cat("Mínimo:", minimo_nota, "\n")
cat("Máximo:", maximo_nota, "\n")
cat("Desvio Padrão:", desvio_padrao_nota, "\n")
cat("Variância:", variancia, "\n")
cat("Coeficiente de Variação:", coeficiente_variacao, "%\n")


# Visualização geral da distribuição das notas do grupo
# Histograma
ggplot(dados, aes(x = Nota)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(x = "Nota", y = "Frequência", title = "Distribuição das Notas do Grupo") +
  theme_minimal()


# Boxplot
ggplot(dados, aes(x = Grupo, y = Nota, fill = Grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "Nota", title = "Boxplot das Notas por Grupo") +
  theme_minimal()

# Converter a variável Nota em um vetor numérico
dados$Nota <- as.numeric(as.character(dados$Nota))

# Remover valores NA da variável Nota
dados <- dados[!is.na(dados$Nota), ]

# Calcular a assimetria e curtose da variável Nota
assimetria <- mean((dados$Nota - mean(dados$Nota))^3) / sd(dados$Nota)^3
curtose <- mean((dados$Nota - mean(dados$Nota))^4) / sd(dados$Nota)^4 - 3

# Exibir as medidas de assimetria e curtose
cat("Assimetria:", assimetria, "\n")
cat("Curtose:", curtose, "\n")

# Criar um gráfico de caixa para a variável Nota
ggplot(dados, aes(x = "", y = Nota)) +
  geom_boxplot() +
  ggtitle("Gráfico de Caixa da Variável Nota")

# Calcular a assimetria e curtose da variável Nota
dados$Nota[dados$Nota == "NC"] <- NA
dados$Nota <- as.numeric(dados$Nota)

sort(dados$Nota)
assimetria <- skewness(dados$Nota, na.rm = TRUE)
curtose <- kurtosis(dados$Nota, na.rm = TRUE)

cat("Assimetria:", assimetria, "\n")
cat("Curtose:", curtose, "\n")

# QUARTIS NOTAS
dados$Nota[dados$Nota == "NC"] <- NA
dados$Nota <- as.numeric(dados$Nota)

quartis <- quantile(dados$Nota, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quartis

#resumo dos dados
summary(dados$Nota)


#3 ANALISE VARIAVEL analise os resultados da variável Nota por categoria da variável Grupo

#DISPERSÃO NOTAS DOS ALUNOS POR GRUPO
dados$Nota[dados$Nota == "NC"] <- NA
dados$Nota <- as.numeric(dados$Nota)

ggplot(dados, aes(x = Aluno, y = Nota, color = Grupo)) +
  geom_point() +
  facet_wrap(~Grupo) +
  theme_bw()

#MEDIA, MEDIANA, ASSIMETRIA, DESVIO, Q1,Q3 E CURTOSE

# Calcular as estatísticas das notas para cada grupo
estatisticas_por_grupo <- dados %>%
  group_by(Grupo) %>%
  summarise(Média = mean(Nota, na.rm = TRUE),
            Mediana = median(Nota, na.rm = TRUE),
            Desvio_Padrão = sd(Nota, na.rm = TRUE),
            Q1 = quantile(Nota, 0.25, na.rm = TRUE),
            Q3 = quantile(Nota, 0.75, na.rm = TRUE),
            Assimetria = skewness(Nota, na.rm = TRUE),
            Curtose = kurtosis(Nota, na.rm = TRUE))

# Exibir as estatísticas por grupo
kable(estatisticas_por_grupo, format = "markdown")

#boxplot notas por grupo
ggplot(dados, aes(x = Grupo, y = Nota)) +
  geom_boxplot() +
  theme_bw()

# Verificar a quantidade de valores únicos em cada grupo
quantidade_valores_unicos <- dados %>%
  group_by(Grupo) %>%
  summarise(Quantidade_unicos = n_distinct(Nota))

# Calcular o mínimo e máximo das notas para cada grupo
min_max_notas_por_grupo <- dados %>%
  group_by(Grupo) %>%
  summarise(Mínimo_Nota = min(Nota, na.rm = TRUE),
            Máximo_Nota = max(Nota, na.rm = TRUE))

# Exibir a quantidade de valores únicos em cada grupo
print("Quantidade de valores únicos em cada grupo:")
print(quantidade_valores_unicos)

# Exibir o mínimo e máximo das notas para cada grupo
print("Mínimo e Máximo das notas para cada grupo:")
print(min_max_notas_por_grupo)
kable(min_max_notas_por_grupo, format = "markdown")

# Criar o histograma separado para cada grupo
histograma_notas <- ggplot(dados, aes(x = Nota, fill = Grupo)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  labs(title = "Distribuição das Notas por Grupo", x = "Nota", y = "Frequência") +
  scale_fill_manual(values = c("skyblue", "pink", "lightgreen")) +
  facet_wrap(~ Grupo, ncol = 1) +
  theme_minimal()

# Exibir o histograma
print(histograma_notas)


# Criar o gráfico de densidade para cada grupo
grafico_densidade <- ggplot(dados, aes(x = Nota, fill = Grupo)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuição de Densidade das Notas por Grupo", x = "Nota", y = "Densidade") +
  scale_fill_discrete(name = "Grupo") +
  theme_minimal()

# Exibir os gráficos de densidade
print(grafico_densidade)


# Criar o gráfico de barras empilhadas
grafico_barras_empilhadas <- ggplot(dados, aes(x = Nota, fill = Grupo)) +
  geom_bar(position = "fill") +
  labs(title = "Proporção de Notas em Cada Grupo", x = "Nota", y = "Proporção") +
  scale_fill_discrete(name = "Grupo") +
  theme_minimal()

# Exibir o gráfico de barras empilhadas
print(grafico_barras_empilhadas)

# Agrupe e conte o número de alunos por grupo e nota usando dplyr
contagem_alunos <- dados %>%
  group_by(Grupo, Nota) %>%
  summarise(Num_Alunos = n()) %>%
  arrange(Nota)

# Formate a tabela usando kableExtra
tabela_formatada <- kable(contagem_alunos, format = "html", caption = "Número de Alunos por Grupo e Nota") %>%
  kable_styling("striped", full_width = FALSE)

# Imprima a tabela formatada
print(tabela_formatada)


#4 avaliação das notas nas diferentes categorias da variável Grupo usando os quartis (modelo Enade).

# Transformar "NC" em NA (valor ausente)
dados$Nota[dados$Nota == "NC"] <- NA

# Calcular os quartis (q1, q2, e q3) de todos os dados
q1 <- quantile(dados$Nota, 0.25, na.rm = TRUE)
q2 <- quantile(dados$Nota, 0.50, na.rm = TRUE)
q3 <- quantile(dados$Nota, 0.75, na.rm = TRUE)

# Definir os intervalos
intervalo_q1 <- dados$Nota < q1
intervalo_q2 <- dados$Nota >= q1 & dados$Nota < q2
intervalo_q3 <- dados$Nota >= q2 & dados$Nota < q3
intervalo_q4 <- dados$Nota >= q3

# Calcular as porcentagens dos alunos em cada intervalo para o grupo X
grupo_x <- subset(dados, Grupo == "X")
porcentagem_x_q1 <- mean(grupo_x$Nota < q1, na.rm = TRUE) * 100
porcentagem_x_q2 <- mean(grupo_x$Nota >= q1 & grupo_x$Nota < q2, na.rm = TRUE) * 100
porcentagem_x_q3 <- mean(grupo_x$Nota >= q2 & grupo_x$Nota < q3, na.rm = TRUE) * 100
porcentagem_x_q4 <- mean(grupo_x$Nota >= q3, na.rm = TRUE) * 100

# Calcular as porcentagens dos alunos em cada intervalo para o grupo Y
grupo_y <- subset(dados, Grupo == "Y")
porcentagem_y_q1 <- mean(grupo_y$Nota < q1, na.rm = TRUE) * 100
porcentagem_y_q2 <- mean(grupo_y$Nota >= q1 & grupo_y$Nota < q2, na.rm = TRUE) * 100
porcentagem_y_q3 <- mean(grupo_y$Nota >= q2 & grupo_y$Nota < q3, na.rm = TRUE) * 100
porcentagem_y_q4 <- mean(grupo_y$Nota >= q3, na.rm = TRUE) * 100

# Calcular as porcentagens dos alunos em cada intervalo para o grupo Z
grupo_z <- subset(dados, Grupo == "Z")
porcentagem_z_q1 <- mean(grupo_z$Nota < q1, na.rm = TRUE) * 100
porcentagem_z_q2 <- mean(grupo_z$Nota >= q1 & grupo_z$Nota < q2, na.rm = TRUE) * 100
porcentagem_z_q3 <- mean(grupo_z$Nota >= q2 & grupo_z$Nota < q3, na.rm = TRUE) * 100
porcentagem_z_q4 <- mean(grupo_z$Nota >= q3, na.rm = TRUE) * 100

# Exibir os resultados
print("Porcentagem de alunos em cada intervalo para o grupo X:")
print(c(q1 = porcentagem_x_q1, q2 = porcentagem_x_q2, q3 = porcentagem_x_q3, q4 = porcentagem_x_q4))

print("Porcentagem de alunos em cada intervalo para o grupo Y:")
print(c(q1 = porcentagem_y_q1, q2 = porcentagem_y_q2, q3 = porcentagem_y_q3, q4 = porcentagem_y_q4))

print("Porcentagem de alunos em cada intervalo para o grupo Z:")
print(c(q1 = porcentagem_z_q1, q2 = porcentagem_z_q2, q3 = porcentagem_z_q3, q4 = porcentagem_z_q4))


