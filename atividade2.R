# Carregar os pacotes necessários
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

dados <- read.table("/Users/felipekucharski/Desktop/bancodedados.txt",header = TRUE)

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


#ANÁLISE VARIAVEL GRUPO

# Calcular a frequência de ocorrência dos grupos
frequencia_grupos <- table(dados$Grupo)

# Exibir a soma da quantidade de cada grupo
print(frequencia_grupos)

# Criar a tabela de distribuição de frequência simples
tabela_freq_simples <- dados %>%
  group_by(Grupo) %>%
  summarise(FrequenciaAbsoluta = n()) %>%
  mutate(FrequenciaPercentual = FrequenciaAbsoluta / sum(FrequenciaAbsoluta) * 100,
         FrequenciaRelativa = FrequenciaAbsoluta / sum(FrequenciaAbsoluta))

# Criar a tabela de distribuição de frequência acumulada
tabela_freq_acumulada <- tabela_freq_simples %>%
  mutate(FrequenciaAcumuladaAbsoluta = cumsum(FrequenciaAbsoluta),
         FrequenciaAcumuladaPercentual = cumsum(FrequenciaPercentual),
         FrequenciaAcumuladaRelativa = cumsum(FrequenciaRelativa))

# Exibir as tabelas
cat("Tabela de Frequência Absoluta, Percentual e Relativa:\n")
tabela_freq_simples %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

cat("\nTabela de Frequência Acumulada Absoluta, Percentual e Relativa:\n")
tabela_freq_acumulada %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


# Calcular a frequência de ocorrência dos grupos
frequencia_grupos <- table(dados$Grupo)
frequencia_grupos

# Converter a tabela de frequência em um data frame
df <- as.data.frame(frequencia_grupos)

# Renomear as colunas do data frame
colnames(df) <- c("Grupo", "FrequenciaAbsoluta")

# Calcular a frequência relativa dos grupos
df$FrequenciaRelativa <- df$FrequenciaAbsoluta / sum(df$FrequenciaAbsoluta)

# Criar o gráfico de barras
ggplot(df, aes(x = Grupo, y = FrequenciaAbsoluta)) +
  geom_bar(stat = "identity") +
  ggtitle("Gráfico de Barras da Variável Grupo")

# Criar o gráfico de setores
ggplot(df, aes(x = "", y = FrequenciaAbsoluta, fill = Grupo)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  ggtitle("Gráfico de Setores da Variável Grupo")

# Identificar a moda
moda <- names(which.max(frequencia_grupos))

# Exibir a moda
cat("A moda da variável Grupo é:", moda)




#ANÁLISE VARIAVEL NOTA

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

# Exibir os resultados
cat("Média:", media_nota, "\n")
cat("Mediana:", mediana_nota, "\n")
cat("Mínimo:", minimo_nota, "\n")
cat("Máximo:", maximo_nota, "\n")
cat("Desvio Padrão:", desvio_padrao_nota, "\n")

# 1. Visualização geral da distribuição das notas do grupo
# Histograma
ggplot(dados, aes(x = Nota)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", na.rm = TRUE) +
  labs(x = "Nota", y = "Frequência", title = "Distribuição das Notas do Grupo") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(floor(min(dados$Nota, na.rm = TRUE)), ceiling(max(dados$Nota, na.rm = TRUE)), 1))

# Boxplot
ggplot(dados, aes(x = Grupo, y = Nota, fill = Grupo)) +
  geom_boxplot() +
  labs(x = "Grupo", y = "Nota", title = "Boxplot das Notas por Grupo") +
  theme_minimal()

