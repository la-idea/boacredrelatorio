install.packages("tidyverse")
install.packages("tidyr")
install.packages("dyplr")

library(tidyverse)
library(reshape2)


# Ler bases Original>Relatorio1>Relatorio2
original <- read_csv2("Florianopolis_original.csv")
relatorio1 <- read_csv2("Pref_Florianópolis_Relatório_1.csv")
relatorio2 <- read_csv2("Pref_Florianópolis_Relatório_2.csv")

# Transformando sexo como um fator
original$Sexo <- as.factor(original$Sexo)

# Transformando nome das colunas e excluindo primeira linha
colnames(relatorio1) <- c("Tel", "Status", "Agente", "Data_hora",
                          "Duracao", "Uniqueid", "Failure_code",
                          "Failure_cause", "Nome", "CPF", "Sexo",
                          "Data_nasc", "Idade", "E_mail", 
                          "Tabulacao")
relatorio1 <- relatorio1[-1,]

colnames(relatorio2) <- c("Tel", "Status", "Agente", "Data_hora",
                          "Duracao", "Uniqueid", "Failure_code",
                          "Failure_cause", "Nome", "CPF", "Sexo",
                          "Data_nasc", "Idade", "E_mail",
                          "Tabulacao")
relatorio2 <- relatorio2[-1,]

relatorio1$Status <- NULL
relatorio1$Agente <- NULL
relatorio2$Status <- NULL
relatorio2$Agente <- NULL

# Adicionando coluna cidade
original["cidade"] <- "Florianópolis"
relatorio1["cidade"] <- "Florianópolis"
relatorio2["cidade"] <- "Florianópolis"

# Transformando fatores e status dos relatórios
relatorio1$Sexo <- as.factor(relatorio1$Sexo)
relatorio1$Tabulacao <- as.factor(relatorio1$Tabulacao)
relatorio1$Failure_code <- as.factor(relatorio1$Failure_code)
relatorio1$Failure_code <- as.factor(relatorio1$Failure_cause)
relatorio1$Idade <- as.integer(relatorio1$Idade)
relatorio1$Data_hora <- strptime(relatorio1$Data_hora,
                                 format = "%Y-%m-%d %H:%M:%S")



relatorio2$Sexo <- as.factor(relatorio2$Sexo)
relatorio2$Tabulacao <- as.factor(relatorio2$Tabulacao)
relatorio2$Failure_code <- as.factor(relatorio2$Failure_code)
relatorio2$Failure_code <- as.factor(relatorio2$Failure_cause)
relatorio2$Idade <- as.integer(relatorio2$Idade)
relatorio2$Data_hora <- strptime(relatorio2$Data_hora, format = "%Y-%m-%d %H:%M:%S")


# Separando clientes que aceitaram proposta
clientes2 <- relatorio2 %>%
  filter(relatorio2$Tabulacao == "ROBO - TRANSFERINDO PARA ANALISTA")

clientes1 <- relatorio1 %>%
  filter(relatorio1$Tabulacao == "ROBO - TRANSFERINDO PARA ANALISTA")

# Separando os que atenderam
atendidas <- relatorio2 %>%
  filter(relatorio2$Tabulacao != "ROBO - CAIXA POSTAL")

atendidas <- atendidas %>%
  filter(atendidas$Tabulacao != "ROBO - Telefone Indisponivel/Invalido")

atendidas_rel1 <- relatorio1 %>%
  filter(relatorio1$Tabulacao != "ROBO - CAIXA POSTAL")

atendidas_rel1 <- atendidas_rel1 %>%
  filter(atendidas_rel1$Tabulacao != "ROBO - Telefone Indisponivel/Invalido")

# Escrevendo arquivos com resumo de clientes e atendidas
write_csv2(atendidas_rel1, "atendidas_etapa1_florianopolis.csv")
write_csv2(clientes2,"clientes_florianopolis.csv")
write_csv2(atendidas, "atendidas_florianopolis.csv")

# Dados das chamadas atendidas
summary(atendidas)

# Ver qual a coluna de telefone de melhor aproveitamento
contagem_colunas_telefone <- data.frame(
  c(count(original %>%
            filter(original$TELEFONE_1
                   %in% atendidas$Tel)),
    count(original %>%
            filter(
              original$TELEFONE_2
              %in% atendidas$Tel)),
    count(original %>%
            filter(
              original$TELEFONE_3
              %in% atendidas$Tel)),
    count(original %>%
            filter(
              original$TELEFONE_4
              %in% atendidas$Tel)),
    count(original %>%
            filter(
              original$TELEFONE_5
              %in% atendidas$Tel)),
    count(original %>%
            filter(
              original$TELEFONE_6
              %in% atendidas$Tel)),
    count(original %>%
            filter(
              original$TELEFONE_7
              %in% atendidas$Tel))))

colnames(contagem_colunas_telefone) <- c("Telefone1", "Telefone2", "Telefone3",
                                         "Telefone4", "Telefone5", "Telefone6",
                                         "Telefone7")
write_csv2(contagem_colunas_telefone, "Florianopolis Contagem Sucesso de Colunas Telefone.csv")

# Contagem de telefones fixos e telefones celulares de sucesso
contagem_fixo_cel <- data.frame(c(
  count(filter(atendidas, str_sub(atendidas$Tel, 3, 3) >=6)),
  count(filter(atendidas, str_sub(atendidas$Tel, 3, 3) <=5))))
colnames(contagem_fixo_cel) <- c("Celular", "Fixo")
write_csv2(contagem_fixo_cel, "Florianopolis Contagem de quantos fixos e celulares.csv")
