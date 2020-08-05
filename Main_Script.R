install.packages("plotly")

library(tidyverse)
library(dplyr)
library(reshape2)
library(plotly)

##### Importando os dados agregados dos outros Scripts e concatenando em tabelas únicas
atendidas_floripa <- read_csv2("atendidas_florianopolis.csv")
atendidas_biguacu <- read_csv2("atendidas_biguacu.csv")
atendidas_sj <- read_csv2("atendidas_saojose.csv")
atendidas_joinville <- read_csv2("atendidas_joinville.csv")

#Limpando colunas inúteis
atendidas_joinville$E_mail2 <- NULL
atendidas_sj$E_mail2 <- NULL
atendidas_biguacu$E_mail2 <- NULL
atendidas_sj$E_mail3 <- NULL
atendidas_sj$E_mail4 <- NULL
atendidas_sj["Sexo"] <- NA
d2 <- atendidas_sj[,c("Tel", "Data_hora", "Duracao", "Uniqueid", "Failure_code",
                      "Failure_cause", "Nome", "CPF", "Sexo", "Data_nasc",
                      "Idade", "E_mail", "Tabulacao", "cidade")]

atendidas_floripa_e1 <- read_csv2("atendidas_etapa1_florianopolis.csv")
atendidas_biguacu_e1 <- read_csv2("atendidas_etapa1_biguacu.csv")
atendidas_sj_e1 <- read_csv2("Atendidas_etapa1_saojose.csv")
atendidas_joinville_e1 <- read_csv2("atendidas_etapa1_joinville.csv")

atendidas_joinville_e1$E_mail2 <- NULL
atendidas_sj_e1$E_mail2 <- NULL
atendidas_biguacu_e1$E_mail2 <- NULL
atendidas_sj_e1$E_mail3 <- NULL
atendidas_sj_e1$E_mail4 <- NULL
atendidas_sj_e1["Sexo"] <- NA
d3 <- atendidas_sj_e1[,c("Tel", "Data_hora", "Duracao", "Uniqueid", "Failure_code",
                      "Failure_cause", "Nome", "CPF", "Sexo", "Data_nasc",
                      "Idade", "E_mail", "Tabulacao", "cidade")]

chamadas_atendidas <- rbind(atendidas_biguacu, atendidas_floripa,
                            atendidas_joinville, d2)

chamadas_atendidas_etapa1 <- rbind(atendidas_biguacu_e1, atendidas_floripa_e1,
                                   atendidas_joinville_e1, d3)

rm(atendidas_biguacu, atendidas_biguacu_e1, atendidas_floripa, 
   atendidas_floripa_e1, atendidas_joinville, atendidas_joinville_e1,
   atendidas_sj, atendidas_sj_e1, d2, d3)

chamadas_atendidas$Sexo <- as.factor(chamadas_atendidas$Sexo)
chamadas_atendidas$Tabulacao <- as.factor(chamadas_atendidas$Tabulacao)
chamadas_atendidas$cidade <- as.factor(chamadas_atendidas$cidade)

chamadas_atendidas_etapa1$Sexo <- as.factor(chamadas_atendidas_etapa1$Sexo)
chamadas_atendidas_etapa1$Tabulacao <- as.factor(chamadas_atendidas_etapa1$Tabulacao)
chamadas_atendidas_etapa1$cidade <- as.factor(chamadas_atendidas_etapa1$cidade)

clientes <- chamadas_atendidas %>%
  filter(Tabulacao == "ROBO - TRANSFERINDO PARA ANALISTA")

sucesso_por_coluna <- 
  rbind(read_csv2("Joinvile Contagem Sucesso de Colunas Telefone.csv"),
        read_csv2("Biguaçu Contagem Sucesso de Colunas Telefone.csv"),
        read_csv2("São José Contagem Sucesso de Colunas Telefone.csv"),
        read_csv2("Florianopolis Contagem Sucesso de Colunas Telefone.csv"))

sucesso_celular_fixo <-
  rbind(read_csv2("Joinville Contagem de quantos fixos e celulares.csv"),
        read_csv2("Biguaçu Contagem de quantos fixos e celulares.csv"),
        read_csv2("São José Contagem de quantos fixos e celulares.csv"),
        read_csv2("Florianopolis Contagem de quantos fixos e celulares.csv"))

colnames(chamadas_atendidas_etapa1) <- c("Tel", "Data_hora", "Duracao",
                                         "Uniqueid","Failure_code",
                                         "Failure_cause", "Nome", "CPF",
                                         "Sexo", "Data_nasc", "Idade",
                                         "E_mail", "Tabulacao1","cidade")
atendidas <- inner_join(chamadas_atendidas_etapa1, chamadas_atendidas[,c("CPF","Tabulacao")], by = "CPF")
atendidas_mudou_tabulacao <- atendidas %>%
  filter(atendidas$Tabulacao != atendidas$Tabulacao1)
atendidas$Data_hora <- strptime(atendidas$Data_hora, format = "%Y-%m-%d %H:%M:%S")

atendidas_converteu <- atendidas_mudou_tabulacao %>%
  filter(atendidas_mudou_tabulacao$Tabulacao == "ROBO - TRANSFERINDO PARA ANALISTA")                                                      

### Gráficos

# Chamadas atendidas agrupadas por sexo
atendidas[complete.cases(atendidas$Sexo),] %>%
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))+
  xlab("Sexo")+
  ylab("Quantidade")+
  ggtitle("Sexo do Lead")

# Chamadas atendidas por hora
atendidas %>%
  ggplot()+
  geom_bar(aes(x = Data_hora$hour, fill = ..x..))+
  ggtitle("Chamadas atendidas por Hora")+
  xlab("Hora")+
  ylab("Quantidade")+
  theme_light()

# Chamadas atendidas por dia da semana
atendidas %>%
  ggplot()+
  geom_bar(aes(x = Data_hora$wday, fill = ..x..))+
  theme_light()+
  ggtitle("Chamadas atendidas por dia da semana")+
  xlab("Hora")+
  ylab("Quantidade")

# Chamadas atendidas por idade
atendidas[complete.cases(atendidas$Idade),] %>%
  ggplot()+
  geom_bar(aes(x = Idade))+
  theme_light()+
  ggtitle("Chamadas atendidas por Idade")+
  xlab("Idade")+
  ylab("Quantidade")


# Chamadas atendidas por Tabulação
atendidas %>%
  ggplot()+
  geom_bar(aes(x = Tabulacao1,fill = Tabulacao1))+
  geom_bar(aes(Tabulacao, alpha = 0.7, color = "red"))

atendidas %>%
  ggplot()+
  geom_bar(aes(Tabulacao1, fill = Tabulacao1))


# Clientes por Sexo, Idade
clientes[complete.cases(clientes$Sexo),] %>%
  ggplot()+
  geom_bar(aes(x = Sexo, fill = Sexo))+
  xlab("Sexo")+
  ylab("Quantidade")+
  ggtitle("Sexo do Lead") 

clientes %>%
  ggplot()+
  geom_bar(aes(x = Idade, fill = Idade))+
  xlab("Idade")+
  ylab("Quantidade")+
  ggtitle("Sexo do Lead") 

# Mudanças de tabulacao 
atendidas_mudou_tabulacao %>%
  ggplot()+
  geom_bar(aes(x = Tabulacao1, fill = Tabulacao1))+
  geom_bar(aes(x = Tabulacao, alpha = 0.2))+
  xlab("Sexo")+
  ylab("Quantidade")+
  ggtitle("Sexo do Lead") 

filter(atendidas_mudou_tabulacao,
       atendidas_mudou_tabulacao$Tabulacao == "ROBO - TRANSFERINDO PARA ANALISTA") %>%
  ggplot()+
  geom_bar(aes(x = Tabulacao1, fill = Tabulacao1
    ))+
  xlab("Tabulação Anterior")+
  ylab("Quantidade")+
  ggtitle("Origem do Lead") 

# Cliente fixo e celular
sucesso_celular_fixo %>% 
  ggplot()+
  geom_bar(aes(summarise(sucesso_celular_fixo,
                             celular = sum(sucesso_celular_fixo$Celular),
                             Fixo = sum(sucesso_celular_fixo$Fixo))))
  
