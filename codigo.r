#########################
setwd("C:/Users/NT-CWB-0129/Downloads")
options(scipen = 999)

#INSTALANDO PACOTES
library(dplyr)
library(srvyr)
library(readr)
library(ggplot2)
library(tidyr)
library(Cairo)
library(scales)
library(ggrepel)
library(survey)
library(dplyr)
library(plm)
library(foreign)
library(tidyverse)
library(haven)
library(gt)
library(janitor)
library(apa)

### Carregando dataset ###

PNAD_COVID_062020 <- read_csv("PNAD_COVID_062020.csv", col_types = cols(.default = "d"))

PNAD_COVID_062020$count <- 1

PNAD_COVID_072020 <- read_csv("PNAD_COVID_072020.csv", col_types = cols(.default = "d"))
PNAD_COVID_072020$count <- 1

#JUNTANDO TODOS OS MESES EM UM ÚNICO DATA FRAME
PNAD_COVIDINCOMPLETO <-
bind_rows(PNAD_COVID_062020,PNAD_COVID_072020)

#trocando NA por 0
PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>% mutate_all(replace_na, 0)

#SOMA VALORES RECEBIDOS
PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>% mutate(soma_valores = C011A12+C011A22+D0013+D0023+D0033+D0043+D0053+D0063+D0073)

PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>% mutate(soma_rendimentos = C011A12+C011A22)

PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>% mutate(soma_beneficios = D0013+D0023+D0033+D0043+D0053+D0063+D0073)

#dummy se auxilio emergencial
PNAD_COVIDINCOMPLETO$AUX_EMERGENCIAL
PNAD_COVIDINCOMPLETO$AUX_EMERGENCIAL[PNAD_COVIDINCOMPLETO$D0051 == "2"] <- 0
PNAD_COVIDINCOMPLETO$AUX_EMERGENCIAL[PNAD_COVIDINCOMPLETO$D0051 == "1"] <- 1


#dummy se B.F
PNAD_COVIDINCOMPLETO$BF
PNAD_COVIDINCOMPLETO$BF[PNAD_COVIDINCOMPLETO$D0031 == "2"] <- 0
PNAD_COVIDINCOMPLETO$BF[PNAD_COVIDINCOMPLETO$D0031 == "1"] <- 1

#CRIANDO VARIAVAL INDEX (indentificacao unica)
PNAD_COVIDINCOMPLETO$INDEX = paste(PNAD_COVIDINCOMPLETO$V1013,
PNAD_COVIDINCOMPLETO$UPA, PNAD_COVIDINCOMPLETO$V1008, sep="_")
PNAD_COVIDINCOMPLETO$DOMICILIO=paste(PNAD_COVIDINCOMPLETO$UPA, PNAD_COVIDINCOMPLETO$V1008,sep="_")

#tabela com rendimento por domicilio
rendimentos_domicilio <- PNAD_COVIDINCOMPLETO %>% group_by(INDEX) %>% summarise(rendimento_domicilio = sum(soma_rendimentos))

#procv
PNAD_COVIDINCOMPLETO<- PNAD_COVIDINCOMPLETO %>% left_join(rendimentos_domicilio, by = 'INDEX')

PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>%
mutate(soma_valores_por_domicilio = soma_beneficios+rendimento_domicilio)

#ALUGUEL
PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>% mutate(CDRCALUGUEL = F0021/soma_valores_por_domicilio)

#>%30renda
PNAD_COVIDINCOMPLETO$pobreza_aluguel[PNAD_COVIDINCOMPLETO$CDRCALUGUEL < 0.3] <- 1 #compromete menos que 30%


PNAD_COVIDINCOMPLETO$pobreza_aluguel[PNAD_COVIDINCOMPLETO$CDRCALUGUEL >= 0.3] <- 0 #compromete mais que 30%

#rendimentos(renda) + auxilio emergencial por domicilio
PNAD_COVIDINCOMPLETO$RENDIMENTOEAUX_DOMICILIO <- PNAD_COVIDINCOMPLETO$rendimento_domicilio+PNAD_COVIDINCOMPLETO$D0053

#rendimentos totais sem auxlilio emergencial
PNAD_COVIDINCOMPLETO$RENDIMENTO_SEM_AUXEMERG <- PNAD_COVIDINCOMPLETO$soma_valores_por_domicilio-PNAD_COVIDINCOMPLETO$D0053

###POBREZA R$450; EXTREMA POBREZA R$155

#RENDA POR DOMICILIO

#tabela com habitantes por domicilio
pessoas <- PNAD_COVIDINCOMPLETO %>% group_by(INDEX) %>% summarise(pessoas= sum(count))

#procv
PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>%left_join(pessoas, by = 'INDEX')

#RECALCULAR TUDO DIVIDINDO POR PESSOAS

PNAD_COVIDINCOMPLETO$PP_RENDIMENTO_SEM_AUXEMERG<-PNAD_COVIDINCOMPLETO$RENDIMENTO_SEM_AUXEMERG/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_RENDIMENTOEAUX_DOMICILIO<-PNAD_COVIDINCOMPLETO$RENDIMENTOEAUX_DOMICILIO/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio<-PNAD_COVIDINCOMPLETO$soma_valores_por_domicilio/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_rendimento_domicilio<-PNAD_COVIDINCOMPLETO$rendimento_domicilio/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_APOSEN_PENSAO<-PNAD_COVIDINCOMPLETO$D0013/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_PENSAOALIMEN<-PNAD_COVIDINCOMPLETO$D0023/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_BF<-PNAD_COVIDINCOMPLETO$D0033/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_BPCLOAS<-PNAD_COVIDINCOMPLETO$D0043/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$PP_AUXEMERG<-PNAD_COVIDINCOMPLETO$D0053/PNAD_COVIDINCOMPLETO$pessoas

PNAD_COVIDINCOMPLETO$SEG_DESEMP<-PNAD_COVIDINCOMPLETO$D0063/PNAD_COVIDINCOMPLETO$pessoas


PNAD_COVIDINCOMPLETO$RENDA_RENDIMENTOS<-PNAD_COVIDINCOMPLETO$D0073/PNAD_COVIDINCOMPLETO$pessoas

###POBREZA R$450; EXTREMA POBREZA R$155

#pobreza (apenas renda)
PNAD_COVIDINCOMPLETO$pobreza_renda
PNAD_COVIDINCOMPLETO$pobreza_renda[PNAD_COVIDINCOMPLETO$PP_rendimento_domicilio < 450 ] <- 1
PNAD_COVIDINCOMPLETO$pobreza_renda[PNAD_COVIDINCOMPLETO$PP_rendimento_domicilio >= 450 ] <- 0

#pobreza rendimentos totais (com auxilios e etc)
PNAD_COVIDINCOMPLETO$pobreza_rend_totais
PNAD_COVIDINCOMPLETO$pobreza_rend_totais[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio < 450 ] <- 1
PNAD_COVIDINCOMPLETO$pobreza_rend_totais[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio >= 450 ] <- 0

PNAD_COVIDINCOMPLETO$pobreza_rend_totais_inverso
PNAD_COVIDINCOMPLETO$pobreza_rend_totais_inverso[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio < 450 ] <- 0
PNAD_COVIDINCOMPLETO$pobreza_rend_totais_inverso[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio >= 450 ] <- 1

#pobreza rendimentos totais(se n tivesse o auxilio emergencial)
PNAD_COVIDINCOMPLETO$pobreza_rend_totais_sem_auxemerg
PNAD_COVIDINCOMPLETO$pobreza_rend_totais_sem_auxemerg[PNAD_COVIDINCOMPLETO$PP_RENDIMENTO_SEM_AUXEMERG < 450 ] <- 1
PNAD_COVIDINCOMPLETO$pobreza_rend_totais_sem_auxemerg[PNAD_COVIDINCOMPLETO$PP_RENDIMENTO_SEM_AUXEMERG >= 450 ] <- 0

#pobreza extrema (apenas renda)
PNAD_COVIDINCOMPLETO$pobrezaext_renda
PNAD_COVIDINCOMPLETO$pobrezaext_renda[PNAD_COVIDINCOMPLETO$PP_rendimento_domicilio < 155 ] <- 1
PNAD_COVIDINCOMPLETO$pobrezaext_renda[PNAD_COVIDINCOMPLETO$PP_rendimento_domicilio >= 155 ] <- 0

#pobreza extrema rendimentos totais (com auxilios e etc)
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio < 155 ] <- 1
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio >= 155 ] <- 0

PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais_inverso
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais_inverso[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio < 155 ] <- 0
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais_inverso[PNAD_COVIDINCOMPLETO$PP_soma_valores_por_domicilio >= 155 ] <- 1

#pobreza extrema rendimentos totais(se n tivesse o auxilio emergencial)
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais_sem_auxemerg
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais_sem_auxemerg[PNAD_COVIDINCOMPLETO$PP_RENDIMENTO_SEM_AUXEMERG < 155 ] <- 1
PNAD_COVIDINCOMPLETO$pobrezaext_rend_totais_sem_auxemerg[PNAD_COVIDINCOMPLETO$PP_RENDIMENTO_SEM_AUXEMERG >= 155 ] <- 0

#Tem carteria de trabalho assinada ou é funcionário público estatutário?
PNAD_COVIDINCOMPLETO$carteira_assinada
PNAD_COVIDINCOMPLETO$carteira_assinada[PNAD_COVIDINCOMPLETO$C007B == 1 ] <- 1
PNAD_COVIDINCOMPLETO$carteira_assinada[PNAD_COVIDINCOMPLETO$C007B == 2 ] <- 1

PNAD_COVIDINCOMPLETO$carteira_assinada[PNAD_COVIDINCOMPLETO$C007B == 3 ] <- 0 #naotem
PNAD_COVIDINCOMPLETO$carteira_assinada[PNAD_COVIDINCOMPLETO$C007B == 0 ] <- 0 #naotem

tabela_carteira_assinada <- PNAD_COVIDINCOMPLETO %>%group_by(INDEX) %>% summarise(teste_carteira_assinada = mean(carteira_assinada))

#procv
PNAD_COVIDINCOMPLETO<- PNAD_COVIDINCOMPLETO %>%left_join(tabela_carteira_assinada, by = 'INDEX')

PNAD_COVIDINCOMPLETO$PP_carteira_assinada[PNAD_COVIDINCOMPLETO$teste_carteira_assinada==0 ] <- 0
PNAD_COVIDINCOMPLETO$PP_carteira_assinada[PNAD_COVIDINCOMPLETO$teste_carteira_assinada!=0 ] <- 1 #TEM CARTEIRA

#É funcionário público estatutário?
PNAD_COVIDINCOMPLETO$funcionario_pub
PNAD_COVIDINCOMPLETO$funcionario_pub[PNAD_COVIDINCOMPLETO$C007B== 2 ] <- 1
PNAD_COVIDINCOMPLETO$funcionario_pub[PNAD_COVIDINCOMPLETO$C007B!= 2 ] <- 0 #naoé

tabela_funcionario_pub <- PNAD_COVIDINCOMPLETO %>%group_by(INDEX) %>%summarise(teste_funcionario_pub = mean(funcionario_pub))

#procv
PNAD_COVIDINCOMPLETO<- PNAD_COVIDINCOMPLETO %>%left_join(tabela_funcionario_pub, by = 'INDEX')

PNAD_COVIDINCOMPLETO$PP_funcionario_pub[PNAD_COVIDINCOMPLETO$teste_funcionario_pub==0 ] <- 0 #naoé
PNAD_COVIDINCOMPLETO$PP_funcionario_pub[PNAD_COVIDINCOMPLETO$teste_funcionario_pub!=0 ] <- 1 #éfuncionariopubl

#Tem algum plano de saúde médico, seja particular, de empresa ou de órgão
público
PNAD_COVIDINCOMPLETO$plano_saude
PNAD_COVIDINCOMPLETO$plano_saude[PNAD_COVIDINCOMPLETO$B007 ==1 ] <- 1
PNAD_COVIDINCOMPLETO$plano_saude[PNAD_COVIDINCOMPLETO$B007 ==2 ] <- 0 #nao tem
PNAD_COVIDINCOMPLETO$plano_saude[PNAD_COVIDINCOMPLETO$B007 ==9 ] <- 0 #nao tem

tabela_plano_saude<- PNAD_COVIDINCOMPLETO %>%group_by(INDEX) %>%summarise(teste_plano_saude = mean(plano_saude))

#procv
PNAD_COVIDINCOMPLETO<- PNAD_COVIDINCOMPLETO %>%left_join(tabela_plano_saude, by = 'INDEX')

PNAD_COVIDINCOMPLETO$PP_plano_saude[PNAD_COVIDINCOMPLETO$teste_plano_saude==0 ] <- 0 #nao tem plano
PNAD_COVIDINCOMPLETO$PP_plano_saude[PNAD_COVIDINCOMPLETO$teste_plano_saude!=0 ] <- 1 #tem plano de saude

#domicilio rural ou urbano
PNAD_COVIDINCOMPLETO$urbana
PNAD_COVIDINCOMPLETO$urbana[PNAD_COVIDINCOMPLETO$V1022==1 ] <-1 #urbana

PNAD_COVIDINCOMPLETO$urbana[PNAD_COVIDINCOMPLETO$V1022==2 ] <-0 #rural

#ensino superior individual
PNAD_COVIDINCOMPLETO$ens_superior_ind
PNAD_COVIDINCOMPLETO$ens_superior_ind[PNAD_COVIDINCOMPLETO$A005> 6 ] <- 0
PNAD_COVIDINCOMPLETO$ens_superior_ind[PNAD_COVIDINCOMPLETO$A005<=6 ] <- 1 #sim

#tabela com ensino superior individual
tabela_ens_sup_ind <- PNAD_COVIDINCOMPLETO %>%group_by(INDEX) %>%summarise(teste_ens_superior_dom = mean(ens_superior_ind))

#procv
PNAD_COVIDINCOMPLETO<- PNAD_COVIDINCOMPLETO %>%left_join(tabela_ens_sup_ind, by = 'INDEX')

PNAD_COVIDINCOMPLETO$ens_superior_dom[PNAD_COVIDINCOMPLETO$teste_ens_superior_dom==0 ] <- 0
PNAD_COVIDINCOMPLETO$ens_superior_dom[PNAD_COVIDINCOMPLETO$teste_ens_superior_dom!=0 ] <- 1 #tem

#domicilio proprio?
PNAD_COVIDINCOMPLETO$dom_proprio
PNAD_COVIDINCOMPLETO$dom_proprio[PNAD_COVIDINCOMPLETO$F001>2 ]<- 0 #n proprio
PNAD_COVIDINCOMPLETO$dom_proprio[PNAD_COVIDINCOMPLETO$F001<=2 ]<- 1 #proprio

#AUXILIO EMERGENCIAL: VARIAVEL SE RECEBEU NO 5 E NAO RECEBEU NO 6

t1 <- PNAD_COVIDINCOMPLETO %>%distinct (INDEX,.keep_all=TRUE)

tt <- data.frame(V1013 = t1$V1013,INDEX = t1$INDEX,DOMICILIO = t1$DOMICILIO,AUX_EMERGENCIAL = t1$AUX_EMERGENCIAL)

tt$nova[tt$V1013==6 & tt$AUX_EMERGENCIAL==0] <- 40
tt$nova[tt$V1013==7 & tt$AUX_EMERGENCIAL==1] <- 60
tt$nova[tt$V1013==6 & tt$AUX_EMERGENCIAL==1] <- 3
tt$nova[tt$V1013==7 & tt$AUX_EMERGENCIAL==0] <- 4

tt_hehe <- tt %>% group_by(DOMICILIO) %>% summarise(FOCO = sum(nova))

#procv
tt<- tt %>% left_join(tt_hehe, by = 'DOMICILIO')

finalmente <- data.frame(INDEX = tt$INDEX, FOCO = tt$FOCO)

PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>% left_join(finalmente, by = 'INDEX')

PNAD_COVIDINCOMPLETO


PNAD_COVIDINCOMPLETO$ROUND[PNAD_COVIDINCOMPLETO$V1013==6 ] <-0
PNAD_COVIDINCOMPLETO$ROUND[PNAD_COVIDINCOMPLETO$V1013==7 ] <-1

PNAD_COVIDINCOMPLETO
PNAD_COVIDINCOMPLETO$RECEBEU[PNAD_COVIDINCOMPLETO$FOCO!=100 ] <- 0
PNAD_COVIDINCOMPLETO$RECEBEU[PNAD_COVIDINCOMPLETO$FOCO==100 ] <- 1

#BOLSA FAMILIA: VARIAVEL SE RECEBEU NO 5 E NAO RECEBEU NO 6

f1 <- PNAD_COVIDINCOMPLETO %>% distinct (INDEX,.keep_all=TRUE)

ff <- data.frame(V1013 = f1$V1013, INDEX = f1$INDEX, DOMICILIO = f1$DOMICILIO, BF = f1$BF)

ff$nova[ff$V1013==6 & ff$BF==0] <- 40
ff$nova[ff$V1013==7 & ff$BF==1] <- 60
ff$nova[ff$V1013==6 & ff$BF==1] <- 3
ff$nova[ff$V1013==7 & ff$BF==0] <- 4

ff_hehe <- ff %>% group_by(DOMICILIO) %>% summarise(FOCO = sum(nova))

#procv
ff<- ff %>%

left_join(ff_hehe, by = 'DOMICILIO')

fifi <- data.frame(INDEX = ff$INDEX, FOCO_BF = ff$FOCO)

PNAD_COVIDINCOMPLETO <- PNAD_COVIDINCOMPLETO %>% left_join(fifi, by = 'INDEX')

PNAD_COVIDINCOMPLETO
PNAD_COVIDINCOMPLETO$RECEBEU_BF[PNAD_COVIDINCOMPLETO$FOCO_BF!=100 ] <- 0
PNAD_COVIDINCOMPLETO$RECEBEU_BF[PNAD_COVIDINCOMPLETO$FOCO_BF==100 ] <- 1

PNAD_COVIDINCOMPLETO$UF <- as.factor(PNAD_COVIDINCOMPLETO$UF)PNAD_COVIDINCOMPLETO$A003 <- as.factor(PNAD_COVIDINCOMPLETO$A003)
PNAD_COVIDINCOMPLETO$A001A <- as.factor(PNAD_COVIDINCOMPLETO$A001A)
PNAD_COVIDINCOMPLETO$A004 <- as.factor(PNAD_COVIDINCOMPLETO$A004)
PNAD_COVIDINCOMPLETO$A005 <- as.factor(PNAD_COVIDINCOMPLETO$A005)
PNAD_COVIDINCOMPLETO$D0011 <- as.factor(PNAD_COVIDINCOMPLETO$D0011)
PNAD_COVIDINCOMPLETO$D0021 <- as.factor(PNAD_COVIDINCOMPLETO$D0021)
PNAD_COVIDINCOMPLETO$D0031 <- as.factor(PNAD_COVIDINCOMPLETO$D0031)
PNAD_COVIDINCOMPLETO$D0041 <- as.factor(PNAD_COVIDINCOMPLETO$D0041)
PNAD_COVIDINCOMPLETO$D0061 <- as.factor(PNAD_COVIDINCOMPLETO$D0061)
PNAD_COVIDINCOMPLETO$D0071 <- as.factor(PNAD_COVIDINCOMPLETO$D0071)

PNAD_COVIDINCOMPLETO$D0031 <- as.factor(PNAD_COVIDINCOMPLETO$D0031)
PNAD_COVIDINCOMPLETO$RECEBEU <- as.factor(PNAD_COVIDINCOMPLETO$RECEBEU)

#CRIANDO DADOS ESTRUTURADOS E CALIBRADOS
PNAD_COVID_PESOS <- PNAD_COVIDINCOMPLETO %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)

#SOMENTE COM QUEM N RECEBEU NOS 2 PERIODOS E QUEM SÓ RECEBEU NO SEXTO
PNAD_COVID_PESOS_3 <- PNAD_COVID_PESOS %>% filter(FOCO==100 |FOCO==44)

#PNAD_COVID_PESOS
O <- svymean(~PP_soma_valores_por_domicilio, subset(PNAD_COVID_PESOS, V1013 == 6 & RECEBEU == 0) , na.rm = T)
N <- svymean(~PP_soma_valores_por_domicilio, subset(PNAD_COVID_PESOS, V1013 == 7 & RECEBEU == 0) , na.rm = T)
M <- svymean(~PP_soma_valores_por_domicilio, subset(PNAD_COVID_PESOS, V1013 == 6 & RECEBEU == 1) , na.rm = T)
L <- svymean(~PP_soma_valores_por_domicilio, subset(PNAD_COVID_PESOS, V1013 == 7 & RECEBEU == 1) , na.rm = T)

O
N
M
L

#realizando o dif/dif

(L-M)
(N-O)
(L-M)-(N-O)

# a renda sem auxilio caiu?
svymean(~PP_rendimento_domicilio, subset(PNAD_COVID_PESOS, V1013 == 6 & RECEBEU == 0) , na.rm = T)
svymean(~PP_rendimento_domicilio, subset(PNAD_COVID_PESOS, V1013 == 7 & RECEBEU == 0) , na.rm = T)
svymean(~PP_rendimento_domicilio, subset(PNAD_COVID_PESOS, V1013 == 6 & RECEBEU == 1) , na.rm = T)
svymean(~PP_rendimento_domicilio, subset(PNAD_COVID_PESOS, V1013 == 7 & RECEBEU == 1) , na.rm = T)

svymean(~PP_RENDIMENTO_SEM_AUXEMERG, subset(PNAD_COVID_PESOS, V1013 == 6 & RECEBEU == 0) , na.rm = T)
svymean(~PP_RENDIMENTO_SEM_AUXEMERG, subset(PNAD_COVID_PESOS, V1013 == 7 & RECEBEU == 0) , na.rm = T)
svymean(~PP_RENDIMENTO_SEM_AUXEMERG, subset(PNAD_COVID_PESOS, V1013 == 6 & RECEBEU == 1) , na.rm = T)
svymean(~PP_RENDIMENTO_SEM_AUXEMERG, subset(PNAD_COVID_PESOS, V1013 == 7 & RECEBEU == 1) , na.rm = T)

O1 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0 & pobreza_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0))
N1 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0 & pobreza_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0))
M1 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1 & pobreza_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1))
L1 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1 & pobreza_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1))

O1
N1
M1

L1

#realizando cálculo de dif/dif manualmente,

LLMM <- (L1/M1-1)*100
NNOO <- (N1/O1-1)*100
LLMM - NNOO

O11 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0 & pobrezaext_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0))
N11 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0 & pobrezaext_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0))
M11 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1 & pobrezaext_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1))
L11 <- svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1 & pobrezaext_rend_totais==1))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1))

O11
N11
M11
L11

#pobreza aluguel
svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0 & pobreza_aluguel==0))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0))
svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0 & pobreza_aluguel==0))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0))

svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1 & pobreza_aluguel==0))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1))
svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1 & pobreza_aluguel==0))/svytotal(~ count, PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1))

#doente ficou em casa
PNAD_COVIDINCOMPLETO$B0011[PNAD_COVIDINCOMPLETO$B0011!=1] <- 0
PNAD_COVIDINCOMPLETO$B0012[PNAD_COVIDINCOMPLETO$B0012!=1] <- 0
PNAD_COVIDINCOMPLETO$B0013[PNAD_COVIDINCOMPLETO$B0013!=1] <- 0
PNAD_COVIDINCOMPLETO$B0014[PNAD_COVIDINCOMPLETO$B0014!=1] <- 0
PNAD_COVIDINCOMPLETO$B0015[PNAD_COVIDINCOMPLETO$B0015!=1] <- 0
PNAD_COVIDINCOMPLETO$B0016[PNAD_COVIDINCOMPLETO$B0016!=1] <- 0
PNAD_COVIDINCOMPLETO$B0017[PNAD_COVIDINCOMPLETO$B0017!=1] <- 0
PNAD_COVIDINCOMPLETO$B0018[PNAD_COVIDINCOMPLETO$B0018!=1] <- 0
PNAD_COVIDINCOMPLETO$B0019[PNAD_COVIDINCOMPLETO$B0019!=1] <- 0
PNAD_COVIDINCOMPLETO$B00110[PNAD_COVIDINCOMPLETO$B00110!=1] <- 0
PNAD_COVIDINCOMPLETO$B00111[PNAD_COVIDINCOMPLETO$B00111!=1] <- 0
PNAD_COVIDINCOMPLETO$B00112[PNAD_COVIDINCOMPLETO$B00112!=1] <- 0

PNAD_COVIDINCOMPLETO$SOMA_SINTOMAS <- PNAD_COVIDINCOMPLETO$B0011 + PNAD_COVIDINCOMPLETO$B0012 + PNAD_COVIDINCOMPLETO$B0013 + PNAD_COVIDINCOMPLETO$B0014 + PNAD_COVIDINCOMPLETO$B0015 + PNAD_COVIDINCOMPLETO$B0016 + PNAD_COVIDINCOMPLETO$B0017 + PNAD_COVIDINCOMPLETO$B0018 + PNAD_COVIDINCOMPLETO$B0019 + PNAD_COVIDINCOMPLETO$B00110 + PNAD_COVIDINCOMPLETO$B00111 + PNAD_COVIDINCOMPLETO$B00112

PNAD_COVIDINCOMPLETO$SINTOMAS[PNAD_COVIDINCOMPLETO$SOMA_SINTOMAS == 0] <- 0
PNAD_COVIDINCOMPLETO$SINTOMAS[PNAD_COVIDINCOMPLETO$SOMA_SINTOMAS != 0] <- 1

#CRIANDO DADOS ESTRUTURADOS E CALIBRADOS
PNAD_COVID_PESOS <- PNAD_COVIDINCOMPLETO %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)

#doente ficou em casa
svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & B0031==1 & RECEBEU==0 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0 & SINTOMAS==1 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & B0031==1 & RECEBEU==0 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0 & SINTOMAS==1 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & B0031==1 & RECEBEU==1 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1 & SINTOMAS==1 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & B0031==1 & RECEBEU==1 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1 & SINTOMAS==1 ))

#doentes/populacao
svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==0))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1 & SINTOMAS==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1 ))

#pobres ficaram + doentes?
svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & SINTOMAS==1 & pobreza_rend_totais==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & pobreza_rend_totais==1))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & SINTOMAS==1 & pobreza_rend_totais==1)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & pobreza_rend_totais==1))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & SINTOMAS==1 & pobreza_rend_totais==0 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & pobreza_rend_totais==0 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & SINTOMAS==1 & pobreza_rend_totais==0)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & pobreza_rend_totais==0 ))

# EXTREMOS pobres ficaram + doentes?
svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & SINTOMAS==1 & pobrezaext_rend_totais==1 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & pobrezaext_rend_totais==1))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & SINTOMAS==1 & pobrezaext_rend_totais==1)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & pobrezaext_rend_totais==1))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & SINTOMAS==1 & pobrezaext_rend_totais==0 )) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & pobrezaext_rend_totais==0 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & SINTOMAS==1 & pobrezaext_rend_totais==0)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & pobrezaext_rend_totais==0 ))

#afastado por isolamento
PNAD_COVIDINCOMPLETO$ISOLAMENTO[PNAD_COVIDINCOMPLETO$C003!=1] <- 0
PNAD_COVIDINCOMPLETO$ISOLAMENTO[PNAD_COVIDINCOMPLETO$C003==1] <- 1

#CRIANDO DADOS ESTRUTURADOS E CALIBRADOS
PNAD_COVID_PESOS <- PNAD_COVIDINCOMPLETO %>% as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE)

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & ISOLAMENTO==1 & RECEBEU== 0)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU== 0 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & ISOLAMENTO==1 & RECEBEU== 0)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==0 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & ISOLAMENTO==1 & RECEBEU== 1)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==6 & RECEBEU==1 ))

svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & ISOLAMENTO==1 & RECEBEU== 1)) / svytotal(~ count,PNAD_COVID_PESOS %>% filter(V1013==7 & RECEBEU==1 ))

#renda
modelinhu <- svyglm(PP_soma_valores_por_domicilio ~ RECEBEU + ROUND + RECEBEU*ROUND + RECEBEU_BF + RECEBEU_BF*ROUND, PNAD_COVID_PESOS)
summary(modelinhu)

modelo <- svyglm(pobreza_rend_totais_inverso ~ RECEBEU + ROUND + RECEBEU*ROUND + RECEBEU_BF + RECEBEU_BF*ROUND, PNAD_COVID_PESOS, family="binomial")
summary(modelo)
confint(modelo)
exp(modelo$coefficients)

modelo3 <- svyglm(pobrezaext_rend_totais_inverso ~ RECEBEU + ROUND + RECEBEU*ROUND + RECEBEU_BF + RECEBEU_BF*ROUND, PNAD_COVID_PESOS, family="binomial")
summary(modelo3)
confint(modelo3)
exp(modelo3$coefficients)

#pobreza aluguel
modeloz <- svyglm(pobreza_aluguel ~ RECEBEU + ROUND + RECEBEU*ROUND + RECEBEU_BF + RECEBEU_BF*ROUND, PNAD_COVID_PESOS, family="binomial")
summary(modeloz)
confint(modeloz)
exp(modeloz$coefficients)

#ISOLAMENTO
modelinho <- svyglm(ISOLAMENTO ~ RECEBEU + ROUND + RECEBEU*ROUND + RECEBEU_BF + RECEBEU_BF*ROUND, PNAD_COVID_PESOS, family="binomial")
summary(modelinho)
confint(modelinho)
exp(modelinho$coefficients)
