require(vctrs)
require(lmerTest)
require(lme4)
require(foreign)
require(haven)
require(MASS)
require(dplyr)
require(ggplot2)
require(gtools)
require(gridExtra)
require(car)
require(readxl)
require(openxlsx)



# BARÓMETRO DAS AMÉRICAS:

# Definir diretório:
setwd("C:/Users/Duda/Desktop/DissertacaoCods")

# Carregar bases:
lapop <- read_dta("Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")
lapop17 <- read_dta("780314464Brazil LAPOP AmericasBarometer 2017 V1.0_W.dta")
lapop14 <- read_dta("636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")
lapop12 <- read_dta("54861031Brazil LAPOP AmericasBarometer 2012 Rev1_W.dta")
lapop10 <- read_dta("7948266051039660950Brazil_LAPOP_AmericasBarometer 2010 data set  approved v4.dta")
lapop08 <- read_dta("30541815brazil_lapop_dims_2008_final_data_set_v10.dta")
lapop07 <- read_dta("2138048899brazil_lapop_dims final 2007 v5.dta")

---

# Somar colunas "interesse":
lapop$pol1[is.na(lapop$pol1)] <- 0
lapop$pol1exp[is.na(lapop$pol1exp)] <- 0
lapop$interesse <- lapop$pol1 + lapop$pol1exp

lapopsel <- data.frame("partprot" = lapop$prot3,
                       "idade" = lapop$q2,
                       "educacao" = lapop$ed,
                       "renda" = lapop$q10new,             
                       "apoiopartido" = lapop$vb10,            
                       "confiancaeleicoes" = lapop$b47a,
                       "confiancamidia" = lapop$b37,
                       "efext" = lapop$eff1,
                       "efint" = lapop$eff2,
                       "interesse" = lapop$interesse,
                       "votoucandidato" = lapop$vb3n,
                       "votou" = lapop$vb2,
                       "freqnoticias" = lapop$gi0n,
                       "freqnotFB" = lapop$smedia3,
                       "freqnotTwitter" = lapop$smedia6,
                       "freqnotWhatsapp" = lapop$smedia9)

lapopsel["ano"] <- 2019

# 2008:

lapop08$prot1 # participou de protesto
lapop08$prot2 # participou de protesto no ANO
lapop08$q2 # idade
lapop08$ed # Educação
lapop08$q10 # Renda
lapop08$vb10 # Simpatiza partido
lapop08$b47 # Confinça nas eleições
lapop08$b37 # Confinça nos meios de comunicação 
lapop08$eff1 # Eficácia externa
lapop08$eff2 # Eficácia interna
lapop08$pol1 # Interesse em política
lapop08$vb3 # Votou 1o turno
lapop08$a1 # Freq noticias rádio
lapop08$a2 # Freq noticias tv
lapop08$a3 # Freq noticias jornal
lapop08$a4i # Freq noticias internet

lapopsel08 <- data.frame("partprot" = lapop08$prot2,
                         "idade" = lapop08$q2,
                         "educacao" = lapop08$ed,
                         "renda" = lapop08$q10,             
                         "apoiopartido" = lapop08$vb10,            
                         "confiancaeleicoes" = lapop08$b47,
                         "confiancamidia" = lapop08$b37,
                         "efext" = lapop08$eff1,
                         "efint" = lapop08$eff2,
                         "interesse" = lapop08$pol1,
                         "votoucandidato" = lapop08$vb3,
                         "votou" = lapop08$vb2,
                         "freqnoticiasradio" = lapop08$a1,
                         "freqnoticiastv" = lapop08$a2,
                         "freqnoticiasjornal" = lapop08$a3,
                         "freqnoticiasnet" = lapop08$a4i)
lapopsel08["ano"] <- 2008

# 2010:

lapop10$prot3 # participou de protesto no ANO
lapop10$q2 # idade
lapop10$ed # Educação
lapop10$q10 # Renda
lapop10$vb10 # Simpatiza partido
lapop10$b47 # Confinça nas eleições
lapop10$b37 # Confinça nos meios de comunicação 
lapop10$eff1 # Eficácia externa
lapop10$eff2 # Eficácia interna
lapop10$pol1 # Interesse em política
lapop10$vb3 # Votou 1o turno
lapop10$gl0 # Freq noticias

lapopsel10 <- data.frame("partprot" = lapop10$prot3,
           "idade" = lapop10$q2,
           "educacao" = lapop10$ed,
           "renda" = lapop10$q10,             
           "apoiopartido" = lapop10$vb10,            
           "confiancaeleicoes" = lapop10$b47,
           "confiancamidia" = lapop10$b37,
           "efext" = lapop10$eff1,
           "efint" = lapop10$eff2,
           "interesse" = lapop10$pol1,
           "votoucandidato" = lapop10$vb3,
           "votou" = lapop10$vb2,
           "freqnoticias" = lapop10$gl0)

lapopsel10["ano"] <- 2010


# 2012:

lapop12$prot3 # participou de protesto no ANO
lapop12$q2 # idade
lapop12$ed # Educação (escala diferente)
lapop12$q10new # Renda
lapop12$vb10 # Simpatiza partido
lapop12$b47a # Confinça nas eleições
lapop12$b37 # Confinça nos meios de comunicação 
lapop12$eff1 # Eficácia externa
lapop12$eff2 # Eficácia interna
lapop12$pol1 # Interesse em política
lapop12$vb3 # Votou 1o turno
lapop12$gi0 # Freq noticias

lapopsel12 <- data.frame("partprot" = lapop12$prot3,
                         "idade" = lapop12$q2,
                         "educacao" = lapop12$ed,
                         "renda" = lapop12$q10new,             
                         "apoiopartido" = lapop12$vb10,            
                         "confiancaeleicoes" = lapop12$b47a,
                         "confiancamidia" = lapop12$b37,
                         "efext" = lapop12$eff1,
                         "efint" = lapop12$eff2,
                         "interesse" = lapop12$pol1,
                         "votoucandidato" = lapop12$vb3,
                         "votou" = lapop12$vb2,
                         "freqnoticias" = lapop12$gi0)

lapopsel12["ano"] <- 2012
count(lapopsel12, votoucandidato)

# 2014:
lapop14$prot3 # participou de protesto no ANO
lapop14$q2 # idade
lapop14$ed # Educação
lapop14$q10new # Renda
lapop14$vb10 # Simpatiza partido
lapop14$b47a # Confinça nas eleições
lapop14$eff1 # Eficácia externa
lapop14$eff2 # Eficácia interna
lapop14$pol1 # Interesse em política
lapop14$vb2 # Votou em 2010
lapop14$vb3n # Votou em candidato no 1o turno
lapop14$gi0 # Freq noticias

lapopsel14 <- data.frame("partprot" = lapop14$prot3,
                         "idade" = lapop14$q2,
                         "educacao" = lapop14$ed,
                         "renda" = lapop14$q10new,             
                         "apoiopartido" = lapop14$vb10,            
                         "confiancaeleicoes" = lapop14$b47a,
                         "efext" = lapop14$eff1,
                         "efint" = lapop14$eff2,
                         "interesse" = lapop14$pol1,
                         "votou" = lapop14$vb2,
                         "votoucandidato" = lapop14$vb3n,
                         "freqnoticias" = lapop14$gi0)
lapopsel14["ano"] <- 2014

# 2017:
lapop17$prot3 # participou de protesto no ANO
lapop17$q2 # idade
lapop17$ed # Educação
lapop17$q10new # Renda
lapop17$vb10 # Simpatiza partido
lapop17$b47a # Confinça nas eleições
lapop17$b37 # Confinça nos meios de comunicação 
lapop17$eff1 # Eficácia externa
lapop17$eff2 # Eficácia interna
lapop17$pol1 # Interesse em política
lapop17$vb2 # Votou em 2010
lapop17$vb3n # Votou em candidato no 1o turno
lapop17$gi0 # Freq noticias

lapopsel17 <- data.frame("partprot" = lapop17$prot3,
                         "idade" = lapop17$q2,
                         "educacao" = lapop17$ed,
                         "renda" = lapop17$q10new,             
                         "apoiopartido" = lapop17$vb10,            
                         "confiancaeleicoes" = lapop17$b47a,
                         "confiancamidia" = lapop17$b37,
                         "efext" = lapop17$eff1,
                         "efint" = lapop17$eff2,
                         "interesse" = lapop17$pol1,
                         "votou" = lapop17$vb2,
                         "votoucandidato" = lapop17$vb3n,
                         "freqnoticias" = lapop17$gi0)

lapopsel17["ano"] <- 2017

---

remove_labels(lapopsel)
remove_labels(lapopsel08)
remove_labels(lapopsel10)
remove_labels(lapopsel12)
remove_labels(lapopsel14)
remove_labels(lapopsel17)

# Base LAPOP com anos agregados(2008-2019)
lapopagreg <- smartbind(lapopsel, lapopsel08, lapopsel10, lapopsel12, lapopsel14,
      lapopsel17)

# Recodificar variáveis necessárias:
lapopagreg$interesse <- dplyr::recode(lapopagreg$interesse, "1" = "4", "2" = "3",
                             "3" = "2", "4" = "1", "0" = "NA")  
count(lapopagreg, interesse)

lapopagreg$partprot <- dplyr::recode(lapopagreg$partprot, "1" = "1", "0" = "0", "2" = "0", "3" = "1", "NA" = "NA")  
count(lapopagreg, partprot)

lapopagreg$votoucandidato <- dplyr::recode(lapopagreg$votoucandidato, "0" = "0", 
                                           "97" = "0", "77" = "1577", "1501" = "1501", "1502" = "1502", "1503" = "1503",
                                           "1504" = "1504", "1505" = "1505", "1506" = "1506", "1507" = "1507",
                                           "1508" = "1508", "1509" = "1509", "1510" = "1510", "1577" = "1577", "1" = "1501",
                                           "2" = "1502", "3" = "1503", "5" = "1505", "10" = "1510")  
count(lapopagreg, votoucandidato)

# Criar variável binária para voto nulo/branco (1) em candidato (0):
candidatos <- c(1501:1510, 1577)
lapopagreg <- mutate(lapopagreg, votounulo = ifelse(votoucandidato %in% candidatos, "0",
                                                    ifelse(votoucandidato == 0, "1", "NA")))
count(lapopagreg, votounulo)

lapopagreg$freqnoticias <- dplyr::recode(lapopagreg$freqnoticias, "1" = "5", "2" = "4",
                                "3" = "3", "4" = "2", "5" = "1", "NA" = "NA")
count(lapopagreg, freqnoticias)

lapopagreg$freqnotFB <- dplyr::recode(lapopagreg$freqnotFB, "1" = "5", "2" = "4",
                             "3" = "3", "4" = "2", "5" = "1", "NA" = "NA")
count(lapopagreg, freqnotFB)

lapopagreg$freqnotTwitter <- dplyr::recode(lapopagreg$freqnotTwitter, "1" = "5", "2" = "4",
                                  "3" = "3", "4" = "2", "5" = "1", "NA" = "NA")
count(lapopagreg, freqnotTwitter)

lapopagreg$freqnotWhatsapp <- dplyr::recode(lapopagreg$freqnotWhatsapp, "1" = "5", "2" = "4",
                                   "3" = "3", "4" = "2", "5" = "1", "NA" = "NA")
count(lapopagreg, freqnotWhatsapp)

lapopagreg$freqnoticiasradio <- dplyr::recode(lapopagreg$freqnoticiasradio, "1" = "4", "2" = "3",
                                            "3" = "2", "4" = "1", "8" = "NA", "NA" = "NA")
count(lapopagreg, freqnoticiasradio)

lapopagreg$freqnoticiastv <- dplyr::recode(lapopagreg$freqnoticiastv, "1" = "4", "2" = "3",
                                              "3" = "2", "4" = "1", "8" = "NA", "NA" = "NA")
count(lapopagreg, freqnoticiastv)

lapopagreg$freqnoticiasjornal <- dplyr::recode(lapopagreg$freqnoticiasjornal, "1" = "4", "2" = "3",
                                           "3" = "2", "4" = "1", "8" = "NA", "NA" = "NA")
count(lapopagreg, freqnoticiasjornal)

lapopagreg$freqnoticiasnet <- dplyr::recode(lapopagreg$freqnoticiasnet, "1" = "4", "2" = "3",
                                               "3" = "2", "4" = "1", "8" = "NA", "NA" = "NA")
count(lapopagreg, freqnoticiasnet)

lapopagreg$apoiopartido <- dplyr::recode(lapopagreg$apoiopartido, "1" = "1", "2" = "0",
                                "NA" = "NA")
count(lapopagreg, apoiopartido)

lapopagreg$votou <- dplyr::recode(lapopagreg$votou, "1" = "1", "2" = "0",
                                         "NA" = "NA")

lapopagreg <- filter(lapopagreg, idade <=70)
lapopagreg <- filter(lapopagreg, idade >=18)


lapopagreg$interesse <- as.numeric(as.character(lapopagreg$interesse))
lapopagreg$votoucandidato <- as.numeric(as.character(lapopagreg$votoucandidato))
lapopagreg$votounulo <- as.numeric(as.character(lapopagreg$votounulo))
lapopagreg$votou <- as.numeric(as.character(lapopagreg$votou))
lapopagreg$freqnoticias <- as.numeric(as.character(lapopagreg$freqnoticias))
lapopagreg$freqnotFB <- as.numeric(as.character(lapopagreg$freqnotFB))
lapopagreg$freqnotTwitter <- as.numeric(as.character(lapopagreg$freqnotTwitter))
lapopagreg$freqnotWhatsapp <- as.numeric(as.character(lapopagreg$freqnotWhatsapp))
lapopagreg$freqnoticiasradio <- as.numeric(as.character(lapopagreg$freqnoticiasradio))
lapopagreg$freqnoticiastv <- as.numeric(as.character(lapopagreg$freqnoticiastv))
lapopagreg$freqnoticiasjornal <- as.numeric(as.character(lapopagreg$freqnoticiasjornal))
lapopagreg$freqnoticiasnet <- as.numeric(as.character(lapopagreg$freqnoticiasnet))
lapopagreg$apoiopartido <- as.numeric(as.character(lapopagreg$apoiopartido))
lapopagreg$partprot <- as.numeric(as.character(lapopagreg$partprot))
lapopagreg$idade <- as.numeric(as.character(lapopagreg$idade))
lapopagreg$educacao <- as.numeric(as.character(lapopagreg$educacao))
lapopagreg$renda <- as.numeric(as.character(lapopagreg$renda))
lapopagreg$confiancaeleicoes <- as.numeric(as.character(lapopagreg$confiancaeleicoes))
lapopagreg$confiancamidia <- as.numeric(as.character(lapopagreg$confiancamidia))
lapopagreg$efext <- as.numeric(as.character(lapopagreg$efext))
lapopagreg$efint <- as.numeric(as.character(lapopagreg$efint))

# Salvar base:
save(lapopagreg,file="lapopanosagregada.Rda")
load(file = "lapopanosagregada.Rda")

lapopagreg$ano <- as.factor(as.numeric(lapopagreg$ano))

# Regressões "exploratórias": 
  
reglapopagreg1 <- lm(data = lapopagreg, formula = confiancaeleicoes ~ interesse +
                    idade + renda + educacao + efint + efext + apoiopartido + ano)
summary(reglapopagreg1)

reglapopagreg2 <- lm(data = lapopagreg, formula = confiancamidia ~ interesse +
                       idade + renda + educacao + efint + efext + apoiopartido + ano)
summary(reglapopagreg2)

# Regressão hierárquica:
reglapopagreg12 <- lmer(confiancaeleicoes ~ interesse + idade + renda + 
                          educacao + efint + efext + apoiopartido + (1 | ano), data = lapopagreg)

reglapopagreg22 <- lmer(confiancamidia ~ interesse + idade + renda + 
                          educacao + efint + efext + apoiopartido + (1 | ano), data = lapopagreg)

summary(reglapopagreg12)
summary(reglapopagreg22)
count(lapopagreg, partprot)
aggregate(confiancaeleicoes ~ ano, data=lapopagreg, mean)
aggregate(confiancamidia ~ ano, data=lapopagreg, mean)

# Regressões com VI confiança e VD participação:
# Votou vs nao votou:
reglapopagreg31 <- glmer(votou ~ confiancaeleicoes + interesse + idade + renda +
                           educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial)
summary(reglapopagreg31)
car::vif(reglapopagreg31)
(ci <- confint(reglapopagreg31))
exp(cbind(OR = coef(reglapopagreg31), ci))

plot(reglapopagreg41, pch = 20, col = "black", lty = "dotted")


# Votou em candidato vs branco e nulo:
reglapopagreg41 <- glmer(votounulo ~ confiancaeleicoes + interesse + idade + 
                           renda + educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(reglapopagreg41)
exp(fixef(reglapopagreg41))
exp(confint(reglapopagreg41,method="Wald"))
car::vif(reglapopagreg41)


(ci <- confint(reglapopagreg41))
exp(cbind(OR = coef(reglapopagreg41), ci))

# Participou de protestos:
reglapopagreg51 <- glmer(partprot ~ confiancaeleicoes + interesse + idade + renda +
                           educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial)
summary(reglapopagreg51)
exp(fixef(reglapopagreg51))
exp(confint(reglapopagreg51,method="Wald"))


# Frequencia de consumo de noticias:
# Recodificar a variável para o consumo de notícias:
count(lapopagreg, freqnoticias)
summary(lapopagreg$freqnoticias)
lapopagreg$freqnoticias <- dplyr::recode(lapopagreg$freqnoticias, "1" = "0", "2" = "0",
                                         "3" = "0", "4" = "1", "5" = "1", "NA" = "NA")

lapopagreg$freqnoticias <- as.numeric(as.character(lapopagreg$freqnoticias))

reglapopagreg61 <- glmer(freqnoticias ~ confiancaeleicoes + interesse + idade + renda +
                           educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial)
summary(reglapopagreg61)

# Diagnóstico dos modelos:
require(magrittr)

# Protesto:
mydata1 <- data.frame(lapopagreg$votounulo, lapopagreg$confiancaeleicoes, lapopagreg$interesse,
                      lapopagreg$idade, lapopagreg$renda, lapopagreg$educacao, 
                      lapopagreg$efint, lapopagreg$efext, lapopagreg$apoiopartido)

mydata1 <- na.omit(mydata1)
predictors <- colnames(mydata1)
# Bind the logit and tidying the data for plot
probabilities <- predict(reglapopagreg41, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

mydata <- lapopagreg %>%
  dplyr::select_if(is.numeric) 

require(tidyr)
mydata1 <- mydata1 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# Plotar coeficientes:
# Voto:
plot_model(reglapopagreg31, terms = c("confiancaeleicoes", "interesse", "idade", "renda",
                                      "educacao", "efint", "efext", "apoiopartido"),
           colors = c("#5CE1E6"), title = "Comparecimento às urnas", se = T,
           auto.label = F, axis.labels = c("Apoio a partido", "Eficácia externa", "Eficácia interna",
                                           "Educação", "Renda", "Idade", "Interesse em política",
                                           "Confiança nas eleições"))+
  theme_minimal()

# Voto em candidato
plot_model(reglapopagreg41, terms = c("confiancaeleicoes", "interesse", "idade", "renda",
                                      "educacao", "efint", "efext", "apoiopartido"),
           colors = c("#5CE1E6"), title = "Voto em candidato", se = T,
           auto.label = F, axis.labels = c("Apoio a partido", "Eficácia externa", "Eficácia interna",
                                           "Educação", "Renda", "Idade", "Interesse em política",
                                           "Confiança nas eleições"))+
  theme_minimal()

# Participação em protestos:

plot_model(reglapopagreg51, terms = c("confiancaeleicoes", "interesse", "idade", "renda",
                                      "educacao", "efint", "efext", "apoiopartido"),
           colors = c("#5CE1E6"), title = "Participação em protestos", se = T,
           auto.label = F, axis.labels = c("Apoio a partido", "Eficácia externa", "Eficácia interna",
                                           "Educação", "Renda", "Idade", "Interesse em política",
                                           "Confiança nas eleições"))+
  theme_minimal()


# Adicionar nova variável para faixa de idade:

a <- c(30:39)
b <- c(40:49)
c <- c(50:59)
d <- c(60:70)

lapopagreg <- mutate(lapopagreg, faixaidade = ifelse(idade <= 29, "menos30",
                                                       ifelse(idade %in% a, "30a40",
                                                              ifelse(idade %in% b, "40a50",
                                                                     ifelse(idade %in% c, "50a60",
                                                                            ifelse(idade >= 60, "60mais", "NA"))))))


lapopagreg$faixaidade <- factor(lapopagreg$faixaidade, levels = c("menos30",
                                                              "30a40",
                                                              "40a50",
                                                              "50a60",
                                                              "60mais"))
# Modelos de regressão com a interação confiança*idade:

reglapopagreg41ex <- glmer(votounulo ~ confiancaeleicoes*faixaidade + interesse + 
                           renda + educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(reglapopagreg41ex)
exp(fixef(reglapopagreg41ex))
exp(confint(reglapopagreg41ex,method="Wald"))

reglapopagreg31ex <- glmer(votou ~ confiancaeleicoes*faixaidade + interesse + renda +
                          educacao + efint + efext + apoiopartido + (1|ano), 
                        data = lapopagreg, family = binomial)
summary(reglapopagreg31ex)


reglapopagreg51ex <- glmer(partprot ~ confiancaeleicoes*faixaidade + interesse + renda +
                           educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial)
summary(reglapopagreg51ex)


reglapopagreg61ex <- polr(freqnoticias ~ confiancaeleicoes*faixaidade + interesse + renda +
                          educacao + efint + efext + apoiopartido, 
                        data = lapopagreg, Hess = T)
summary(reglapopagreg61ex)
(ctable <- coef(summary(reglapopagreg61ex)))
p61ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p61ex))


library(sjPlot)
library(sjmisc)
plot_model(reglapopagreg41ex, type = "pred", terms = c("confiancaeleicoes"))

count(lapopagreg, freqnoticias)



--------------------------------------------------------------------------------
  
  
# Análises por ano:
 
# 2008:
basereglapop08 <- filter(lapopagreg, ano == 2008) 

# Criar variável binária para candidatos principais:
basereglapop08 <- mutate(basereglapop08, Lula = ifelse(votoucandidato == 1501, "1",
                                                       ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop08 <- mutate(basereglapop08, Alckmin = ifelse(votoucandidato == 1502, "1",
                                                          ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop08$Lula <- as.numeric(basereglapop08$Lula)
basereglapop08$Alckmin <- as.numeric(basereglapop08$Alckmin)

# VD votou vs não votou:
reglapop081 <- glm(votou ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop08, family = "binomial")
summary(reglapop081)

  
# VD votou candidato:
reglapop0821 <- glm(votounulo ~ confiancaeleicoes + interesse + idade + 
                     renda + educacao + efint + efext + apoiopartido,
                   data = basereglapop08, family = "binomial")
summary(reglapop0821)

reglapop0822 <- glm(Lula ~ confiancaeleicoes + interesse + idade + 
                     renda + educacao + efint + efext + apoiopartido,
                   data = basereglapop08, family = "binomial")
summary(reglapop0822)

reglapop0823 <- glm(Alckmin ~ confiancaeleicoes + interesse + idade + 
                     renda + educacao + efint + efext + apoiopartido,
                   data = basereglapop08, family = "binomial")
summary(reglapop0823)


# VD protesto:
reglapop083 <- glm(partprot ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop08, family = "binomial")

summary(reglapop083)
(ci <- confint(reglapop083))
exp(cbind(OR = coef(reglapop083), ci))

# VD frequencia notícias no rádio:
basereglapop08$freqnoticiasradio <- as.factor(basereglapop08$freqnoticiasradio)
reglapop084ord <- polr(freqnoticiasradio ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop084ord)
(ctable <- coef(summary(reglapop084ord)))
p84 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p84))


# VD frequencia notícias na TV:
basereglapop08$freqnoticiastv <- as.factor(basereglapop08$freqnoticiastv)
reglapop085ord <- polr(freqnoticiastv ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop085ord)
(ctable <- coef(summary(reglapop085ord)))
p85 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p85))

# VD frequencia notícias no jornal:
basereglapop08$freqnoticiasjornal <- as.factor(basereglapop08$freqnoticiasjornal)
reglapop086ord <- polr(freqnoticiasjornal ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop086ord)
(ctable <- coef(summary(reglapop086ord)))
p86 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p86))
(ci <- confint(reglapop086ord))
exp(cbind(OR = coef(reglapop086ord), ci))

# VD frequencia notícias na internet:
basereglapop08$freqnoticiasnet <- as.factor(basereglapop08$freqnoticiasnet)
reglapop087ord <- polr(freqnoticiasnet ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop087ord)
(ctable <- coef(summary(reglapop087ord)))
p87 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p87))

---
  
# 2010:
basereglapop10 <- filter(lapopagreg, ano == 2010) 

# Criar variável binária para candidatos principais:
basereglapop10 <- mutate(basereglapop10, Lula = ifelse(votoucandidato == 1501, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop10 <- mutate(basereglapop10, Alckmin = ifelse(votoucandidato == 1502, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop10$Lula <- as.numeric(basereglapop10$Lula)
basereglapop10$Alckmin <- as.numeric(basereglapop10$Alckmin)

# VD votou vs não votou:
reglapop101 <- glm(votou ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop10, family = "binomial")
summary(reglapop101)

# VD votou candidato:
reglapop1021 <- glm(votounulo ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop10, family = "binomial")
summary(reglapop1021)
(ci <- confint(reglapop1021))
exp(cbind(OR = coef(reglapop1021), ci))

reglapop1022 <- glm(Lula ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop10, family = "binomial")
summary(reglapop1022)
(ci <- confint(reglapop1022))
exp(cbind(OR = coef(reglapop1022), ci))

reglapop1023 <- glm(Alckmin ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop10, family = "binomial")
summary(reglapop1023)


# VD protesto:
reglapop103 <- glm(partprot ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop10, family = "binomial")

summary(reglapop103)
(ci <- confint(reglapop103))
exp(cbind(OR = coef(reglapop103), ci))

# VD frequencia notícias:
basereglapop10$freqnoticias <- as.factor(basereglapop10$freqnoticias)
reglapop104ord <- polr(freqnoticias ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop10,
                       Hess = T)

summary(reglapop104ord)
(ctable <- coef(summary(reglapop104ord)))
p104 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p104))

---
  
# 2012:
basereglapop12 <- filter(lapopagreg, ano == 2012) 

# Criar variável binária para candidatos principais:
basereglapop12 <- mutate(basereglapop12, Dilma = ifelse(votoucandidato == 1501, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop12 <- mutate(basereglapop12, Serra = ifelse(votoucandidato == 1502, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop12$Dilma <- as.numeric(basereglapop12$Dilma)
basereglapop12$Serra <- as.numeric(basereglapop12$Serra)

# VD votou vs não votou:
reglapop121 <- glm(votou ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop12, family = "binomial")
summary(reglapop121)

# VD votou candidato:
reglapop1221 <- glm(votounulo ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop12, family = "binomial")
summary(reglapop1221)

reglapop1222 <- glm(Dilma ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop12, family = "binomial")
summary(reglapop1222)

reglapop1223 <- glm(Serra ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop12, family = "binomial")
summary(reglapop1223)


# VD protesto:
reglapop123 <- glm(partprot ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop12, family = "binomial")

summary(reglapop123)

# VD frequencia notícias:
basereglapop12$freqnoticias <- as.factor(basereglapop12$freqnoticias)
reglapop124ord <- polr(freqnoticias ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop12,
                       Hess = T)

summary(reglapop124ord)
(ctable <- coef(summary(reglapop124ord)))
p124 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p124))
(ci <- confint(reglapop124ord))
exp(cbind(OR = coef(reglapop124ord), ci))


---

# 2014:
basereglapop14 <- filter(lapopagreg, ano == 2014) 

# Criar variável binária para candidatos principais:
basereglapop14 <- mutate(basereglapop14, Dilma = ifelse(votoucandidato == 1501, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop14 <- mutate(basereglapop14, Serra = ifelse(votoucandidato == 1502, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))

basereglapop14$Dilma <- as.numeric(basereglapop14$Dilma)
basereglapop14$Serra <- as.numeric(basereglapop14$Serra)

# VD votou vs não votou:
reglapop141 <- glm(votou ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop14, family = "binomial")
summary(reglapop141)

# VD votou candidato:
reglapop1421 <- glm(votounulo ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop14, family = "binomial")
summary(reglapop1421)
(ci <- confint(reglapop1421))
exp(cbind(OR = coef(reglapop1421), ci))

reglapop1422 <- glm(Dilma ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop14, family = "binomial")
summary(reglapop1422)

reglapop1423 <- glm(Serra ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop14, family = "binomial")
summary(reglapop1423)


# VD protesto:
reglapop143 <- glm(partprot ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop14, family = "binomial")

summary(reglapop143)

# VD frequencia notícias:
basereglapop14$freqnoticias <- as.factor(basereglapop14$freqnoticias)
reglapop144ord <- polr(freqnoticias ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop14,
                       Hess = T)

summary(reglapop144ord)
(ctable <- coef(summary(reglapop144ord)))
p144 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p144))


---

# 2017:
basereglapop17 <- filter(lapopagreg, ano == 2017) 

# Criar variável binária para candidatos principais:
basereglapop17 <- mutate(basereglapop17, Dilma = ifelse(votoucandidato == 1501, "1",
                                               ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop17 <- mutate(basereglapop17, Aecio = ifelse(votoucandidato == 1502, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))

basereglapop17$Dilma <- as.numeric(basereglapop17$Dilma)
basereglapop17$Aecio <- as.numeric(basereglapop17$Aecio)

# VD votou vs não votou:
reglapop171 <- glm(votou ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop17, family = "binomial")
summary(reglapop171)

# VD votou candidato:
reglapop1721 <- glm(votounulo ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop17, family = "binomial")
summary(reglapop1721)

reglapop1722 <- glm(Dilma ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop17, family = "binomial")
summary(reglapop1722)

reglapop1723 <- glm(Aecio ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop17, family = "binomial")
summary(reglapop1723)


# VD protesto:
reglapop173 <- glm(partprot ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop17, family = "binomial")

summary(reglapop173)

# VD frequencia notícias:
basereglapop17$freqnoticias <- as.factor(basereglapop17$freqnoticias)
reglapop174ord <- polr(freqnoticias ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop17,
                       Hess = T)

summary(reglapop174ord)
(ctable <- coef(summary(reglapop174ord)))
p174 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p174))


# Regressões com interações por escolha de candidato:
basereglapop172 <- mutate(basereglapop17, EscCandidato = ifelse(votoucandidato == 1501, "Dilma",
                                                               ifelse(votoucandidato == 1502, "Aecio", "NA")))
basereglapop172 <- filter(basereglapop172, EscCandidato != "NA")
basereglapop172$EscCandidato <- factor(basereglapop172$EscCandidato, levels = c("Dilma",
                                                                                "Aecio"))
# Protesto
reglapop173ex <- glm(partprot ~ confiancaeleicoes*EscCandidato + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop172, family = "binomial")

summary(reglapop173ex)

# Frequência notícias:
basereglapop172$freqnoticias <- as.factor(basereglapop172$freqnoticias)
reglapop174ordex <- polr(freqnoticias ~ confiancaeleicoes*EscCandidato + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop172,
                       Hess = T)

summary(reglapop174ordex)
(ctable <- coef(summary(reglapop174ordex)))
p174ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p174ex))

---
# 2019:
basereglapop19 <- filter(lapopagreg, ano == 2019) 

# Criar variável binária para candidatos principais:
basereglapop19 <- mutate(basereglapop19, Bolsonaro = ifelse(votoucandidato == 1501, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop19 <- mutate(basereglapop19, Haddad = ifelse(votoucandidato == 1502, "1",
                                                        ifelse(votoucandidato == "NA", "NA", "0")))
basereglapop19$Bolsonaro <- as.numeric(basereglapop19$Bolsonaro)
basereglapop19$Haddad <- as.numeric(basereglapop19$Haddad)



# Criar variável para a análise com interação entre confiança e candidato:
basereglapop19 <- mutate(basereglapop19, EscCandidato = ifelse(votoucandidato == 1501, "Bolsonaro",
                                                            ifelse(votoucandidato == 1502, "Haddad", "NA")))

basereglapop193 <- mutate(basereglapop19, VotouBolsonaro = ifelse(votoucandidato == 1501, "1", "0"))


# VD votou vs não votou:
reglapop191 <- glm(votou ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop19, family = "binomial")
summary(reglapop191)

# VD votou candidato:
reglapop1921 <- glm(votounulo ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop19, family = "binomial")
summary(reglapop1921)

reglapop1922 <- glm(Bolsonaro ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop19, family = "binomial")
summary(reglapop1922)

reglapop1923 <- glm(Haddad ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop19, family = "binomial")
summary(reglapop1923)


# VD protesto:
reglapop193 <- glm(partprot ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop19, family = "binomial")

summary(reglapop193)

# VD frequencia notícias:
basereglapop19$freqnoticias <- as.factor(basereglapop19$freqnoticias)
reglapop194ord <- polr(freqnoticias ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop19,
                       Hess = T)

summary(reglapop194ord)
(ctable <- coef(summary(reglapop194ord)))
p194 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p194))


# VD frequencia notícias FaceBook:
basereglapop19$freqnotFB <- as.factor(basereglapop19$freqnotFB)
reglapop195ord <- polr(freqnotFB ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop19,
                       Hess = T)

summary(reglapop195ord)
(ctable <- coef(summary(reglapop195ord)))
p195 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p195))

basereglapop19$freqnotFB <- as.factor(basereglapop19$freqnotFB)
reglapop195ord.2 <- polr(freqnotFB ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop19,
                       Hess = T)

summary(reglapop195ord.2)
(ctable <- coef(summary(reglapop195ord.2)))
p195.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p195.2))


# VD frequencia notícias Twitter:
basereglapop19$freqnotTwitter <- as.factor(basereglapop19$freqnotTwitter)
reglapop196ord <- polr(freqnotTwitter ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop19,
                       Hess = T)

summary(reglapop196ord)
(ctable <- coef(summary(reglapop196ord)))
p196 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p196))

reglapop196ord.2 <- polr(freqnotTwitter ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop19,
                       Hess = T)

summary(reglapop196ord.2)
(ctable <- coef(summary(reglapop196ord.2)))
p196.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p196.2))

# VD frequencia notícias WhatsApp:
basereglapop19$freqnotWhatsapp <- as.factor(basereglapop19$freqnotWhatsapp)
reglapop197ord <- polr(freqnotWhatsapp ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop19,
                       Hess = T)

summary(reglapop197ord)
(ctable <- coef(summary(reglapop197ord)))
p197 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p197))
(ci <- confint(reglapop197ord))
exp(cbind(OR = coef(reglapop197ord), ci))


reglapop197ord.2 <- polr(freqnotWhatsapp ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop19,
                       Hess = T)

summary(reglapop197ord.2)
(ctable <- coef(summary(reglapop197ord.2)))
p197.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p197.2))



# Regressões com interação:
# Filtrar NAs:
basereglapop193$VotouBolsonaro <- as.numeric(basereglapop193$VotouBolsonaro)

basereglapop192 <- filter(basereglapop19, EscCandidato != "NA")
basereglapop192$EscCandidato <- factor(basereglapop192$EscCandidato, levels = c("Haddad",
                                                                         "Bolsonaro"))
t.test(confiancaeleicoes ~ EscCandidato, data = basereglapop192)


# VD frequencia notícias FaceBook:
basereglapop192$freqnotFB <- as.factor(basereglapop192$freqnotFB)
reglapop195ordex <- polr(freqnotFB ~ confiancaeleicoes*EscCandidato + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop192,
                       Hess = T)

summary(reglapop195ordex)
(ctable <- coef(summary(reglapop195ordex)))
p195ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p195ex))


# VD frequencia notícias Twitter:
basereglapop192$freqnotTwitter <- as.factor(basereglapop192$freqnotTwitter)
reglapop196ordex <- polr(freqnotTwitter ~ confiancaeleicoes*EscCandidato + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop192,
                       Hess = T)

summary(reglapop196ordex)
(ctable <- coef(summary(reglapop196ordex)))
p196ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p196ex))


# VD frequencia notícias WhatsApp:
basereglapop193$freqnotWhatsapp <- as.factor(basereglapop193$freqnotWhatsapp)
reglapop197ordex <- polr(freqnotWhatsapp ~ confiancaeleicoes*VotouBolsonaro +
                            interesse + idade + renda + educacao + efint + 
                           efext + apoiopartido, data=basereglapop193,
                       Hess = T)

summary(reglapop197ordex)
(ctable <- coef(summary(reglapop197ordex)))
p197ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p197ex))
(ci <- confint(reglapop197ordex))
exp(cbind(OR = coef(reglapop197ord), ci))

# Participação em protestos:
reglapop193ex <- glm(partprot ~ confiancaeleicoes*VotouBolsonaro + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop193, family = "binomial")

summary(reglapop193ex)

# Busca notícias:
basereglapop193$freqnoticias <- as.factor(basereglapop193$freqnoticias)
reglapop194ordex <- polr(freqnoticias ~ confiancaeleicoes*VotouBolsonaro + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop193,
                       Hess = T)

summary(reglapop194ordex)
(ctable <- coef(summary(reglapop194ordex)))
p194ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p194ex))


# Gráfico confiança nas eleições:
basegraficoeleicoes <- read.csv("GrafConfLAPOP.csv", sep = ";")
basegraficoeleicoes$Percentual <- as.numeric(basegraficoeleicoes$Percentual)
basegraficoeleicoes$Ano <- as.numeric(basegraficoeleicoes$Ano)

basegraficoeleicoes$Escore <- factor(basegraficoeleicoes$Escore, levels = c("Muita confiança",
                                                                      "Alguma confiança",
                                                                      "Nenhuma confiança"))

windowsFonts(Times=windowsFont("Times New Roman"))
ggplot(basegraficoeleicoes,aes(x=Ano,y=Percentual,colour=Escore))+
  geom_line(lwd=1)+
  geom_point()+
  labs(title = "Confiança nas eleições", y= "Percentual", x = "", colour = "")+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2017, 2019))+
  theme_grey()+
  theme(text=element_text(family="Times"))+
  scale_color_manual(values=c("#031926", "#dfc27d", "#8c510a"))




---
  
# CONFIANÇA NA MÍDIA:

# Gráfico:
basegraficomidia <- read.csv("GrafConfMidiaLAPOP.csv", sep = ";")
basegraficomidia$Percentual <- as.numeric(basegraficomidia$Percentual)
basegraficomidia$Ano <- as.numeric(basegraficomidia$Ano)

basegraficomidia$Escore <- factor(basegraficomidia$Escore, levels = c("Muita confiança",
                                                            "Alguma confiança",
                                                            "Nenhuma confiança"))

windowsFonts(Times=windowsFont("Times New Roman"))
ggplot(basegraficomidia,aes(x=Ano,y=Percentual,colour=Escore))+
  geom_line(lwd=1)+
  geom_point()+
  labs(title = "Confiança na mídia", y= "Percentual", x = "", colour = "")+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2017, 2019))+
  theme_grey()+
  theme(text=element_text(family="Times"))+
  scale_color_manual(values=c("#031926", "#dfc27d", "#8c510a"))


    
reglapopagreg22 <- lmer(confiancamidia ~ interesse + idade + renda + 
                          educacao + efint + efext + apoiopartido + (1 | ano), data = lapopagreg)

summary(reglapopagreg22)  

# Votou vs nao votou:
reglapopagreg31.2 <- glmer(votou ~ confiancamidia + interesse + idade + renda +
                             educacao + efint + efext + apoiopartido + (1|ano), 
                           data = lapopagreg, family = binomial)
summary(reglapopagreg31.2)


# Voto nulo:
reglapopagreg41.2 <- glmer(votounulo ~ confiancamidia + interesse + idade + renda + educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary(reglapopagreg41.2)
exp(fixef(reglapopagreg41.2))
exp(confint(reglapopagreg41.2,method="Wald"))

# Participou de protestos:
reglapopagreg51.2 <- glmer(partprot ~ confiancamidia + interesse + idade + renda +
                           educacao + efint + efext + apoiopartido + (1|ano), 
                         data = lapopagreg, family = binomial)
summary(reglapopagreg51.2)
exp(fixef(reglapopagreg51.2))
exp(confint(reglapopagreg51.2,method="Wald"))
  
# Consumo de notícias:
reglapopagreg61.2 <- glmer(freqnoticias ~ confiancamidia + interesse + idade + renda +
                             educacao + efint + efext + apoiopartido + (1|ano),
                           data = lapopagreg, family = binomial)
summary(reglapopagreg61.2)

aggregate(confiancamidia ~ ano, data=lapopagreg, mean)
count(lapopagreg, confiancamidia, ano)
---
  

# 2008:

# VD votou vs não votou:
reglapop081.2 <- glm(votou ~ confiancamidia + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop08, family = "binomial")
summary(reglapop081.2)


# VD votou candidato:
reglapop0821.2 <- glm(votounulo ~ confiancamidia + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop08, family = "binomial")
summary(reglapop0821.2)

reglapop0822.2 <- glm(Lula ~ confiancamidia + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop08, family = "binomial")
summary(reglapop0822.2)

reglapop0823.2 <- glm(Alckmin ~ confiancamidia + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop08, family = "binomial")
summary(reglapop0823.2)


# VD protesto:
reglapop083.2 <- glm(partprot ~ confiancamidia + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop08, family = "binomial")

summary(reglapop083.2)

# VD frequencia notícias no rádio:
basereglapop08$freqnoticiasradio <- as.factor(basereglapop08$freqnoticiasradio)
reglapop084.2ord <- polr(freqnoticiasradio ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop084.2ord)
(ctable <- coef(summary(reglapop084.2ord)))
p84.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p84.2))


# VD frequencia notícias na TV:
basereglapop08$freqnoticiastv <- as.factor(basereglapop08$freqnoticiastv)
reglapop085.2ord <- polr(freqnoticiastv ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop085.2ord)
(ctable <- coef(summary(reglapop085.2ord)))
p85.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p85.2))

# VD frequencia notícias no jornal:
basereglapop08$freqnoticiasjornal <- as.factor(basereglapop08$freqnoticiasjornal)
reglapop086.2ord <- polr(freqnoticiasjornal ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop086.2ord)
(ctable <- coef(summary(reglapop086.2ord)))
p86.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p86.2))

(ci <- confint(reglapop086.2ord))
exp(cbind(OR = coef(reglapop086ord), ci))

# VD frequencia notícias na internet:
basereglapop08$freqnoticiasnet <- as.factor(basereglapop08$freqnoticiasnet)
reglapop087.2ord <- polr(freqnoticiasnet ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop08,
                       Hess = T)

summary(reglapop087.2ord)
(ctable <- coef(summary(reglapop087.2ord)))
p87.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p87.2))

  
---
  
  
# 2010:
# VD votou vs não votou:
reglapop101.2 <- glm(votou ~ confiancamidia + interesse + idade + renda + 
                       educacao + efint + efext + apoiopartido, 
                     data = basereglapop10, family = "binomial")
summary(reglapop101.2)

# VD votou candidato:
reglapop1021.2 <- glm(votounulo ~ confiancamidia + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop10, family = "binomial")
summary(reglapop1021.2)

reglapop1022.2 <- glm(Lula ~ confiancamidia + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop10, family = "binomial")
summary(reglapop1022.2)


reglapop1023.2 <- glm(Alckmin ~ confiancamidia + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop10, family = "binomial")
summary(reglapop1023.2)


# VD protesto:
reglapop103.2 <- glm(partprot ~ confiancamidia + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop10, family = "binomial")

summary(reglapop103.2)

# VD frequencia notícias:
basereglapop10$freqnoticias <- as.factor(basereglapop10$freqnoticias)
reglapop104.2rd <- polr(freqnoticias ~ confiancamidia + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop10,
                       Hess = T)

summary(reglapop104.2ord)
(ctable <- coef(summary(reglapop104.2ord)))
p104.2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p104.2))
  
---
  
  
#2012:
  # VD votou vs não votou:
  reglapop121 <- glm(votou ~ confiancaeleicoes + interesse + idade + renda + 
                       educacao + efint + efext + apoiopartido, 
                     data = basereglapop12, family = "binomial")
summary(reglapop121)

# VD votou candidato:
reglapop1221 <- glm(votounulo ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop12, family = "binomial")
summary(reglapop1221)

reglapop1222 <- glm(Dilma ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop12, family = "binomial")
summary(reglapop1222)

reglapop1223 <- glm(Serra ~ confiancaeleicoes + interesse + idade + 
                      renda + educacao + efint + efext + apoiopartido,
                    data = basereglapop12, family = "binomial")
summary(reglapop1223)


# VD protesto:
reglapop123 <- glm(partprot ~ confiancaeleicoes + interesse + idade + renda + 
                     educacao + efint + efext + apoiopartido, 
                   data = basereglapop12, family = "binomial")

summary(reglapop123)

# VD frequencia notícias:
basereglapop12$freqnoticias <- as.factor(basereglapop12$freqnoticias)
reglapop124ord <- polr(freqnoticias ~ confiancaeleicoes + interesse + idade +
                         renda + educacao + efint + efext + apoiopartido, data=basereglapop12,
                       Hess = T)

summary(reglapop124ord)
(ctable <- coef(summary(reglapop124ord)))
p124 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p124))
(ci <- confint(reglapop124ord))
exp(cbind(OR = coef(reglapop124ord), ci))

  
---
  
  
#2017:




lapopagregsem08 <- filter(lapopagreg, ano != 2008)




reglapopagregnda <- lm(confiancamidia ~ VotouBolsonaro + interesse + idade + renda + 
                          educacao + efint + efext + apoiopartido, data = basereglapop193)

summary(reglapopagregnda)  

