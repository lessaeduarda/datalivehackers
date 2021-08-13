
require(readxl)

basesp <- read_excel("BaseSPOnda1.xlsx")
basegrafico <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha3")
basegraficohaddad <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha4")
basegraficobolsonaro <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha5")
basegraficomedias <- read_excel("BaseSPOnda1.xlsx", sheet = "grafmedias2")



count(basesp,TERCALIVRE)
count(basesp,JOVEMPAN)
count(basesp,UOL)
count(basesp,BRASILPARALELO)
count(basesp,BRASIL247)
count(basesp,RECORD)
count(basesp,FOLHA)
count(basesp,GLOBO)

# Ajustar base para plotar gráfico:
df2 <- tidyr::pivot_longer(basegrafico, cols=c('Globo', 'Record', 'Folha',
                                               'UOL', 'Tercalivre', 'Jovempan',
                                               'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")
head(df2)

windowsFonts(Times=windowsFont("Times New Roman"))
geral <- ggplot(df2, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  labs(title="Geral - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                           labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                                      "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                                      "UOL"))+
    theme(text=element_text(family="Times"))+
  guides(fill=FALSE, color=FALSE)


# Contar uso a partir de escolha de candidato:
count(basesp, VOTOUPRESIDENTE2TURNO)
tl <- count(basesp,TERCALIVRE, VOTOUPRESIDENTE2TURNO)
jp <- count(basesp,JOVEMPAN, VOTOUPRESIDENTE2TURNO)
uol <- count(basesp,UOL, VOTOUPRESIDENTE2TURNO)
bp <- count(basesp,BRASILPARALELO, VOTOUPRESIDENTE2TURNO)
b247 <- count(basesp,BRASIL247, VOTOUPRESIDENTE2TURNO)
rec <- count(basesp,RECORD, VOTOUPRESIDENTE2TURNO)
fsp <- count(basesp,FOLHA, VOTOUPRESIDENTE2TURNO)
glob <- count(basesp,GLOBO, VOTOUPRESIDENTE2TURNO)


# Gráfico Haddad:
df3 <- tidyr::pivot_longer(basegraficohaddad, cols=c('Globo', 'Record', 'Folha',
                                               'UOL', 'Tercalivre', 'Jovempan',
                                               'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
haddad <- ggplot(df3, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  labs(title="Eleitores de Haddad - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  guides(fill=FALSE, color=FALSE)

# Gráfico Bolsonaro:
df4 <- tidyr::pivot_longer(basegraficobolsonaro, cols=c('Globo', 'Record', 'Folha',
                                               'UOL', 'Tercalivre', 'Jovempan',
                                               'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
bolsonaro <- ggplot(df4, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  labs(title="Eleitores de Bolsonaro - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo:")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")

grid.arrange(geral, haddad, bolsonaro, nrow=3)

---
  
# Gráficos com médias:

# Divisão por nível de confiança:
mediasconf <- filter(basegraficomedias, Grupo != "Bolsonaro")
mediasconf <- filter(mediasconf, Grupo != "Haddad")
mediasconf <- filter(mediasconf, Grupo != "Geral")

levels(mediasconf$Grupo) <- c("Total Confiança", "Alguma Confiança", "Nenhuma Confiança")
ggplot(data=mediasconf, aes(x=Veiculo, y=Media, fill=Veiculo)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme_grey()+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
scale_x_discrete(labels = NULL, breaks = NULL)+
labs(title="Frequência média de busca por informação por veículo", 
       x="",
       y="Frequência Média \n (em número de dias na semana)",
       fill = "Veículo:")+
facet_wrap(~Grupo, ncol = 3)

# Divisão por escolha de candidato:
mediascand <- filter(basegraficomedias, Grupo != "Maxconf")
mediascand <- filter(mediascand, Grupo != "Minconf")
mediascand <- filter(mediascand, Grupo != "Medconf")
mediascand$Grupo <- as.factor(mediascand$Grupo)

mediascand$Grupo <- factor(mediascand$Grupo, levels = c("Bolsonaro",
                                                        "Geral",
                                                        "Haddad"))


levels(mediascand$Grupo) <- c("Jair Bolsonaro", "Amostra Geral", "Fernando Haddad")
ggplot(data=mediascand, aes(x=Veiculo, y=Media, fill=Veiculo)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme_grey()+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  labs(title="Frequência média de busca por informação por veículo", 
       x="",
       y="Frequência Média \n (em número de dias na semana)",
       fill = "Veículo:")+
  facet_wrap(~Grupo, ncol = 3)








basesp$IDEOLOGIA <- as.numeric(basesp$IDEOLOGIA) 
basesp$IDADE <- as.numeric(basesp$IDADE)
basesp$CONFIANCAGRANDEMIDIA <- as.numeric(basesp$CONFIANCAGRANDEMIDIA) 
basesp$TERCALIVRE <- as.factor(basesp$TERCALIVRE) 
basesp$JOVEMPAN <- as.factor(basesp$JOVEMPAN) 
basesp$UOL <- as.factor(basesp$UOL) 
basesp$BRASILPARALELO <- as.factor(basesp$BRASILPARALELO) 
basesp$BRASIL247 <- as.factor(basesp$BRASIL247) 
basesp$RECORD <- as.factor(basesp$RECORD) 
basesp$FOLHA <- as.factor(basesp$FOLHA) 
basesp$GLOBO <- as.factor(basesp$GLOBO) 
basesp$VOTOUMUNICIPAL <- as.numeric(basesp$VOTOUMUNICIPAL)
basesp$VOTOUMUNICIPAL <- as.factor(basesp$VOTOUMUNICIPAL)
basesp$VOTOUNULOMUNICIPAL <- as.numeric(basesp$VOTOUNULOMUNICIPAL)
basesp$VOTOUNULOMUNICIPAL <- as.factor(basesp$VOTOUNULOMUNICIPAL)
basesp$VOTOUNULO <- as.numeric(basesp$VOTOUNULO) 
basesp$VOTOUNULO <- as.factor(basesp$VOTOUNULO) 
basesp$VOTOU <- as.numeric(basesp$VOTOU) 
basesp$VOTOU <- as.factor(basesp$VOTOU) 
basesp$VOTOU <- as.numeric(basesp$VOTOU) 
basesp$VOTOUBOLSONARO <- as.numeric(basesp$VOTOUBOLSONARO) 


basesp2 <- filter(basesp, VOTOUPRESIDENTE2TURNO != "NA")
basesp2$VOTOUPRESIDENTE2TURNO <- as.factor(basesp2$VOTOUPRESIDENTE2TURNO)



# Modelos de regressão:

# Terça Livre:
regsp1 <- polr(TERCALIVRE ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                           SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                        data = basesp, Hess = T)
summary(regsp1)
(ctable <- coef(summary(regsp1)))
psp1 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp1))
(ci <- confint(regsp1))
exp(cbind(OR = coef(regsp1), ci))


# Jovem Pan:
regsp2 <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp2)
(ctable <- coef(summary(regsp2)))
psp2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2))

# UOL: 
regsp3 <- polr(UOL ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp3)
(ctable <- coef(summary(regsp3)))
psp3 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp3))

# Brasil Paralelo:
regsp4 <- polr(BRASILPARALELO ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA + 
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp4)
(ctable <- coef(summary(regsp4)))
psp4 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp4))

# Brasil 247:
regsp5 <- polr(BRASIL247 ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp5)
(ctable <- coef(summary(regsp5)))
psp5 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp5))
(ci <- confint(regsp5))
exp(cbind(OR = coef(regsp5), ci))



# Rede Record:
regsp6 <- polr(RECORD ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp6)
(ctable <- coef(summary(regsp6)))
psp6 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp6))

# Folha de São Paulo:
regsp7 <- polr(FOLHA ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp7)
(ctable <- coef(summary(regsp7)))
psp7 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp7))

# Rede Globo:
regsp8 <- polr(GLOBO ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp8)
(ctable <- coef(summary(regsp8)))
psp8 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp8))
(ci <- confint(regsp8))
exp(cbind(OR = coef(regsp8), ci))

# Votou nas eleições municipais:
regsp9 <- glm(VOTOUMUNICIPAL ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                  SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
               data = basesp, family = binomial)
summary(regsp9)


# Votou em candidato nas eleições municipais:
regsp9.2 <- glm(VOTOUNULOMUNICIPAL ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
              data = basesp, family = binomial)
summary(regsp9.2)
(ci <- confint(regsp9.2))
exp(cbind(OR = coef(regsp9.2), ci))

# Votou nas eleições presidenciais:
regsp10 <- glm(VOTOU ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                  SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
               data = basesp, family = binomial)
summary(regsp10)

# Votou em candidato:
regsp11 <- glm(VOTOUNULO ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                  SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
               data = basesp, family = binomial)
summary(regsp11)
vif(regsp11)

count(basesp,TERCALIVRE)
count(basesp,JOVEMPAN)
count(basesp,UOL)
count(basesp,BRASILPARALELO)
count(basesp,BRASIL247)
count(basesp,RECORD)
count(basesp,FOLHA)
count(basesp,GLOBO)

regsp2EX <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp2EX)
(ctable <- coef(summary(regsp2EX)))
psp2EX <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2EX))

regsp4EX <- polr(BRASILPARALELO ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                 data = basesp, Hess = T)
summary(regsp4EX)
(ctable <- coef(summary(regsp4EX)))
psp4EX <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp4EX))



cor(select(basesp, CONFIANCAGRANDEMIDIA, VOTOUBOLSONARO, INTERESSE, IDADE, RENDA, SOFISTICACAO, IDEOLOGIA, APOIOPARTIDO))

cor(select(basesp, CONFIANCAGRANDEMIDIA, INTERESSE, IDADE, RENDA, SOFISTICACAO, IDEOLOGIA, APOIOPARTIDO))

cor.test(basesp$VOTOUBOLSONARO, basesp$IDEOLOGIA)

count(basesp, conf)


# Contar uso a partir do escore de confiança:
basegraficozerotrust <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha8")
basegraficotentrust <- read_excel("BaseSPOnda1.xlsx", sheet = "Planilha7")

count(basesp, CONFIANCAGRANDEMIDIA)

tl2 <- count(basesp,TERCALIVRE, CONFIANCAGRANDEMIDIA)
jp2 <- count(basesp,JOVEMPAN, CONFIANCAGRANDEMIDIA)
uol2 <- count(basesp,UOL, CONFIANCAGRANDEMIDIA)
bp2 <- count(basesp,BRASILPARALELO, CONFIANCAGRANDEMIDIA)
b2472 <- count(basesp,BRASIL247, CONFIANCAGRANDEMIDIA)
rec2 <- count(basesp,RECORD, CONFIANCAGRANDEMIDIA)
fsp2 <- count(basesp,FOLHA, CONFIANCAGRANDEMIDIA)
glob2 <- count(basesp,GLOBO, CONFIANCAGRANDEMIDIA)


# Gráfico nenhuma confiança:
df5 <- tidyr::pivot_longer(basegraficozerotrust, cols=c('Globo', 'Record', 'Folha',
                                                     'UOL', 'Tercalivre', 'Jovempan',
                                                     'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
zerotrust <- ggplot(df5, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  scale_y_continuous(limits = c(0, 100))+
  labs(title="Nenhuma Confiança - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  guides(fill=FALSE, color=FALSE)

# Gráfico total confiança:
df6 <- tidyr::pivot_longer(basegraficotentrust, cols=c('Globo', 'Record', 'Folha',
                                                        'UOL', 'Tercalivre', 'Jovempan',
                                                        'Brasilparalelo', 'Brasil247'), names_to='variable', 
                           values_to="value")

windowsFonts(Times=windowsFont("Times New Roman"))
tentrust <- ggplot(df6, aes(x=Frequencia, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  scale_y_continuous(limits = c(0, 100))+
  labs(title="Total Confiança - Frequência de busca por informação por veículo", 
       x="",
       y="%",
       fill = "Veículo")+
  theme(text=element_text(family="Times"))+
  theme_grey()+
  scale_fill_manual(values=c("#031926", "#468189", "#9dbebb",
                             "#bebb9d", "#dfc27d", "#bf812d",
                             "#8c510a", "#543005"),
                    labels = c("Brasil 247", "Brasil Paralelo", "Folha de São Paulo",
                               "Rede Globo", "Jovem Pan", "Rede Record", "Terça Livre",
                               "UOL"))+
  theme(text=element_text(family="Times"))+
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+
  guides(fill=FALSE, color=FALSE)

grid.arrange(zerotrust, tentrust, nrow=2)

# Adicionar interação com voto em Bolsonaro:
# Rede Globo:
regsp8ex <- polr(GLOBO ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp8ex)
(ctable <- coef(summary(regsp8ex)))
psp8ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp8ex))

(ci <- confint(regsp8))
exp(cbind(OR = coef(regsp8), ci))


# Folha de São Paulo:
regsp7ex <- polr(FOLHA ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp7ex)
(ctable <- coef(summary(regsp7ex)))
psp7ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp7ex))

# Brasil Paralelo:
regsp4ex <- polr(BRASILPARALELO ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA + 
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp4ex)
(ctable <- coef(summary(regsp4ex)))
psp4ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp4ex))

# Brasil 247:
regsp5ex <- polr(BRASIL247 ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp5ex)
(ctable <- coef(summary(regsp5ex)))
psp5ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp5ex))

(ci <- confint(regsp5))
exp(cbind(OR = coef(regsp5), ci))

# Jovem Pan:
regsp2ex <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
data = basesp, Hess = T)
summary(regsp2ex)
(ctable <- coef(summary(regsp2ex)))
psp2ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2ex))

basesp2 <- basesp
basesp2$JOVEMPAN <- as.numeric(as.character(basesp2$JOVEMPAN))

mod2<-lm(JOVEMPAN ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
           SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
         data = basesp2)

vif(mod2)


regsp2ex2 <- polr(JOVEMPAN ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                 data = basesp, Hess = T)
summary(regsp2ex2)
(ctable <- coef(summary(regsp2ex2)))
psp2ex2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp2ex2))


# Terça Livre:
regsp1ex <- polr(TERCALIVRE ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp1ex)
(ctable <- coef(summary(regsp1ex)))
psp1ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp1ex))

basesp2$TERCALIVRE <- as.numeric(as.character(basesp2$TERCALIVRE))
mod3<-lm(TERCALIVRE ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
           SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
         data = basesp2)

vif(mod3)

# Rede Record:
regsp6ex <- polr(RECORD ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp6ex)
(ctable <- coef(summary(regsp6ex)))
psp6ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp6ex))

regsp6ex2 <- polr(RECORD ~ CONFIANCAGRANDEMIDIA + VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                   SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
                 data = basesp, Hess = T)
summary(regsp6ex2)
(ctable <- coef(summary(regsp6ex2)))
psp6ex2 <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp6ex2))


# UOL:
regsp3ex <- polr(UOL ~ CONFIANCAGRANDEMIDIA*VOTOUBOLSONARO + INTERESSE + IDADE + RENDA +
                 SEXOFEMININO + SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO, 
               data = basesp, Hess = T)
summary(regsp3ex)
(ctable <- coef(summary(regsp3ex)))
psp3ex <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = psp3ex))


---
  
  
# Análise com médias:
basesp3 <- basesp
basesp3$FOLHA <- as.numeric(as.character(basesp3$FOLHA))
basesp3$GLOBO <- as.numeric(as.character(basesp3$GLOBO))
basesp3$RECORD <- as.numeric(as.character(basesp3$RECORD))
basesp3$JOVEMPAN <- as.numeric(as.character(basesp3$JOVEMPAN))
basesp3$UOL <- as.numeric(as.character(basesp3$UOL))
basesp3$BRASIL247 <- as.numeric(as.character(basesp3$BRASIL247))
basesp3$BRASILPARALELO <- as.numeric(as.character(basesp3$BRASILPARALELO))
basesp3$TERCALIVRE <- as.numeric(as.character(basesp3$TERCALIVRE))

basesp3 <- mutate(basesp3, consumoacimamediafolha = ifelse(FOLHA >= 2, "1", "0"))
basesp3 <- mutate(basesp3, consumoacimamediaglobo = ifelse(GLOBO >= 3, "1", "0"))
basesp3 <- mutate(basesp3, consumoacimamediarecord = ifelse(RECORD >= 2, "1", "0"))
basesp3 <- mutate(basesp3, consumoacimamediajovempan = ifelse(JOVEMPAN >= 2, "1", "0"))
basesp3 <- mutate(basesp3, consumoacimamediauol = ifelse(UOL >= 3, "1", "0"))
basesp3 <- mutate(basesp3, consumoacimamediabrasil247 = ifelse(BRASIL247 >= 1, "1", "0"))
basesp3 <- mutate(basesp3, consumoacimamediabrasilparalelo = ifelse(BRASILPARALELO >= 1, "1", "0"))
basesp3 <- mutate(basesp3, consumoacimamediatercalivre = ifelse(TERCALIVRE >= 1, "1", "0"))

write.xlsx(basesp3, "basespmedia.xlsx")
basesp4 <- read.xlsx("basespmedia.xlsx")
                                                      
basesp4$IDEOLOGIA <- as.numeric(basesp4$IDEOLOGIA) 
basesp4$IDADE <- as.numeric(basesp4$IDADE)
basesp4$CONFIANCAGRANDEMIDIA <- as.numeric(basesp4$CONFIANCAGRANDEMIDIA) 
basesp4$TERCALIVRE <- as.factor(basesp4$TERCALIVRE) 
basesp4$JOVEMPAN <- as.factor(basesp4$JOVEMPAN) 
basesp4$UOL <- as.factor(basesp4$UOL) 
basesp4$BRASILPARALELO <- as.factor(basesp4$BRASILPARALELO) 
basesp4$BRASIL247 <- as.factor(basesp4$BRASIL247) 
basesp4$RECORD <- as.factor(basesp4$RECORD) 
basesp4$FOLHA <- as.factor(basesp4$FOLHA) 
basesp4$GLOBO <- as.factor(basesp4$GLOBO) 
basesp4$VOTOUMUNICIPAL <- as.numeric(basesp4$VOTOUMUNICIPAL)
basesp4$VOTOUMUNICIPAL <- as.factor(basesp4$VOTOUMUNICIPAL)
basesp4$VOTOUNULOMUNICIPAL <- as.numeric(basesp4$VOTOUNULOMUNICIPAL)
basesp4$VOTOUNULOMUNICIPAL <- as.factor(basesp4$VOTOUNULOMUNICIPAL)
basesp4$VOTOUNULO <- as.numeric(basesp4$VOTOUNULO) 
basesp4$VOTOUNULO <- as.factor(basesp4$VOTOUNULO) 
basesp4$VOTOU <- as.numeric(basesp4$VOTOU) 
basesp4$VOTOU <- as.factor(basesp4$VOTOU) 
basesp4$VOTOU <- as.numeric(basesp4$VOTOU) 
basesp4$VOTOUBOLSONARO <- as.numeric(basesp4$VOTOUBOLSONARO) 
basesp4$consumoacimamediaalternativa <- as.numeric(basesp4$consumoacimamediaalternativa)
basesp4$consumoacimamediatradicional <- as.numeric(basesp4$consumoacimamediatradicional)
basesp4$consomemaisalterna <- as.numeric(basesp4$consomemaisalterna)
basesp4$FREQMAISALTERNA <- as.numeric(basesp4$FREQMAISALTERNA)
basesp4$FREQMEDIAMIDIASTRAD <- as.numeric(basesp4$FREQMEDIAMIDIASTRAD)



count(basesp4, consumoacimamediatradicional)
count(basesp4, consumoacimamediaalternativa)

basesp4 <- mutate(basesp4, dummytradicional = ifelse(consumoacimamediatradicional >= 1, "1", "0"))
basesp4 <- mutate(basesp4, dummyalternativa = ifelse(consumoacimamediaalternativa >= 1, "1", "0"))

basesp4$dummytradicional <- as.factor(basesp4$dummytradicional) 
basesp4$dummyalternativa <- as.factor(basesp4$dummyalternativa) 
count(basesp4, dummytradicional)
count(basesp4, dummyalternativa)
count(basesp4, dummyalternativa, dummytradicional)
count(basesp4, dummyalternativa, CONFIANCAGRANDEMIDIA)
count(basesp4, dummytradicional, CONFIANCAGRANDEMIDIA)


regspdummy1 <- glm(dummytradicional ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
              data = basesp4, family = binomial)
summary(regspdummy1)
  
regspdummy2 <- glm(dummyalternativa ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                     SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
                   data = basesp4, family = binomial)
summary(regspdummy2)


# Dummy que indica consumo com mais frequencia através das mídias alternativas:
regspdummy3 <- glm(FREQMAISALTERNA ~ CONFIANCAGRANDEMIDIA + INTERESSE + IDADE + RENDA +
                     SOFISTICACAO + IDEOLOGIA + APOIOPARTIDO,
                   data = basesp4, family = binomial)
summary(regspdummy3)


count(basesp4, FREQMAISALTERNA)

count(basesp4, FREQMAISALTERNA, FREQMEDIAMIDIASTRAD )


---

  
# Diff-in-diffs

# Carregar base tratada:
basespmanip <- read_dta("panel.dta")  
par <- c(0,1)
basespmanip <- filter(basespmanip, v2 %in% par)  
write.csv(basespmanip, file = "basespduasondas.csv")
basesptrat2 <- read_xlsx("basespduasondas.xlsx")
basesptrat3 <- read_xlsx("basespduasondas.xlsx", sheet = "tratado")
basesptrat4 <- read_xlsx("basespduasondas.xlsx", sheet = "abriuemail")
basesptrat5 <- read_xlsx("basespduasondas.xlsx", sheet = "ativouvoucher")
basesptrat6 <- read_xlsx("basespduasondas.xlsx", sheet = "fezcadastro")
basesptrat7 <- read_xlsx("basespduasondas.xlsx", sheet = "usou")



# Diff-in-Diff dos tratados versus não tratados:
regdiff <- lm(trust ~ wave*treated, data = basesptrat3)
summary(regdiff)

# Abriu email versus não abriu:
regdiff2 <- lm(trust ~ wave*treated, data = basesptrat4)
summary(regdiff2)

# Ativou voucher versus não ativou:
regdiff3 <- lm(trust ~ wave*treated, data = basesptrat5)
summary(regdiff3)

# Fez cadastro versus não fez:
regdiff4 <- lm(trust ~ wave*treated, data = basesptrat6)
summary(regdiff4)

# Usou versus não usou:
regdiff5 <- lm(trust ~ wave*treated, data = basesptrat7)
summary(regdiff5)
