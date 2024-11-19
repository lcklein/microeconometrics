# 0) Packs ----------------------------------------------------------------

source("scripts/packs.R")
library("caTools")
library("ROCR")
library(MASS)
library(DataExplorer)
library("PNADcIBGE")
library(psych)
library(lmtest)
library(mlogit)
library(AER)
library("pscl")

# 1) Dados ----------------------------------------------------------------

pnad_1 <- readRDS("data/pnad_02_12") %>%
  dplyr::select("UF", "V2007", "V2010", "VD4002", "V1022") %>%
  clean_names() %>%
  rename(
    sexo = 2,
    raça = 3,
    desemprego = 4,
    local = 5
  ) %>%
  mutate(
    raça = case_when(
      raça == "Branca" ~ 1,
      raça %in% c("Parda", "Preta", "Amarela", "Indígena",
                  "Ignorado") ~ 0
    ),
    desemprego = case_when(
      desemprego == "Pessoas ocupadas" ~ 1,
      desemprego == "Pessoas desocupadas" ~ 0
    )
  ) %>%
  na.omit()

pnad_2 <- readRDS("data/pnad_02_22") %>%
  dplyr::select("UF", "V2007", "V2010", "VD4002", "V1022") %>%
  clean_names() %>%
  rename(
    sexo = 2,
    raça = 3,
    desemprego = 4,
    local = 5
  ) %>%
  mutate(
    raça = case_when(
      raça == "Branca" ~ 1,
      raça %in% c("Parda", "Preta", "Amarela", "Indígena",
                  "Ignorado") ~ 0
    ),
    desemprego = case_when(
      desemprego == "Pessoas ocupadas" ~ 1,
      desemprego == "Pessoas desocupadas" ~ 0
    )
  ) %>%
  na.omit()

# Multinomial
mpnad_1 <- read_csv("data/PNADC_022012_resumida.csv")

mpnad_2 <- read_csv("data/PNADC_022022_resumida.csv")

mpnad_1 <- mpnad_1 %>%
  select(UF, sexo, rendtrabtot, urbano, horastrabtot,trabalha) %>%
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit() %>%
  sex_change()

summarise(total = n(mpnad_1$trabalha))
# k <- mpnad_1$trabalha %>% as.tibble() %>% group_by(value) %>% summarise(t=n())

mpnad_2 <- mpnad_2 %>%
  dplyr::select(UF, sexo, rendtrabtot, urbano, horastrabtot) %>%
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit() %>%
  sex_change()


po_pnad_1 <- read_csv("data/PNADC_022012_resumida.csv")
po_pnad_2 <- read_csv("data/PNADC_022022_resumida.csv")

po_pnad_1 <- po_pnad_1 %>%
  select(UF, sexo, rendtrabtot, urbano, freqesc,horastrabtot,anosest) %>%
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit() %>%
  sex_change()

po_pnad_2 <- po_pnad_2 %>%
  select(UF, sexo, rendtrabtot, urbano, freqesc,horastrabtot,anosest) %>%
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit() %>%
  sex_change()

contagem <- mpnad_1 %>% 
  dplyr::select(UF,anosest,urbano,idade,sexo,horasoferta,rendtrabtot,nfilho014) %>% 
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit() 

contagem_2<- mpnad_2 %>% 
  dplyr::select(UF,anosest,urbano,idade,sexo,horasoferta,rendtrabtot,nfilho014) %>% 
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit()


# 1.1) Datasets do Logit e Probit --------------------------------------------------

sc_1 <- pnad_1 %>%
  filter(uf == "Santa Catarina") %>%
  dplyr::select(-uf)

plot_bar(msc_1)

ba_1 <- pnad_1 %>%
  filter(uf == "Bahia") %>%
  dplyr::select(-uf)


sc_2 <- pnad_2 %>%
  filter(uf == "Santa Catarina") %>%
  dplyr::select(-uf)

ba_2 <- pnad_2 %>%
  filter(uf == "Bahia") %>%
  dplyr::select(-uf)

summary(sc_1)

summary(ba_1)

summary(sc_2)

summary(ba_2)


sc_1 %>%
  group_by(desemprego, sexo) %>% summarise(total = n())

sc_2 %>%
  group_by(desemprego, sexo) %>% summarise(total = n())

b <- teste %>% 
  group_by(trabalha) %>% summarise(total = n())

ba_1 %>%
  group_by(desemprego, sexo) %>% summarise(total = n())

ba_2 %>%
  group_by(desemprego, sexo) %>% summarise(total = n())


ggpairs(msc_1, ggplot2::aes(colour=sexo))


# 1.2) Analise descritiva -------------------------------------------------

plot_bar(sc_1)

plot_bar(ba_1)

plot_bar(sc_2)

plot_bar(ba_2)


# 2) Modelos --------------------------------------------------------------


# 2.1) Logit --------------------------------------------------------------


logic_sc_1 <- glm(desemprego ~ raça + sexo + local ,
                  family = binomial(link = "logit"),
                  data = sc_1)

logic_ba_1 <- glm(desemprego ~ raça + sexo + local ,
                  family = binomial(link = "logit"),
                  data = ba_1)



logic_sc_2 <- glm(desemprego ~ raça + sexo + local ,
                  family = binomial(link = "logit"),
                  data = sc_2)


logic_ba_2 <- glm(desemprego ~ raça + sexo + local ,
                  family = binomial(link = "logit"),
                  data = ba_2)

summary(logic_sc_1)

summary(logic_ba_1)

summary(logic_sc_2)

summary(logic_ba_2)


# 2.11) Outliers ----------------------------------------------------------

par(mar = rep(2, 4))


plot(logic_sc_1, which = 5)

plot(logic_ba_1, which = 5)

plot(logic_sc_2, which = 5)

plot(logic_ba_2, which = 5)


summary(stdres(logic_sc_1))

summary(stdres(logic_ba_1))

summary(stdres(logic_sc_2))

summary(stdres(logic_ba_2))

# Odds Ratio
odds <- cbind(
  exp(logic_sc_1$coefficients),
  exp(logic_ba_1$coefficients),
  exp(logic_sc_2$coefficients),
  exp(logic_ba_2$coefficients)
) %>%
  as_tibble()



exp(logic_sc_1$coefficients)

exp(logic_ba_1$coefficients)

exp(logic_sc_2$coefficients)

exp(logic_ba_2$coefficients)

# 2.12 Multicolinearidade ---------------------------------

# # Em estatística descritiva, o coeficiente de correlação de Pearson,
# também chamado de "coeficiente de correlação produto-momento" ou
# simplesmente de "ρ de Pearson" mede o grau da correlação entre duas
# variáveis de escala métrica. Este coeficiente, normalmente
# representado por ρ assume apenas valores entre -1 e 1.
# Interresante que ρ < |0.8|, cc, haverá multicolinearidade


pairs.panels(sc_1)
pairs.panels(ba_1)
pairs.panels(sc_2)
pairs.panels(ba_2)

# Todos os resultados sugerem a ausência de multicolinearidade


car::vif(logic_sc_1)

car::vif(logic_ba_1)

car::vif(logic_sc_2)

car::vif(logic_ba_2)

# Em estatística, o fator de inflação de variância é a razão da
# variância de estimar algum parâmetro em um modelo que inclui vários
# outros termos pela variância de um modelo construído usando apenas
# um termo. Ele quantifica a gravidade da multicolinearidade em uma
# análise de regressão de mínimos quadrados ordinários
# Idealmente, VIF < 10 para haver ausência de multicolinearidade




# 2.2) Probit -------------------------------------------------------------


probit_sc_1 <- glm(desemprego ~ raça + sexo + local ,
                   family = binomial(link = "probit"),
                   data = sc_1)

probit_ba_1 <- glm(desemprego ~ raça + sexo + local ,
                   family = binomial(link = "probit"),
                   data = ba_1)



probit_sc_2 <- glm(desemprego ~ raça + sexo + local ,
                   family = binomial(link = "probit"),
                   data = sc_2)


probit_ba_2 <- glm(desemprego ~ raça + sexo + local ,
                   family = binomial(link = "probit"),
                   data = ba_2)

summary(probit_sc_1)

summary(probit_ba_1)

summary(probit_sc_2)

summary(probit_ba_2)

coeftest(probit_sc_1)


# 2.21) Prob ---------------------------------------------------------------

marg_sc_1 <-
  mean(dnorm(predict(probit_sc_1, type = "link"))) * coef(probit_sc_1)


marg_ba_1 <-
  mean(dnorm(predict(probit_ba_1, type = "link"))) * coef(probit_ba_1)


marg_sc_2 <-
  mean(dnorm(predict(probit_sc_2, type = "link"))) * coef(probit_sc_2)


marg_ba_2 <-
  mean(dnorm(predict(probit_ba_2, type = "link"))) * coef(probit_ba_2)


# 3) Modelos Multinomiais -------------------------------------------------


# 3.1) Datasets -----------------------------------------------------------

msc_1 <- mpnad_1 %>%
  filter(uf == "SC") %>%
  dplyr::select(-uf, -UF) %>%
  mutate(renda = ifelse(
    rendtrabtot <= 1000,
    1,
    ifelse(
      rendtrabtot > 1000 & rendtrabtot <= 2500,
      2,
      ifelse(rendtrabtot > 2500, 3, 0)
    )
  )) %>%
  dplyr::select(-rendtrabtot)


mba_1 <- mpnad_1 %>%
  filter(uf == "BA") %>%
  dplyr::select(-uf) %>%
  mutate(renda = ifelse(
    rendtrabtot <= 1000,
    1,
    ifelse(
      rendtrabtot > 1000 & rendtrabtot <= 2500,
      2,
      ifelse(rendtrabtot > 2500, 3, 0)
    )
  ))

msc_2 <- mpnad_2 %>%
  filter(uf == "SC") %>%
  dplyr::select(-uf) %>%
  mutate(renda = ifelse(
    rendtrabtot <= 1000,
    1,
    ifelse(
      rendtrabtot > 1000 & rendtrabtot <= 2500,
      2,
      ifelse(rendtrabtot > 2500, 3, 0)
    )
  ))

mba_2 <- mpnad_2 %>%
  filter(uf == "BA") %>%
  dplyr::select(-uf) %>%
  mutate(renda = ifelse(
    rendtrabtot <= 1000,
    1,
    ifelse(
      rendtrabtot > 1000 & rendtrabtot <= 2500,
      2,
      ifelse(rendtrabtot > 2500, 3, 0)
    )
  ))

# 3.2) Logit --------------------------------------------------------------

m_l_sc1 <-
  mlogit(
    renda ~ 1 | sexo + urbano ,
    data = msc_1,
    shape = "wide",
    reflevel = "1"
  )

m_teste_sc_1 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = msc_1,
  shape = "wide",
  reflevel = "1",
  alt.subset = c("1", "2")
)

m_l_ba1 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_1,
  shape = "wide",
  reflevel = "1"
)

m_teste_ba_1 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_1,
  shape = "wide",
  reflevel = "1",
  alt.subset = c("1", "2")
)

m_l_sc2 <- mlogit(
  renda ~ 1 | sexo + urbano,
  data = msc_2,
  shape = "wide",
  reflevel = "1"
)

m_teste_sc_2 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = msc_2,
  shape = "wide",
  reflevel = "1",
  alt.subset = c("1", "2")
)

m_l_ba2 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_2,
  shape = "wide",
  reflevel = "1"
)

m_teste_ba_2 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_2,
  shape = "wide",
  reflevel = "1",
  alt.subset = c("1", "2")
)



# 3.3) Testes -------------------------------------------------------------

hmftest(m_l_sc1, m_teste_sc_1)
hmftest(m_l_ba1, m_teste_ba_1)
hmftest(m_l_sc2, m_teste_sc_2)
hmftest(m_l_ba2, m_teste_ba_2)

# 3.4) Probit -------------------------------------------------------------

p_l_sc1 <-
  mlogit(
    renda ~ 1 | sexo + urbano ,
    data = msc_1,
    shape = "wide",
    reflevel = "1",
    probit = TRUE
  )

p_teste_sc_1 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = msc_1,
  shape = "wide",
  reflevel = "1",
  probit = TRUE,
  alt.subset = c("1", "2")
)

p_l_ba1 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_1,
  shape = "wide",
  reflevel = "1",
  probit = TRUE
)

p_teste_ba_1 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_1,
  shape = "wide",
  reflevel = "1",
  probit = TRUE,
  alt.subset = c("1", "2")
)

p_l_sc2 <- mlogit(
  renda ~ 1 | sexo + urbano,
  data = msc_2,
  shape = "wide",
  reflevel = "1",
  probit = TRUE
)

p_teste_sc_2 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = msc_2,
  shape = "wide",
  reflevel = "1",
  probit = TRUE,
  alt.subset = c("1", "2")
)

p_l_ba2 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_2,
  shape = "wide",
  reflevel = "1",
  probit = TRUE
)

p_teste_ba_2 <- mlogit(
  renda ~ 1 | sexo + urbano ,
  data = mba_2,
  shape = "wide",
  reflevel = "1",
  probit = TRUE,
  alt.subset = c("1", "2")
)



# 3.5) Testes -------------------------------------------------------------

hmftest(p_l_sc1, p_teste_sc_1)
hmftest(p_l_ba1, p_teste_ba_1)
hmftest(p_l_sc2, p_teste_sc_2)
hmftest(p_l_ba2, p_teste_ba_2)


# 4) Contagem -------------------------------------------------------------

po_sc_1 <- contagem %>%
  filter(uf == "SC") %>% 
  dplyr::select(nfilho014 ,idade  , anosest  , rendtrabtot)

po_sc_2 <- contagem_2 %>%
  filter(uf == "SC") %>% 
  dplyr::select(nfilho014 ,idade  , anosest  , rendtrabtot)

po_ba_1 <- contagem %>%
  filter(uf == "BA") %>% 
  dplyr::select(nfilho014 ,idade  , anosest  , rendtrabtot)

po_ba_2 <- contagem_2 %>%
  filter(uf == "BA") %>% 
  dplyr::select(nfilho014 ,idade  , anosest  , rendtrabtot)

# Analise descritiva

summary(po_sc_1)
mean(po_sc_1$nfilho014)
var(po_sc_1$nfilho014)
plot_histogram(po_sc_1)

summary(po_sc_2)
mean(po_sc_2$nfilho014)
var(po_sc_2$nfilho014)
plot_histogram(po_sc_2)


summary(po_ba_1)
mean(po_ba_1$nfilho014)
var(po_ba_1$nfilho014)
plot_histogram(po_ba_1)


summary(po_ba_2)
mean(po_ba_2$nfilho014)
var(po_ba_2$nfilho014)
plot_histogram(po_ba_2)



# Modelos
#Poisson
mpo_sc_1 <-
  glm(nfilho014 ~ idade  + anosest  + rendtrabtot ,
      data = po_sc_1,
      family = poisson(link = "log"))


summary(mpo_sc_1)
dispersiontest(mpo_sc_1)
plot_density(residuals(mpo_sc_1))
countreg::rootogram(mpo_sc_1)
deviance(mpo_sc_1)

mpo_sc_2 <-
  glm(nfilho014 ~ idade  + anosest  + rendtrabtot ,
      data = po_sc_2,
      family = poisson(link = "log"))


summary(mpo_sc_2)
dispersiontest(mpo_sc_2)
plot_density(residuals(mpo_sc_2))
countreg::rootogram(mpo_sc_2)
deviance(mpo_sc_2)


mpo_ba_1 <-
  glm(nfilho014 ~ idade  + anosest  + rendtrabtot ,
      data = po_ba_1,
      family = poisson(link = "log"))


summary(mpo_ba_1)
dispersiontest(mpo_ba_1)
plot_density(residuals(mpo_ba_1))
countreg::rootogram(mpo_ba_1)
deviance(mpo_ba_1)

mpo_ba_2 <-
  glm(nfilho014 ~ idade  + anosest  + rendtrabtot ,
      data = po_ba_2,
      family = poisson(link = "log"))


summary(mpo_ba_2)
dispersiontest(mpo_ba_2)
plot_density(residuals(mpo_ba_2))
countreg::rootogram(mpo_ba_2)
deviance(mpo_sc_2)




# Binomial negativa

nb_sc_1 <- glm.nb(nfilho014 ~ idade  + anosest  + rendtrabtot ,
                  data = po_sc_1)

summary(nb_sc_1)
plot_density(residuals(nb_sc_1))
countreg::rootogram(nb_sc_1)
deviance(nb_sc_1)



nb_sc_2 <- glm.nb(nfilho014 ~ idade  + anosest  + rendtrabtot ,
                  data = po_sc_2)

summary(nb_sc_2)
plot_density(residuals(nb_sc_2))
countreg::rootogram(nb_sc_2)
deviance(nb_sc_2)



nb_ba_1 <- glm.nb(nfilho014 ~ idade  + anosest  + rendtrabtot ,
                  data = po_ba_1)

summary(nb_ba_1)
plot_density(residuals(nb_ba_1))
countreg::rootogram(nb_ba_1)
deviance(nb_ba_1)



nb_ba_2 <- glm.nb(nfilho014 ~ idade  + anosest  + rendtrabtot ,
                  data = po_ba_2)

summary(nb_ba_2)
plot_density(residuals(nb_ba_2))
countreg::rootogram(nb_ba_2)
deviance(nb_ba_2)
plot(density(resid(nb_ba_1, type='response')))
lines(density(resid(mpo_ba_1, type='response')), col='red')
plot(density(resid(nb_ba_1, type='deviance')))
lines(density(resid(mpo_ba_1, type='deviance')), col='red')

# stargazer::stargazer(nb_sc_1,mpo_sc_1,type = "text")




par(mfrow = c(1, 2))

countreg::rootogram(nb_sc_1);countreg::rootogram(mpo_sc_1)




