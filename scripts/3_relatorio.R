# 0) Packs ----------------------------------------------------------------

source("scripts/packs.R")
library(DataExplorer)
library("VGAM")
library(AER)
library("GGally")
library(lmtest)
library(quantreg)
library("mhurdle")
library(stargazer)
library(pscl)
library(countreg)
library(censReg)
library("sampleSelection")
# 1) Dados ----------------------------------------------------------------

pnad_1 <- read_csv("data/PNADC_022012_resumida.csv")
pnad_2 <- read_csv("data/PNADC_022022_resumida.csv")


#Datasets
#Tobit
t_sp_1 <- pnad_1 %>% 
  dplyr::select(UF,horastrab,sexo,anosest,idade) %>%
  clean_names() %>% 
  cod_uf() %>% 
  sex_change() %>% 
  filter(uf == "SP") %>% 
  dplyr::select(-uf)
  
t_pi_1 <- pnad_1 %>% 
dplyr::select(UF,horastrab,sexo,anosest,idade) %>%
  clean_names() %>% 
  cod_uf() %>% 
  sex_change() %>% 
  filter(uf == "PI") %>% 
  dplyr::select(-uf)


t_sp_2 <- pnad_2 %>% 
dplyr::select(UF,horastrab,sexo,anosest,idade) %>%
  clean_names() %>% 
  cod_uf() %>% 
  sex_change() %>% 
  filter(uf == "SP") %>% 
  dplyr::select(-uf)

t_pi_2 <- pnad_2 %>% 
dplyr::select(UF,horastrab,sexo,anosest,idade) %>%
  clean_names() %>% 
  cod_uf() %>% 
  sex_change() %>% 
  filter(uf == "PI") %>% 
  dplyr::select(-uf)

# 1.1) Analise Descritiva -------------------------------------------------

#2012
summary(t_sp_1)

plot_missing(t_sp_1)

ggpairs(t_sp_1, ggplot2::aes(colour=sexo))


summary(t_pi_1)

plot_missing(t_pi_1)

ggpairs(t_pi_1, ggplot2::aes(colour=sexo))


#2022
summary(t_sp_2)

plot_missing(t_sp_2)

plot_density(t_sp_2)

plot_histogram(t_sp_2)

ggpairs(t_sp_2, ggplot2::aes(colour=sexo))


summary(t_pi_2)

plot_missing(t_pi_2)

plot_density(t_pi_2)

plot_histogram(t_pi_2)

ggpairs(t_pi_2, ggplot2::aes(colour=sexo))



# 3) Tobit ----------------------------------------------------------------

#2012
# SP
tobit_sp_1 <- tobit(horastrab ~ sexo + idade + anosest,
                   data = t_sp_1)

teste <- tobit(horastrab ~  idade + anosest,
               data = sp_t)

sp_t <- t_sp_1 %>% filter(sexo == "feminino") %>% dplyr::select(-sexo)

summary(tobit_sp_1)
# plot(fitted(tobit_sp_1), residuals(tobit_sp_1))
residuals(tobit_sp_1) %>% plot()
mean(residuals(tobit_sp_1))
plot_density(tobit_sp_1$var)
plot_density(residuals(tobit_sp_1))
bptest(tobit_sp_1)
plot_histogram(residuals(tobit_sp_1))
# PI
tobit_pi_1 <- tobit(horastrab ~ sexo + anosest,
                    data = t_pi_1)

summary(tobit_pi_1)
# plot(fitted(tobit_pi_1), residuals(tobit_pi_1))
plot_density(residuals(tobit_pi_1))
residuals(tobit_pi_1) %>% plot()
mean(residuals(tobit_pi_1))


#2022
# SP
tobit_sp_2 <- tobit(horastrab ~ sexo + anosest,
                    data = t_sp_2)

summary(tobit_sp_2)
# plot(fitted(tobit_sp_2), residuals(tobit_sp_2))
residuals(tobit_sp_2) %>% plot()
mean(residuals(tobit_sp_2))
plot_density(residuals(tobit_sp_2))


# PI
tobit_pi_2 <- tobit(horastrab ~ sexo + anosest,
                    data = t_pi_2)

summary(tobit_pi_2)
# plot(fitted(tobit_pi_2), residuals(tobit_pi_2))
plot_density(residuals(tobit_pi_2))
residuals(tobit_pi_2) %>% plot()
mean(residuals(tobit_pi_2))


# 4) Hurdle ------------------------------------------------------------------

h_sp_1 <- t_sp_1 %>% 
  mutate_at(vars(horastrab), ~replace(., is.na(.), 0))
  
plot_density(h_sp_1$horastrab)

d_sp_1 <- hurdle(horastrab ~ sexo + idade + anosest,
                 data = h_sp_1)
summary(d_sp_1)
rootogram(d_sp_1)
sum(predict(d_sp_1, type = "prob")[,1]) # numero de zeros
predict(d_sp_1, type = "response")[1:5]
# ratio of non-zero probabilities
predict(d_sp_1, type = "zero")[1:5]


# 5) Heckman --------------------------------------------------------------

#datasets

h_ma_1 <- pnad_1 %>% 
  dplyr::select(UF,rendtrabprinc,idade,anosest,nfilhotot) %>% 
  clean_names() %>% 
  mutate(renda = rendtrabprinc) %>% 
  cod_uf() %>% 
  filter(uf == "MA") %>% 
  dplyr::select(-1) %>% 
  mutate_at(vars(rendtrabprinc), ~ replace(., is.na(.), 0)) %>% 
  mutate(trab = ifelse(rendtrabprinc == 0, 0,1)) %>% 
  dplyr::select(-rendtrabprinc)

summary(h_df_2)
plot_histogram(h_ma_1)

# Modelos

ols_ma_1 <- lm(log(renda) ~ idade + anosest  ,
               data = h_ma_1)

heck_ma_1 <- selection(trab ~ idade + anosest + nfilhotot,
                       log(renda) ~ idade + anosest ,
                       method = "2step",
                       data = h_ma_1)
summary(heck_ma_1)

stargazer(heck_df_1, heck_ma_1, type = "text",
          column.labels = c("lab 1", "lab 2e"))


