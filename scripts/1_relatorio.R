# 0) Packs ----------------------------------------------------------------

source("scripts/packs.R")
library("kdensity")
library("DataExplorer")
library("quantreg")
library("easystats")
library("janitor")
library("readr")
library("performance")
library("ggside")
library("DataExplorer")
library("ggridges")
library("sjPlot")
library("oaxaca")
library("broom.mixed")
library("ggstance")
library("jtools")
library("dineq")

# 1) Data -----------------------------------------------------------------
pnad_1 <- read_csv("data/PNADC_022012_resumida.csv")

pnad_2 <- read_csv("data/PNADC_022022_resumida.csv")

pnad_1 <- pnad_1 %>%
  select(UF, sexo, anosest, horastrabtot, rendtrabtot, idade) %>%
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit()

pnad_2 <- pnad_2 %>%
  select(UF, sexo, anosest, horastrabtot, rendtrabtot, idade) %>%
  mutate(uf = UF) %>%
  cod_uf() %>%
  na.omit()

renda_media_1 <- pnad_1 %>%
  select(uf, rendtrabtot) %>%
  group_by(uf) %>%
  summarise(teste = mean(rendtrabtot))

renda_media_2 <- pnad_2 %>%
  select(uf, rendtrabtot) %>%
  group_by(uf) %>%
  summarise(teste = mean(rendtrabtot))

sp_1 <- pnad_1 %>%
  filter(uf == "SP") %>%
  select(-UF, -uf, -sexo)

sp_2 <- pnad_2 %>%
  filter(uf == "SP") %>%
  select(-UF, -uf, -sexo)

ma_1 <- pnad_1 %>%
  filter(uf == "MA") %>%
  select(-UF, -uf, -sexo)

ma_2 <- pnad_2 %>%
  filter(uf == "MA") %>%
  select(-UF, -uf, -sexo)


# 2) Estatística Descritiva -----------------------------------------------


# 2.1) Scatter Plots ------------------------------------------------------

sp_1_scatter <- plot_scatterplot(sp_1, by = "rendtrabtot",
                                 title = "SP no 2T2012")

sp_2_scatter <- plot_scatterplot(sp_2, by = "rendtrabtot",
                                 title = "SP no 2T2022")

ma_1_scatter <- plot_scatterplot(ma_1, by = "rendtrabtot",
                                 title = "MA no 2T2012")

ma_2_scatter <- plot_scatterplot(ma_2, by = "rendtrabtot",
                                 title = "MA no 2T2022")


# 2.2) Histograma ---------------------------------------------------------

sp_1_hist <- plot_histogram(sp_1,
                            title = "SP no 2T2012")

sp_2_hist <- plot_histogram(sp_2,
                            title = "SP no 2T2022")

ma_1_hist <- plot_histogram(ma_1,
                            title = "MA no 2T2012")

ma_2_hist <- plot_histogram(ma_2,
                            title = "MA no 2T2022")


# 2.2) Kernels ------------------------------------------------------------


# 2.2.1) SP ---------------------------------------------------------------


renda_sp_1 <- sp_1 %>%
  filter(rendtrabtot <= 5000)

renda_sp_2 <- sp_2 %>%
  filter(rendtrabtot <= 5000)


kernels = eval(formals(density.default)$kernel)


plot (density(renda_sp_1$rendtrabtot),
      xlab = "",
      main = "Kernels da Renda em SP no 2T12")

for (i in 2:length(kernels))
  lines(density(renda_sp_1$rendtrabtot, kernel =  kernels[i]), col = i)

plot (density(renda_sp_2$rendtrabtot),
      xlab = "",
      main = "Densidade da Renda em SP no 2T22")

for (i in 2:length(kernels))
  lines(density(renda_sp_2$rendtrabtot, kernel =  kernels[i]), col = i)


# 2.2.2) MA ---------------------------------------------------------------

renda_ma_1 <- ma_1 %>%
  filter(rendtrabtot <= 5000)

renda_ma_2 <- ma_2 %>%
  filter(rendtrabtot <= 5000)


kernels = eval(formals(density.default)$kernel)


plot (density(renda_ma_1$rendtrabtot),
      xlab = "",
      main = "Densidade da Renda no MA no 2T12")

for (i in 2:length(kernels))
  lines(density(renda_ma_1$rendtrabtot, kernel =  kernels[i]), col = i)

plot (density(renda_ma_2$rendtrabtot),
      xlab = "",
      main = "Kernels da Renda no MA no 2T22")

for (i in 2:length(kernels))
  lines(density(renda_ma_2$rendtrabtot, kernel =  kernels[i]), col = i)



# 3) Regressão Quantílica -------------------------------------------------


# 3.1.1) SP ---------------------------------------------------------------

model_sp_1_05 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_sp_1)

model_sp_1_01 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_1,
     tau = 0.1)

model_sp_1_75 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_1,
     tau = 0.75)

model_sp_1_25 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_1,
     tau = 0.25)

model_sp_1_09 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_1,
     tau = 0.9)

model_sp_1_lm <-
  lm(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_sp_1)

coef(model_sp_1_01)
coef(model_sp_1_25)
coef(model_sp_1_05)
coef(model_sp_1_75)
coef(model_sp_1_09)
coef(model_sp_1_lm)

summary(model_sp_1_lm)

jtools::plot_coefs(
  model_sp_1_01,
  model_sp_1_25,
  model_sp_1_05,
  model_sp_1_75,
  model_sp_1_09,
  model_sp_1_lm,
  model.names = c(
    "Quantil 0.10",
    "Quantil 0.25",
    "Quantil 0.50",
    "Quantil 0.75",
    "Quantil 0.90",
    "OLS"
  ),
  legend.title = "Parâmetros Estimados"
)

model_sp_2_05 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_sp_2)

model_sp_2_01 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_2,
     tau = 0.1)

model_sp_2_75 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_2,
     tau = 0.75)

model_sp_2_25 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_2,
     tau = 0.25)

model_sp_2_09 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_sp_2,
     tau = 0.9)

model_sp_2_lm <-
  lm(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_sp_2)



coef(model_sp_2_01)
coef(model_sp_2_25)
coef(model_sp_2_05)
coef(model_sp_2_75)
coef(model_sp_2_09)
coef(model_sp_2_lm)


jtools::plot_coefs(
  model_sp_2_01,
  model_sp_2_25,
  model_sp_2_05,
  model_sp_2_75,
  model_sp_2_09,
  model_sp_2_lm,
  model.names = c(
    "Quantil 0.10",
    "Quantil 0.25",
    "Quantil 0.50",
    "Quantil 0.75",
    "Quantil 0.90",
    "OLS"
  ),
  legend.title = "Parâmetros Estimados"
)

# 3.1.2) MA ---------------------------------------------------------------

model_ma_1_05 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_ma_1)

model_ma_1_01 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_1,
     tau = 0.1)

model_ma_1_75 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_1,
     tau = 0.75)

model_ma_1_25 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_1,
     tau = 0.25)

model_ma_1_09 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_1,
     tau = 0.9)

model_ma_1_lm <-
  lm(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_ma_1)


coef(model_ma_1_01)
coef(model_ma_1_25)
coef(model_ma_1_05)
coef(model_ma_1_75)
coef(model_ma_1_09)
coef(model_ma_1_lm)

jtools::plot_coefs(
  model_ma_1_01,
  model_ma_1_25,
  model_ma_1_05,
  model_ma_1_75,
  model_ma_1_09,
  model_ma_1_lm,
  model.names = c(
    "Quantil 0.10",
    "Quantil 0.25",
    "Quantil 0.50",
    "Quantil 0.75",
    "Quantil 0.90",
    "OLS"
  ),
  legend.title = "Parâmetros Estimados"
)


model_ma_2_05 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_ma_2)

model_ma_2_01 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_2,
     tau = 0.1)

model_ma_2_75 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_2,
     tau = 0.75)

model_ma_2_25 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_2,
     tau = 0.25)


model_ma_2_09 <-
  rq(rendtrabtot ~ horastrabtot + idade + anosest,
     data = renda_ma_2,
     tau = 0.9)

model_ma_2_lm <-
  lm(rendtrabtot ~ horastrabtot + idade + anosest, data = renda_ma_2)

coef(model_ma_2_01)
coef(model_ma_2_25)
coef(model_ma_2_05)
coef(model_ma_2_75)
coef(model_ma_2_09)
coef(model_ma_2_lm)

jtools::plot_coefs(
  model_ma_2_01,
  model_ma_2_25,
  model_ma_2_05,
  model_ma_2_75,
  model_ma_2_09,
  model_ma_2_lm,
  model.names = c(
    "Quantil 0.10",
    "Quantil 0.25",
    "Quantil 0.50",
    "Quantil 0.75",
    "Quantil 0.90",
    "OLS"
  ),
  legend.title = "Parâmetros Estimados"
)

# m <-
#   ggplot(renda_ma_2, aes(horastrabtot + idade + anosest, rendtrabtot)) +
#   geom_point()
#
# m + geom_quantile()
#
#
# q10 <- seq(0.05, 0.95, by = 0.05)
# m + geom_quantile(quantiles = q10)


# 4) Decomposição ---------------------------------------------------------


# 4.1) Oaxaca Blinder -----------------------------------------------------

data_1 <- pnad_1 %>%
  filter(uf %in% c ("MA", "SP")) %>%
  mutate(z = ifelse(uf == "SP", 0,
                    ifelse(uf == "MA", 1, "no"))) %>% 
  select(-sexo)

data_2 <- pnad_2 %>%
  filter(uf %in% c ("MA", "SP")) %>%
  mutate(z = ifelse(uf == "SP", 0,
                    ifelse(uf == "MA", 1, "no"))) %>% 
  select(-sexo)

oaxaca_1 <-
  oaxaca(formula = rendtrabtot ~ horastrabtot + idade + anosest | z,
         data = data_1)


oaxaca_2 <-
  oaxaca(formula = rendtrabtot ~ horastrabtot + idade + anosest | z,
         data = data_2)

plot(oaxaca_1, decomposition = "twofold", group.weight = 1)
plot(oaxaca_2, decomposition = "twofold", group.weight = 1)



# 4.2) RIF ----------------------------------------------------------------

## SP 2012
rif_sp_01 <-
  rifrSE(
   rendtrabtot ~ horastrabtot + idade + anosest,
   data = sp_1,
    method = "quantile",
    quantile = 0.1,
    kernel = "gaussian"
  )

rif_sp_25 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_1,
    method = "quantile",
    quantile = 0.25,
    kernel = "gaussian"
  )

rif_sp_50 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_1,
    method = "quantile",
    quantile = 0.5,
    kernel = "gaussian"
  )

rif_sp_75 <-
    rifrSE(
      rendtrabtot ~ horastrabtot + idade + anosest,
      data = sp_1,
      method = "quantile",
      quantile = 0.75,
      kernel = "gaussian"
    )

rif_sp_90 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_1,
    method = "quantile",
    quantile = 0.9,
    kernel = "gaussian"
  )


## SP 2022
rif_sp_01_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_2,
    method = "quantile",
    quantile = 0.1,
    kernel = "gaussian"
  )

rif_sp_25_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_2,
    method = "quantile",
    quantile = 0.25,
    kernel = "gaussian"
  )

rif_sp_50_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_2,
    method = "quantile",
    quantile = 0.5,
    kernel = "gaussian"
  )

rif_sp_75_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_2,
    method = "quantile",
    quantile = 0.75,
    kernel = "gaussian"
  )

rif_sp_90_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = sp_2,
    method = "quantile",
    quantile = 0.9,
    kernel = "gaussian"
  )

## MA 2012
rif_ma_01 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_1,
    method = "quantile",
    quantile = 0.1,
    kernel = "gaussian"
  )

rif_ma_25 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_1,
    method = "quantile",
    quantile = 0.25,
    kernel = "gaussian"
  )

rif_ma_50 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_1,
    method = "quantile",
    quantile = 0.5,
    kernel = "gaussian"
  )

rif_ma_75 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_1,
    method = "quantile",
    quantile = 0.75,
    kernel = "gaussian"
  )

rif_ma_90 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_1,
    method = "quantile",
    quantile = 0.9,
    kernel = "gaussian"
  )


## MA 2022
rif_ma_01_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_2,
    method = "quantile",
    quantile = 0.1,
    kernel = "gaussian"
  )

rif_ma_25_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_2,
    method = "quantile",
    quantile = 0.25,
    kernel = "gaussian"
  )

rif_ma_50_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_2,
    method = "quantile",
    quantile = 0.5,
    kernel = "gaussian"
  )

rif_ma_75_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_2,
    method = "quantile",
    quantile = 0.75,
    kernel = "gaussian"
  )

rif_ma_90_22 <-
  rifrSE(
    rendtrabtot ~ horastrabtot + idade + anosest,
    data = ma_2,
    method = "quantile",
    quantile = 0.9,
    kernel = "gaussian"
  )


