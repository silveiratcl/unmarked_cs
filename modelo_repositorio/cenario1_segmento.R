# cenario 2
# segmento como unidade amostral

library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(vegan)
library(labdsv)

# monitoramento 

df_monit <- read_delim("data/dados_monitoramento_cs_2024-03-22.csv",
                       col_types = list(localidade = col_character(),
                                        data = col_date(format = "%d/%m/%Y"),
                                        visib_horiz = col_double(),
                                        faixa_bat = col_character(),
                                        prof_interface_min = col_double(),
                                        prof_interface_max = col_double(),
                                        metodo = col_character(),
                                        observer = col_character(),
                                        n_divers = col_double(),
                                        tempo_censo = col_double(),
                                        dafor = col_double(),
                                        iar_medio = col_double(),
                                        n_trans_vis = col_double(),
                                        n_trans_pres = col_double(),
                                        dafor_id = col_double(),
                                        geo_id = col_character(),
                                        obs = col_character()
                       ))



spec(df_monit)
df_monit
print(df_monit, n = 100)
df_monit[, 1:17]


## aplicando os filtros 

dfmonit_filt <- df_monit %>% 
  filter(data > as.Date("2022-01-01") &
           !(obs %in% c("Sem geo", "estimado dos dados do ICMBio", "geo não realizada", "geo não realizada, sem ficha de campo")) &
           !(geo_id %in% c("Na"))) %>%
  arrange(geo_id)

summary(dfmonit_filt)
print(dfmonit_filt)

## selecionando as variáveis importantes

monit.trans <- dfmonit_filt[ ,c("localidade", "data", "faixa_bat", "n_trans_vis", "n_trans_pres", "n_divers", "geo_id")] %>%
  group_by(geo_id, localidade, data, faixa_bat) %>%
  reframe(trans_vis = max(n_trans_vis),
          detections = max(n_trans_pres),
          divers = max(n_divers)) %>%
  arrange(geo_id)

print(monit.trans, n = 95)

## criando a variável minutos por mergulhador

monit.trans2 <- monit.trans %>%
  group_by(geo_id, data, localidade, faixa_bat) %>%
  reframe(t_trans_vis = sum(trans_vis),
          t_detections = sum(detections),
          t_divers = sum(divers),
          min.div = sum(t_trans_vis*t_divers)) %>%
  arrange(data)

print(monit.trans2, n = 114)

monit.trans2$geo_id <- as.double(monit.trans2$geo_id)


# geomorfologia

df_geo = read_delim("data/dados_geo_cs_2024-03-22.csv", 
                    col_types = list(localidade = col_character(),
                                     data = col_date(format = "%d/%m/%Y"),
                                     visibilidade = col_double(),
                                     faixa_bat = col_character(),
                                     prof_interface_min = col_double(),
                                     prof_interface_max = col_double(),
                                     metodo = col_character(),
                                     observador = col_character(),
                                     tempo_geo = col_double(),
                                     geo_cat = col_character(),
                                     iar_geo = col_double(),
                                     n_trans_vis = col_double(),
                                     geo_id = col_double())
)
spec(df_geo)
df_geo = df_geo[, 1:14]
df_geo

## agrupando e obtendo o total do IAR para cada geomorfologia em cada segmento

geo.seg <- df_geo[ ,c("localidade", "data", "faixa_bat", "visibilidade", "geo_cat", "iar_geo", "geo_id")] %>%
  group_by(geo_id, localidade, data, faixa_bat, geo_cat) %>%
  reframe(visibilidade = max(visibilidade),
          iar_geo = mean(iar_geo)) %>%
  arrange(geo_id)

print(geo.seg, n = 405)

## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

geo.seg2 <- geo.seg[ ,c("geo_id", "localidade", "faixa_bat", "visibilidade", "geo_cat", "iar_geo")] %>%
  group_by(geo_id, localidade, faixa_bat, geo_cat) %>%
  reframe(t_visib = max(visibilidade),
          iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)

print(geo.seg2, n = 81)


# unindo os data frames

geomonit.seg <- left_join(monit.trans2, geo.seg2) %>%
  arrange(geo_id) %>%
  drop_na()

print(geomonit.seg, n = 69)


# categorizando em faixa a variável minutos por mergulhador

max(geomonit.seg$min.div)
min(geomonit.seg$min.div)

min.div2 <- cut(geomonit.seg$min.div, breaks = seq(0, max(geomonit.seg$min.div), by = 5), right = TRUE)
levels(min.div2) <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50",
                      "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100",
                      "1000-105", "105-110", "110-115", "115-120", "120-125", "125-130", "130-135", "135-140",
                      "140-145", "145-150", "150-155", "155-160", "160-165", "165-170", "170-175", "175-180",
                      "180-185", "185-190", "190-195", "195-200", "200-205", "205-210", "210-215", "215-220",
                      "220-225", "225-230", "230-235", "235-240")
print(min.div2)

geomonit.seg$min.div2 <- min.div2

# categorizando em faixa a variável visibilidade

max(geomonit.seg$t_visib)
min(geomonit.seg$t_visib)

t_visib2 <- cut(geomonit.seg$t_visib, breaks = seq(0, max(geomonit.seg$t_visib), by = 3), right = TRUE)
levels(t_visib2) <- c("0-3", "3-6", "6-9", "9-12", "12-15")
print(t_visib2)

geomonit.seg$t_visib2 <- t_visib2

geomonit.seg <- as.data.frame(geomonit.seg)


# modelos glmm

library(lme4)
library(MASS)
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(ggpubr)
library(DHARMa)
library(lattice)

## avaliando a familia de distribuicao

controle <- glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"),
                           optCtrl=list(iter.max=1e3,eval.max=1e3))

modelo <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2) + (1|localidade),
                  data = geomonit.seg, control=controle, 
                  family = poisson)

modelo.gp <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2)+ (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = genpois)

modelo.nb1 <- glmmTMB(t_detections ~mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2) + (1|localidade),
                      data = geomonit.seg, control=controle,
                      family = nbinom1)

modelo.nb2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2)+ (1|localidade),
                      data = geomonit.seg, control=controle,
                      family = nbinom2)

### selecionando o melhor modelo

ICtab(modelo, modelo.gp, modelo.nb1, modelo.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo <- simulateResiduals(fittedModel=modelo, n=1000)
windows(12,8)
plot(res.modelo)


## comparando os efeitos aleatorios

modelo1 <-glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2),
                  data = geomonit.seg, control=controle, 
                  family = poisson)

modelo2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|localidade),
                   data = geomonit.seg, control=controle, 
                   family = poisson)

modelo3 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|min.div2) + (1|localidade),
                   data = geomonit.seg, control=controle, 
                   family = poisson)

ICtab(modelo, modelo1,  modelo2, modelo3, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo2 <- simulateResiduals(fittedModel=modelo2, n=1000)
windows(12,8)
plot(res.modelo2)

res.modelo3 <- simulateResiduals(fittedModel=modelo3, n=1000)
windows(12,8)
plot(res.modelo3)

modelo2a <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2),
                    data = geomonit.seg, control=controle, 
                    family = poisson)

modelo2b <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|localidade),
                    data = geomonit.seg, control=controle, 
                    family = poisson)

ICtab(modelo2, modelo2a, modelo2b, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo2b <- simulateResiduals(fittedModel=modelo2b, n=1000)
windows(12,8)
plot(res.modelo2b)

## avaliando o efeito fixo

modelo2.a <- glmmTMB(t_detections ~ gc + lg + rpm + tf + (1|t_visib2) + (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = poisson)

modelo2.b <- glmmTMB(t_detections ~ mp + lg + rpm + tf + (1|t_visib2) + (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = poisson)

modelo2.c <- glmmTMB(t_detections ~ mp + gc + rpm + tf + (1|t_visib2) + (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = poisson)

modelo2.d <- glmmTMB(t_detections ~ mp + gc + lg + tf + (1|t_visib2) + (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = poisson)

modelo2.e <- glmmTMB(t_detections ~ mp + gc + lg + rpm + (1|t_visib2) + (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = poisson)

ICtab(modelo2, modelo2.a, modelo2.b, modelo2.c, modelo2.d, modelo2.e, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo2.c <- simulateResiduals(fittedModel=modelo2.c, n=1000)
windows(12,8)
plot(res.modelo2.c)

res.modelo2.e <- simulateResiduals(fittedModel=modelo2.e, n=1000)
windows(12,8)
plot(res.modelo2.e)

res.modelo2.d <- simulateResiduals(fittedModel=modelo2.d, n=1000)
windows(12,8)
plot(res.modelo2.d)

modelo2.c1 <- glmmTMB(t_detections ~ gc + rpm + tf + (1|t_visib2) + (1|localidade),
                      data = geomonit.seg, control=controle, 
                      family = poisson)

modelo2.c2 <- glmmTMB(t_detections ~ mp + rpm + tf + (1|t_visib2) + (1|localidade),
                      data = geomonit.seg, control=controle, 
                      family = poisson)

modelo2.c3 <- glmmTMB(t_detections ~ mp + gc + tf + (1|t_visib2) + (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = poisson)

modelo2.c4 <- glmmTMB(t_detections ~ mp + gc + rpm + (1|t_visib2) + (1|localidade),
                     data = geomonit.seg, control=controle, 
                     family = poisson)

ICtab(modelo2.c, modelo2.c1, modelo2.c2, modelo2.c3, modelo2.c4, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo2.c4 <- simulateResiduals(fittedModel=modelo2.c4, n=1000)
windows(12,8)
plot(res.modelo2.c4)

res.modelo2.c3 <- simulateResiduals(fittedModel=modelo2.c3, n=1000)
windows(12,8)
plot(res.modelo2.c3)

modelo2.c3a <- glmmTMB(t_detections ~ gc + tf + (1|t_visib2) + (1|localidade),
                      data = geomonit.seg, control=controle, 
                      family = poisson)

modelo2.c3b <- glmmTMB(t_detections ~ mp + tf + (1|t_visib2) + (1|localidade),
                      data = geomonit.seg, control=controle, 
                      family = poisson)

modelo2.c3c <- glmmTMB(t_detections ~ mp + gc + (1|t_visib2) + (1|localidade),
                      data = geomonit.seg, control=controle, 
                      family = poisson)

ICtab(modelo2.c3, modelo2.c3a, modelo2.c3b, modelo2.c3c, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo2.c3c <- simulateResiduals(fittedModel=modelo2.c3c, n=1000)
windows(12,8)
plot(res.modelo2.c3c)

modelo2.c3d <- glmmTMB(t_detections ~ gc + (1|t_visib2) + (1|localidade),
                       data = geomonit.seg, control=controle, 
                       family = poisson)

modelo2.c3e <- glmmTMB(t_detections ~ mp + (1|t_visib2) + (1|localidade),
                       data = geomonit.seg, control=controle, 
                       family = poisson)

ICtab(modelo2.c3c, modelo2.c3d, modelo2.c3e, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

## avaliando as variaveis relacionadas a inflacao por zero

modelo2.c3c.zi <- glmmTMB(t_detections ~ mp + gc + (1|t_visib2) + (1|localidade),
                          data = geomonit.seg, ziformula = ~t_trans_vis + t_divers,
                          control=controle, family = poisson)

modelo2.c3c.zi1 <- glmmTMB(t_detections ~ mp + gc + (1|t_visib2) + (1|localidade),
                           data = geomonit.seg, ziformula = ~t_trans_vis,
                           control=controle, family = poisson)
###erro de overparametrization

modelo2.c3c.zi2 <- glmmTMB(t_detections ~ mp + gc + (1|t_visib2) + (1|localidade),
                           data = geomonit.seg, ziformula = ~t_divers,
                           control=controle, family = poisson)
###erro de overparametrization

## comparando o modelo zi com o modelo final

ICtab(modelo2.c3c, modelo2.c3c.zi, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo2.c3c.zi <- simulateResiduals(fittedModel=modelo2.c3c.zi, n=1000)
windows(12,8)
plot(res.modelo2.c3c.zi)


# modelo final

segment.glmm <- modelo2.c3c <- glmmTMB(t_detections ~ mp + gc + (1|t_visib2) + (1|localidade),
                                       data = geomonit.seg, control=controle, 
                                       family = poisson)
summary(segment.glmm)


# extraindo o predict do modelo

data(sleepstudy, package = "lme4")

pred <- predict(segment.glmm, type = "response")

model_pred <- as.numeric(pred)

# inserindo no data frame

geomonit.seg$model_pred <- model_pred

arrange(geomonit.seg, -(model_pred))
