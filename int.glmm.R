# intensidade de ocorrência 

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
                                        obs = col_character()))


spec(df_monit)
df_monit


## aplicando os filtros 

dfmonit_filt <- df_monit %>% 
  filter(data > as.Date("2022-01-01") &
           !(obs %in% c("Sem geo", "estimado dos dados do ICMBio", "geo não realizada", "geo não realizada, sem ficha de campo")) &
           !(geo_id %in% c("Na"))) %>%
  arrange(geo_id)

summary(dfmonit_filt)
print(dfmonit_filt)


## selecionando as variáveis importantes

int.monit <- dfmonit_filt[ ,c("localidade", "data", "n_trans_vis", "dafor", "n_divers", "geo_id")] %>%
  group_by(geo_id, localidade, data) %>%
  reframe(total_min = max(n_trans_vis),
          total_dafor = sum(dafor),
          divers = max(n_divers)) %>%
  arrange(geo_id)

print(int.monit, n = 95)

int.monit$geo_id <- as.double(int.monit$geo_id)


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
                                     geo_id = col_double()))

spec(df_geo)
df_geo


## agrupando e obtendo o total do IAR para cada geomorfologia em cada transecto

int.geo <- df_geo[ ,c("localidade", "data", "visibilidade", "geo_cat", "iar_geo", "geo_id")] %>%
  group_by(geo_id, localidade, data, geo_cat) %>%
  reframe(visibilidade = max(visibilidade),
          iar_geo = mean(iar_geo)) %>%
  arrange(geo_id)

print(int.geo, n = 185)


## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

int.geo2 <- int.geo[ ,c("geo_id", "localidade", "visibilidade", "geo_cat", "iar_geo")] %>%
  group_by(geo_id, localidade, geo_cat) %>%
  summarise(t_visib = max(visibilidade),
            iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)

print(int.geo2, n = 81)


# unindo os data frames

int.geomonit <- left_join(int.monit, int.geo2) %>%
  arrange(geo_id) %>%
  drop_na()

print(int.geomonit, n = 79)


# categorizando em faixa a variável visibilidade

t_visib.chr <- cut(int.geomonit$t_visib, breaks = seq(0, max(int.geomonit$t_visib), by = 3), right = TRUE)
levels(t_visib.chr) <- c("0-3", "3-6", "6-9", "9-12", "12-15")
print(t_visib.chr)

int.geomonit$t_visib.chr <- t_visib.chr

int.geomonit <- as.data.frame(int.geomonit)




# generalize linear mixed models

library(lme4)
library(MASS)
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(ggpubr)
library(DHARMa)
library(lattice)


# modelos glmm

## avaliando o efeito aleatório

controle <- glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"),
                           optCtrl=list(iter.max=1e3,eval.max=1e3))

int.model <- glmmTMB(total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(total_min)) + (1|t_visib.chr) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)


#modelocp <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2),
#                    data = geomonit.trans, control=controle, 
#                    family = compois)


int.model.nb1 <- glmmTMB(total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(total_min)) + (1|t_visib.chr) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = nbinom1)

int.model.nb2 <- glmmTMB(total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(total_min)) + (1|t_visib.chr) + (1|localidade),
                         data = int.geomonit, control=controle, 
                         family = nbinom2)

### selecionando o melhor modelo a depender da distribuição

ICtab(int.model, int.model.nb1, int.model.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.model.nb1 <- simulateResiduals(fittedModel=int.model.nb1, n=1000)
windows(12,8)
plot(res.int.model.nb1)

nb1 <- int.model.nb1

### comparando os modelos mais completos com os mais simples

nb1.a <- glmmTMB(total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(total_min)) + (1|t_visib.chr),
                 data = int.geomonit, control=controle, 
                 family = nbinom1)

nb1.b <- glmmTMB(total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(total_min)) + (1|localidade),
                 data = int.geomonit, control=controle, 
                 family = nbinom1)

ICtab(nb1, nb1.a,  nb1.b, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)


res.nb1.a <- simulateResiduals(fittedModel=nb1.a, n=1000)
windows(12,8)
plot(res.nb1.a)

res.nb1.b <- simulateResiduals(fittedModel=nb1.b, n=1000)
windows(12,8)
plot(res.nb1.b)


## avaliando o efeito fixo

nb1.b1 <- glmmTMB(total_dafor  ~ gc + lg + rpm + tf + offset(log(total_min)) + (1|localidade),
                 data = int.geomonit, control=controle, 
                 family = nbinom1)

nb1.b2 <- glmmTMB(total_dafor  ~ mp + lg + rpm + tf + offset(log(total_min)) + (1|localidade),
                 data = int.geomonit, control=controle, 
                 family = nbinom1)

nb1.b3 <- glmmTMB(total_dafor  ~ mp + gc + rpm + tf + offset(log(total_min)) + (1|localidade),
                 data = int.geomonit, control=controle, 
                 family = nbinom1)

nb1.b4 <- glmmTMB(total_dafor  ~ mp + gc + lg + tf + offset(log(total_min)) + (1|localidade),
                 data = int.geomonit, control=controle, 
                 family = nbinom1)

nb1.b5 <- glmmTMB(total_dafor  ~ mp + gc + lg + rpm + offset(log(total_min)) + (1|localidade),
                 data = int.geomonit, control=controle, 
                 family = nbinom1)

ICtab(nb1.b, nb1.b1,  nb1.b2, nb1.b3, nb1.b4, nb1.b5, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)


nb1.b6 <- glmmTMB(total_dafor  ~ mp + rpm + offset(log(total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = nbinom1)

nb1.b7 <- glmmTMB(total_dafor  ~ gc + lg + tf + offset(log(total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = nbinom1)

nb1.b8 <-  glmmTMB(total_dafor  ~ mp + gc + offset(log(total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = nbinom1)

ICtab(nb1.b6, nb1.b7, nb1.b8, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb1.b6 <- simulateResiduals(fittedModel=nb1.b6, n=1000)
windows(12,8)
plot(res.nb1.b6)

par(mfrow = c(1,2))
plotResiduals(res.nb1.b6, int.geomonit$mp)
plotResiduals(res.nb1.b6, int.geomonit$rpm)


res.nb1.b8 <- simulateResiduals(fittedModel=nb1.b8, n=1000)
windows(12,8)
plot(res.nb1.b8)

par(mfrow = c(1,2))
plotResiduals(res.nb1.b8, int.geomonit$mp)
plotResiduals(res.nb1.b8, int.geomonit$gc)

nb1.b6.a <- glmmTMB(total_dafor  ~ mp + offset(log(total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = nbinom1)

nb1.b6.b <- glmmTMB(total_dafor  ~ rpm + offset(log(total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = nbinom1)

ICtab(nb1.b6, nb1.b6.a, nb1.b6.b, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb1.b6.a <- simulateResiduals(fittedModel=nb1.b6.a, n=1000)
windows(12,8)
plot(res.nb1.b6.a)

#pelos residuos, o melhor modelo é o 6

## avaliando a variavel relacionada a inflação por zero

nb1.b6.zi <- glmmTMB(total_dafor  ~ mp + rpm + offset(log(total_min)) + (1|localidade),
                  data = int.geomonit, ziformula = ~divers, control=controle, 
                  family = nbinom1)

ICtab(nb1.b6, nb1.b6.zi, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb1.b6.zi <- simulateResiduals(fittedModel=nb1.b6.zi, n=1000)
windows(12,8)
plot(res.nb1.b6.zi)

int.model <- nb1.b6

summary(int.model)
