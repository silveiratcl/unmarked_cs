# cenario 2
# intensidade de ocorrência 
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
                                        obs = col_character()))


## aplicando os filtros 

dfmonit_filt <- df_monit %>% 
  filter(data > as.Date("2022-01-01") &
           !(obs %in% c("estimado dos dados do ICMBio"))) %>%
  arrange(geo_id)


## selecionando as variáveis importantes

int.monit <- dfmonit_filt[ ,c("localidade", "data", "n_trans_vis", "dafor", "n_divers", "geo_id")] %>%
  group_by(geo_id, localidade, data) %>%
  reframe(total_min = max(n_trans_vis),
          total_dafor = sum(dafor),
          divers = max(n_divers)) %>%
  arrange(geo_id)


int.monit2 <- int.monit[ ,c("localidade", "total_min", "total_dafor", "divers")] %>%
  group_by(localidade) %>%
  reframe(t.total_min = sum(total_min),
          t.total_dafor = sum(total_dafor),
          t.divers = sum(divers))

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


## agrupando e obtendo o total do IAR para cada geomorfologia em cada segmento

int.geo <- df_geo[ ,c("localidade", "data", "visibilidade", "geo_cat", "iar_geo", "geo_id")] %>%
  group_by(geo_id, localidade, data, geo_cat) %>%
  reframe(visibilidade = max(visibilidade),
          iar_geo = mean(iar_geo)) %>%
  arrange(geo_id)


## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

int.geo2 <- int.geo[ ,c("localidade", "visibilidade", "geo_cat", "iar_geo")] %>%
  group_by(localidade, geo_cat) %>%
  summarise(t_visib = mean(visibilidade),
            iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)


# unindo os data frames

int.geomonit <- left_join(int.monit2, int.geo2) %>%
  drop_na()


# categorizando em faixa a variável visibilidade

max(int.geomonit$t_visib)
min(int.geomonit$t_visib)

t_visib.chr <- cut(int.geomonit$t_visib, breaks = seq(0, max(int.geomonit$t_visib), by = 3), right = TRUE)
levels(t_visib.chr) <- c("0-3", "3-6", "6-9", "9-12", "12-15")
print(t_visib.chr)

int.geomonit$t_visib.chr <- t_visib.chr

int.geomonit <- as_tibble(int.geomonit)
print(int.geomonit, n = 38)


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

int.model <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|t_visib.chr) + (1|localidade),
                     data = int.geomonit, control=controle, 
                     family = poisson)

int.model.gp <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|t_visib.chr) + (1|localidade),
                        data = int.geomonit, control=controle,                      
                        family = genpois)

int.model.nb1 <- glmmTMB(t.total_dafor ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|t_visib.chr) + (1|localidade),
                         data = int.geomonit, control=controle, 
                         family = nbinom1)

int.model.nb2 <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|t_visib.chr) + (1|localidade),
                         data = int.geomonit, control=controle, 
                         family = nbinom2)

### selecionando o melhor modelo

ICtab(int.model, int.model.gp, int.model.nb1, int.model.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.model.gp <- simulateResiduals(fittedModel=int.model.gp, n=1000)
windows(12,8)
plot(res.int.model.gp)

res.int.model <- simulateResiduals(fittedModel=int.model, n=1000)
windows(12,8)
plot(res.int.model)


## comparando os efeitos aleatórios

int.a <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|localidade),
                 data = int.geomonit, control=controle, 
                 family = poisson)

int.b <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|t_visib.chr),
                 data = int.geomonit, control=controle, 
                 family = poisson)

int.c <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)),
                 data = int.geomonit, control=controle, 
                 family = poisson)

ICtab(int.model, int.a, int.b, int.c, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)


res.int.a <- simulateResiduals(fittedModel=int.a, n=1000)
windows(12,8)
plot(res.int.a)


## avaliando o efeito fixo

int.a1 <- glmmTMB(t.total_dafor  ~ gc + lg + rpm + tf + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

int.a2 <- glmmTMB(t.total_dafor  ~ mp + lg + rpm + tf + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

int.a3 <- glmmTMB(t.total_dafor  ~ mp + gc + rpm + tf + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

int.a4 <- glmmTMB(t.total_dafor  ~ mp + gc + lg + tf + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

int.a5 <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

ICtab(int.a, int.a1,  int.a2, int.a3, int.a4, int.a5, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.a4 <- simulateResiduals(fittedModel=int.a4, n=1000)
windows(12,8)
plot(res.int.a4)

res.int.a2 <- simulateResiduals(fittedModel=int.a2, n=1000)
windows(12,8)
plot(res.int.a2)

res.int.a5 <- simulateResiduals(fittedModel=int.a5, n=1000)
windows(12,8)
plot(res.int.a5)

int.a4a <- glmmTMB(t.total_dafor  ~ gc + lg + tf + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

int.a4b <- glmmTMB(t.total_dafor  ~ mp + lg + tf + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

int.a4c <- glmmTMB(t.total_dafor  ~ mp + gc + tf + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

int.a4d <- glmmTMB(t.total_dafor  ~ mp + gc + lg + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

ICtab(int.a4, int.a4a, int.a4b, int.a4c, int.a4d, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.a4b <- simulateResiduals(fittedModel=int.a4b, n=1000)
windows(12,8)
plot(res.int.a4b)

res.int.a4d <- simulateResiduals(fittedModel=int.a4d, n=1000)
windows(12,8)
plot(res.int.a4d)

int.a4d1 <- glmmTMB(t.total_dafor  ~ gc + lg + offset(log(t.total_min)) + (1|localidade),
                    data = int.geomonit, control=controle, 
                    family = poisson)

int.a4d2 <- glmmTMB(t.total_dafor  ~ mp + lg + offset(log(t.total_min)) + (1|localidade),
                    data = int.geomonit, control=controle, 
                    family = poisson)

int.a4d3 <- glmmTMB(t.total_dafor  ~ mp + gc + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

ICtab(int.a4d, int.a4d1, int.a4d2, int.a4d3, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.a4d2 <- simulateResiduals(fittedModel=int.a4d2, n=1000)
windows(12,8)
plot(res.int.a4d2)

int.a4da <- glmmTMB(t.total_dafor  ~ mp + offset(log(t.total_min)) + (1|localidade),
                      data = int.geomonit, control=controle, 
                      family = poisson)

int.a4db <- glmmTMB(t.total_dafor  ~ gc + offset(log(t.total_min)) + (1|localidade),
                      data = int.geomonit, control=controle, 
                      family = poisson)

int.a4dc <- glmmTMB(t.total_dafor  ~ lg + offset(log(t.total_min)) + (1|localidade),
                      data = int.geomonit, control=controle, 
                      family = poisson)

ICtab(int.a4d, int.a4da, int.a4db, int.a4dc, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.a4db <- simulateResiduals(fittedModel=int.a4db, n=1000)
windows(12,8)
plot(res.int.a4db)

## avaliando o offset

int.a4d.off <- glmmTMB(t.total_dafor  ~ mp + gc + lg + (1|localidade),
                       data = int.geomonit, control=controle, 
                       family = poisson)

ICtab(int.a4d, int.a4d.off, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

## avaliando a variavel relacionada a inflacao por zero

int.zi <- glmmTMB(t.total_dafor  ~ mp + gc + lg + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle,
                  ziformula = ~t.divers, family = poisson)

ICtab(int.a4d, int.zi, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)


# modelo final

intensity.glmm <- int.a4d <- glmmTMB(t.total_dafor  ~ mp + gc + lg + offset(log(t.total_min)) + (1|localidade),
                                     data = int.geomonit, control=controle, 
                                     family = poisson)
summary(intensity.glmm)


# extraindo o predict do modelo

data(sleepstudy, package = "lme4")

pred <- predict(intensity.glmm, type = "response")

model_pred <- as.numeric(pred)

# inserindo no data frame

int.geomonit$model_pred <- model_pred

arrange(int.geomonit, -(model_pred))

pred_intensity <- int.geomonit[ ,c("localidade", "model_pred")] %>%
  arrange(desc(model_pred))

pred_intensity

write.csv(int.geomonit, "C:/TCC_Vic/cenario_int", row.names = FALSE)
