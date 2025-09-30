#cenario 1
#localidade como unidade amostral

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

dfmonit_filt1 <- df_monit %>% 
  filter(data > "2022-01-01" &
           !(obs %in% c("estimado dos dados do ICMBio"))) %>%
  arrange(geo_id)


## selecionando a localidade por data para filtrar o numero de transectos

monit.loc <- dfmonit_filt1[ ,c("localidade", "data", "faixa_bat", "n_trans_vis", "n_trans_pres", "n_divers")] %>%
  group_by(localidade, data, faixa_bat) %>%
  reframe(trans_vis = max(n_trans_vis),
          detections  = max(n_trans_pres),
          divers = max(n_divers)) %>%
  arrange(data)


## obtendo o total de transectos vistos e detecções para cada localidade
## criando a variável minutos por mergulhador

monit2 <- monit.loc %>%
  group_by(localidade, data, faixa_bat) %>%
  summarise(t_trans_vis = sum(trans_vis),
            t_detections = sum(detections),
            t_divers = sum(divers),
            min.div = sum(t_trans_vis*t_divers)) %>%
  arrange(desc(data))


monit3 <- monit2 %>%
  group_by(localidade, faixa_bat) %>%
  summarise(t_trans_vis = sum(t_trans_vis),
            t_detections = sum(t_detections),
            t_divers = sum(t_divers),
            t_min.div = sum(min.div)) %>%
  arrange(desc(t_trans_vis))


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

## agrupando e obtendo o total do IAR para cada geomorfologia em cada localidade

geo <- df_geo[ ,c("localidade", "data", "faixa_bat","geo_cat", "iar_geo")] %>%
  group_by(localidade, data, faixa_bat, geo_cat) %>%
  reframe(iar_geo = mean(iar_geo)) %>%
  arrange(data)


## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

geo2 <- geo[ ,c("localidade", "faixa_bat","geo_cat", "iar_geo")] %>%
  group_by(localidade, faixa_bat, geo_cat) %>%
  summarise(iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)


# unindo os data frames

geomonit <- left_join(monit3, geo2) %>%
  arrange(localidade) %>%
  drop_na()

geomonit <- as.data.frame(geomonit)

# categorizando em faixa a variável minutos por mergulhador

max(geomonit$t_min.div)
min(geomonit$t_min.div)

min.div1 <- cut(geomonit$t_min.div, breaks = seq(0, max(geomonit$t_min.div), by = 8), right = TRUE)
levels(min.div1) <- c("0-8", "8-16", "16-24", "24-32", "32-40", "40-48", "48-56", "56-64", "64-72", "72-80", "80-88", "88-96",
                      "96-104", "104-112", "112-120", "120-128", "128-136", "136-144", "144-152", "152-160", "160-168", "168-176",
                      "176-184", "184-192", "192-200", "200-208", "208-216", "216-224", "224-232", "232-240", "240-248",
                      "248-256", "256-264", "264-272", "272-280", "280-288", "288-296", "296-304", "304-312", "312-320",
                      "320-328", "328-336", "336-344", "344-352", "352-360", "360-368", "368-376", "376-384", "384-392",
                      "392-400", "400-408", "408-416")

geomonit2 <- geomonit
geomonit2$min.div1 <- min.div1

geomonit2 <- as.data.frame(geomonit2)


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

model <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf +  (1|faixa_bat) + (1|min.div1),
                 data = geomonit2, control=controle, 
                 family = poisson)

model.gp <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|faixa_bat) + (1|min.div1),
                    data = geomonit2, control=controle, 
                    family = genpois)

model.nb1 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|faixa_bat) + (1|min.div1),
                     data = geomonit2, control=controle,
                     family = nbinom1)

model.nb2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|faixa_bat) + (1|min.div1),
                     data = geomonit2, control=controle, 
                     family = nbinom2)

### selecionando o melhor modelo

ICtab(model, model.gp, model.nb1, model.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.model.gp <- simulateResiduals(fittedModel=model.gp, n=1000)
windows(12,8)
plot(res.model.gp)

res.model.nb1 <- simulateResiduals(fittedModel=model.nb1, n=1000)
windows(12,8)
plot(res.model.nb1)

## comparando os efeitos aleatorios

nb1 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|faixa_bat),
               data = geomonit2, control=controle, 
               family = nbinom1)


nb2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|min.div1),
               data = geomonit2, control=controle, 
               family = nbinom1)

ICtab(model.nb1, nb1, nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.model.nb1 <- simulateResiduals(fittedModel=model.nb1, n=1000)
windows(12,8)
plot(res.model.nb1)

res.nb2 <- simulateResiduals(fittedModel=nb2, n=1000)
windows(12,8)
plot(res.nb2)

## avaliando o efeito fixo

nb2.a <- glmmTMB(t_detections ~ gc + lg + rpm + tf + (1|min.div1),
                 data = geomonit2, control=controle, 
                 family = nbinom1)

nb2.b <- glmmTMB(t_detections ~ mp + lg + rpm + tf + (1|min.div1),
                 data = geomonit2, control=controle, 
                 family = nbinom1)

nb2.c <- glmmTMB(t_detections ~ mp + gc + rpm + tf + (1|min.div1),
                 data = geomonit2, control=controle, 
                 family = nbinom1)

nb2.d <- glmmTMB(t_detections ~ mp + gc + lg + tf + (1|min.div1),
                 data = geomonit2, control=controle, 
                 family = nbinom1)

nb2.e <- glmmTMB(t_detections ~ mp + gc + lg + rpm + (1|min.div1),
                 data = geomonit2, control=controle, 
                 family = nbinom1)

ICtab(nb2, nb2.a, nb2.b, nb2.c, nb2.d, nb2.e, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb2.a <- simulateResiduals(fittedModel=nb2.a, n=1000)
windows(12,8)
plot(res.nb2.a)

res.nb2.e <- simulateResiduals(fittedModel=nb2.e, n=1000)
windows(12,8)
plot(res.nb2.e)

res.nb2.b <- simulateResiduals(fittedModel=nb2.b, n=1000)
windows(12,8)
plot(res.nb2.b)

nb2.a1 <- glmmTMB(t_detections ~ lg + rpm + tf + (1|min.div1),
                  data = geomonit2, control=controle, 
                  family = nbinom1)

nb2.a2 <- glmmTMB(t_detections ~ gc + rpm + tf + (1|min.div1),
                  data = geomonit2, control=controle, 
                  family = nbinom1)

nb2.a3 <- glmmTMB(t_detections ~ gc + lg + tf + (1|min.div1),
                  data = geomonit2, control=controle, 
                  family = nbinom1)

nb2.a4 <- glmmTMB(t_detections ~ gc + lg + rpm + (1|min.div1),
                  data = geomonit2, control=controle, 
                  family = nbinom1)

ICtab(nb2.a, nb2.a1, nb2.a2, nb2.a3, nb2.a4, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb2.a4 <- simulateResiduals(fittedModel=nb2.a4, n=1000)
windows(12,8)
plot(res.nb2.a4)

res.nb2.a1 <- simulateResiduals(fittedModel=nb2.a1, n=1000)
windows(12,8)
plot(res.nb2.a1)

res.nb2.a <- simulateResiduals(fittedModel=nb2.a, n=1000)
windows(12,8)
plot(res.nb2.a)


nb2.a1.1 <- glmmTMB(t_detections ~ lg + rpm + (1|min.div1),
                    data = geomonit2, control=controle, 
                    family = nbinom1)

nb2.a1.2 <- glmmTMB(t_detections ~ lg + tf + (1|min.div1),
                    data = geomonit2, control=controle, 
                    family = nbinom1)

nb2.a1.3 <- glmmTMB(t_detections ~ rpm + tf + (1|min.div1),
                    data = geomonit2, control=controle, 
                    family = nbinom1)

ICtab(nb2.a1, nb2.a1.1, nb2.a1.2, nb2.a1.3, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb2.a1.3 <- simulateResiduals(fittedModel=nb2.a1.3, n=1000)
windows(12,8)
plot(res.nb2.a1.3)

res.nb2.a1.1 <- simulateResiduals(fittedModel=nb2.a1.1, n=1000)
windows(12,8)
plot(res.nb2.a1.1)


nb2.a1a <- glmmTMB(t_detections ~ tf + (1|min.div1),
                   data = geomonit2, control=controle, 
                   family = nbinom1)

nb2.a1b <- glmmTMB(t_detections ~ lg + (1|min.div1),
                   data = geomonit2, control=controle, 
                   family = nbinom1)

nb2.a1c <- glmmTMB(t_detections ~ rpm + (1|min.div1),
                   data = geomonit2, control=controle, 
                   family = nbinom1)

ICtab(nb2.a1, nb2.a1a, nb2.a1b, nb2.a1c, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb2.a1c <- simulateResiduals(fittedModel=nb2.a1c, n=1000)
windows(12,8)
plot(res.nb2.a1c)

## avaliando a variavel relacionada a inflação por zero

nb2.a1.zi <- glmmTMB(t_detections ~ lg + rpm + tf + (1|min.div1),
                     data = geomonit2, control=controle,
                     ziformula = ~t_trans_vis, family = nbinom1)

## comparando o modelo zi com o modelo final 

ICtab(nb2.a1, nb2.a1.zi, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.nb2.a1.zi <- simulateResiduals(fittedModel=nb2.a1.zi, n=1000)
windows(12,8)
plot(res.nb2.a1.zi)

res.nb2.a1 <- simulateResiduals(fittedModel=nb2.a1, n=1000)
windows(12,8)
plot(res.nb2.a1)

# modelo final

locality.glmm <- nb2.a1.zi <- glmmTMB(t_detections ~ lg + rpm + tf + (1|min.div1),
                                      data = geomonit2, control=controle,
                                      ziformula = ~t_trans_vis, family = nbinom1)
summary(locality.glmm)


# extraindo o predict do modelo

data(sleepstudy, package = "lme4")

pred <- predict(locality.glmm, type = "response")

model_pred <- as.numeric(pred)

# inserindo no data frame

geomonit2$model_pred <- model_pred

arrange(geomonit2, (localidade))

pred_locality <- geomonit2[ ,c("localidade", "faixa_bat", "model_pred")] %>%
  arrange(localidade)

install.packages("clipr")
library('clipr')

write_clip(pred_locality$model_pred)
write.csv(pred_locality, "pred_locality.csv", row.names = FALSE)
