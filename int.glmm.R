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
        !(obs %in% c("estimado dos dados do ICMBio"))) %>%
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

print(int.monit, n = 125)

int.monit2 <- int.monit[ ,c("localidade", "total_min", "total_dafor", "divers")] %>%
  group_by(localidade) %>%
  reframe(t.total_min = sum(total_min),
          t.total_dafor = sum(total_dafor),
          t.divers = sum(divers))

print(int.monit2, n = 42)

#int.monit$geo_id <- as.double(int.monit$geo_id)


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

int.geo2 <- int.geo[ ,c("localidade", "visibilidade", "geo_cat", "iar_geo")] %>%
  group_by(localidade, geo_cat) %>%
  summarise(t_visib = mean(visibilidade),
            iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)

print(int.geo2, n = 81)


# unindo os data frames

int.geomonit <- left_join(int.monit2, int.geo2) %>%
  drop_na()

print(int.geomonit, n = 37)


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

### selecionando o melhor modelo a depender da distribuição

ICtab(int.model, int.model.gp, int.model.nb1, int.model.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.model <- simulateResiduals(fittedModel=int.model, n=1000)
windows(12,8)
plot(res.int.model)
#melhor

res.int.model.gp <- simulateResiduals(fittedModel=int.model.gp, n=1000)
windows(12,8)
plot(res.int.model.gp)


### comparando os modelos mais completos com os mais simples

int.a <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|localidade),
                     data = int.geomonit, control=controle, 
                     family = poisson)

int.b <- glmmTMB(t.total_dafor  ~ mp + gc + lg + rpm + tf + offset(log(t.total_min)) + (1|t_visib.chr),
                     data = int.geomonit, control=controle, 
                     family = poisson)

ICtab(int.a, int.b, int.model, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)


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
#melhor

res.int.a5 <- simulateResiduals(fittedModel=int.a5, n=1000)
windows(12,8)
plot(res.int.a5)


int.a2a <- glmmTMB(t.total_dafor  ~ lg + rpm + tf + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

int.a2b <- glmmTMB(t.total_dafor  ~ mp + rpm + tf + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

int.a2c <- glmmTMB(t.total_dafor  ~ mp + lg + tf + offset(log(t.total_min)) + (1|localidade),
                  data = int.geomonit, control=controle, 
                  family = poisson)

int.a2d <- glmmTMB(t.total_dafor  ~ lg + rpm + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

ICtab(int.a2, int.a2a, int.a2b, int.a2c, int.a2d, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.a2c <- simulateResiduals(fittedModel=int.a2c, n=1000)
windows(12,8)
plot(res.int.a2c)


int.a2c1 <- glmmTMB(t.total_dafor  ~ lg + tf + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

int.a2c2 <- glmmTMB(t.total_dafor  ~ mp + tf + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

int.a2c3 <- glmmTMB(t.total_dafor  ~ mp + lg + offset(log(t.total_min)) + (1|localidade),
                   data = int.geomonit, control=controle, 
                   family = poisson)

ICtab(int.a2c, int.a2c1, int.a2c2, int.a2c3, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.int.a2c3 <- simulateResiduals(fittedModel=int.a2c3, n=1000)
windows(12,8)
plot(res.int.a2c3)

res.int.a2c2 <- simulateResiduals(fittedModel=int.a2c2, n=1000)
windows(12,8)
plot(res.int.a2c2)
#pelos residuos é o melhor

int.a2c2a <- glmmTMB(t.total_dafor  ~ mp + tf + (1|localidade),
                    data = int.geomonit, control=controle, 
                    family = poisson)

ICtab(int.a2c2, int.a2c2a, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

## avaliando a variavel relacionada a inflação por zero

int.zi <- glmmTMB(t.total_dafor ~ mp + tf + offset(log(t.total_min)) + (1|localidade),
                    data = int.geomonit, control=controle, 
                    ziformula = ~t.divers, family = poisson)


ICtab(int.a2c2, int.zi, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.zi <- simulateResiduals(fittedModel=nb1.b2d.zi, n=1000)
windows(12,8)
plot(res.zi)

intensity.glmm <- int.a2c2 <- glmmTMB(t.total_dafor  ~ mp + tf + offset(log(t.total_min)) + (1|localidade),
                                      data = int.geomonit, control=controle, 
                                      family = poisson)
summary(intensity.glmm)

# extraindo o predict do modelo

data(sleepstudy, package = "lme4")

pred <- predict(intensity.glmm, type = "response")

model_pred <- round(as.numeric(pred), 3)

# inserindo no data frame

int.geomonit$model_pred <- model_pred

arrange(int.geomonit, -(model_pred))

pred_intensity <- int.geomonit[ ,c("localidade", "model_pred")] %>%
  arrange(desc(model_pred))


pred_intensity

write.csv(pred_intensity, "predict_intensity.csv", row.names = FALSE)

library(gt)

predict.table <- pred_intensity %>%
  gt() %>%
  fmt_number(everything(), decimals = 3) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center"),
      cell_fill(color = "#f5f5f5")
    ),
    locations = cells_body()
  ) %>%
  tab_options(
    table.font.size = px(14),
    table.border.top.color = "black"
  )

library('gridExtra')

# Criando a tabela

df1 <- pred_intensity[1:20, ]
df2 <- pred_intensity[20:37, ]

# Criar "grob" (objetos gráficos para as tabelas)
tabela1 <- tableGrob(df1)
tabela2 <- tableGrob(df2)

# Plotar as duas tabelas na mesma imagem
grid.arrange(tabela1, tabela2, ncol = 2)

png("predict table.png", width = 10, height = 6, units = "in", res = 300)
grid.arrange(tabela1, tabela2, ncol = 2)
