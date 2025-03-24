# localidade como unidade amostral
# data frames

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

## aplicando os filtros 

dfmonit_filt <- df_monit %>% 
  filter(data > "2022-01-01" &
           !(obs %in% c("Sem geo", "estimado dos dados do ICMBio", "geo nao realizada", "geo nao realizada, sem ficha de campo")) &
           !(geo_id %in% c("Na"))) %>%
  arrange(geo_id)

summary(dfmonit_filt$geo_id)
print(dfmonit_filt)

## selecionando a localidade por data para filtrar o numero de transectos

monit <- dfmonit_filt[ ,c("localidade", "data", "faixa_bat", "n_trans_vis", "n_trans_pres", "n_divers")] %>%
  group_by(localidade, data, faixa_bat) %>%
  reframe(trans_vis = max(n_trans_vis),
          detections  = max(n_trans_pres),
          divers = max(n_divers)) %>%
  arrange(data)

print(monit)

## obtendo o total de transectos vistos e detecções para cada localidade
## criando a variável minutos por mergulhador

monit2 <- monit %>%
  group_by(localidade, faixa_bat) %>%
  summarise(t_trans_vis = sum(trans_vis),
            t_detections = sum(detections),
            t_divers = sum(divers),
            min.div = sum(t_trans_vis*t_divers)) %>%
  arrange(desc(t_trans_vis))

print(monit2, n = 67)

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

## agrupando e obtendo o total do IAR para cada geomorfologia em cada localidade

geo <- df_geo[ ,c("localidade", "data", "faixa_bat","geo_cat", "iar_geo")] %>%
  group_by(localidade, data, faixa_bat, geo_cat) %>%
  reframe(iar_geo = mean(iar_geo)) %>%
  arrange(data)


print(geo, n = 185)

## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

geo2 <- geo[ ,c("localidade", "faixa_bat","geo_cat", "iar_geo")] %>%
  group_by(localidade, faixa_bat, geo_cat) %>%
  summarise(iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)


print(geo2, n = 34)

## padronizando as geos
###média = 0
###desvio padrão = 1

#gc_pad <- as.matrix(geo2$gc - mean(as.matrix(geo2$gc)))/sd(as.matrix(geo2$gc))
#lg_pad <- as.matrix(geo2$lg - mean(as.matrix(geo2$lg)))/sd(as.matrix(geo2$lg))
#mp_pad <- as.matrix(geo2$mp - mean(as.matrix(geo2$mp)))/sd(as.matrix(geo2$mp))
#rpm_pad <- as.matrix(geo2$rpm - mean(as.matrix(geo2$rpm)))/sd(as.matrix(geo2$rpm))
#tf_pad <- as.matrix(geo2$tf - mean(as.matrix(geo2$tf)))/sd(as.matrix(geo2$tf))

## inserindo no dataframe

#geo2$gc_pad <- gc_pad
#geo2$lg_pad <- lg_pad
#geo2$mp_pad <- mp_pad
#geo2$rpm_pad <- rpm_pad
#geo2$tf_pad <- tf_pad

#geo_pad <- (geo2[, c(1:2, 8:12)]) %>%
  #group_by(localidade, faixa_bat)

#print(geo_pad, n = 53)

# unindo os data frames

geomonit <- left_join(monit2, geo2) %>%
  arrange(localidade) %>%
  drop_na()

max(geomonit$min.div)
min(geomonit$min.div)

print(geomonit, n = 51)

geomonit <- as.data.frame(geomonit)

# categorizando em faixa a variável minutos por mergulhador

min.div2 <- rep("very_intense", nrow(geomonit))
quantile(geomonit$min.div)

min.div2[geomonit$min.div <= quantile(geomonit$min.div)[4]] <- "intense"
min.div2[geomonit$min.div <= quantile(geomonit$min.div)[3]] <- "moderate"
min.div2[geomonit$min.div <= quantile(geomonit$min.div)[2]] <- "low"
min.div2 <- factor(min.div2, levels=c("low", "moderate", "intense", "very_intense"))
levels(min.div2)
table(min.div2)

geomonit2 <- geomonit
geomonit2$min.div2 <- min.div2
colnames(geomonit2)

geomonit2 <- as.data.frame(geomonit2)



# generalize linear mixed models

library(lme4)
library(MASS)
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(ggpubr)
library(DHARMa)
library(lattice)
library(MuMIn)

# gráfico da geo em função das detecções

## categorizando as detecções

gm.loc <- geomonit2[, c(1, 4, 7:11)] %>%
  group_by(localidade) %>%
  summarise(det = sum(t_detections),
            m_gc = mean(gc),
            m_lg = mean(lg),
            m_mp = mean (mp),
            m_rpm = mean(rpm),
            m_tf = mean(tf))

categoria <- ifelse(gm.loc$det == 0, "zero", "non-zero")

gm.loc$categoria <- categoria
gm.loc

## transformando em dados longos

gm.loc_long <- gather(gm.loc, key = "geomorfologia", value = "valor", 
                   m_gc, m_lg, m_mp, m_rpm, m_tf)

gm.loc_long2 <- gm.loc_long[ ,c("det", "categoria", "geomorfologia", "valor")] %>%
  group_by(det, categoria, geomorfologia) %>%
  summarise(valor_medio = mean(valor))


## plot barras empilhadas 

ggplot(gm.loc_long2, aes(x = geomorfologia, y = valor_medio, fill = categoria)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Valor médio das geo_class em função das detecções", 
       x = "Classes geomorfológicas", 
       y = "Detecções") +
  scale_fill_manual(values = c("pink1", "darkorange")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

## plot em pontos

plot(geomonit2$gc, geomonit2$t_detections, type = "p", col = "red", 
     main = "Geos em função das det", 
     xlab = "Classes geomorfológicas", ylab = "Detecções")
lines(geomonit2$lg, geomonit2$t_detections, type = "p", col = "blue")
lines(geomonit2$mp, geomonit2$t_detections, type = "p", col = "green")
lines(geomonit2$rpm, geomonit2$t_detections, type = "p", col = "orange")
lines(geomonit2$tf, geomonit2$t_detections, type = "p", col = "brown")
legend("topright", legend = c("gc", "lg", "mp", "rpm", "tf"), 
       col = c("red", "blue", "green", "orange", "brown"), lty = 1)


# modelos glmm

## avaliando o efeito aleatório

controle <- glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"),
                           optCtrl=list(iter.max=1e3,eval.max=1e3))

model <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf +  (1|faixa_bat) + (1|localidade) + (1|min.div2),
                 data = geomonit2, control=controle, 
                 family = poisson)

model.gp <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|faixa_bat) + (1|localidade) + (1|min.div2),
                 data = geomonit2, control=controle, 
                 family = genpois)

model.nb1 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|faixa_bat) + (1|localidade) + (1|min.div2),
                     data = geomonit2, control=controle,
                     family = nbinom1)

model.nb2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|faixa_bat) + (1|localidade) + (1|min.div2),
                     data = geomonit2, control=controle, 
                     family = nbinom2)

### selecionando o melhor modelo a depender da distribuição
ICtab(model, model.gp, model.nb1, model.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)
#menor AICc, melhor modelo

res.model <- simulateResiduals(fittedModel=model, n=1000)
windows(12,8)
plot(res.model)

res.model.gp <- simulateResiduals(fittedModel=model.gp, n=1000)
windows(12,8)
plot(res.model.gp)

res.model.nb1 <- simulateResiduals(fittedModel=model.nb1, n=1000)
windows(12,8)
plot(res.model.nb1)
#melhor model


### comparando os modelos mais completos com os mais simples

model1 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf +  (1|faixa_bat) + (1|localidade),
                  data = geomonit2, control=controle, 
                  family = poisson)


model2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf +  (1|faixa_bat) + (1|min.div2),
                 data = geomonit2, control=controle, 
                 family = poisson)


model3 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|localidade) + (1|min.div2),
                 data = geomonit2, control=controle, 
                 family = poisson)


ICtab(model, model1, model2, model3, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)
#ver o melhor modelo e seguir comparando o mais completo com o mais simples

res.model1 <- simulateResiduals(fittedModel=model1, n=1000)
windows(12,8)
plot(res.model1)

res.model3 <- simulateResiduals(fittedModel=model3, n=1000)
windows(12,8)
plot(res.model3)

#melhor model1


model1a <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf +  (1|faixa_bat),
                  data = geomonit2, control=controle, 
                  family = poisson)

model1b <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|localidade),
                  data = geomonit2, control=controle, 
                  family = poisson)


ICtab(model1, model1a, model1b, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.model1b <- simulateResiduals(fittedModel=model1b, n=1000)
windows(12,8)
plot(res.model1b)
#melhor model1


## avaliando o efeito fixo

model1.a <- glmmTMB(t_detections ~ gc + lg + rpm + tf +  (1|faixa_bat) + (1|localidade),
                  data = geomonit2, control=controle, 
                  family = poisson)

model1.b <- glmmTMB(t_detections ~ mp + lg + rpm + tf +  (1|faixa_bat) + (1|localidade),
                  data = geomonit2, control=controle, 
                  family = poisson)

model1.c <- glmmTMB(t_detections ~ mp + gc + rpm + tf +  (1|faixa_bat) + (1|localidade),
                  data = geomonit2, control=controle, 
                  family = poisson)

model1.d <- glmmTMB(t_detections ~ mp + gc + lg + tf +  (1|faixa_bat) + (1|localidade),
                  data = geomonit2, control=controle, 
                  family = poisson)

model1.e <- glmmTMB(t_detections ~ mp + gc + lg + rpm +  (1|faixa_bat) + (1|localidade),
                  data = geomonit2, control=controle, 
                  family = poisson)

ICtab(model1, model1.a, model1.b, model1.c, model1.d, model1.e, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)


res.model1.c <- simulateResiduals(fittedModel=model1.c, n=1000)
windows(12,8)
plot(res.model1.c)


res.model1.d <- simulateResiduals(fittedModel=model1.d, n=1000)
windows(12,8)
plot(res.model1.d)


res.model1.b <- simulateResiduals(fittedModel=model1.b, n=1000)
windows(12,8)
plot(res.model1.b)


res.model1.e <- simulateResiduals(fittedModel=model1.e, n=1000)
windows(12,8)
plot(res.model1.e)
#melhor 1.e

model1.e1 <- glmmTMB(t_detections ~ gc + lg + rpm +  (1|faixa_bat) + (1|localidade),
                    data = geomonit2, control=controle, 
                    family = poisson)

model1.e2 <- glmmTMB(t_detections ~ mp + lg + rpm +  (1|faixa_bat) + (1|localidade),
                    data = geomonit2, control=controle, 
                    family = poisson)

model1.e3 <- glmmTMB(t_detections ~ mp + gc + rpm +  (1|faixa_bat) + (1|localidade),
                    data = geomonit2, control=controle, 
                    family = poisson)

model1.e4 <- glmmTMB(t_detections ~ mp + gc + lg +  (1|faixa_bat) + (1|localidade),
                    data = geomonit2, control=controle, 
                    family = poisson)

ICtab(model1.e, model1.e1, model1.e2, model1.e3, model1.e4, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.model1.e3 <- simulateResiduals(fittedModel=model1.e3, n=1000)
windows(12,8)
plot(res.model1.e3)


res.res.model1.e4 <- simulateResiduals(fittedModel=model1.e4, n=1000)
windows(12,8)
plot(res.res.model1.e4)
#melhor ainda é o a2

loc.nb1a.a2

## avaliando a variavel relacionada a inflação por zero

loc.nb1a.a2.zimodel <- glmmTMB(t_detections ~ mp + lg + (1|faixa_bat),
                    data = geomonit2, ziformula = ~t_trans_vis, 
                    control=controle, family = nbinom1)


## comparando os modelos zi com o modelo final 

ICtab(loc.nb1a.a2, loc.nb1a.a2.zimodel, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

## avaliando o modelo mais simples pela simulaçao de residuos

res.loc.nb1a.a2 <- simulateResiduals(fittedModel=loc.nb1a.a2, n=1000)
windows(12,8)
plot(res.loc.nb1a.a2)

res.loc.nb1a.a2.zimodel <- simulateResiduals(fittedModel=loc.nb1a.a2.zimodel, n=1000)
windows(12,8)
plot(res.loc.nb1a.a2.zimodel)

locality.glmm <- loc.nb1a.a2
locality.glmm

plotResiduals(res.model, geomonit2$mp)
plotResiduals(res.model, geomonit2$gc)
