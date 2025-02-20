
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
           !(obs %in% c("Sem geo", "estimado dos dados do ICMBio"))) %>%
  arrange(data)
print(dfmonit_filt, n = 100)

## selecionando a localidade por data para filtrar o numero de transectos

monit <- dfmonit_filt[ ,c("localidade", "data", "faixa_bat", "n_trans_vis", "n_trans_pres", "n_divers")] %>%
  group_by(localidade, data, faixa_bat) %>%
  reframe(visuals = max(n_trans_vis),
          detec = max(n_trans_pres),
          divers = max(n_divers)) %>%
  arrange(data)

print(monit, n = 84)

## obtendo o total de transectos vistos e detecções para cada localidade
## criando a variável minutos por mergulhador

monit2 <- monit %>%
  group_by(localidade) %>%
  summarise(vis = sum(visuals),
            det = sum(detec),
            t_divers = sum(divers),
            min.div = sum(vis*t_divers)) %>%
  arrange(desc(vis))

print(monit2, n = 76)

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

geo <- df_geo[ ,c("localidade", "data", "geo_cat", "iar_geo")] %>%
  group_by(localidade, data, geo_cat) %>%
  reframe(iar_geo = mean(iar_geo)) %>%
  arrange(data)

print(geo, n = 185)

## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

geo2 <- geo[ ,c("localidade", "geo_cat", "iar_geo")] %>%
  group_by(localidade, geo_cat) %>%
  summarise(iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)


print(geo2, n = 34)

## padronizando as geos
###média = 0
###desvio padrão = 1

gc_pad <- as.matrix(geo2$gc - mean(as.matrix(geo2$gc)))/sd(as.matrix(geo2$gc))
lg_pad <- as.matrix(geo2$lg - mean(as.matrix(geo2$lg)))/sd(as.matrix(geo2$lg))
mp_pad <- as.matrix(geo2$mp - mean(as.matrix(geo2$mp)))/sd(as.matrix(geo2$mp))
rpm_pad <- as.matrix(geo2$rpm - mean(as.matrix(geo2$rpm)))/sd(as.matrix(geo2$rpm))
tf_pad <- as.matrix(geo2$tf - mean(as.matrix(geo2$tf)))/sd(as.matrix(geo2$tf))

## inserindo no dataframe

geo2$gc_pad <- gc_pad
geo2$lg_pad <- lg_pad
geo2$mp_pad <- mp_pad
geo2$rpm_pad <- rpm_pad
geo2$tf_pad <- tf_pad

geo_pad <- geo2[, c(1, 7:11)]
print(geo_pad, n = 34)

# unindo os data frames

geomonit <- left_join(monit2, geo_pad) %>%
  arrange(desc(det)) %>%
  drop_na()

print(geomonit, n = 33)


# generalize linear mixed models

library(lme4)
library(MASS)
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(ggpubr)
library(DHARMa)
library(lattice)

# gráfico das detecçoes em função das geomorfologias

plot(geomonit$det, geomonit$gc_pad, type = "p", col = "red", 
     main = "Geos em função das det", 
     xlab = "Detecções", ylab = "Geomorfologias")
lines(geomonit$det, geomonit$lg_pad, type = "p", col = "blue")
lines(geomonit$det, geomonit$mp_pad, type = "p", col = "green")
lines(geomonit$det, geomonit$rpm_pad, type = "p", col = "orange")
lines(geomonit$det, geomonit$tf_pad, type = "p", col = "brown")
legend("topright", legend = c("gc", "lg", "mp", "rpm", "tf"), 
       col = c("red", "blue", "green", "orange", "brown"), lty = 1)


# modelos glm 

modelo1 <- glm.nb(det ~ gc_pad, data = geomonit)
plot(modelo1)
summary(modelo1)

modelo2 <- glm(det ~ gc_pad + lg_pad + gc_pad:lg_pad, family = poisson, data = geomonit)
summary(modelo2)


modelo3 <- glm(det ~ lg_pad, family = poisson, data = geomonit)
summary(modelo3)


modelo4 <- glm(det ~ mp_pad, family = poisson, data = geomonit)
summary(modelo4)


modelo5 <- glm(det ~ mp_pad + gc_pad, family = poisson, data = geomonit)
summary(modelo5)

modelo6 <- glm(det ~ mp_pad + gc_pad + mp_pad:gc_pad, family = poisson, data = geomonit)
summary(modelo6)

# comparando os modelos para ver qual é o mais significante

anova(modelo4, modelo5, modelo6, test = 'Chisq')


#deviance é o quanto o modelo ta explicando
#residual deviance é o quanto o modelo deixou de explicar
#o que ele deixou de explicar somado ao que explicou vai dar o resultado do quanto o outro modelo não tinha explciado
#mesmo que o modelo explique pouco, se ele for bastante significativo (p value) ainda sim é considerado


# modelos glmm
##primeiro ajustar um modelo completo, com todos os efeitos fixos que estão sendo testados 
##depois que monta o modelo completo, coloca os efeitos aleatorios


m0 <- lm(det ~ mp_pad, data = geomonit)
m1 <- lmer(det ~ mp_pad + (min.div|t_divers), data = geomonit)
summary(m1)

m2 <- lm(det ~ gc_pad, data = geomonit)
m3 <- lmer(det ~ gc_pad + (min.div|t_divers), data = geomonit)


anova(m1, m3, refit = FALSE)


m0 <- glmmTMB(det ~ mp_pad, data = geomonit.transp, family = poisson)
summary(m0)
m0.bin1 <- update(m0, family=nbinom1)
summary(m0.bin1)
m0.bin2 <- update(m0, family=nbinom2)
m0.inflated <- update(m0, ziformula = ~1)
summary(m0.inflated)
AICtab(m0, m0.bin1, m0.bin2, m0.inflated)
#m0.bin1 foi o melhor

modelo <- glmmTMB(det ~ mp, data = geo_mon, family = poisson)
summary(modelo)

m1 <- glmmTMB(det ~ gc_pad, data = geomonit, family = poisson)
summary(m1)  
m1.bin1 <- update(m1, family=nbinom1)
m1.bin2 <- update(m1, family=nbinom2)
AICtab(m1, m1.bin1, m1.bin2)
#m1.bin1 foi o melhor


m2 <- glmmTMB(det ~ lg_pad, data = geomonit, family = poisson)
summary(m2) 
m2.bin1 <- update(m2, family=nbinom1)
m2.bin2 <- update(m2, family=nbinom2)
AICtab(m2, m2.bin1, m2.bin2)
#m2.bin2 o melhor

AICtab(m0.bin1, m1.bin1, m2.bin2)

res.m0.bin1 <- simulateResiduals(fittedModel=m0.bin1, n=1000)
windows(12,8)
plot(res.m0.bin1)


plot(table(geomonit$det))


zeroinflated.model <- glmmTMB(det ~ mp_pad + (1|min.div), ziformula = ~1,  family = poisson, data = geomonit)

teste <- glmmTMB(det ~ gc + (1|min.div), ziformula = ~1, family = poisson, data = geo_mon)

print(geomonit, n = 33)
summary(zeroinflated.model)
summary(geomonit)
