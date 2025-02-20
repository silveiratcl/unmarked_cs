# transecto como unidade amostral

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
           !(obs %in% c("Sem geo", "estimado dos dados do ICMBio", "geo nao realizada", "geo nao realizada, sem ficha de campo")) &
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

print(monit.trans, n = 169)


## criando a variável minutos por mergulhador

monit.trans2 <- monit.trans %>%
  group_by(geo_id, data, localidade, faixa_bat) %>%
  summarise(t_trans_vis = sum(trans_vis),
            t_detections = sum(detections),
            t_divers = sum(divers),
            min.div = sum(t_trans_vis*t_divers))

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

## agrupando e obtendo o total do IAR para cada geomorfologia em cada transecto

geo.trans <- df_geo[ ,c("localidade", "data", "faixa_bat", "visibilidade", "geo_cat", "iar_geo", "geo_id")] %>%
  group_by(geo_id, localidade, data, faixa_bat, geo_cat) %>%
  reframe(visibilidade = max(visibilidade),
          iar_geo = mean(iar_geo)) %>%
  arrange(geo_id)

print(geo.trans, n = 185)

## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

geo.trans2 <- geo.trans[ ,c("geo_id", "localidade", "faixa_bat", "visibilidade", "geo_cat", "iar_geo")] %>%
  group_by(geo_id, localidade, faixa_bat, geo_cat) %>%
  summarise(t_visib = max(visibilidade),
            iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)

print(geo.trans2, n = 75)


# unindo os data frames

geomonit.trans <- left_join(monit.trans2, geo.trans2) %>%
  arrange(geo_id) %>%
  drop_na()

print(geomonit.trans, n = 59)


## padronizando as geos
###média = 0
###desvio padrão = 1

gc_pad <- as.matrix(geomonit.trans$gc - mean(as.matrix(geomonit.trans$gc)))/sd(as.matrix(geomonit.trans$gc))
lg_pad <- as.matrix(geomonit.trans$lg - mean(as.matrix(geomonit.trans$lg)))/sd(as.matrix(geomonit.trans$lg))
mp_pad <- as.matrix(geomonit.trans$mp - mean(as.matrix(geomonit.trans$mp)))/sd(as.matrix(geomonit.trans$mp))
rpm_pad <- as.matrix(geomonit.trans$rpm - mean(as.matrix(geomonit.trans$rpm)))/sd(as.matrix(geomonit.trans$rpm))
tf_pad <- as.matrix(geomonit.trans$tf - mean(as.matrix(geomonit.trans$tf)))/sd(as.matrix(geomonit.trans$tf))

geomonit.transp <- (geomonit.trans[, c(1, 2:9)])

geomonit.transp$gc_pad <- gc_pad
geomonit.transp$lg_pad <- lg_pad
geomonit.transp$mp_pad <- mp_pad
geomonit.transp$rpm_pad <- rpm_pad
geomonit.transp$tf_pad <- tf_pad

print(geomonit.transp, n = 59)


# generalize linear mixed models

library(lme4)
library(MASS)
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(ggpubr)
library(DHARMa)
library(lattice)

# gráfico da geo em função das detecções

geomonit.trans_long <- geomonit.trans[5, 9:13] %>%
  pivot_longer(cols = c(gc, lg, mp, rpm, tf), 
               names_to = "geo_class", 
               values_to = "geo_detec")

ggplot(geomonit.trans, aes(x = gc, y = de, fill = geo_class)) +
  geom_bar(stat = "identity",  position = "stack") +
  labs(title = "Geos em função das Detecções", 
       x = "Geomorfologias", 
       y = "Detecções") +
  scale_fill_manual(values = c("red", "blue", "green", "orange", "brown")) +
  theme_minimal()

plot(geomonit.transp$gc_pad, geomonit.transp$t_detections, type = "p", col = "red", 
     main = "Geos em função das det", 
     xlab = "Geomorfologias", ylab = "Detecções")
lines(geomonit.transp$lg_pad, geomonit.transp$t_detections, type = "p", col = "blue")
lines(geomonit.transp$mp_pad, geomonit.transp$t_detections, type = "p", col = "green")
lines(geomonit.transp$rpm_pad, geomonit.transp$t_detections, type = "p", col = "orange")
lines(geomonit.transp$tf_pad, geomonit.transp$t_detections, type = "p", col = "brown")
legend("topright", legend = c("gc", "lg", "mp", "rpm", "tf"), 
       col = c("red", "blue", "green", "orange", "brown"), lty = 1)


ggplot(geomonit.transp, aes(x = t_detections, y = gc_pad, fill = gc_pad)) +
  geom_bar(stat = "identity", position = "stack") +  # "stack" para barras empilhadas
  labs(title = "Gráfico de Barras Empilhadas", x = "Categoria", y = "Valor") +
  theme_minimal()

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
