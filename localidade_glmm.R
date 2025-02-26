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


# modelos glmm

## avaliando o efeito aleatório

m0 <- glmmTMB(t_detections ~ mp + gc + lg + (1|faixa_bat), data = geomonit2, family = poisson)


summary(m0)

m0.bin1 <- update(m0, family=nbinom1)
summary(m0.bin1)
m0.bin2 <- update(m0, family=nbinom2)
summary(m0.bin2)
m0.inflated <- update(m0, ziformula = ~t_trans_vis)
summary(m0.inflated)

model.sel(m0, m0.bin1, m0.bin2, m0.inflated)








res.m0.bin1 <- simulateResiduals(fittedModel=m0.bin1, n=1000)
windows(12,8)
plot(res.m0.bin1)


plot(table(geomonit$det))


controle <- glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"),
                           optCtrl=list(iter.max=1e3,eval.max=1e3))



teste <- glmmTMB(t_detections ~ gc_pad + offset(log(min.div)), family = poisson, data = geomonit)
summary(teste)
#erro na hessian matrix da pra tentar corrigir com o controle



zeroinflated.model <- glmmTMB(det ~ mp_pad + (1|min.div), family = poisson, data = geomonit)

teste <- glmmTMB(det ~ gc_pad + (1|min.div), ziformula = ~, family = poisson, data = geomonit)

print(geomonit, n = 33)
summary(zeroinflated.model)
summary(geomonit)
