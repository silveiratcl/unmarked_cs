# transecto como unidade amostral
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
  summarise(t_trans_vis = sum(trans_vis),
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

## agrupando e obtendo o total do IAR para cada geomorfologia em cada transecto

geo.trans <- df_geo[ ,c("localidade", "data", "faixa_bat", "visibilidade", "geo_cat", "iar_geo", "geo_id")] %>%
  group_by(geo_id, localidade, data, faixa_bat, geo_cat) %>%
  reframe(visibilidade = max(visibilidade),
          iar_geo = mean(iar_geo)) %>%
  arrange(geo_id)

print(geo.trans, n = 405)

## colocando as geos como coluna (variáveis) e iargeo como linha (valores das variáveis)

geo.trans2 <- geo.trans[ ,c("geo_id", "localidade", "faixa_bat", "visibilidade", "geo_cat", "iar_geo")] %>%
  group_by(geo_id, localidade, faixa_bat, geo_cat) %>%
  summarise(t_visib = max(visibilidade),
            iar_geo = mean(sum(iar_geo))) %>%
  spread(geo_cat, iar_geo)

print(geo.trans2, n = 81)


# unindo os data frames

geomonit.trans <- left_join(monit.trans2, geo.trans2) %>%
  arrange(geo_id) %>%
  drop_na()

print(geomonit.trans, n = 69)

# categorizando em faixa a variável minutos por mergulhador

min.div2 <- cut(geomonit.trans$min.div, breaks = seq(0, max(geomonit.trans$min.div), by = 5), right = TRUE)
levels(min.div2) <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50",
                      "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100",
                      "1000-105", "105-110", "110-115", "115-120", "120-125", "125-130", "130-135", "135-140",
                      "140-145", "145-150", "150-155", "155-160", "160-165", "165-170", "170-175", "175-180",
                      "180-185", "185-190", "190-195", "195-200", "200-205", "205-210", "210-215", "215-220",
                      "220-225", "225-230", "230-235", "235-240")
print(min.div2)

geomonit.trans$min.div2 <- min.div2

# categorizando em faixa a variável visibilidade

t_visib2 <- cut(geomonit.trans$t_visib, breaks = seq(0, max(geomonit.trans$t_visib), by = 3), right = TRUE)
levels(t_visib2) <- c("0-3", "3-6", "6-9", "9-10", "10-13", "13-15")
print(t_visib2)

geomonit.trans$t_visib2 <- t_visib2

geomonit.trans <- as.data.frame(geomonit.trans)

## padronizando as geos
###média = 0
###desvio padrão = 1

#gc_pad <- as.matrix(geomonit.trans$gc - mean(as.matrix(geomonit.trans$gc)))/sd(as.matrix(geomonit.trans$gc))
#lg_pad <- as.matrix(geomonit.trans$lg - mean(as.matrix(geomonit.trans$lg)))/sd(as.matrix(geomonit.trans$lg))
#mp_pad <- as.matrix(geomonit.trans$mp - mean(as.matrix(geomonit.trans$mp)))/sd(as.matrix(geomonit.trans$mp))
#rpm_pad <- as.matrix(geomonit.trans$rpm - mean(as.matrix(geomonit.trans$rpm)))/sd(as.matrix(geomonit.trans$rpm))
#tf_pad <- as.matrix(geomonit.trans$tf - mean(as.matrix(geomonit.trans$tf)))/sd(as.matrix(geomonit.trans$tf))

#geomonit.transp <- (geomonit.trans[, c(1, 2:9)])

#geomonit.transp$gc_pad <- gc_pad
#geomonit.transp$lg_pad <- lg_pad
#geomonit.transp$mp_pad <- mp_pad
#geomonit.transp$rpm_pad <- rpm_pad
#geomonit.transp$tf_pad <- tf_pad

#print(geomonit.transp, n = 59)


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

## categorizando as detecções

gmt <- geomonit.trans[, c(3, 6, 10:14)] %>%
  group_by(localidade) %>%
  summarise(det = sum(t_detections),
            m_gc = mean(gc),
            m_lg = mean(lg),
            m_mp = mean (mp),
            m_rpm = mean(rpm),
            m_tf = mean(tf))
            
categoria <- ifelse(gmt$det == 0, "Zero", "Diferente de Zero")

gmt$categoria <- categoria
gmt

## transformando em dados longos

gmt_long <- gather(gmt, key = "geomorfologia", value = "valor", 
                   m_gc, m_lg, m_mp, m_rpm, m_tf)

ggmt_long <- gmt_long[ ,c("det", "categoria", "geomorfologia", "valor")] %>%
  group_by(det, categoria, geomorfologia) %>%
  summarise(valor_medio = mean(valor))


## plot barras empilhadas 

ggplot(ggmt_long, aes(x = geomorfologia, y = valor_medio, fill = categoria)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Valor médio das geo_class em função das detecções", 
       x = "Classes geomorfológicas", 
       y = "Detecções") +
  scale_fill_manual(values = c("pink1", "darkorange")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

## plot em pontos

plot(geomonit.trans$gc, geomonit.trans$t_detections, type = "p", col = "red", 
     main = "Geos em função das det", 
     xlab = "Geomorfologias", ylab = "Detecções")
lines(geomonit.trans$lg, geomonit.trans$t_detections, type = "p", col = "blue")
lines(geomonit.trans$mp, geomonit.trans$t_detections, type = "p", col = "green")
lines(geomonit.trans$rpm, geomonit.trans$t_detections, type = "p", col = "orange")
lines(geomonit.trans$tf, geomonit.trans$t_detections, type = "p", col = "brown")
legend("topright", legend = c("gc", "lg", "mp", "rpm", "tf"), 
       col = c("red", "blue", "green", "orange", "brown"), lty = 1)



# modelos glmm

## avaliando o efeito aleatório

controle <- glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"),
                           optCtrl=list(iter.max=1e3,eval.max=1e3))

modelo <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2) + (1|localidade),
                 data = geomonit.trans, control=controle, 
                 family = poisson)

modelo.gp <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2)+ (1|localidade),
                  data = geomonit.trans, control=controle, 
                  family = genpois)

modelo.nb1 <- glmmTMB(t_detections ~mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2) + (1|localidade),
                      data = geomonit.trans, control=controle,
                      family = nbinom1)

modelo.nb2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2)+ (1|localidade),
                     data = geomonit.trans, control=controle,
                     family = nbinom2)

### selecionando o melhor modelo a depender da distribuição
ICtab(modelo, modelo.gp, modelo.nb1, modelo.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)
#menor AICc, melhor modelo
#diferença maior que 2 para serem 'diferentes'

res.modelo <- simulateResiduals(fittedModel=modelo, n=1000)
windows(12,8)
plot(res.modelo)

res.modelo.gp <- simulateResiduals(fittedModel=modelo.gp, n=1000)
windows(12,8)
plot(res.modelo.gp)

res.modelo.nb1 <- simulateResiduals(fittedModel=modelo.nb1, n=1000)
windows(12,8)
plot(res.modelo.nb1)
#modelo melhor


### comparando os modelos mais completos com os mais simples

modelo1 <-glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|min.div2),
                  data = geomonit.trans, control=controle, 
                  family = poisson)

modelo2 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|t_visib2) + (1|localidade),
              data = geomonit.trans, control=controle, 
              family = poisson)

modelo3 <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|min.div2) + (1|localidade),
        data = geomonit.trans, control=controle, 
        family = poisson)

ICtab(modelo, modelo1,  modelo2, modelo3, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)
#ver o melhor modelo e seguir comparando o mais completo com o mais simples

res.modelo2 <- simulateResiduals(fittedModel=modelo2, n=1000)
windows(12,8)
plot(res.modelo2)

res.modelo3 <- simulateResiduals(fittedModel=modelo3, n=1000)
windows(12,8)
plot(res.modelo3)
#3 melhor

modelo3a <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|min.div2),
                   data = geomonit.trans, control=controle, 
                   family = poisson)

modelo3b <- glmmTMB(t_detections ~ mp + gc + lg + rpm + tf + (1|localidade),
                   data = geomonit.trans, control=controle, 
                   family = poisson)

ICtab(modelo3, modelo3a, modelo3b, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo3b <- simulateResiduals(fittedModel=modelo3b, n=1000)
windows(12,8)
plot(res.modelo3b)
#3b melhor

## avaliando o efeito fixo

modelo3b.a <- glmmTMB(t_detections ~ gc + lg + rpm + tf + (1|localidade),
                    data = geomonit.trans, control=controle, 
                    family = poisson)

modelo3b.b <- glmmTMB(t_detections ~ mp + lg + rpm + tf + (1|localidade),
                    data = geomonit.trans, control=controle, 
                    family = poisson)

modelo3b.c <- glmmTMB(t_detections ~ mp + gc + rpm + tf + (1|localidade),
                    data = geomonit.trans, control=controle, 
                    family = poisson)

modelo3b.d <- glmmTMB(t_detections ~ mp + gc + lg + tf + (1|localidade),
                    data = geomonit.trans, control=controle, 
                    family = poisson)

modelo3b.e <- glmmTMB(t_detections ~ mp + gc + lg + rpm + (1|localidade),
                    data = geomonit.trans, control=controle, 
                    family = poisson)

ICtab(modelo3b, modelo3b.a, modelo3b.b, modelo3b.c, modelo3b.d, modelo3b.e, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo3b.c <- simulateResiduals(fittedModel=modelo3b.c, n=1000)
windows(12,8)
plot(res.modelo3b.c)

res.modelo3b.e <- simulateResiduals(fittedModel=modelo3b.e, n=1000)
windows(12,8)
plot(res.modelo3b.e)

res.modelo3b.d <- simulateResiduals(fittedModel=modelo3b.d, n=1000)
windows(12,8)
plot(res.modelo3b.d)
#3be melhor


modelo3b.e1 <- glmmTMB(t_detections ~ gc + lg + rpm + (1|localidade),
                      data = geomonit.trans, control=controle, 
                      family = poisson)

modelo3b.e2 <- glmmTMB(t_detections ~ mp + lg + rpm + (1|localidade),
                      data = geomonit.trans, control=controle, 
                      family = poisson)

modelo3b.e3 <- glmmTMB(t_detections ~ mp + gc + rpm + (1|localidade),
                      data = geomonit.trans, control=controle, 
                      family = poisson)

modelo3b.e4 <- glmmTMB(t_detections ~ mp + gc + lg + (1|localidade),
                      data = geomonit.trans, control=controle, 
                      family = poisson)

ICtab(modelo3b.e, modelo3b.e1, modelo3b.e2, modelo3b.e3, modelo3b.e4, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo3b.e3 <- simulateResiduals(fittedModel=modelo3b.e3, n=1000)
windows(12,8)
plot(res.modelo3b.e3)

res.modelo3b.e4 <- simulateResiduals(fittedModel=modelo3b.e4, n=1000)
windows(12,8)
plot(res.modelo3b.e4)
#e4

modelo3b.e4a <- glmmTMB(t_detections ~ gc + lg + (1|localidade),
                       data = geomonit.trans, control=controle, 
                       family = poisson)

modelo3b.e4b <- glmmTMB(t_detections ~ mp + lg + (1|localidade),
                       data = geomonit.trans, control=controle, 
                       family = poisson)

modelo3b.e4c <- glmmTMB(t_detections ~ mp + gc + (1|localidade),
                       data = geomonit.trans, control=controle, 
                       family = poisson)

ICtab(modelo3b.e4, modelo3b.e4a, modelo3b.e4b, modelo3b.e4c, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo3b.e4c <- simulateResiduals(fittedModel=modelo3b.e4c, n=1000)
windows(12,8)
plot(res.modelo3b.e4c)
#3be4 melhor

modelo3b.e4d <- glmmTMB(t_detections ~ mp + (1|localidade),
                       data = geomonit.trans, control=controle, 
                       family = poisson)

modelo3b.e4e <- glmmTMB(t_detections ~ gc + (1|localidade),
                       data = geomonit.trans, control=controle, 
                       family = poisson)

modelo3b.e4f <- glmmTMB(t_detections ~ lg + (1|localidade),
                       data = geomonit.trans, control=controle, 
                       family = poisson)

ICtab(modelo3b.e4, modelo3b.e4d, modelo3b.e4e, modelo3b.e4f, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)
#3be4 melhor

## avaliando as variaveis relacionadas a inflação por zero

modelo3b.e4.zi <- glmmTMB(t_detections ~ mp + gc + lg + (1|localidade),
                       data = geomonit.trans, ziformula = ~t_trans_vis + t_divers,
                       control=controle, family = poisson)

modelo3b.e4.zi1 <- glmmTMB(t_detections ~ mp + gc + lg + (1|localidade),
                                   data = geomonit.trans, ziformula = ~t_trans_vis,
                                   control=controle, family = poisson)

modelo3b.e4.zi2 <- glmmTMB(t_detections ~ mp + gc + lg + (1|localidade),
                                   data = geomonit.trans, ziformula = ~t_divers,
                                   control=controle, family = poisson)

## comparando o modelo zi com o modelo final

ICtab(modelo3b.e4, modelo3b.e4.zi, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo3b.e4.zi <- simulateResiduals(fittedModel=modelo3b.e4.zi, n=1000)
windows(12,8)
plot(res.modelo3b.e4.zi)

res.modelo3b.e4 <- simulateResiduals(fittedModel=modelo3b.e4, n=1000)
windows(12,8)
plot(res.modelo3b.e4)
#e4 melhor

transect.glmm <- modelo3b.e4
summary(transect.glmm)
