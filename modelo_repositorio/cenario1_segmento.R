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

print(geomonit.trans, n = 69)


# categorizando em faixa a variável minutos por mergulhador

max(geomonit.seg$min.div)
min(geomonit.seg$min.div)

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

max(geomonit.seg$t_visib)
min(geomonit.seg$t_visib)

t_visib2 <- cut(geomonit.trans$t_visib, breaks = seq(0, max(geomonit.trans$t_visib), by = 3), right = TRUE)
levels(t_visib2) <- c("0-3", "3-6", "6-9", "9-12", "12-15")
print(t_visib2)

geomonit.trans$t_visib2 <- t_visib2

geomonit.trans <- as.data.frame(geomonit.trans)


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

### selecionando o melhor modelo

ICtab(modelo, modelo.gp, modelo.nb1, modelo.nb2, type="AICc",  weights =  TRUE, delta = TRUE, base = TRUE)

res.modelo <- simulateResiduals(fittedModel=modelo, n=1000)
windows(12,8)
plot(res.modelo)

res.modelo.gp <- simulateResiduals(fittedModel=modelo.gp, n=1000)
windows(12,8)
plot(res.modelo.gp)

res.modelo.nb1 <- simulateResiduals(fittedModel=modelo.nb1, n=1000)
windows(12,8)
plot(res.modelo.nb1)

## comparando os efeitos aleatorios

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

res.modelo2 <- simulateResiduals(fittedModel=modelo2, n=1000)
windows(12,8)
plot(res.modelo2)

res.modelo3 <- simulateResiduals(fittedModel=modelo3, n=1000)
windows(12,8)
plot(res.modelo3)

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

## avaliando as variaveis relacionadas a inflacao por zero

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

segment.glmm <- modelo3b.e4 <- glmmTMB(t_detections ~ mp + gc + lg + (1|localidade),
                                        data = geomonit.trans, control=controle, 
                                        family = poisson)
summary(segment.glmm)

# figures

pred_transect.glmmf <- ggpredict(transect.glmm, terms=c("mp"), type = "fixed")
pred_transect.glmmr <- ggpredict(transect.glmm, terms=c("mp","localidade"), type = "random")
df_pred_transect.glmmf <- data.frame(x=pred_transect.glmmf$x,
                                     predict=pred_transect.glmmf$predicted,
                                     conf.low=pred_transect.glmmf$conf.low,
                                     conf.high=pred_transect.glmmf$conf.high)

g_transect.glmm <- plot(pred_transect.glmmr, facets=F, show_title=F, show_ci=F) + 
  geom_ribbon(data=df_pred_transect.glmmf, aes(x=x, ymin=conf.low, ymax=conf.high),
              alpha=0.2, inherit.aes=F) +
  geom_line(data=df_pred_transect.glmmf, aes(x=x, y=predict), 
            color="black", linewidth=1.5, inherit.aes=F) +
  theme_bw() + xlab("Média de MP") + labs(color="localidade") +
  ylab("Variância da localidade") +
  scale_color_manual(values=c("coral","indianred","gold2","darkolivegreen3","forestgreen",
                              "violet","purple","rosybrown", "seashell3", "peachpuff1",
                              "bisque3", "chocolate3", "burlywood2", "lightskyblue1",
                              "magenta2", "mediumpurple", "olivedrab4", "thistle", "firebrick",
                              "yellow2", "tomato", "plum1", "palevioletred", "orchid",
                              "midnightblue", "moccasin", "cornsilk2", "mistyrose", "chartreuse3",
                              "lemonchiffon", "khaki3", "lightblue","royalblue","gray60","black")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.y=element_text(size=12),axis.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        legend.position="right", legend.text=element_text(size=12),
        legend.background=element_rect(fill="transparent",color="transparent"),
        legend.title=element_text(size=12, face="bold"))
g_transect.glmm

pred_transect.glmm.gcf<- ggpredict(transect.glmm, terms=c("gc"), type = "fixed")
pred_transect.glmm.gcr <- ggpredict(transect.glmm, terms=c("gc","localidade"), type = "random")
df_pred_transect.glmm.gcf <- data.frame(x=pred_transect.glmm.gcf$x,
                                        predict=pred_transect.glmm.gcf$predicted,
                                        conf.low=pred_transect.glmm.gcf$conf.low,
                                        conf.high=pred_transect.glmm.gcf$conf.high)

g_transect.glmm.gc <- plot(pred_transect.glmm.gcr, facets=F, show_title=F, show_ci=F) + 
  geom_ribbon(data=df_pred_transect.glmm.gcf, aes(x=x, ymin=conf.low, ymax=conf.high),
              alpha=0.2, inherit.aes=F) +
  geom_line(data=df_pred_transect.glmm.gcf, aes(x=x, y=predict), 
            color="black", linewidth=1.5, inherit.aes=F) +
  theme_bw() + xlab("Média de GC") + labs(color="localidade") +
  ylab("Variância da localidade") +
  scale_color_manual(values=c("coral","indianred","gold2","darkolivegreen3","forestgreen",
                              "violet","purple","rosybrown", "seashell3", "peachpuff1",
                              "bisque3", "chocolate3", "burlywood2", "lightskyblue1",
                              "magenta2", "mediumpurple", "olivedrab4", "thistle", "firebrick",
                              "yellow2", "tomato", "plum1", "palevioletred", "orchid",
                              "midnightblue", "moccasin", "cornsilk2", "mistyrose", "chartreuse3",
                              "lemonchiffon", "khaki3", "lightblue","royalblue","gray60","black")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.y=element_text(size=12),axis.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        legend.position="right", legend.text=element_text(size=12),
        legend.background=element_rect(fill="transparent",color="transparent"),
        legend.title=element_text(size=12, face="bold"))
g_transect.glmm.gc

pred_transect.glmm.lgf<- ggpredict(transect.glmm, terms=c("lg"), type = "fixed")
pred_transect.glmm.lgr <- ggpredict(transect.glmm, terms=c("lg","localidade"), type = "random")
df_pred_transect.glmm.lgf <- data.frame(x=pred_transect.glmm.lgf$x,
                                        predict=pred_transect.glmm.lgf$predicted,
                                        conf.low=pred_transect.glmm.lgf$conf.low,
                                        conf.high=pred_transect.glmm.lgf$conf.high)

g_transect.glmm.lg <- plot(pred_transect.glmm.lgr, facets=F, show_title=F, show_ci=F) + 
  geom_ribbon(data=df_pred_transect.glmm.lgf, aes(x=x, ymin=conf.low, ymax=conf.high),
              alpha=0.2, inherit.aes=F) +
  geom_line(data=df_pred_transect.glmm.lgf, aes(x=x, y=predict), 
            color="black", linewidth=1.5, inherit.aes=F) +
  theme_bw() + xlab("Média de LG") + labs(color="localidade") +
  ylab("Variância da localidade") +
  scale_color_manual(values=c("coral","indianred","gold2","darkolivegreen3","forestgreen",
                              "violet","purple","rosybrown", "seashell3", "peachpuff1",
                              "bisque3", "chocolate3", "burlywood2", "lightskyblue1",
                              "magenta2", "mediumpurple", "olivedrab4", "thistle", "firebrick",
                              "yellow2", "tomato", "plum1", "palevioletred", "orchid",
                              "midnightblue", "moccasin", "cornsilk2", "mistyrose", "chartreuse3",
                              "lemonchiffon", "khaki3", "lightblue","royalblue","gray60","black")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.y=element_text(size=12),axis.title=element_text(size=14),
        axis.text.x=element_text(size=12),
        legend.position="right", legend.text=element_text(size=12),
        legend.background=element_rect(fill="transparent",color="transparent"),
        legend.title=element_text(size=12, face="bold"))
g_transect.glmm.lg

colours()