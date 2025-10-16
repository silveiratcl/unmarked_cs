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

pred3 <- predict(intensity.glmm, type = "response")

model3_pred <- as.numeric(pred3)

# inserindo no data frame

int.geomonit$model3_pred <- model3_pred

arrange(int.geomonit, -(model3_pred))

pred_intensity <- int.geomonit[ ,c("localidade", "model3_pred")] %>%
  arrange(localidade) 

print(pred_intensity, n = 38)

write.csv(int.geomonit, "C:/TCC_Vic/cenario_int", row.names = FALSE)

min3_val <- min(pred_intensity$model3_pred)
max3_val <- max(pred_intensity$model3_pred)


pred_intensity <- pred_intensity %>%
  mutate(localidade = fct_reorder(localidade, -(model3_pred), .desc = TRUE),
         recode(localidade,
                "engenho" = "SACO DO ENGENHO", "rancho_norte" = "RANCHO NORTE", "vidal" = "SACO DO VIDAL", 
                "farol" = "BAÍA DO FAROL", "pedra_do_elefante" = "PEDRA DO ELEFANTE", "costao_do_saco_dagua" = "COSTÃO DO SACO D'ÁGUA", 
                "saco_dagua" = "SACO D'ÁGUA", "deserta_sul" = "DESERTA SUL", "enseada_do_lili" = "ENSEADA DO LILI", "saco_do_batismo" = "SACO DO BATISMO", "baia_das_tartarugas" = "BAÍA DAS TARTARUGAS", "saquinho_dagua" = "SAQUINHO D'ÁGUA", 
                "saco_do_capim" = "SACO DO CAPIM", "deserta_norte" = "DESERTA NORTE", "letreiro" = "PONTA DO LETREIRO", "portinho_sul" = "PORTINHO SUL", "saco_dagua" = "SACO D'ÁGUA", "tamboretes_sul" = "TAMBORETES SUL",
                "saco_da_mulata_norte" = "SACO DA MULATA NORTE", "estaleiro_2" = "ESTALEIRO 2", "ilha_dos_lobos" = "ILHA DOS LOBOS", "costa_do_elefante" = "COSTA DO ELEFANTE", "irma_de_fora" = "IRMÃ DE FORA", 
                "ilha_porto_belo" = "ILHA PORTO BELO", "portinho_norte" = "PORTINHO NORTE", "mata_fome" = "ILHA MATA FOME", "xavier" = "ILHA DO XAVIER", "tipitinga" = "TIPITINGA", "campeche_norte" = "CAMPECHE NORTE",
                "aranhas_oeste" = "ARANHAS OESTE", "estaleiro_1" = "ESTALEIRO 1", "saco_da_mulata_sul" = "SACO DA MULATA SUL", "ilha_do_coral" = "ILHA DO CORAL", "irma_do_meio" = "IRMÃ DO MEIO", 
                "tamboretes_norte" = "TAMBORETES NORTE", "moleques_do_sul" = "MOLEQUES DO SUL", "sepultura" = "SEPULTURA", "aranhas_leste" = "ARANHAS LESTE", "macuco" = "ILHA DO MACUCO")
  )

g3 <- ggplot(pred_intensity, aes(x = model3_pred, y = `recode(...)`, fill = model3_pred)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = expression(
      paste("Probabilidade de intensidade da invasão de ", italic("T. coccinea"),
            " pela área de abrangência de MP, GC e L")
    ),
    x = "",
    y = "",
    fill = "Valor predito por localidade"
  ) +
  scale_fill_gradientn(
    colours = c("#009dff", "#00c514", "#f0e000", "#fd7a00", "#d7191c"),
    breaks = c(min3_val, max3_val),
    labels = c("0", "387"),
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = unit(5, "cm"),
      title.hjust = 0.5,
      title.vjust = 3
    )
  ) +
  annotate(
    "segment",
    x = 0, xend = 0, y = 0, yend = 39,
    arrow = arrow(type = "open", length = unit(0.3, "cm")),
    color = "black", size = 0.5
  ) +
  theme(
    plot.title = element_text(hjust = 0, size = 12),
    legend.position = c(0.8, 0.1),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title.y = element_text(size = 10, margin = margin(r = 0)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, margin = margin(r = 0), color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )


