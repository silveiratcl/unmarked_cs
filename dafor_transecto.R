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

print(monit.trans, n = 89)


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

print(geo.trans, n = 375)

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

geomonit.transp <- geomonit.transp[, c(1, 3:14)]
#sem data
print(geomonit.transp, n = 59)
