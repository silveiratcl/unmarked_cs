### Data ETL
## data from monitoring DAFOR

## covariates
# visib_m
# effort - minutes of observation
# effort - lenght segment
# tf - tocas e fendas 
# mp - matacoes e paredoes
# gc - grutas e cavernas
# rpm - rochas pequenas e medias
# lg - lages

# unidade amostral - localidade


library(tidyverse)
library(readr)
library(tidyr)

## loading data

# monitoring
df_monit = read_delim("data/dados_monitoramento_cs_2024-03-18.csv",
                      col_types = list(localidade = col_character(),
                                       data = col_date(format = "%d/%m/%Y"),
                                       visib_horiz = col_double(),
                                       faixa_bat = col_character(),
                                       prof_interface_min = col_double(),
                                       prof_interface_max = col_double(),
                                       metodo = col_character(),
                                       observadores = col_character(),
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
df_monit[2000,]

# geomophology

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
df_geo


# localities

df_localidade = read_delim("data/localidade_rebio.csv", delim = ";", 
                          col_types = c("d","c","d"))
df_localidade
df_localidade$comp_m = df_localidade$comp_m/1000
df_localidade$comp_m/1


## Data tranformation
# Aggregarte by locality 
# total time
# total de detections 
# number of observers

df_monit_effort <- df_monit %>% 
  group_by(localidade, data) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  reframe(max_min = max(n_trans_vis),
          detection = max(n_trans_pres),
          n_divers = max(n_divers)) %>%
  ungroup()
df_monit_effort

df_monit_effort <- df_monit_effort %>% 
  group_by(localidade) %>% 
  reframe(effort_m = sum(max_min*n_divers),
          detection_total = sum(detection)) %>% 
  ungroup()
df_monit_effort

print(df_monit_effort, n=37)


## Adding length of localities to df effort

df_monit_effort <- left_join(df_monit_effort, df_localidade, by = "localidade")
df_monit_effort
# standardize by lenght? 



## Standardize geomorphology data

glimpse(df_geo)
df_geo[1:255,]
tail(df_geo[255,])


# mean of scores to each geo category at each locality
# deep and shallow

df_geo_pd <- df_geo[1:255,] %>% 
  group_by(localidade, geo_cat) %>%
  mutate(geo_avg = mean(iar_geo)) %>% 
 ungroup()
print(df_geo_pd, n = 255)



## scaling mean scores to categories at each locality

df_geo_pd <- df_geo_pd %>% 
  select(localidade, geo_cat, geo_avg) %>% 
  group_by(localidade) %>%
  mutate(geo_st = scale(geo_avg)) %>%
  distinct() %>% 
  ungroup()
print(df_geo_pd, n = 255)


df_geo_pd_local  <- df_geo_pd %>%
  select(localidade, geo_cat, geo_st) %>% 
  mutate(geo_st = geo_st[,1]) %>% 
  pivot_wider(names_from = geo_cat, values_from = geo_st)

df_geo_pd_local



####Total

# mean of scores to each geo category at each locality
# deep and shallow

df_geo_pd <- df_geo %>% 
  group_by(localidade, geo_cat) %>%
  mutate(geo_avg = mean(iar_geo)) %>% 
  ungroup()



## scaling mean scores to categories at each locality

df_geo_pd <- df_geo_pd %>% 
  select(localidade, geo_cat, geo_avg) %>% 
  group_by(localidade) %>%
  mutate(geo_st = scale(geo_avg)) %>%
  distinct() %>% 
  ungroup()

df_geo_pd_local  <- df_geo_pd %>%
  select(localidade, geo_cat, geo_st) %>% 
  mutate(geo_st = geo_st[,1]) %>% 
  pivot_wider(names_from = geo_cat, values_from = geo_st)

df_geo_pd_local
print(df_geo_pd_local, n = 31)


## Shortening names

effort <- df_monit_effort
predictors <- df_geo_pd_local 


save(effort,predictors, file = "unmarked_data.RData")





