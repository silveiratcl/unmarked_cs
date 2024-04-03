# ETL

### Data ETL
## data from monitoring DAFOR

## covariates


# detection - detection by group (fundo, raso e snorkeling) at each locality
# effort - number of detections by group

# visib_m
# tf - tocas e fendas 
# mp - matacoes e paredoes
# gc - grutas e cavernas
# rpm - rochas pequenas e medias
# lg - lages

# unidade amostral - localidade



library(tidyverse)
library(readr)
library(tidyr)

# monitoring
df_monit = read_delim("data/dados_monitoramento_cs_2024-03-22.csv",
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
print(df_localidade, n = 41)


df_localidade$comp_m = df_localidade$comp_m/1000
df_localidade$comp_m/1



## Data tranformation
# Aggregate by locality 
# total time
# total detections 
# number of observers


# Obtaining the detection and effort df
# detection is presence/absence by locality by each monitoring strata
# effort is the number of visual transects wheer cs were detected

df_monit_effort <- df_monit %>% 
  group_by(localidade, data, faixa_bat) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers)) %>%
  ungroup()
df_monit_effort
print(df_monit_effort, n=85)

df_monit_effort <- df_monit_effort %>%
  group_by(localidade) %>%
  reframe(max_tv = max(max_trsct_vis),
          n_divers = sum(n_divers),
          eff_raso = sum(ifelse(faixa_bat == "raso", n_detection, 0)),
          eff_entremare = sum(ifelse(faixa_bat == "entremare", n_detection, 0)),
          eff_fundo = sum(ifelse(faixa_bat == "fundo", n_detection, 0))) %>%
  ungroup()

df_monit_effort ###

print(df_monit_effort, n=35)

df_monit_detec <- df_monit_effort %>%
  group_by(localidade) %>%
  reframe(
          detec_raso = ifelse(eff_raso >= 1 , 1, 0),
          detec_entremare = ifelse(eff_entremare >= 1 , 1, 0),
          detec_fundo = ifelse(eff_fundo >= 1 , 1, 0)) %>%
  ungroup()


df_monit_detec ###



## Adding length of localities to df effort

df_monit_effort <- left_join(df_monit_effort, df_localidade, by = "localidade")
df_monit_effort 



## Processing and Standardization of geomorphology data

glimpse(df_geo)
df_geo[1:255,]
tail(df_geo[255,])


# mean of scores to each geo category at each locality
# deep and shallow

df_geo_pd <- df_geo %>% 
  group_by(localidade, geo_cat) %>%
  mutate(geo_avg = mean(iar_geo)) %>% 
  ungroup()

df_geo_pd

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
  pivot_wider(names_from = geo_cat, values_from = geo_st) %>% 
  arrange(localidade)

df_geo_pd_local
print(df_geo_pd_local, n = 32)

## Checking if columms match
## shortening names and storing as rdata




df_geo_pd_local

effort = df_monit_effort[, 4:6] 
detection = df_monit_detec[,2:4]


save(effort,predictors, file = "unmarked_data.RData")








