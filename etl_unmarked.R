# ETL

### Data ETL - Extraction - Transformation - loading
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
df_geo = df_geo[, 1:14]
df_geo

# localities

df_localidade = read_delim("data/localidade_rebio.csv", delim = ";", 
                           col_types = c("d","c","d"))
df_localidade
print(df_localidade, n = 41)

# checking deciamls
#df_localidade$comp_m = df_localidade$comp_m/1000
#df_localidade$comp_m/1

################################################################################
# cheking matching by locality
# for the analysis using geomorphology 
# use just the detection where the geomorphology was evaluated 

df_monit
df_geo
df_localidade

# #locality inspection
monit <-sort(unique(df_monit$localidade))
geo <-sort(unique(df_geo$localidade))
localidade <-sort(unique(df_localidade$localidade))


setdiff(localidade, monit) 
#[1] "gale_sul"         "ponta_do_meio"    "saco_das_balas"   "saco_do_letreiro" "toca_da_salema"  
# those places werent monitored in monit df


setdiff(localidade, geo) 
#[1] "gale_sul"          "lagoinha_do_norte" "naufragio_do_lili"
#[4] "ponta_das_canas"   "ponta_do_meio"     "saco_das_balas"   
#[7] "saco_do_letreiro"  "toca_da_salema"   
# those localities were not avaluated for geomorphology

setdiff(geo, monit)
# [1] "tamboretes"
# jst the id 25 should be checked in the filed notes




################################################################################




## Data tranformation
# Aggregate by locality 
# total time
# total detections 
# number of observers


# Obtaining the detection and effort df
# detection is presence/absence by locality by each monitoring strata
# effort is the number of visual transects where cs were detected

df_monit_effort <- df_monit %>% 
  group_by(localidade, data, faixa_bat) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
  ungroup()
df_monit_effort
print(df_monit_effort, n=85)

# creating one line by locality

# effort data
df_monit_effort <- df_monit_effort %>%
  group_by(localidade) %>%
  reframe(max_tv = max(max_trsct_vis),
          n_divers = sum(n_divers),
          visib_m = max(visib_m),
          eff_raso = sum(ifelse(faixa_bat == "raso", n_detection, 0)),
          eff_entremare = sum(ifelse(faixa_bat == "entremare", n_detection, 0)),
          eff_fundo = sum(ifelse(faixa_bat == "fundo", n_detection, 0))) %>%
  ungroup()

df_monit_effort ###
print(df_monit_effort, n=35)



# detection data
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



## Processing and Standardization of geomorphology data by the deviation from average

glimpse(df_geo)

# mean of scores to each geo category at each locality
# deep and shallow

df_geo_pd <- df_geo %>% 
  filter(obs != "verificar") %>% 
  group_by(localidade, geo_cat) %>%
  mutate(geo_avg = mean(iar_geo)) %>% 
  ungroup()

df_geo_pd

## scaling mean scores from geo categories at each locality
## The value at each locality show the deviance from average

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
print(df_geo_pd_local, n = 33)
df_geo_pd_local = df_geo_pd_local[1:33, 1:6]
print(df_geo_pd_local, n = 33)

## Checking if columns matching by locality and data availability
## shortening names and storing as rdata


# effort data
effort = df_monit_effort
effort = effort %>% 
  left_join(df_geo_pd_local,effort,  by = "localidade") %>% 
  drop_na() %>% 
  select(localidade, eff_raso, eff_entremare, eff_fundo, max_tv, n_divers, visib_m, locality_comp_m = comp_m)
print(effort,n= 35)


# detection data
detection = df_monit_detec

detection = df_monit_detec %>% 
  left_join(df_geo_pd_local,detection,  by = "localidade") %>% 
  drop_na() %>% 
  select(localidade, detec_raso, detec_entremare, detec_fundo)
print(detection,n= 35)



# predictors

predictors = df_geo_pd_local %>% 
  left_join(effort, df_geo_pd_local, by = "localidade") %>%
  select(localidade, tf , mp , gc , rpm , lg )

print(predictors,n= 35)




print(effort, n = 33)
print(detection, n = 33)
print(predictors, n = 33)



save(effort, detection, predictors, file = "unmarked_data.RData")








