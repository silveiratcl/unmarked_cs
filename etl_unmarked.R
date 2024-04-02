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
# total detections 
# number of observers

df_monit_effort <- df_monit %>% 
  group_by(localidade, data, faixa_bat) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  summarise(max_min = max(n_trans_vis),
            detection = max(n_trans_pres),
            n_divers = max(n_divers),
            eff_entremare = max(faixa_bat == "entremare"),
            eff_raso = max(faixa_bat == "raso"),
            eff_fundo = max(faixa_bat == "fundo")) %>%
  ungroup()
df_monit_effort


