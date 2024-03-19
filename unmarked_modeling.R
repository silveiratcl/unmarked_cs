### Data ETL
## data from monitoring DAFOR

## covariates
# visib_m
# effort - minutes of observation
# tf - tocas e fendas - ver como padronizar
# mp - matacoes e paredoes
# gc - grutas e cavernas
# rpm - rochas pequenas e medias
# lg - lages

# unidade amostral - localidade


library(tidyverse)
library(readr)


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



df_geo = read_csv2("data/dados_geo_cs_2024-03-18.csv", 
                   col_types = list(localidade = col_character(),
                                    data = col_date(format = "%d/%m/%Y"),
                                    visibilidade = col_double(),
                                    faixa_bat = col_character(),
                                    prof_interface_min = col_double(),
                                    prof_interface_max = col_double(),
                                    metodo = col_character(),
                                    observador = col_character(),
                                    geo_cat = col_character(),
                                    iar_geo = col_double(),
                                    iar_geo_med = col_double(),
                                    iah_seg = col_double(),
                                    n_trans_vis = col_double(),
                                    geo_id = col_double())
                     )
spec(df_geo)
df_geo

# Agregar por localidade 
# total de minutos por localidade
# total de detecções no espaço de um minuto

df_monit_effort <- df_monit %>% 
  group_by(localidade) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  reframe(effort_min = max(n_trans_vis)*n_divers,
            detection = max(n_trans_pres)) %>%
  ungroup()

print(df_monit_effort, n = 27)





library(tidyverse)
eval_list_table <- eval_list %>% 
  # bind tables by row
  bind_rows() %>% 
  # filtering by metric eval and algo
  filter(metric.eval == "ROC") %>% 
  group_by(model, algo, metric.eval, preds) %>%
  summarise(avg_validation = mean(validation),
            sd_validation = sd(validation)) %>% 
  arrange(-avg_validation) %>% 
  
  ungroup()

eval_list_table
