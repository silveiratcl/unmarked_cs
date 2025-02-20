# transecto como unidade amostral

library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(vegan)
library(labdsv)


#monitoramento 

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
  filter(data > "2022-01-01" &
           !(obs %in% c("Sem geo", "geo n?o realizada", "estimado dos dados do ICMBio"))) %>%
  arrange(dafor_id)

print(dfmonit_filt)

# selecionando as variáveis importantes

monit.trans <- dfmonit_filt[ ,c("localidade", "data", "visib_horiz", "faixa_bat", "n_trans_vis", "n_trans_pres", "n_divers", "dafor_id", "obs")] %>%
  group_by(localidade, data, faixa_bat, dafor_id, obs) %>%
  reframe(visib = max(visib_horiz),
          trans_vis = max(n_trans_vis),
          detections = max(n_trans_pres),
          divers = max(n_divers)) %>%
  arrange(dafor_id)

print(monit.trans[, 1:5], n = 169)


## obtendo o total de transectos vistos e detecções para cada localidade
## criando a variável minutos por mergulhador

monit2 <- monit %>%
  group_by(localidade) %>%
  summarise(vis = sum(visuals),
            det = sum(detec),
            t_divers = sum(divers),
            min.div = sum(vis*t_divers)) %>%
  arrange(desc(vis))

print(monit2, n = 76)