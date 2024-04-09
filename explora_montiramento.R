## 
# Indicadores Monitoramento

library(tidyverse)
library(readr)
library(tidyr)

## data

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
df_geo = df_geo[, 1:13]
df_geo

# localities

df_localidade = read_delim("data/localidade_rebio.csv", delim = ";", 
                           col_types = c("d","c","d"))
df_localidade
print(df_localidade, n = 41)


df_localidade$comp_m = df_localidade$comp_m/1000
df_localidade$comp_m/1


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
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
    ungroup()
df_monit_effort
print(df_monit_effort, n=85)



#### graph by bathimetric stra
# n detections minutes

library(ggplot2)

plot_detec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, n_detection, sum)) %>% 
  filter(n_detection > 0)  %>% 
  ggplot(aes(fill=faixa_bat, y=localidade, x=n_detection)) + 
  geom_bar(position="stack", stat="identity")

plot_detec_strata


# n transects

plot_transec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, max_trsct_vis, sum)) %>% 
  ggplot(aes(fill=faixa_bat, y=localidade, x=max_trsct_vis)) + 
  geom_bar(position="stack", stat="identity")

plot_transec_strata



## Positivos por localidade

# detection data
df_monit_detec <- df_monit_effort %>%
  group_by(localidade) %>%
  reframe(
    detec_raso = ifelse(eff_raso >= 1 , 1, 0),
    detec_entremare = ifelse(eff_entremare >= 1 , 1, 0),
    detec_fundo = ifelse(eff_fundo >= 1 , 1, 0)) %>%
  ungroup()

print(df_monit_detec, n = 35) ###











# Esforço por localidade

# Geomorfolgia localidades

# Modelos ocorrência por geomorfoloagia

# Pontos de invasão
