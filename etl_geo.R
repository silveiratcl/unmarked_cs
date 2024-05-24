
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
problems(df_geo)
df_geo = df_geo[, 1:14]
df_geo


# localities

df_localidade = read_delim("data/localidade_rebio.csv", delim = ";", 
                           col_types = c("d","c","d"))
df_localidade
print(df_localidade, n = 41)



####
####
# Tasks
# Create a table id_dafor with the corresponding values iar_geo



### test table dafor

df_monit_test = df_monit %>% 
  filter(data > "2023-01-01",
         obs != "estimado dos dados do ICMBio",
         dafor > 0,
         faixa_bat != "entremare"
         ) %>% 
  mutate(dafor_id = as.integer(dafor_id),
         geo_id = as.integer(geo_id))

df_monit_test  #ok


df_geo_test =    df_geo %>%
  filter(data > "2023-01-01",
         faixa_bat != "entremare"
         ) %>% 
  mutate(geo_id = as.integer(geo_id))
  
df_geo_test  #ok

#leftjoin

df_monit_geo = df_monit_test %>% 
  left_join(df_geo_test, join_by(geo_id), relationship = "many-to-many")  %>% 
  select(localidade.x, 
         data.x,
         tempo_censo, 
         dafor, 
         n_trans_vis.x, 
         n_trans_pres, 
         dafor_id, 
         geo_id, 
         tempo_geo, 
         geo_cat, 
         iar_geo,
        )


print(df_monit_geo, n = 50)



# creating time in minutes to geo and investigating differences in time 

df_monit_geo = df_monit_geo %>% 
  group_by(geo_id, tempo_geo) %>% 
  mutate(tempo_geo_minute = (max(tempo_geo)*5)) %>% 
  ungroup() %>% 
  group_by(geo_id) %>% 
  mutate(tempo_geo_max = (max(tempo_geo)*5), 
         dif_t_dafor_geo = max(tempo_censo) - tempo_geo_max,
         )


df_monit_geo
print(df_monit_geo, n = 30)


# creating table




df_model <- df_monit_geo %>%
  group_by(dafor_id) %>%
  reframe(
    n_trans = max(n_trans_pres),
    tf_std = (iar_geo[geo_cat == "tf"] - mean(iar_geo[geo_cat == "tf"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    mp_std = (iar_geo[geo_cat == "mp"] - mean(iar_geo[geo_cat == "mp"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    gc_std = (iar_geo[geo_cat == "gc"] - mean(iar_geo[geo_cat == "gc"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    rpm_std = (iar_geo[geo_cat == "rpm"] - mean(iar_geo[geo_cat == "rpm"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    lg_std = (iar_geo[geo_cat == "lg"] - mean(iar_geo[geo_cat == "lg"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "lg"], na.rm = TRUE)
  )

df_model
  



