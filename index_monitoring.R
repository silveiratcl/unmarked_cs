# load localidade centroid
# to extract distace values from raster

library("tidyverse")
library("raster")

df_centroide_localidade = read_delim("data/centroide_localidade.csv", delim = ",", 
                           col_types = c("d","d","d","i","s"))
df_centroide_localidade

# loading raster layer inv_dist
dist_inv <- raster("./data/dist_inv.tiff")


# loadind monitoring data
df_monit_index = read_delim("data/dados_monitoramento_cs_2024-03-22.csv",
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


# Contruir tabela
# localidade, dist_inv, dpue, days since last check


df_monit_effort_dpue_date = df_monit %>% 
  
  group_by(localidade, faixa_bat, data) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
  reframe(max_trsct_vis = sum(max(n_trans_vis)),
         n_detection = max(n_trans_pres),
         #dpue = n_detection/(sum(max(max_trsct_vis)/60)),
         days_sl_check = max(data - Sys.Date())) %>%

  ungroup()

print(df_monit_effort_dpue_date, n= 45)



df_last_check = df_monit_effort_dpue_date %>%
  group_by(localidade)
  reframe(sum_ntrsct = sum(max_trsct_vis),
          #sum_detec = sum(n_detection),
          dpue = sum(n_detection)/
          
  
  )
  






