# load localidade centroid
# to extract distace values from raster

library("tidyverse")
library("raster")
library("sp")
library("forcats")

df_centroide_localidade = read_delim("data/centroide_localidade.csv", delim = ",", 
                           col_types = c("d","d","d","i","s"))
df_centroide_localidade = df_centroide_localidade %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) 

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


# Build DF 
# localidade, dist_inv, dpue, days since last check


df_monit_effort_dpue_date = df_monit_index %>% 
  
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
  group_by(localidade) %>% 
  reframe(sum_ntrsct = sum(max_trsct_vis),
          sum_detec = sum(n_detection),
          dpue = sum(n_detection)/(sum_ntrsct/60),
          days_sl_check = max(data - Sys.Date())
          
  )
  
print(df_last_check, n = 35)


## Extract dist from raster ###
plot(dist_inv)
points(df_centroide_localidade[,1:2])

coords = data.frame(df_centroide_localidade$X, 
                    df_centroide_localidade$Y
                    )
coord_points <- SpatialPointsDataFrame(coords,
                                       data.frame(ID = 1:nrow(coords)),
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
values <- extract(dist_inv, coord_points)
values
df_centroide_localidade$distance = values/1000
print(df_centroide_localidade, n = 41)



### combining data ###

df_index = df_last_check %>% 
  left_join(df_centroide_localidade, df_last_check, by = "localidade") 

print(df_index, n = 35 )

 
############## first version ###################
min_max_normalize <- function(x) {
  if (all(is.na(x)) || all(is.infinite(x))) {
    return(x)  # Return input if all values are NA or infinite
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100)
  }
}





min_max_normalize(c(20, 30, 0.2, 12 ,14, 2000))

df_index_total = df_index %>% 
  group_by(localidade) %>% 
  mutate(
    dpue_scaled = min_max_normalize(dpue),
    days_sl_check_scaled = min_max_normalize(as.numeric(days_sl_check)),
    distance_scaled = min_max_normalize(distance),
    index_priority = dpue_scaled * days_sl_check_scaled * (1/distance_scaled), 
    index_priority_scaled = min_max_normalize(index_priority)
  ) %>% 
  arrange( -index_priority)


print(df_index_total, n = 35)

############################################################# stoped here

###############################################
plot_index <- df_index_total %>% 
  ggplot(aes(y = fct_reorder(localidade, index_priority), x = index_priority, fill = fct_relevel(regiao, "rebio", "entorno_imediato", "entorno"))) +
  # scale_fill_manual(values = c( 'grey')) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(position = "top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Índice de Prioridade para manejo/monitoramento ") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"),
    axis.line.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color = "#284b80"),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 15, color = "#284b80"),
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm')
  )


plot_index
ggsave("plots/index.png", width = 10, height = 5, dpi = 300)


#### teste




####



####

plot_dpue <- df_index %>%
  filter(dpue > 0) %>%
  ggplot(aes(y = fct_reorder(localidade, dpue), x = dpue, fill = fct_relevel(regiao, "rebio", "entorno_imediato"))) +
  scale_fill_manual(values = c( 'grey', '#990000' ),
                    labels = c("REBIO", "Entorno Imediato")) +
  
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(position = "top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("DPUE - Detecções/60mim") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"),
    axis.line.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color = "#284b80"),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 15, color = "#284b80"),
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm')
  )


plot_dpue
ggsave("plots/detec_dpue.png", width = 10, height = 5, dpi = 300)







