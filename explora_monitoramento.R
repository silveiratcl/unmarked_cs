## 
# Indicadores Monitoramento

library(tidyverse)
library(readr)
library(tidyr)
library(stringr)

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
  mutate(faixa_bat = str_to_title(str_replace_all(faixa_bat, "entremare", "entremaré")),
         localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
    ungroup()
df_monit_effort
print(df_monit_effort, n=85)



################################################################################

#### graph by bathimetric strata ############################################### 

################################################################################

library(ggplot2)
library(RColorBrewer)




# n detections minutes

plot_detec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, n_detection, sum)) %>% 
  filter(n_detection > 0)  %>% 
  ggplot(aes(fill=factor(faixa_bat,levels=c("Entremaré", "Raso", "Fundo")), y=localidade, x=n_detection)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Número de Transectos com Coral-sol") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.key.size = unit(2, 'cm')
    ) 
  

plot_detec_strata 


# n transects

plot_transec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, max_trsct_vis, sum)) %>% 
  ggplot(aes(fill=factor(faixa_bat,levels=c("Entremaré", "Raso", "Fundo")), y=localidade, x=max_trsct_vis)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Número de Transectos por localidade") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(), ###
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.key.size = unit(2, 'cm')
  )

plot_transec_strata


# CPUE
# Creating dpue colunm


df_monit_effort_dpue <- df_monit %>% 
  
  group_by(localidade, data, faixa_bat) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  mutate(faixa_bat = str_to_title(str_replace_all(faixa_bat, "entremare", "entremaré")),
         localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            dpue = n_detection/(sum(max(max_trsct_vis)/60))) %>%
  ungroup()
print(df_monit_effort_dpue, n= 85)


plot_dpue_strata <- df_monit_effort_dpue %>% 
  filter(n_detection > 0)  %>% 
  mutate(localidade = fct_reorder(localidade, dpue, sum)) %>% 
  ggplot(aes(fill = factor(faixa_bat,levels=c("Entremaré", "Raso", "Fundo")), y=localidade, x=dpue)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("DPUE - Detecções/60mim ") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.key.size = unit(2, 'cm')
  )

plot_dpue_strata

# Geomorfolgia localidades

df_geo

# Modelos ocorrência por geomorfologia

# Pontos de invasão
