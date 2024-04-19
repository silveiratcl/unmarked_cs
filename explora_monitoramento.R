## 
# Indicadores Monitoramento

library(tidyverse)
library(readr)
library(tidyr)
library(stringr)
library(hb)

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
library(hrbrthemes)

# Positive instead DAFOR
# Geting the numbers

df_monit_dafor
table(df_monit_dafor$dafor_DAFOR)
length(df_monit_dafor$dafor_DAFOR)




# n detections minutes

plot_detec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, n_detection, sum)) %>% 
  filter(n_detection > 0)  %>% 
  ggplot(aes(fill=factor(faixa_bat,levels=c("Entremaré", "Raso", "Fundo")), y=localidade, x=n_detection)) +
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("0-3m", "3-8m", "8m-Interface")) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Detecções por faixa batimétrica") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm')
    ) 
  

plot_detec_strata 
ggsave("plots/detec_batimetria.png", width = 10, height = 5, dpi = 300)


# n transects

plot_transec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, max_trsct_vis, sum)) %>% 
  ggplot(aes(fill=factor(faixa_bat,levels=c("Entremaré", "Raso", "Fundo")), y=localidade, x=max_trsct_vis)) +
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("0-3m", "3-8m", "8m-Interface")) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Transectos (1min) por localidade") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm')
  )

plot_transec_strata
ggsave("plots/transec_batimetria.png", width = 10, height = 5, dpi = 300)

# CPUE
# Creating dpue colunm ######## VERIFY THE SUM TO OBTAIM MAX MINUTES BY LOCALITY


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
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99')) +
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
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm')
  )

plot_dpue_strata
ggsave("plots/detec_dpue.png", width = 10, height = 5, dpi = 300)

# Geomorfolgia total

df_geo_local <- df_geo %>% 
  filter(iar_geo != "Na") %>% 
  group_by(localidade, geo_cat) %>%
  mutate(geo_value = mean(iar_geo)) %>% 
  ungroup()

max(df_geo_local$geo_value)


##
bp_all_local = df_geo_local  %>%
  mutate(geo_cat = fct_relevel(geo_cat, "mp", "tf", "gc", "lg", "rpm" )) %>%
  ggplot( aes(fill = geo_cat, x = geo_cat, y = iar_geo)) +
  scale_fill_manual(values=c('#db6d10', '#db6d10', '#db6d10', '#536e99', '#536e99' )) +
  scale_x_discrete(labels = c('Matacões e Paredões','Tocas e Fendas','Grutas', "Lages", "Rochas P e M")) +
  geom_boxplot(lwd = 0.2) +
  scale_y_continuous(position="left", n.breaks = 10, expand = c(0, 0.05)) +
  ggtitle("Índice de Abragência Relativa das Geomorfologias (IAR GEO)") +
  xlab("") +
  labs(y = "IAR GEO") +
  #geom_jitter(color="black", size=0.2, alpha=0.5) +
    theme(
      panel.background = element_blank(),
      axis.ticks.y = element_line(colour = "grey",
                                  linewidth = 0.8, linetype = "solid"),
      axis.line.y = element_line(colour = "grey",
                                 linewidth = 0.8, linetype = "solid"),
      axis.text.x = element_text(size = 13,  color = "#284b80" ),
      axis.text.y = element_text(size = 15,  color = "grey" ),
      axis.title.y = element_text(size = 14,  color = "#284b80" ),
      legend.position="none",
      axis.ticks.x = element_blank(), 
      plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" )
  )



bp_all_local
ggsave("plots/geo_local.png", width = 10, height = 5, dpi = 300)



# Correlation entre as geo e ocorrencias
# combinar com id geo id
##### Stoped here
### TO DO

##### grou_by geo_id

df_monit_effort_eval = df_monit_effort %>% 
  group_by(localidade) %>% 
  filter(n_detection != 0) %>% 
  reframe(detections = sum(n_detection))
df_monit_effort_eval


df_geo_local_eval <- df_geo %>% 
  filter(iar_geo != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
  group_by(localidade, geo_cat) %>%
  reframe(mean_geo_value = max(iar_geo)) %>% 
  ungroup()
df_geo_local_eval


detec_vs_geo = df_monit_effort_eval %>% 
  left_join(df_geo_local_eval, df_monit_effort_eval, by = "localidade")
  

detec_vs_geo %>% 
  ggplot(aes(x=sqrt(mean_geo_value), y= sqrt(detections), shape = geo_cat, color=geo_cat)) + 
  geom_point() +
  geom_jitter() 
  # fazer esse figura melhor



# Dados padronizados unmarked 



# Standardidized data
#detections and predictors

load("unmarked_data.RData")

# exclude location where detection were 0
effort_positive <- effort %>%
  group_by(localidade) %>% 
  mutate(sum_detection = sum( eff_raso, eff_entremare, eff_fundo)) %>% 
  filter(sum_detection > 0)

effort_positive



detection
predictors





# Modelos ocorrência por geomorfologia

# Pontos de invasão
