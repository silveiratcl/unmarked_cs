 # Indicadores Monitoramento

library(tidyverse)
library(readr)
library(tidyr)
library(stringr)
library(hb)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(patchwork)
library(ggforce)

#### Data ####

# monitoring
df_monit = read_delim("data/dados_monitoramento_cs_2025-04-30.csv",
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

# add column localidade_rebio

unique(df_monit$localidade_rebio)



df_monit <- df_monit %>%
  mutate(localidade_rebio = if_else(localidade %in% c("rancho_norte",
                                                      "letreiro",
                                                      "pedra_do_elefante",
                                                      "costa_do_elefante",
                                                      "deserta_norte",  
                                                      "deserta_sul",
                                                      "portinho_norte",
                                                      "portinho_sul",
                                                      "enseada_do_lili",
                                                      "letreiro",
                                                      "costao_do_saco_dagua",
                                                      "saco_dagua",
                                                      "saco_da_mulata_norte",
                                                      "saco_da_mulata_sul",
                                                      "naufragio_do_lili",
                                                      "saquinho_dagua"
                                                      
                                                      ), "rebio", 
                                    
                                    if_else(localidade %in% c( "baia_das_tartarugas", 
                                                               "saco_do_batismo",
                                                               "vidal",
                                                               "farol",
                                                               "engenho",
                                                               "saco_do_capim"), "entorno_imediato", "entorno")))
##################!!!!!!!!!!!!!!!!!!!!!!


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

df_localidade = read_delim("data/localidade_rebio2.csv", delim = ";", 
                           col_types = c("i","c","c","d"))
df_localidade
print(df_localidade, n = 48)


df_localidade = df_localidade %>% 
mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")))

df_localidade

df_localidade$comp_m = df_localidade$comp_m/1000
df_localidade$comp_m/1


# management data from "modeling_weight_class.R" 

df_manag_mass = read_delim("data/data_mass_class_pred.csv",
                           col_types = list(Local_1 = col_character(),
                                            Local_2 = col_character(),
                                            Local_3 = col_character(),
                                            Data = col_date(format = "%d/%m/%Y"),
                                            class_1 = col_double(),
                                            class_2 = col_double(),
                                            class_3 = col_double(),
                                            class_4 = col_double(),
                                            class_5 = col_double(),
                                            total = col_double(),
                                            pred_mass = col_double()
                                            ))

print(df_manag_mass, n = 160)





# Shapefile localities and rebio

shp_localidades = st_read("data/localidades_shapefile.shp")
shp_rebio = st_read("Rebio_Arvoredo_Ilhas_POL_CGS_WGS84.shp")


### Data processing ### 

# Aggregate by locality 
# total time
# total detections 

# create faixa_bat based on prof_min e prof_max data

# Obtaining the detection and effort df
# detection is presence/absence by locality by each monitoring strata
# effort is the number of visual transects where cs were detected

table(df_monit$prof_min)
table(df_monit$prof_max)

df_monit_effort <- df_monit  %>% 
  group_by(localidade_rebio, localidade, data, faixa_bat) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
    ungroup()
df_monit_effort
print(df_monit_effort, n=86)


df_monit_effort$localidade

df_monit_effort %>% 
filter(localidade == "BAIA DAS TARTARUGAS")

######################################
######################################
df_monit_effort <- df_monit %>% 
  # Convert prof_min and prof_max to numeric
  mutate(prof_min_num = as.numeric(prof_min),
         prof_max_num = as.numeric(prof_max)) %>%
  # Create the new depth interval variable
  mutate(faixa_bat_depth = case_when(
    prof_max_num <= 2 ~ "0-2m",
    prof_max_num > 2.1 & prof_max_num <= 8 ~ "2.1-8m",
    prof_max_num > 8.1 & prof_max_num <= 14 ~ "8.1-14m",
    prof_max_num > 14.1 ~ "14.1m+",
    TRUE ~ NA_character_
  )) %>%
  # Now proceed with your original processing but using faixa_bat_depth
  group_by(localidade_rebio, localidade, data, faixa_bat_depth) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
  ungroup()

df_monit_effort
print(df_monit_effort, n=140)


#####################################
#####################################


# left_join distance of localities

df_monit_effort <- df_monit_effort %>% 
  left_join(
    df_localidade %>% dplyr::select(localidade, comp_m), 
    by = "localidade"
  )


print(df_monit_effort, n= 140
      )


#### TEST df

#df_monit %>% 
# filter(dafor > 0 , metodo == "scuba", obs != "estimado dos dados do ICMBio", obs != "Sem geo" ) %>% 
 # print( n=63)

#df_monit_effort %>% 
 #filter(obs == "estimado dos dados do ICMBio") 


#df_monit_effort %>% 
  #filter(faixa_bat != "Na") 

#df_monit_effort %>% 
  #filter(faixa_bat == "Na") 

#### df

############# Relative abundance index weighed by effort
######################
library(dplyr)
library(stringr)
library(tidyr)

clean_num <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("Na") %>%
    str_replace_all(",", ".") %>%  # decimal comma -> dot
    as.numeric()
}

df_monit_iarw <- df_monit %>% 
  # Clean & convert to numeric safely
  mutate(
    prof_min_num   = clean_num(prof_min),
    prof_max_num   = clean_num(prof_max),
    visib_horiz_num= clean_num(visib_horiz)
  ) %>%
  # Depth bins (contiguous)
  mutate(faixa_bat_depth = case_when(
    !is.na(prof_max_num) & prof_max_num <= 2                      ~ "0-2m",
    !is.na(prof_max_num) & prof_max_num > 2  & prof_max_num <= 8  ~ "2.1-8m",
    !is.na(prof_max_num) & prof_max_num > 8  & prof_max_num <= 14 ~ "8.1-14m",
    !is.na(prof_max_num) & prof_max_num > 14                      ~ "14.1m+",
    TRUE ~ NA_character_
  )) %>%
  group_by(localidade_rebio, localidade, data, faixa_bat_depth) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(
    localidade       = str_to_upper(str_replace_all(localidade, "_", " ")),
    localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")),
    weight           = coalesce(as.numeric(dafor) / 10, 0)  # robust if dafor has NA
  ) %>%
  summarise(
    n_minutes   = dplyr::n(),                # 1 row = 1 minute
    Nhours      = n_minutes / 60,
    sum_weight  = sum(weight, na.rm = TRUE),
    n_detection = max(n_trans_pres, na.rm = TRUE) %>% na_if(-Inf),
    n_divers    = max(n_divers,    na.rm = TRUE) %>% na_if(-Inf),
    visib_m     = max(visib_horiz_num, na.rm = TRUE) %>% na_if(-Inf),
    .groups = "drop"
  ) %>%
  # Join distance and compute Uni100m + index
  left_join(
    df_localidade %>%
      mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
      dplyr::select(localidade, comp_m),
    by = "localidade"
  ) %>%
  mutate(
    Uni100m = comp_m / 100,
    DAFOR_weighted_index = if_else(
      Nhours > 0 & Uni100m > 0,
      sum_weight / (Nhours * Uni100m),
      NA_real_
    )
  )

print(df_monit_iarw, n = 140)











#### Charting by bathimetry strata ############################################### 


library(ggplot2)
library(RColorBrewer)
library(hrbrthemes)

# Positive instead DAFOR
# Getting the numbers
#df_monit_dafor
#table(df_monit_dafor$dafor_DAFOR)
#length(df_monit_dafor$dafor_DAFOR)

df_monit
table(df_monit$dafor)
sum(table(df_monit$dafor))
length(df_monit$dafor)/60

table(df_monit_effort$localidade)
table(df_monit_effort$faixa_bat_depth)


# without icmbio data
 
df_table  = df_monit %>% 
  filter(obs != "estimado dos dados do ICMBio") 

table(df_table$dafor)
sum(table(df_table$dafor))





# n detections minutes

plot_detec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, n_detection, sum)) %>% 
  filter(n_detection > 0)  %>% 
  ggplot(aes(fill=factor(faixa_bat_depth,levels=c( "0-2m", "2.1-8m", "8.1-14m", "14.1m+")), y=localidade, x=n_detection)) +
  
  scale_fill_manual(values=c('#db6d10', '#aaee4b','#416f02','#536e99')) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Total de transectos (1 min.) com presença de coral-sol (2022-2025)") +
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
    legend.key.size = unit(.8, 'cm')
    ) 
  

plot_detec_strata 
ggsave("plots/detec_batimetria.png", width = 10, height = 5, dpi = 300)

# n transects by locality
plot_transec_strata <- df_monit_effort %>% 
  mutate(localidade_rebio = factor(localidade_rebio, levels = c("REBIO", "ENTORNO IMEDIATO", "ENTORNO")),
         localidade = factor(localidade)) %>%
  ggplot(aes(fill = factor(faixa_bat_depth, levels = c("0-2m", "2.1-8m", "8.1-14m", "14.1m+")), 
             y = reorder(localidade, max_trsct_vis, sum), 
             x = max_trsct_vis)) +
  scale_fill_manual(values=c('#db6d10', '#aaee4b','#416f02','#536e99')) +
  geom_bar(position="stack", stat="identity", width = 0.8) +
  
  #facet_wrap(~ localidade_rebio, ncol = 1, scales = "free_y") + # cat jump
  facet_grid(rows = vars(localidade_rebio), scales = "free_y", space = "free_y", switch = "both") +
 
  
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Esforço - Total de Transectos (1 min.) por localidade (2022-2025)") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80", margin = margin(t = 10, b = 20)  ),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.title = element_blank(),
    legend.key.size = unit(.8, 'cm'),
    axis.text.y = element_text(size = 5),
    panel.spacing = unit(1, "lines"),# Adjust spacing between facets
    strip.text.y = element_text(size = 6)
    
  )
plot_transec_strata
ggsave("plots/transec_batimetria.png", width = 10, height = 10, dpi = 300)


#### CPUE #########################################################################
# Detections/60min*100m

df_monit_effort_dpue <- df_monit_effort %>% 
  # Calculate DPUE per 60min per 100m
  mutate(
    effort_hours = max_trsct_vis / 60,  # Convert minutes to hours
    locality_100m = comp_m / 100,       # Convert meters to 100m units
    dpue_standard = n_detection / (effort_hours * locality_100m)
  ) %>%
  
  # Group and summarize (if needed)
  group_by(localidade, data, faixa_bat_depth) %>%
  summarise(
    comp_m = first(comp_m),
    total_effort_hours = sum(effort_hours),
    total_detections = sum(n_detection),
    dpue_standard = sum(n_detection) / (sum(effort_hours) * first(locality_100m)),
    .groups = "drop"
  ) %>%
  
  # Round for readability
  mutate(across(where(is.numeric), ~round(., 2)))


print(df_monit_effort_dpue, n= 140)


### sum faixa bat

plot_dpue_strata <- df_monit_effort_dpue %>% 
  filter(total_detections > 0)  %>% 
  mutate(localidade = fct_reorder(localidade, dpue_standard, sum)) %>% 
  ggplot(aes(fill = factor(faixa_bat_depth,levels=c("0-2m", "2.1-8m", "8.1-14m", "14.1m+")), y=localidade, x=dpue_standard)) +
  scale_fill_manual(values=c('#db6d10', '#aaee4b','#416f02','#536e99')) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  
  labs(
    title = "Detecções por Unidade de Esforço (2022-2025)",
    #subtitle = "DPUE - Detecções / (Horas * Unidades de 100m)"
    ) +
 # ggtitle("Detecções por Unidade de Esforço - Detecções/H/100m ") +
  
  
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
    plot.subtitle = element_text(hjust = 0.5, size = 12, color ="#284b80" ),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.title = element_blank(),
    legend.key.size = unit(.8, 'cm'),
  )

plot_dpue_strata
ggsave("plots/detec_dpue.png", width = 10, height = 5, dpi = 300)


################################################################################
################## RAIW
################## Data processing (manual weights, same denominator as DPUE)
################################################################################
library(dplyr)
library(stringr)
library(tidyr)

# ---- Set manual weights for DAFOR scores ----
# Edit these to taste (concave example below):
# D=10→1.00, A=8→0.55, F=6→0.25, O=4→0.10, R=2→0.04, Absent=0→0
manual_weights <- c(
  `10` = 1.00,
  `8`  = 0.8,
  `6`  = 0.6,
  `4`  = 0.1,
  `2`  = 0.04,
  `0`  = 0.00
)

clean_num <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    na_if("Na") %>%
    str_replace_all(",", ".") %>%  # decimal comma -> dot
    as.numeric()
}

df_monit_iarw <- df_monit %>% 
  # Clean & convert to numeric safely
  mutate(
    prof_min_num    = clean_num(prof_min),
    prof_max_num    = clean_num(prof_max),
    visib_horiz_num = clean_num(visib_horiz)
  ) %>%
  # Depth bins (contiguous)
  mutate(faixa_bat_depth = case_when(
    !is.na(prof_max_num) & prof_max_num <= 2                      ~ "0-2m",
    !is.na(prof_max_num) & prof_max_num > 2  & prof_max_num <= 8  ~ "2.1-8m",
    !is.na(prof_max_num) & prof_max_num > 8  & prof_max_num <= 14 ~ "8.1-14m",
    !is.na(prof_max_num) & prof_max_num > 14                      ~ "14.1m+",
    TRUE ~ NA_character_
  )) %>%
  group_by(localidade_rebio, localidade, data, faixa_bat_depth) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(
    localidade       = str_to_upper(str_replace_all(localidade, "_", " ")),
    localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")),
    dafor_num        = coalesce(as.numeric(dafor), 0),
    # ----- MANUAL WEIGHTING (no gamma) -----
    weight           = manual_weights[as.character(dafor_num)],
    weight           = ifelse(is.na(weight), 0, weight)
  ) %>%
  summarise(
    # SAME denominator as DPUE: minutes from n_trans_vis if available
    effort_minutes = ifelse(all(is.na(n_trans_vis)), n(), max(n_trans_vis, na.rm = TRUE)),
    effort_hours   = effort_minutes / 60,
    Nhours         = effort_hours,  # keep for compatibility
    sum_weight     = sum(weight, na.rm = TRUE),
    n_detection    = max(n_trans_pres, na.rm = TRUE) %>% na_if(-Inf),
    n_divers       = max(n_divers,    na.rm = TRUE) %>% na_if(-Inf),
    visib_m        = max(visib_horiz_num, na.rm = TRUE) %>% na_if(-Inf),
    .groups = "drop"
  ) %>%
  # Join distance and compute Uni100m + index
  left_join(
    df_localidade %>%
      mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
      dplyr::select(localidade, comp_m),
    by = "localidade"
  ) %>%
  mutate(
    Uni100m       = comp_m / 100,
    raiw_standard = dplyr::if_else(
      Nhours > 0 & Uni100m > 0,
      sum_weight / (Nhours * Uni100m),
      NA_real_
    )
  )

print(df_monit_iarw, n = 140)


#### Plot RAIW (stacked by depth stratum) #######################################
plot_raiw_strata <- df_monit_effort_raiw %>%
  filter(total_sum_weight > 0, is.finite(raiw_standard)) %>%
  mutate(localidade = fct_reorder(localidade, raiw_standard, .fun = sum, .na_rm = TRUE)) %>% 
  ggplot(aes(
    fill = factor(faixa_bat_depth, levels = c("0-2m", "2.1-8m", "8.1-14m", "14.1m+")),
    y = localidade,
    x = raiw_standard
  )) +
  scale_fill_manual(values = c('#db6d10', '#aaee4b', '#416f02', '#536e99')) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(position = "top", n.breaks = 10, expand = c(0, 0)) +
  labs(
    title    = "Índice de Abundância Relativa Ponderado (2022–2025)",
    subtitle = paste0("RAI-W = sum_i w(s_i) / (Nhours * Uni100m)
 ")
  ) +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"),
    axis.line.x = element_line(colour = "grey", linewidth = 0.8, linetype = "solid"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color = "#284b80"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#284b80"),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 15, color = "#284b80"),
    legend.title = element_blank(),
    legend.key.size = unit(.8, 'cm')
  )

plot_raiw_strata
ggsave("plots/raiw.png", width = 10, height = 5, dpi = 300)


# Getting values to paste on the figure


df_monit_effort_raiw %>% 
  filter(total_sum_weight > 0, is.finite(raiw_standard)) %>%
  group_by(localidade) %>%
  summarise(
    total_sum_weight = sum(total_sum_weight, na.rm = TRUE),
  )



################################################################################
## Plot combined DEPUE and RAIW by depth strata for paper

library(patchwork)

# Left: DPUE — put label on TOP x-axis
plot_dpue_clean <- plot_dpue_strata +
  labs(title = NULL, subtitle = NULL, x = "DPUE", y = NULL) +
  theme(
    axis.title.x        = element_blank(),                 # hide bottom title
    axis.title.x.top    = element_text(size = 14, color = "#284b80"),
    axis.title.y        = element_text(size = 14, color = "#284b80")
  )

# Right: RAI-W — put label on TOP x-axis, no y label (to avoid duplication)
plot_raiw_clean <- plot_raiw_strata +
  labs(title = NULL, subtitle = NULL, x = "RAI-W", y = NULL) +
  theme(
    axis.title.x        = element_blank(),
    axis.title.x.top    = element_text(size = 14, color = "#284b80"),
    axis.title.y        = element_blank()
  )

# Combine with A/B tags
combined <- (plot_dpue_clean + plot_raiw_clean) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position   = "bottom",
    plot.tag          = element_text(face = "bold", size = 14),
    plot.tag.position = c(1, 1)  # top-left of each subplot
  )

combined
ggsave("plots/dpue_raiw_side_by_side.png", combined, width = 12, height = 5, dpi = 300)





################################################################################
#Density plot faixa_bat_depth Detections Geral


df_depth <- df_monit %>% 
  # Convert prof_min and prof_max to numeric
  mutate(prof_min_num = as.numeric(prof_min),
         prof_max_num = as.numeric(prof_max)) %>%
  # Create the new depth interval variable
  mutate(faixa_bat_depth = case_when(
    prof_max_num <= 2 ~ "0-2 m",
    prof_max_num > 2.1 & prof_max_num <= 4 ~ "2.1-4 m",
    prof_max_num > 4.1 & prof_max_num <= 6 ~ "4.1-6 m",
    prof_max_num > 6.1 & prof_max_num <= 8 ~ "6.1-8 m",
    prof_max_num > 8.1 & prof_max_num <= 10 ~ "8.1-10 m",
    prof_max_num > 10.1 & prof_max_num <= 12 ~ "10.1-12 m",
    prof_max_num > 12.1 & prof_max_num <= 14 ~ "12.1-14 m",
    prof_max_num > 14.1 & prof_max_num <= 16 ~ "14.1-16 m",
    prof_max_num > 16.1 & prof_max_num <= 18 ~ "16.1-18 m",
    prof_max_num > 18.1 & prof_max_num <= 20~ "18.1-20 m",
    prof_max_num > 20.1 ~ "20.1m+",
    TRUE ~ NA_character_
  )) %>% 

group_by(localidade_rebio, localidade, data, faixa_bat_depth) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat_depth != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
  ungroup()


depth_levels <- c("0-2 m", "2.1-4 m", "4.1-6 m", "6.1-8 m", "8.1-10 m", 
                  "10.1-12 m", "12.1-14 m", "14.1-16 m", "16.1-18 m", 
                  "18.1-20 m", "20.1 m+")


ggplot(df_depth, aes(x = factor(faixa_bat_depth, levels = depth_levels ),  y = n_detection)) +
  geom_col(alpha = 0.5, fill = '#536e99' ) +
  labs(x = "Faixa Batimétrica", 
       y = "N. detecções",
       title = "Detecções por Faixa de Profundidade (2022-2025)",
       subtitle = "REBIO Arvoredo e Entorno Imediato") +
  scale_y_continuous(limits = c(0, 50)) +
  theme(

    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.line.y = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color ="#284b80" ),
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.key.size = unit(.8, 'cm'),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12))


ggsave("plots/density_faixa_bat.png", width = 10, height = 5, dpi = 300) 
  
# alternative plot using same intervals from previous plots

df_depth <- df_monit %>% 
  # Convert prof_min and prof_max to numeric
  mutate(prof_min_num = as.numeric(prof_min),
         prof_max_num = as.numeric(prof_max)) %>%
  # Create the new depth interval variable
  mutate(faixa_bat_depth = case_when(
    prof_max_num <= 2 ~ "0-2 m",
    prof_max_num > 2.1 & prof_max_num <= 8 ~ "2.1-8 m",
    prof_max_num > 8.1 & prof_max_num <= 14 ~ "8.1-14 m",
    prof_max_num > 14.1 ~ "14.1-20 m",
    TRUE ~ NA_character_
  )) %>% 
  
  group_by(localidade_rebio, localidade, data, faixa_bat_depth) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat_depth != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
  ungroup()


depth_levels <- c("0-2 m", "2.1-8 m", "8.1-14 m", "14.1-20 m")


print(df_depth, n=136)  


ggplot(df_depth, aes(x = factor(faixa_bat_depth, levels = depth_levels ),  y = n_detection)) +
  geom_col(alpha = 0.5, fill = '#536e99' ) +
  labs(x = "Faixa Batimétrica", 
       y = "Number of detections",
       title = "Detecções por Faixa de Profundidade (2022-2025)",
       subtitle = "REBIO Arvoredo e Entorno Imediato") +
  scale_y_continuous(limits = c(0, 100)) +
  theme(
    
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.line.y = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color ="#284b80" ),
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.key.size = unit(.8, 'cm'),
    
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12))


ggsave("plots/density_faixa_bat2.png", width = 10, height = 5, dpi = 300) 


### interface mean depth

library(dplyr)

# Step 1: filter and reduce to unique depth per dafor_id
base <- df_monit %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat == "fundo") %>%
  group_by(localidade_rebio, dafor_id) %>%
  summarise(depth = mean(as.numeric(prof_max), na.rm = TRUE), .groups = "drop")

# Step 2: summarise by locality
by_locality <- base %>%
  group_by(localidade_rebio) %>%
  summarise(
    n = n(),  # number of unique dafor_id’s considered
    mean_depth = mean(depth, na.rm = TRUE),
    min_depth  = min(depth,  na.rm = TRUE),
    max_depth  = max(depth,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(localidade_rebio)

# Step 3: overall summary across all localities
overall <- base %>%
  summarise(
    n = n(),
    mean_depth = mean(depth, na.rm = TRUE),
    min_depth  = min(depth,  na.rm = TRUE),
    max_depth  = max(depth,  na.rm = TRUE)
  ) %>%
  mutate(localidade_rebio = "ALL") %>%
  relocate(localidade_rebio, .before = n)

# Combine and show
bind_rows(by_locality, overall) %>% print(n = Inf)








###################
# Dafor Density plot (IAR of localidades)

filtered_df <- df_monit %>%
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")),
    year = year(data)  # Extract year from 'data'
  ) %>%
  filter(localidade_rebio != c("ENTORNO")) %>% 
  # Add a column counting rows per locality-year group (keeps all original data)
  add_count(localidade, year, name = "n_trans_count") %>%
  ungroup()

filtered_df    

#install.packages("ggridges")
library(ggridges)

# Create density plot general
ggplot(filtered_df, aes(x = dafor,y = localidade , fill = localidade)) +
  geom_density_ridges(alpha = 0.5) +
  labs(x = "IAR", 
       title = "Distribuição da Densidade IAR por Localidade",
       subtitle = "REBIO Arvoredo e Entorno Imediato",
       fill = "Localidade") +
  theme(legend.position = "none",
        panel.background = element_blank()) +
  scale_x_continuous(limits = c(0, max(filtered_df$dafor)), breaks = c(0,2,4,6,8,10))  # Start at 0 since DAFOR can't be negative

ggsave("plots/density_IAR.png", width = 10, height = 5, dpi = 300)


# Create density plot by year
# Compares the DAFOR between oldest and newest years
# Displays number of visual transects (n=) in each year label

##############################
### 1. Prepare and check data
##############################
data <- df_monit %>%
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")                           ),
    year = year(data)
  ) %>%
  filter(localidade_rebio != "ENTORNO",
         obs != "estimado dos dados do ICMBio")

# Check if data exists
if(nrow(data) == 0) stop("No data found")

density_data <- data %>%
  group_by(year) %>%
  mutate(
    n_trans_count = n(),
    total_dafor = sum(dafor, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(year) %>%
  mutate(
    year_label = paste0(year, " (n=", n_trans_count, ")"),
    year_label = factor(year_label, levels = unique(year_label))
  )

sum(is.na(density_data$dafor))

#checks
sum(table(density_data$data))
#looks like we are inflating

sum(table(df_monit$data))



##############################
### 2. Set up comparison parameters
##############################
years <- levels(density_data$year_label)
oldest <- years[1]
newest <- years[length(years)]

# Get comparison values
dafor_compare <- density_data %>%
  filter(year_label %in% c(oldest, newest)) %>%
  distinct(year_label, .keep_all = TRUE) %>%
  arrange(year_label)

# Color scheme: red if newest > oldest, blue otherwise
plot_colors <- ifelse(
  years == newest,
  ifelse(dafor_compare$total_dafor[2] > dafor_compare$total_dafor[1], 
         "red", "#6B8EFF"),
  ifelse(years == oldest, "grey70", "grey90")
)

##############################
### 2. Set up comparison parameters
##############################
years <- levels(density_data$year_label)

# Initialize all colors as grey90 (default for non-comparison years)
plot_colors <- rep("grey90", length(years))

# Compare each consecutive pair
for (i in 1:(length(years)-1)) {
  # Get comparison values for this pair
  dafor_compare <- density_data %>%
    filter(year_label %in% years[c(i, i+1)]) %>%
    distinct(year_label, .keep_all = TRUE) %>%
    arrange(year_label)
  
  # Only set color for the newer year in each comparison
  if (dafor_compare$total_dafor[2] > dafor_compare$total_dafor[1]) {
    plot_colors[i+1] <- "red"  # Newer year increased
  } else {
    plot_colors[i+1] <- "#6B8EFF"  # Newer year decreased or stayed same
  }
  
  # Set older year to grey70 (only if not already set by a previous comparison)
  if (plot_colors[i] == "grey90") {
    plot_colors[i] <- "grey70"
  }
}

# Now map these colors back to your original data
plot_colors_final <- plot_colors[match(density_data$year_label, years)]
    

##############################
### 3. Create individual plots
##############################
plot_list <- lapply(years, function(yr) {
  is_last_plot <- (yr == years[length(years)])
  
  # Calculate y-max for consistent annotation positioning
  dens <- density(filter(density_data, year_label == yr)$dafor)
  y_max <- max(dens$y)
  
  ggplot(filter(density_data, year_label == yr), aes(x = dafor)) +
    geom_density(fill = plot_colors[which(years == yr)], alpha = 0.8) +
    
    # Year annotation in upper right
    annotate(
      "text",
      x = 9.5,            # Right-aligned (near x=10)
      y = y_max * 0.95,   # 95% of max density height
      label = yr,
      size = 9,
      hjust = 1,          # Right-justified
      vjust = 1           # Top-justified
    ) +
    
    scale_x_continuous(
      limits = c(0, 10),
      breaks = seq(0, 10, 2),
      labels = seq(0, 10, 2)
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    theme_minimal() +
    theme(
      plot.margin = margin(5, 5, 5, 5, "pt"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      axis.title.x = if (is_last_plot) element_text(size = 14) else element_blank(),
      axis.text.x = if (is_last_plot) element_text(size = 12) else element_blank()
    ) +
    labs(x = if (is_last_plot) "IAR" else NULL)
})




##############################
### 4. Combine and display
##############################
final_plot <- wrap_plots(plot_list, ncol = 1) +
  plot_annotation(
    title = "Distribuição de IAR por ano - REBIO e Entorno Imediato",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold",
                                margin = margin(b = 10))
    )
  )

print(final_plot)


 # 5. Save to file as backup
ggsave("plots/density_vertical_rebio.png", final_plot, width = 10, height = 5, dpi = 300)


################################################################################
### Alternative Table for the data
################################################################################
library(dplyr)
library(tidyr)
library(kableExtra)

table(density_data$dafor)


# First create the properly named summary table
dafor_table <- density_data %>%
  mutate(
    year_label = str_sub(year_label, 1, 4),
    dafor_category = case_when(
      is.na(dafor) ~ "Ausente",
      dafor == 10 ~ "D",
      dafor == 8 ~ "A",
      dafor == 6 ~ "F",
      dafor == 4 ~ "O",
      dafor == 2 ~ "R",
      TRUE ~ "Ausente"
    )
  ) %>%
  count(year_label, dafor_category) %>%
  complete(year_label, dafor_category, fill = list(n = 0)) %>%
  pivot_wider(
    names_from = dafor_category,
    values_from = n
  ) %>%
  # Ensure all categories exist even if no observations
  { if(!"Ausente" %in% names(.)) mutate(., Ausente = 0) else . } %>%
  { if(!"D" %in% names(.)) mutate(., D = 0) else . } %>%
  { if(!"A" %in% names(.)) mutate(., A = 0) else . } %>%
  { if(!"F" %in% names(.)) mutate(., F = 0) else . } %>%
  { if(!"O" %in% names(.)) mutate(., O = 0) else . } %>%
  { if(!"R" %in% names(.)) mutate(., R = 0) else . } %>%
  select(year_label, D, A, F, O, R, Ausente) %>%
  mutate(Total = rowSums(across(-year_label)))

# Function to determine cell color based on value
get_color <- function(value, max_value) {
  if (value == 0) {
    return("#d4edda")  # light green
  } else if (value == max_value) {
    return("#f8d7da")  # light red
  } else {
    return("#fff3cd")  # light yellow
  }
}

# Get maximum values for each DAFOR category
max_values <- dafor_table %>% 
  select(D, A, F, O, R, Ausente) %>% 
  summarise(across(everything(), max)) %>% 
  as.list()

# Create a list of background colors for each column
bg_colors <- dafor_table %>%
  mutate(across(D:Ausente, ~ mapply(get_color, ., max_values[[cur_column()]]))) %>%
           select(-year_label, -Total) %>%
           as.list()
         
         # Now create the formatted table with correct column names and coloring
         dafor_table %>%
           rename(`Ano` = year_label) %>%
           kable("html", 
                 digits = 0,
                 align = "c",
                 escape = FALSE) %>%
           kable_styling(
             bootstrap_options = c("striped", "hover", "condensed"),
             full_width = FALSE,
             font_size = 14,
             html_font = "Arial"
           ) %>%
           column_spec(1, bold = TRUE, width = "8em") %>%
           column_spec(2:7, width = "5em") %>%  # Removed static background color
           column_spec(8, bold = TRUE, width = "6em", background = "#e6f2ff") %>%
           add_header_above(
             c(" " = 1, 
               "Escala DAFOR" = 6, 
               " " = 1),
             bold = TRUE,
             font_size = 16,
             background = c("white", "#536e99", "white"),
             color = c("black", "white", "black")
           ) %>%
           footnote(
             general = "D = Dominante, A = Abundante, F = Frequente, O = Ocasional, R = Raro",
             general_title = "",
             footnote_as_chunk = TRUE
           ) %>%
           # Apply conditional formatting to each column
           column_spec(2, background = bg_colors$D) %>%
           column_spec(3, background = bg_colors$A) %>%
           column_spec(4, background = bg_colors$F) %>%
           column_spec(5, background = bg_colors$O) %>%
           column_spec(6, background = bg_colors$R)# %>%
           #column_spec(7, background = bg_colors$Ausente)
         
         

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

         # --- Build from raw data (robust to missing columns) ---
         data_clean <- density_data %>%
           mutate(
             year = str_sub(year_label, 1, 4),
             dafor_cat = case_when(
               dafor == 10 ~ "D",
               dafor == 8  ~ "A",
               dafor == 6  ~ "F",
               dafor == 4  ~ "O",
               dafor == 2  ~ "R",
               TRUE        ~ NA_character_   # treat others (e.g., 0 or NA) as absence
             ),
             is_absence = is.na(dafor_cat)   # <- if you want only dafor==0, use: (dafor == 0)
           )
         
         # Totals and absences per year
         effort_abs <- data_clean %>%
           group_by(year) %>%
           summarise(
             Total   = n(),
             Ausente = sum(is_absence),
             .groups = "drop"
           )
         
         # Category counts (stacked parts), excluding absences
         cats <- data_clean %>%
           filter(!is.na(dafor_cat)) %>%
           count(year, dafor_cat, name = "count") %>%
           complete(year, dafor_cat = c("D","A","F","O","R"), fill = list(count = 0)) %>%
           mutate(
             dafor_cat = factor(dafor_cat, levels = c("D","A","F","O","R")),
             year      = factor(year, levels = sort(unique(year)))
           )
         
         # Where to place the annotation (top of each stacked bar)
         bar_tops <- cats %>%
           group_by(year) %>%
           summarise(bar_top = sum(count), .groups = "drop") %>%
           left_join(effort_abs, by = "year") %>%
           mutate(
             label = paste0("Abs.", Ausente, "| Tot.", Total)
           )
         
        stacked_dafor  = ggplot(cats, aes(x = year, y = count, fill = dafor_cat)) +
           geom_col() +
          ylim(0, 100) +
           # anotações de Ausente + Total
           geom_text(
             data = bar_tops,
             aes(x = year, y = bar_top, label = label),
             vjust = -0.4, size = 4, inherit.aes = FALSE
           ) +
           labs(
             #x = "Year",
             y = "Total Visual Transects",
             fill = "",
             #title = "DAFOR categories by year (stacked)",
             #subtitle = "Annotations show absences (Ausente) and total effort per year"
           ) +
           scale_fill_viridis_d(option = "plasma", begin = 0.9, end = 0.1)+
           # paleta daltônico-friendly
           #scale_fill_brewer(palette = "Set2") +
           theme_minimal(base_size = 12) +
           theme(
             panel.grid = element_blank(),     # remove todas as linhas de grid
             axis.line = element_line(),       # mantém eixos visíveis
             panel.border = element_blank(),
             axis.title.x = element_blank(),
             legend.text = element_text(size=11 ),
             legend.key.size = unit(.8, 'cm'),
             
           )
         
        stacked_dafor
         ggsave("plots/stacked_dafor.png",  stacked_dafor, width = 10, height = 5, dpi = 300)  

################################################################################
################# IARDensity by year

         #IARDensity by year
 
 
 # Stacked raiw_standar by year and depth strata         
 
 
 total_iar_w_by_year<-df_monit_iarw %>%
   filter(is.finite(raiw_standard), raiw_standard > 0) %>%
   mutate(year = year(data),
          localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
          faixa_bat_depth = factor(faixa_bat_depth, levels = c("0-2m", "2.1-8m", "8.1-14m", "14.1m+"))) %>%
   group_by(year, faixa_bat_depth) %>%
   summarise(total_iarw = sum(raiw_standard, na.rm = TRUE), .groups = "drop") %>%
   ggplot(aes(x = factor(year), y = total_iarw, fill = faixa_bat_depth)) +
   geom_col(position = "stack") +
   scale_fill_manual(values = c('#db6d10', '#aaee4b', '#416f02', '#536e99')) +
   labs(x = "Year",
        y = "RAI-W",
        fill = "Depth(m)",
   ) +
    ylim(0, 20
         ) +       
   theme_minimal() +
   theme(
     panel.grid = element_blank(),
     axis.line = element_line(),
     panel.border = element_blank(),
     plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
     plot.subtitle = element_text(hjust = 0.5, size = 12, color ="#284b80" ),
     legend.text = element_text(size=11 ),
     legend.key.size = unit(.8, 'cm'),
     axis.title.x = element_blank()
   ) +
   scale_y_continuous(n.breaks = 10)
         total_iar_w_by_year
 
 
 #############  COMBINE DAFOR YEAR AND IAR-W
 
 
 
 
 library(patchwork)
 
 # Left: DPUE — put label on TOP x-axis
 stacked_dafor_clean_year <- stacked_dafor +
   labs(title = NULL, subtitle = NULL, y = "DPUE") +
   guides(fill = guide_legend(ncol = 1)) +   # vertical legend (1 column)
   theme(
     legend.position.inside      = c(1, 1),         # inside plot: top-right corner
     legend.justification = c(0, 1),
     legend.direction     = "vertical"           
   )
 
 # Right: RAI-W — put label on TOP x-axis, no y label (to avoid duplication)
 plot_raiw_clean_year <- total_iar_w_by_year +
   labs(title = NULL, subtitle = NULL, y = "RAI-W") +
   guides(fill = guide_legend(ncol = 1)) + 
   theme(
     legend.position.inside      = c(1, 1),         # inside plot: top-right corner
     legend.justification = c(0, 1),
     legend.direction     = "vertical"
   )
 
 # Combine with A/B tags
 combined_year <- (stacked_dafor_clean_year + plot_raiw_clean_year) +
   plot_layout(ncol = 1, guides = "keep", axis_titles = "collect_y") +  
   theme(
     text = element_text(size = 12),  
     #plot.tag = element_text(face = "bold", size = 14),
     plot.tag.position = c(0, 0),
     axis.title.y = element_text(size = 14),
     axis.title.x = element_blank(),
     axis.text.x = element_text(size = 12),
     axis.text.y = element_text(size = 12)
     
   )
 
 
 
 
combined_year
ggsave("plots/dpue_raiw_up_and_down.png", combined_year, width = 12, height = 8, dpi = 300)
         


          
###############         
library(patchwork)

# Left: DPUE
stacked_dafor_clean_year <- stacked_dafor +
  labs(title = NULL, subtitle = NULL, y = "DPUE") +
  guides(fill = guide_legend(ncol = 1)) +
  theme(
    legend.position.inside = c(1, 1),
    legend.justification   = c(0, 1),
    legend.direction       = "vertical",
    axis.text.x  = element_blank()  # remove x-axis text from left plot,
  )

# Right: RAI-W
plot_raiw_clean_year <- total_iar_w_by_year +
  labs(title = NULL, subtitle = NULL, y = "RAI-W") +
  guides(fill = guide_legend(ncol = 1)) +
  theme(
    legend.position.inside = c(1, 1),
    legend.justification   = c(0, 1),
    legend.direction       = "vertical"
  )

# Combine with tags
combined_year <- (stacked_dafor_clean_year + plot_raiw_clean_year) +
  plot_layout(ncol = 1, guides = "keep", axis_titles = "collect_y") +
  plot_annotation(tag_levels = "A") +      # automatically tags A, B
  theme(
    text = element_text(size = 12),        # standardize all fonts
    plot.tag = element_text(face = "bold", size = 14),
    plot.tag.position = c(0, 1),           # top-left corner
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

combined_year
ggsave("plots/dpue_raiw_up_and_down.png",
       combined_year, width = 12, height = 8, dpi = 300)

#######         
         
         
         
         
################################################################################
## Automated solution to create all density plots by localidade
################################################################################
library(tidyverse)
library(patchwork)
library(fs)

# 1. Create output directory if it doesn't exist
dir_create("plots/density_by_locality")

# 2. Get all unique localities
localities <- df_monit %>%
  filter(localidade_rebio != "entorno") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>% 
  distinct(localidade) %>%
  pull(localidade)

# 3. Create plotting function
create_density_plot <- function(locality) {
  # Prepare data
  loc_data <- df_monit %>%
    mutate(
      localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
      year = year(data)
    ) %>%
    filter(localidade == locality,
           obs != "estimado dos dados do ICMBio",
           localidade_rebio != "entorno")
  
  # Skip if no data
  if (nrow(loc_data) == 0) {
    message(paste("No data found for", locality))
    return(NULL)
  }
  
  # Process data
  loc_data <- loc_data %>%
    group_by(year) %>%
    mutate(
      n_trans_count = n(),
      total_dafor = sum(dafor, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(year) %>%
    mutate(
      year_label = paste0(year, " (n=", n_trans_count, ")"),
      year_label = factor(year_label, levels = unique(year_label))
    )
  
  # Set up comparison parameters
  years <- levels(loc_data$year_label)
  
  # Color scheme
  if (all(loc_data$dafor == 0)) {
    plot_colors <- rep("#00AA00", length(years))  # Bright green for all zeros
  } else if (length(years) == 1) {
    plot_colors <- rep("grey70", length(years))   # Single year with data > 0
  } else {
    # Initialize all colors as grey90 (default for non-comparison years)
    plot_colors <- rep("grey90", length(years))
    
    # Compare each consecutive pair
    for (i in 1:(length(years)-1)) {
      # Get comparison values for this pair
      dafor_compare <- loc_data %>%
        filter(year_label %in% years[c(i, i+1)]) %>%
        distinct(year_label, .keep_all = TRUE) %>%
        arrange(year_label)
      
      # Only set color for the newer year in each comparison
      if (dafor_compare$total_dafor[2] > dafor_compare$total_dafor[1]) {
        plot_colors[i+1] <- "red"  # Newer year increased
      } else {
        plot_colors[i+1] <- "#6B8EFF"  # Newer year decreased or stayed same
      }
      
      # Set older year to grey70 (only if not already set by a previous comparison)
      if (plot_colors[i] == "grey90") {
        plot_colors[i] <- "grey70"
      }
    }
  }
  
  # Create individual plots
  plot_list <- lapply(years, function(yr) {
    is_last_plot <- (yr == years[length(years)])
    
    dens <- density(filter(loc_data, year_label == yr)$dafor)
    y_max <- max(dens$y)
    
    ggplot(filter(loc_data, year_label == yr), aes(x = dafor)) +
      geom_density(fill = plot_colors[which(years == yr)], alpha = 0.8) +
      annotate(
        "text",
        x = 9.5,
        y = y_max * 0.95,
        label = yr,
        size = 9,
        hjust = 1,
        vjust = 1
      ) +
      scale_x_continuous(
        limits = c(0, 10),
        breaks = seq(0, 10, 2),
        labels = seq(0, 10, 2)
      ) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_minimal() +
      theme(
        plot.margin = margin(5, 5, 5, 5, "pt"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = if (is_last_plot) element_text() else element_blank(),
        axis.text.x = if (is_last_plot) element_text() else element_blank(),
      ) +
      labs(x = if (is_last_plot) "IAR" else NULL)
  })
  
  # Combine plots
  final_plot <- wrap_plots(plot_list, ncol = 1) +
    plot_annotation(
      title = paste(locality),
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold",
                                  margin = margin(b = 10))
      ) +
      theme(plot.background = element_rect(color = "black", size = 1, fill = "white"))
    )
  
  # Save plot
  filename <- paste0("plots/density_by_locality/", 
                     str_replace_all(tolower(locality), " ", "_"), 
                     "_density.png")
  
  ggsave(filename, final_plot, width = 5, height = 2 * length(years), dpi = 300)
  return(filename)
}

# 4. Process all localities (with progress bar)
created_files <- map(localities, ~ {  # Corrected from locality to localities
  tryCatch({
    create_density_plot(.x)
  }, error = function(e) {
    message(paste("Failed for", .x, ":", e$message))
    NULL
  })
})

# 5. Report results
successful <- keep(created_files, ~ !is.null(.x))
message(paste("\nSuccessfully created", length(successful), " plots in plots/density_by_locality/"))
 


################################################################################
################################################################################

################################################################################

#### Map #### 
# 1. LOAD REQUIRED LIBRARIES
library(sf)
library(tidyverse)
library(leaflet)
library(stringi)

# 2. PREPARE DPUE DATA

# data
df_dpue = df_monit_effort_dpue %>%
  group_by(localidade) %>%
  summarise(
    mean_dpue = mean(dpue_standard, na.rm = TRUE),
    max_dpue = max(dpue_standard, na.rm = TRUE),
    total_detections = sum(total_detections),
    .groups = "drop"
  ) %>%
  mutate(
    localidade_clean = tolower(localidade) %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_replace_all("\\s+", "_")
  )


# centroid for circles
centroid_locality = read_delim("data/centroide_localidade.csv",
                               col_types = list(X = col_double(),
                                                Y = col_double(),
                                                Z = col_double(),
                                                localidade = col_character(),
                                                regiao = col_character()))





# 3. PREPARE SHAPEFILE DATA
shp_data <- shp_localidades %>%
  mutate(
    localidade_clean = tolower(localidade)
  )


# 4. JOIN DATASETS
map_data <- shp_data %>% 
  left_join(df_dpue, by = "localidade_clean")


# 5. CREATE ENHANCED COLOR SCALE
color_palette <- c(
  "#D3D3D3",  # Light grey for EXACTLY 0
  "#A3D699",  # Light green for very low (>0)
  "#2ECC71",  # Green for low
  "#F1C40F",  # Yellow for medium-low
  "#F39C12",  # Orange for medium-high
  "#E74C3C"   # Red for high values
)

dpue_values <- map_data$mean_dpue[!is.na(map_data$mean_dpue)]
positive_values <- dpue_values[dpue_values > 0]

if (length(unique(dpue_values)) <= 4) {
  breaks <- sort(unique(c(0, dpue_values)))  # Few values: include all
} else {
  # Force a break right after 0 (e.g., smallest positive value)
  min_positive <- min(positive_values, na.rm = TRUE)
  epsilon <- min_positive / 2  # A tiny value to separate 0 from >0
  
  # Calculate breaks for remaining values (now excluding 0)
  if (length(unique(positive_values)) <= 3) {
    positive_breaks <- sort(unique(positive_values))
  } else {
    positive_breaks <- quantile(
      positive_values,
      probs = seq(0, 1, length.out = length(color_palette) - 1),  # -1 because 0 is separate
      na.rm = TRUE
    )
  }
  
  # Combine breaks: 0, epsilon, then the rest
  breaks <- c(0, epsilon, positive_breaks)
  breaks <- unique(round(breaks, 2))  # Remove duplicates
  
  # Ensure we have enough breaks (matching palette)
  if (length(breaks) < length(color_palette)) {
    extra_breaks <- seq(
      min_positive,
      max(positive_values, na.rm = TRUE),
      length.out = length(color_palette) - 2  # -2 because 0 and epsilon are fixed
    )
    breaks <- c(0, epsilon, extra_breaks)
  }
}

# Create color function with new palette
pal <- colorBin(
  palette = color_palette,
  domain = map_data$mean_dpue,
  bins = breaks,
  na.color = "#111111",  # Darker grey for NA values
  pretty = FALSE
)
 


####################
####################

# 6. CREATE MAP
dpue_map <- leaflet(map_data) %>%
  # Base map tiles
  addTiles() %>%  # Default OpenStreetMap tiles as fallback
  #addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  #addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  
  # Add monitoring lines with new color scale
  addPolylines(
    color = ~pal(mean_dpue),
    weight = 6,
    opacity = 1,
    label = ~sprintf(
      "<strong>%s</strong><br>
       Mean DPUE: %.3f<br>
       Max DPUE: %.3f<br>
       Total Detections: %d",
      localidade.x,
      round(mean_dpue, 3),
      round(max_dpue, 3),
      total_detections
    ) %>% lapply(htmltools::HTML),
    highlightOptions = highlightOptions(
      weight = 6,
      color = "#666",
      bringToFront = TRUE
    )
  ) %>%
  
  # Add  legend
  addLegend(
    pal = pal,
    values = ~mean_dpue,
    title = "DPUE Detecções/H/100m",
    position = "bottomleft",
    labFormat = labelFormat(digits = 3),  # More decimal places
    na.label = "Não amostrado",
    opacity = 1
  ) %>%
  
  # Add scale bar
  addScaleBar(position = "bottomright")

# 7. DISPLAY MAP
dpue_map


######################################
# 8. Static map
library(tmap)
library(sf)
library(dplyr)
library(rnaturalearth)

# Defining the bbox
# 5. Define your bounding box
bbox_arvoredo <- st_bbox(c(
  xmin = -48.34664,
  ymin = -27.26745,
  xmax = -48.40000,
  ymax = -27.30329
), crs = st_crs(4326))

bbox_deserta <- st_bbox(c(
  xmin = -48.325350,
  ymin = -27.266767,
  xmax = -48.339598,
  ymax = -27.277903
), crs = st_crs(4326))


bbox_gale <- st_bbox(c(
  xmin = -48.395163,
  ymin = -27.173507,
  xmax = -48.429423,
  ymax = -27.190714
), crs = st_crs(4326))

# 1. First try to fix your map_rebio_sf data
tryCatch({
  map_rebio_sf <- map_rebio_sf %>% 
    st_make_valid() %>%  # Fix any invalid geometries
    st_transform(4326)
}, error = function(e) {
  message("Couldn't fix map_rebio_sf, using alternative coastline")
  # Use Natural Earth coastline as backup
  if(!require("rnaturalearth")) install.packages("rnaturalearth")
  map_rebio_sf <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") %>%
    st_transform(4326)
})

# 2. Safely crop to bounding box
coastline_cropped <- tryCatch({
  st_intersection(map_rebio_sf, st_as_sfc(bbox_arvoredo))
}, error = function(e) {
  message("Couldn't crop coastline, using full extent")
  map_rebio_sf  # Use uncropped if cropping fails
})

# 3. Create the map with error-proof coastline
dpue_tmap_arvoredo <- tm_shape(coastline_cropped, bbox = bbox_arvoredo) +
  tm_polygons(col = "#E0F2F7", border.col = "#3498DB", lwd = 1.2) +
  
  tm_basemap("Esri.WorldImagery", alpha = 0.7) +
  
  tm_shape(map_data_sf) +
  tm_lines(
    col = "mean_dpue",
    palette = color_palette,
    breaks = breaks,
    lwd = 10,
    colorNA = "#111111",
    lineend = "round",
    linejoin = "round",
    title.col = "DPUE Detecções/ H * Un.100m",
    labels = c("0", "0.01-0.03", "0.03-0.09", "0.09-0.038", "0.38-0.85", "0.85-94.57"), 
    textNA = "Não amostrado",
  ) +
  
  # Rest of your map elements...
  tm_layout(
    inner.margins = c(0,0,0,0),
    frame = FALSE,
    legend.position = c("left", "bottom")
  )

dpue_tmap_arvoredo

tmap_save(dpue_tmap_arvoredo,"plots/maps/dpue_map_arvoredo.png", width = 10, height = 5, dpi = 300)


# 1. First try to fix your map_rebio_sf data
tryCatch({
  map_rebio_sf <- map_rebio_sf %>% 
    st_make_valid() %>%  # Fix any invalid geometries
    st_transform(4326)
}, error = function(e) {
  message("Couldn't fix map_rebio_sf, using alternative coastline")
  # Use Natural Earth coastline as backup
  if(!require("rnaturalearth")) install.packages("rnaturalearth")
  map_rebio_sf <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") %>%
    st_transform(4326)
})

# 2. Safely crop to bounding box
coastline_cropped <- tryCatch({
  st_intersection(map_rebio_sf, st_as_sfc(bbox_deserta))
}, error = function(e) {
  message("Couldn't crop coastline, using full extent")
  map_rebio_sf  # Use uncropped if cropping fails
})

# 3. Create the map with error-proof coastline
dpue_tmap_deserta <- tm_shape(coastline_cropped, bbox = bbox_deserta) +
  tm_polygons(col = "#E0F2F7", border.col = "#3498DB", lwd = 1.2) +
  
  tm_basemap("Esri.WorldImagery", alpha = 0.7) +
  
  tm_shape(map_data_sf) +
  tm_lines(
    col = "mean_dpue",
    palette = color_palette,
    breaks = breaks,
    lwd = 10,
    colorNA = "#111111",
    lineend = "round",
    linejoin = "round",
    title.col = "DPUE Detecções/ H * Un.100m",
    labels = c("0", "0.01-0.03", "0.03-0.09", "0.09-0.038", "0.38-0.85", "0.85-94.57"), 
    textNA = "Não amostrado",
  ) +
  
  # Rest of your map elements...
  tm_layout(
    inner.margins = c(0,0,0,0),
    frame = FALSE,
    legend.position = c("right", "bottom")
  )

dpue_tmap_deserta
tmap_save(dpue_tmap_deserta,"plots/maps/dpue_map_deserta.png", width = 10, height = 5, dpi = 300)


# 1. First try to fix your map_rebio_sf data
tryCatch({
  map_rebio_sf <- map_rebio_sf %>% 
    st_make_valid() %>%  # Fix any invalid geometries
    st_transform(4326)
}, error = function(e) {
  message("Couldn't fix map_rebio_sf, using alternative coastline")
  # Use Natural Earth coastline as backup
  if(!require("rnaturalearth")) install.packages("rnaturalearth")
  map_rebio_sf <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf") %>%
    st_transform(4326)
})

# 2. Safely crop to bounding box
coastline_cropped <- tryCatch({
  st_intersection(map_rebio_sf, st_as_sfc(bbox_gale))
}, error = function(e) {
  message("Couldn't crop coastline, using full extent")
  map_rebio_sf  # Use uncropped if cropping fails
})

# 3. Create the map with error-proof coastline
dpue_tmap_gale <- tm_shape(coastline_cropped, bbox = bbox_gale) +
  tm_polygons(col = "#E0F2F7", border.col = "#3498DB", lwd = 1.2) +
  
  tm_basemap("Esri.WorldImagery", alpha = 0.7) +
  
  tm_shape(map_data_sf) +
  tm_lines(
    col = "mean_dpue",
    palette = color_palette,
    breaks = breaks,
    lwd = 10,
    colorNA = "#111111",
    lineend = "round",
    linejoin = "round",
    title.col = "DPUE Detecções/ H * Un.100m"
  ) +
  
  # Rest of your map elements...
  tm_layout(
    inner.margins = c(0,0,0,0),
    frame = FALSE,
    legend.position = c("left", "bottom")
  )

dpue_tmap_gale
tmap_save(dpue_tmap_gale,"plots/maps/dpue_map_gale.png", width = 10, height = 5, dpi = 300)



#############################################################
## Detection through the years ##############################

library(ggrepel)
library(viridis)

# 1. Process the data with seasonal grouping (unchanged)
detection_summary_filtered <- df_monit_effort_dpue %>%
  #filter(localidade == "BAIA DAS TARTARUGAS") %>% 
  filter(total_detections > 0,
         localidade %in% multi_year_localities) %>% 
  mutate(
    season = case_when(
      month(data) %in% c(11, 12, 1) ~ "Nov-Jan",
      month(data) %in% c(2, 3, 4) ~ "Fev-Abr",
      month(data) %in% c(5, 6, 7) ~ "Mai-Jul",
      month(data) %in% c(8, 9, 10) ~ "Ago-Out",
      TRUE ~ "Other"
    ),
    season_year = if_else(month(data) == 12, year(data) + 1, year(data)),
    season_label = paste(season, season_year),
    season_num = case_when(
      season == "Nov-Jan" ~ 1,
      season == "Fev-Abr" ~ 2,
      season == "Mai-Jul" ~ 3,
      season == "Ago-Out" ~ 4
    ) + (season_year - min(season_year)) * 4
  ) %>%
  group_by(season_num, season_label, localidade) %>%
  summarise(
    total_detections = sum(total_detections, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Create the improved plot
ggplot(detection_summary_filtered, 
            aes(x = season_num, 
                y = total_detections, 
                color = localidade,
                group = localidade)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(size = 3) +
  geom_text_repel(
    data = . %>% group_by(localidade) %>% filter(season_num == max(season_num)),
    aes(label = localidade),
    size = 3.5,
    direction = "y",
    xlim = c(max(detection_summary_filtered$season_num) + 1, NA),
    segment.color = 'grey50',
    min.segment.length = 0
  ) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  scale_x_continuous(
    breaks = unique(detection_summary_filtered$season_num),
    labels = unique(detection_summary_filtered$season_label),
    limits = c(min(detection_summary_filtered$season_num), 
               max(detection_summary_filtered$season_num) + 3)
  ) +
  labs(
    title = "Detecções por Localidade (2022-2025)",
    x = "Período", 
    y = "Número de Detecções"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 16, face = "bold", color = "#284b80"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.line.x = element_line(color = "grey", linewidth = 0.8),
    axis.line.y = element_line(color = "grey", linewidth = 0.8),
    plot.margin = unit(c(1, 5, 1, 1), "lines")  # Extra right margin for labels
  ) +

# 3. Render plot with proper clipping
coord_cartesian(clip = 'off')

# 4. Save the plot
ggsave("plots/detec_years.png", width = 12, height = 7, dpi = 300)

#################################################################################

#  mass managed by locality (mass by model) through years
library(ggrepel)
library(viridis)

# Create plot
ggplot(df_cumulative_max, aes(x = year, y = cumulative_mass/1000, 
                                   color = localidade, group = localidade)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 3) +
  geom_text_repel(
    data = . %>% group_by(localidade) %>% filter(year == max(year)),
    aes(label = localidade),
    size = 3,
    direction = "y",
    xlim = c(max(df_cumulative_max$year) + 0.5, NA),
    segment.color = 'grey50'
  ) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  scale_x_continuous(
    breaks = unique(df_cumulative_max$year),
    limits = c(min(df_cumulative_max$year), max(df_cumulative_max$year) + 2)
  ) +
  labs(
    title = "Massa Manejada Acumulada (2012-2025)",
    x = "Ano", 
    y = "Massa Manejada Acumulada (Kg)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, color = "#284b80"),
    axis.line.x = element_line(color = "grey", linewidth = 0.8),
    axis.line.y = element_line(color = "grey", linewidth = 0.8),
    
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +

# Adjust plot margins to accommodate labels
coord_cartesian(clip = 'off') + 
  theme(plot.margin = unit(c(1, 8, 1, 1), "lines"))

ggsave("plots/mass_years.png", width = 10, height = 5, dpi = 300)





################################################################################
################################################################################
### DEBUGED dpue MAPS
library(tmap)
library(sf)
library(dplyr)

# 1. LOAD AND REPAIR SHAPEFILE
land_polygon <- st_read("data/Rebio_Arvoredo_Ilhas_POL_CGS_WGS84.shp") %>% 
  # Force 2D coordinates (fixes dimension mismatch)
  st_zm(drop = TRUE) %>% 
  # Convert to valid geometries
  st_make_valid() %>% 
  # Ensure correct CRS (WGS84)
  st_transform(4326)

# 2. VERIFY GEOMETRY (diagnostic check)
if(!all(st_is_valid(land_polygon))) {
  message("Found invalid geometries - applying additional fixes")
  land_polygon <- land_polygon %>% 
    st_buffer(0) %>%  # Fix potential geometry issues
    st_make_valid()
}

# 3. DEFINE BOUNDING BOXES (with explicit CRS)
create_bbox <- function(xmin, ymin, xmax, ymax) {
  st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = 4326)
}

bbox_arvoredo <- create_bbox(-48.34664, -27.26745, -48.40000, -27.30329)
bbox_deserta <- create_bbox(-48.325350, -27.266767, -48.339598, -27.277903)
bbox_gale <- create_bbox(-48.397000, -27.173507, -48.419423, -27.190714)

# 4. ENHANCED MAP FUNCTION
create_dpue_map <- function(data, land, bbox, title, legend_pos) {
  # Convert bbox to polygon with small buffer
  bbox_poly <- st_as_sfc(bbox) %>% st_buffer(0.02)
  
  # Safely crop land to area of interest
  land_cropped <- tryCatch({
    st_intersection(land, bbox_poly) %>% 
      st_make_valid() %>% 
      st_collection_extract("POLYGON")
  }, error = function(e) {
    message("Using full land extent due to cropping error")
    land
  })
  
  # Create map
  tm_shape(land_cropped, bbox = bbox) +
    tm_polygons(col = "#E0F2F7", border.col = "#3498DB", lwd = 0.8) +
    
    tm_basemap("Esri.WorldImagery", alpha = 0.7) +
    
    tm_shape(data) +
    tm_lines(
      col = "mean_dpue",
      palette = color_palette,
      breaks = breaks,
      lwd = 6,
      title.col = "DPUE Detecções/H*Uni100m",
      labels = c("0", "0.01-0.03", "0.03-0.09", "0.09-0.38", "0.38-0.85", "0.85-94.57"),
      textNA = "Não amostrado",
      colorNA = "#111111"
      
    ) +
    
    tm_layout(
      main.title = title,
      inner.margins = c(0.02, 0.02, 0.02, 0.02),
      legend.position = legend_pos,
      frame = TRUE
    ) +
    tm_scale_bar(position = c("left", "bottom"))
}

# 5. GENERATE MAPS WITH ERROR HANDLING
generate_safe_map <- function(bbox, title, legend_pos) {
  tryCatch({
    create_dpue_map(map_data_sf, land_polygon, bbox, title, legend_pos)
  }, error = function(e) {
    message("Falling back to simplified map for ", title)
    tm_shape(map_data_sf, bbox = bbox) +
      tm_lines(col = "mean_dpue", palette = color_palette, lwd = 6) +
      tm_layout(main.title = paste(title, "(simplified)"),
                legend.position = legend_pos)
  })
}

# Generate and save maps
dpue_tmap_arvoredo <- generate_safe_map(bbox_arvoredo, "Ilha do Arvoredo", c("left", "bottom"))
dpue_tmap_deserta <- generate_safe_map(bbox_deserta, "Ilha Deserta", c("left", "top"))
dpue_tmap_gale <- generate_safe_map(bbox_gale, "Ilha da Galé", c("left", "top"))

dpue_tmap_arvoredo


# Save outputs
tmap_save(dpue_tmap_arvoredo, "plots/maps/dpue_map_arvoredo.png", width = 10, height = 5, dpi = 300)
tmap_save(dpue_tmap_deserta, "plots/maps/dpue_map_deserta.png", width = 10, height = 5, dpi = 300)
tmap_save(dpue_tmap_gale, "plots/maps/dpue_map_gale.png", width = 10, height = 5, dpi = 300)




################################################################################

################################################################################
## Automated solution to create all bar plots by localidade
################################################################################
library(tidyverse)
library(patchwork)
library(fs)

# 1. Create output directory if it doesn't exist
dir_create("plots/bar_by_locality")

# 2. Get all unique localities
localities <- df_monit %>%
  filter(localidade_rebio != "entorno") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " "))) %>% 
  distinct(localidade) %>%
  pull(localidade)

# 3. Create plotting function
create_bar_plot <- function(locality) {
  # Prepare data
  loc_data <- df_monit %>%
    mutate(
      localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
      year = year(data)
    ) %>%
    filter(localidade == locality,
           obs != "estimado dos dados do ICMBio",
           localidade_rebio != "entorno")
  
  # Skip if no data
  if (nrow(loc_data) == 0) {
    message(paste("No data found for", locality))
    return(NULL)
  }
  
  # Process data
  loc_data <- loc_data %>%
    group_by(year) %>%
    mutate(
      n_trans_count = n(),
      total_dafor = sum(dafor, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(year) %>%
    mutate(
      year_label = paste0(year, " (n=", n_trans_count, ")"),
      year_label = factor(year_label, levels = unique(year_label))
    )
  
  # Set up comparison parameters
  years <- levels(loc_data$year_label)
  
  # Color scheme
  if (all(loc_data$dafor == 0)) {
    plot_colors <- rep("#00AA00", length(years))  # Bright green for all zeros
  } else if (length(years) == 1) {
    plot_colors <- rep("grey70", length(years))   # Single year with data > 0
  } else {
    # Initialize all colors as grey90 (default for non-comparison years)
    plot_colors <- rep("grey90", length(years))
    
    # Compare each consecutive pair
    for (i in 1:(length(years)-1)) {
      # Get comparison values for this pair
      dafor_compare <- loc_data %>%
        filter(year_label %in% years[c(i, i+1)]) %>%
        distinct(year_label, .keep_all = TRUE) %>%
        arrange(year_label)
      
      # Only set color for the newer year in each comparison
      if (dafor_compare$total_dafor[2] > dafor_compare$total_dafor[1]) {
        plot_colors[i+1] <- "red"  # Newer year increased
      } else {
        plot_colors[i+1] <- "#6B8EFF"  # Newer year decreased or stayed same
      }
      
      # Set older year to grey70 (only if not already set by a previous comparison)
      if (plot_colors[i] == "grey90") {
        plot_colors[i] <- "grey70"
      }
    }
  }
  
  # Create individual plots
  plot_list <- lapply(years, function(yr) {
    is_last_plot <- (yr == years[length(years)])
    
    # Calculate counts for each dafor value
    count_data <- loc_data %>%
      filter(year_label == yr) %>%
      count(dafor) %>%
      complete(dafor = 0:10, fill = list(n = 0))
    
    # Calculate max count for y-axis limits
    y_max <- max(count_data$n)
    
    ggplot(count_data, aes(x = dafor, y = n)) +
      geom_bar(stat = "identity", fill = plot_colors[which(years == yr)], alpha = 0.8) +
      annotate(
        "text",
        x = 10,
        y = y_max * 0.95,
        label = yr,
        size = 9,
        hjust = 1,
        vjust = 1
      ) +
      scale_x_continuous(
        limits = c(-0.5, 10.5),
        breaks = seq(0, 10, 2),
        labels = seq(0, 10, 2)
      ) +
      scale_y_continuous(limits = c(0, NA)) +
      theme_minimal() +
      theme(
        plot.margin = margin(5, 5, 5, 5, "pt"),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = if (is_last_plot) element_text() else element_blank(),
        axis.text.x = if (is_last_plot) element_text() else element_blank(),
      ) +
      labs(x = if (is_last_plot) "IAR" else NULL)
  })
  
  # Combine plots
  final_plot <- wrap_plots(plot_list, ncol = 1) +
    plot_annotation(
      title = paste(locality),
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold",
                                  margin = margin(b = 10))
      ) +
        theme(plot.background = element_rect(color = "black", size = 1, fill = "white"))
    )
  
  # Save plot
  filename <- paste0("plots/bar_by_locality/", 
                     str_replace_all(tolower(locality), " ", "_"), 
                     "_bar.png")
  
  ggsave(filename, final_plot, width = 5, height = 2 * length(years), dpi = 300)
  return(filename)
}

# 4. Process all localities (with progress bar)
created_files <- map(localities, ~ {
  tryCatch({
    create_bar_plot(.x)
  }, error = function(e) {
    message(paste("Failed for", .x, ":", e$message))
    NULL
  })
})

# 5. Report results
successful <- keep(created_files, ~ !is.null(.x))
message(paste("\nSuccessfully created", length(successful), " plots in plots/bar_by_locality/"))


################################################################################
## Map withouT DPUE


library(tmap)
library(sf)
library(dplyr)

# 1. LOAD AND REPAIR SHAPEFILE
land_polygon <- st_read("data/Rebio_Arvoredo_Ilhas_POL_CGS_WGS84.shp") %>% 
  # Force 2D coordinates (fixes dimension mismatch)
  st_zm(drop = TRUE) %>% 
  # Convert to valid geometries
  st_make_valid() %>% 
  # Ensure correct CRS (WGS84)
  st_transform(4326)

# 2. VERIFY GEOMETRY (diagnostic check)
if(!all(st_is_valid(land_polygon))) {
  message("Found invalid geometries - applying additional fixes")
  land_polygon <- land_polygon %>% 
    st_buffer(0) %>%  # Fix potential geometry issues
    st_make_valid()
}

# 3. DEFINE BOUNDING BOXES (with explicit CRS)
create_bbox <- function(xmin, ymin, xmax, ymax) {
  st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = 4326)
}

bbox_arvoredo <- create_bbox(-48.34664, -27.26745, -48.40000, -27.30329)
bbox_deserta <- create_bbox(-48.325350, -27.266767, -48.339598, -27.277903)
bbox_gale <- create_bbox(-48.397000, -27.173507, -48.419423, -27.190714)

# 4. ENHANCED MAP FUNCTION
create_dpue_map <- function(data, land, bbox, title, legend_pos) {
  # Convert bbox to polygon with small buffer
  bbox_poly <- st_as_sfc(bbox) %>% st_buffer(0.02)
  
  # Safely crop land to area of interest
  land_cropped <- tryCatch({
    st_intersection(land, bbox_poly) %>% 
      st_make_valid() %>% 
      st_collection_extract("POLYGON")
  }, error = function(e) {
    message("Using full land extent due to cropping error")
    land
  })
  
  # Create map
  tm_shape(land_cropped, bbox = bbox) +
    tm_polygons(col = "#E0F2F7", border.col = "#3498DB", lwd = 0.8) +
    
    tm_basemap("Esri.WorldImagery", alpha = 0.7) +
    
    tm_shape(data) +
    tm_lines(
      col = c("blue"),
      #palette = color_palette,
      #breaks = breaks,
      lwd = 6,
      #title.col = "DPUE Detecções/H*Uni100m",
      #labels = c("0", "0.01-0.03", "0.03-0.09", "0.09-0.38", "0.38-0.85", "0.85-94.57"),
      #textNA = "Não amostrado",
      colorNA = "#111111"
      
    ) +
    
    tm_layout(
     # main.title = title,
      #inner.margins = c(0.02, 0.02, 0.02, 0.02),
      #legend.position = legend_pos,
      frame = F
    ) +
    tm_scale_bar(position = c("left", "bottom"))
}

# 5. GENERATE MAPS WITH ERROR HANDLING
generate_safe_map <- function(bbox, title, legend_pos) {
  tryCatch({
    create_dpue_map(map_data_sf, land_polygon, bbox, title, legend_pos)
  }, error = function(e) {
    message("Falling back to simplified map for ", title)
    tm_shape(map_data_sf, bbox = bbox) +
      tm_lines(col = "mean_dpue", palette = color_palette, lwd = 6) +
      tm_layout(main.title = paste(title, "(simplified)"),
                legend.position = legend_pos)
  })
}

# Generate and save maps
dafor_tmap_arvoredo <- generate_safe_map(bbox_arvoredo, "Ilha do Arvoredo", c("left", "bottom"))
dafor_tmap_deserta <- generate_safe_map(bbox_deserta, "Ilha Deserta", c("left", "top"))
dafor_tmap_gale <- generate_safe_map(bbox_gale, "Ilha da Galé", c("left", "top"))

dafor_tmap_arvoredo
dafor_tmap_deserta
dafor_tmap_gale



# Save outputs
tmap_save(dafor_tmap_arvoredo, "plots/maps/dafor_map_arvoredo.png", width = 10, height = 5, dpi = 300)
tmap_save(dafor_tmap_deserta, "plots/maps/dafor_map_deserta.png", width = 10, height = 5, dpi = 300)
tmap_save(dafor_tmap_gale, "plots/maps/dafor_map_gale.png", width = 10, height = 5, dpi = 300)




