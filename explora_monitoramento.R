## 
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

####
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
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ localidade_rebio, ncol = 1, scales = "free_y") + # cat jump
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
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.title = element_blank(),
    legend.key.size = unit(.8, 'cm'),
    axis.text.y = element_text(size = 5)
  )

plot_transec_strata
ggsave("plots/transec_batimetria.png", width = 10, height = 5, dpi = 300)


#### CPUE #########################################################################
# Detections/60min/100m

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
    subtitle = "DPUE - Detecções / (Horas * Unidades de 100m)"
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
               "Escala IAR" = 6, 
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



################################################################################

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
# New improved color palette with better visual distinction
color_palette <- c("#D3D3D3",  # Light grey for 0 values
                   "#A3D699",  # Light green for very low
                   "#2ECC71",  # Green for low
                   "#F1C40F",  # Yellow for medium-low
                   "#F39C12",  # Orange for medium-high
                   "#E74C3C")  # Red for high values

# Calculate breaks - now with more categories for better resolution
dpue_values <- map_data$mean_dpue[!is.na(map_data$mean_dpue)]
if(length(unique(dpue_values)) <= 4) {
  breaks <- sort(unique(c(0, dpue_values)))
} else {
  # More break points for better gradient
  breaks <- c(0, 
              quantile(dpue_values[dpue_values > 0], 
                       probs = seq(0.2, 0.8, by = 0.2), 
                       na.rm = TRUE),
              max(dpue_values, na.rm = TRUE))
  breaks <- unique(round(breaks, 2))
  # Ensure at least 4 breaks for the color palette
  if(length(breaks) < 4) {
    breaks <- seq(0, max(breaks), length.out = 4)
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
    position = "bottomright",
    labFormat = labelFormat(digits = 3),  # More decimal places
    na.label = "Não amostrado",
    opacity = 1
  ) %>%
  
  # Add scale bar
  addScaleBar(position = "bottomleft")

# 7. DISPLAY MAP
dpue_map


######################################
# 8. Static map
library(tmap)
library(sf)
library(dplyr)

# Defining the bbox
# 5. Define your bounding box
bbox_arvoredo <- st_bbox(c(
  xmin = -48.34664,
  ymin = -27.26745,
  xmax = -48.40350,
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
    title.col = "DPUE Detecções/ H * Un.100m"
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
    title.col = "DPUE Detecções/ H * Un.100m"
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


