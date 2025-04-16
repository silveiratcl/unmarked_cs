## 
# Indicadores Monitoramento

library("tidyverse")
library("readr")
library("tidyr")
library("stringr")
library("hb")
library("dplyr")
library("lubridate")
library("sf")

#### Data ####

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





# Shapefile localities

shp_localidades = st_read("data/localidades_shapefile.shp")



### Data processing ### 

# Aggregate by locality 
# total time
# total detections 
# Obtaining the detection and effort df
# detection is presence/absence by locality by each monitoring strata
# effort is the number of visual transects where cs were detected

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
table(df_monit_effort$faixa_bat)


# n detections minutes

plot_detec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, n_detection, sum)) %>% 
  filter(n_detection > 0)  %>% 
  ggplot(aes(fill=factor(faixa_bat,levels=c("entremare", "raso", "fundo")), y=localidade, x=n_detection)) +
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("Entremarés (~0-2m)", "Raso (~3-6m)", "Fundo (~7m-Interface)")) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Total de transectos (1 min.) com presença de coral-sol") +
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
  ggplot(aes(fill = factor(faixa_bat, levels = c("entremare", "raso", "fundo")), 
             y = reorder(localidade, max_trsct_vis, sum), 
             x = max_trsct_vis)) +
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("Entremarés (~0-2m)", "Raso (~3-6m)", "Fundo (~7m-Interface)")) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ localidade_rebio, ncol = 1, scales = "free_y") + # cat jump
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Esforço - Total de Transectos (1 min.) por localidade") +
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
    dpue_standard = n_detection / (effort_hours / locality_100m)
  ) %>%
  
  # Group and summarize (if needed)
  group_by(localidade, data, faixa_bat) %>%
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
  ggplot(aes(fill = factor(faixa_bat,levels=c("entremare", "raso", "fundo")), y=localidade, x=dpue_standard)) +
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("Entremarés (~0-2m)", "Raso (~3-6m)", "Fundo (~7m-Interface)")) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  
  labs(
    title = "Detecções por Unidade de Esforço",
    subtitle = "DPUE - Detecções/H/100m"
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


###################
###################
# Dafor (RAI das localidades)

filtered_df <- df_monit %>%
  group_by(localidade_rebio, localidade, year(data))
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "),
         localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")                         ),
         n_trans_count =  )) %>% 
  filter(localidade_rebio == "rebio") 
  ungroup()

#################################################
  
filtered_df <- df_monit %>%
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")),
    year = year(data)  # Extract year from 'data'
  ) %>%
  filter(localidade_rebio == "REBIO") %>% 
  # Add a column counting rows per locality-year group (keeps all original data)
  add_count(localidade, year, name = "n_trans_count") %>%
  ungroup()

filtered_df    

#################################################
  
  

# Create density plot general
ggplot(filtered_df, aes(x = dafor, fill = localidade)) +
  geom_density(alpha = 0.5) +
  labs(x = "IAR (DAFOR)", 
       y = "Densidade",
       title = "Distribuição da densidade IAR por Localidade",
       subtitle = "Somente localidade com IAR positivo",
       fill = "Localidade") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, max(filtered_df$dafor)))  # Start at 0 since DAFOR can't be negative

ggsave("plots/density_IAR.png", width = 10, height = 5, dpi = 300)



# Create density plot by locality
# compares the dafor by two date older an newer
# paste the number of visual transects

ggplot(filtered_df, aes(x = dafor, fill = localidade)) +
  geom_density(alpha = 0.5) +
  labs(x = "IAR", 
       y = "Densidade",
       title = "Distribuição da densidade IAR por Localidade",
       subtitle = "Somente localidade com IAR positivo",
       fill = "Localidade") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, max(filtered_df$dafor)))  # Start at 0 since DAFOR can't be negative


##############################
##############################
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

# 1. Prepare the data - with explicit checks
rancho_data <- df_monit %>%
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    year = year(data)
  ) %>%
  filter(localidade == "RANCHO NORTE",
         obs != "estimado dos dados do ICMBio")

# Check if data exists
if(nrow(rancho_data) == 0) stop("No data found for RANCHO NORTE")

rancho_data <- rancho_data %>%
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


rancho_data %>% 
  select(localidade,  data, tempo_censo, dafor, total_dafor)
print(rancho_data %>% 
        select(localidade,  data, tempo_censo, dafor, total_dafor), n= 276)

# 2. Create basic plot with all years together
base_plot <- ggplot(rancho_data, aes(x = dafor, fill = year_label)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_x_continuous(limits = c(0, 10), name = "RAI") +
  labs(title = "RANCHO NORTE - Density by Year") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 3. Create the comparison version (vertical layout)
years <- levels(rancho_data$year_label)
oldest <- years[1]
newest <- years[length(years)]

# Calculate comparison
dafor_compare <- rancho_data %>%
  filter(year_label %in% c(oldest, newest)) %>%
  distinct(year_label, .keep_all = TRUE) %>%
  arrange(year_label)

# Create color scheme
plot_colors <- ifelse(
  years == newest,
  ifelse(dafor_compare$total_dafor[2] > dafor_compare$total_dafor[1], 
         "#FF6B6B", "#6B8EFF"),
  ifelse(years == oldest, "grey70", "grey90")
)

# Create individual plots
plot_list <- lapply(years, function(yr) {
  ggplot(filter(rancho_data, year_label == yr), aes(x = dafor)) +
    geom_density(fill = plot_colors[which(years == yr)], alpha = 0.8) +
    geom_text(
      aes(x = 8, y = 0.1, label = yr),
      size = 4, hjust = 0.5, vjust = 0
    ) +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, NA)) +
    theme_void() +
    theme(
      plot.margin = margin(2, 2, 2, 2),
      axis.title = element_blank()
    )
})

# Combine plots
final_plot <- wrap_plots(plot_list, ncol = 1) +
  plot_annotation(
    title = "RANCHO NORTE - Yearly Comparison",
    subtitle = sprintf("%s %s %s (Total DAFOR)", newest,
                       ifelse(dafor_compare$total_dafor[2] > dafor_compare$total_dafor[1], ">", "≤"),
                       oldest),
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12,
                                   color = ifelse(dafor_compare$total_dafor[2] > dafor_compare$total_dafor[1],
                                                  "#FF6B6B", "#6B8EFF"))
    )
  ) &
  xlab("RAI (0-10 scale)")

# 4. Display BOTH versions
print(base_plot)  # Simple combined version
print(final_plot) # Vertical comparison version

 5. Save to file as backup
ggsave("rancho_density_simple.png", base_plot, width = 8, height = 6)
ggsave("rancho_density_vertical.png", final_plot, width = 6, height = 8)

# Return the data for inspection
list(
  data = rancho_data,
  comparison = dafor_compare,
  colors = setNames(plot_colors, years)
)
##############################
##############################


#### Map #### 
library(leaflet)
library(stringi) # For string manipulation


# 1. LOAD REQUIRED LIBRARIES
library(sf)
library(tidyverse)
library(leaflet)
library(stringi)

# 2. PREPARE DPUE DATA
df_dpue <- df_monit_effort_dpue %>%
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
  na.color = "#808080",  # Darker grey for NA values
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


## Detection through the years ##############################

# 1. Process the data with seasonal grouping
detection_summary_filtered <- df_monit_effort_dpue %>%
  filter(total_detections > 0,
         localidade %in% multi_year_localities) %>% 
  mutate(
    # Create seasonal periods
    season = case_when(
      month(data) %in% c(11, 12, 1) ~ "Nov-Jan",
      month(data) %in% c(2, 3, 4) ~ "Fev-Abr",
      month(data) %in% c(5, 6, 7) ~ "Mai-Jul",
      month(data) %in% c(8, 9, 10) ~ "Ago-Out",
      TRUE ~ "Other"
    ),
    
    # Adjust year for December to group with following Jan
    season_year = if_else(month(data) == 12, year(data) + 1, year(data)),
    
    # Create complete season label
    season_label = paste(season, season_year),
    
    # Create numeric ordering for plotting
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

# 2. Create the plot with connected lines
ggplot(detection_summary_filtered, 
       aes(x = season_num, 
           y = total_detections, 
           color = localidade,
           group = localidade)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  scale_x_continuous(
    breaks = unique(detection_summary_filtered$season_num),
    labels = unique(detection_summary_filtered$season_label)
  ) +
  labs(
    title = "Detecções por Localidade (2022-2025)",
    x = "Período", 
    y = "Número de Detecções",
    color = "Localidade"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    plot.title = element_text(size = 18, color = "#284b80"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.line.x = element_line(color = "grey", linewidth = 0.8),
    axis.line.y = element_line(color = "grey", linewidth = 0.8),
    legend.text = element_text(size = 8)
  ) +
  guides(color = guide_legend(nrow = 3))

# 3. Save the plot
ggsave("plots/detec_years.png", width = 10, height = 6, dpi = 300)
################################################################################
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
ggsave("plots/detec_years_improved.png", width = 12, height = 7, dpi = 300)

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
    size = 4,
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







# Geomorfolgia total

df_geo_local <- df_geo %>% 
  filter(iar_geo != "Na") %>% 
  group_by(localidade, geo_cat) %>%
  mutate(geo_value = mean(iar_geo)) %>% 
  ungroup()

max(df_geo_local$geo_value)


###
df_monit_effort_dpue <- df_monit %>% 
  
  group_by(localidade, data, faixa_bat) %>%
  filter(obs != "estimado dos dados do ICMBio") %>% 
  mutate(faixa_bat = str_to_title(str_replace_all(faixa_bat, "entremare", "entremaré")),
         localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            dpue = n_detection/(sum(max(max_trsct_vis)/60))) %>%
  ungroup()
print(df_monit_effort_dpue, n= 86)

####





##
bp_all_local = df_geo_local  %>%
  mutate(geo_cat = fct_relevel(geo_cat, "mp", "tf", "gc", "lg", "rpm" )) %>%
  ggplot( aes(fill = geo_cat, x = geo_cat, y = iar_geo)) +
  scale_fill_manual(values=c('#ff8200ff', '#ff8200ff', '#ff8200ff', '#ff8200ff', '#ff8200ff' )) +
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
  ggplot(aes(x=(mean_geo_value), y= (detections), shape = geo_cat, color=geo_cat)) + 
  geom_point() +
  geom_jitter() 


# regression
detec_vs_geo %>% 
  ggplot(aes(x=(mean_geo_value), y= (detections), shape = geo_cat, color=geo_cat)) + 
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", se = F)
  




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
