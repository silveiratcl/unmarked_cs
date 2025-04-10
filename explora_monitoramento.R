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


# Shapefile localities


shp_localidades = st_read("data/localidades_shapefile.shp")
shp_localidades


### Data processing ### 

# Aggregate by locality 
# total time
# total detections 
# Obtaining the detection and effort df
# detection is presence/absence by locality by each monitoring strata
# effort is the number of visual transects where cs were detected

df_monit_effort <- df_monit  %>% 
  group_by(localidade, data, faixa_bat) %>%
  filter(obs != "estimado dos dados do ICMBio", faixa_bat != "Na") %>% 
  mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " "))) %>%
  summarise(max_trsct_vis = sum(max(n_trans_vis)),
            n_detection = max(n_trans_pres),
            n_divers = max(n_divers),
            visib_m = max(visib_horiz)) %>%
    ungroup()
df_monit_effort
print(df_monit_effort, n=86)


df_monit_effort$localidade


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
length(df_monit$dafor)


table(df_monit_effort$localidade)
table(df_monit_effort$faixa_bat)


# n detections minutes

plot_detec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, n_detection, sum)) %>% 
  filter(n_detection > 0)  %>% 
  ggplot(aes(fill=factor(faixa_bat,levels=c("entremare", "raso", "fundo")), y=localidade, x=n_detection)) +
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("0-3m", "3-8m", "8m-Interface")) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Total de transectos(1m) com presença de coral-sol") +
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

# n transects by locality

plot_transec_strata <- df_monit_effort %>% 
  mutate(localidade = fct_reorder(localidade, max_trsct_vis, sum)) %>% 
  ggplot(aes(fill=factor(faixa_bat,levels=c("entremare", "raso", "fundo")), y=localidade, x=max_trsct_vis)) +
  scale_fill_manual(values=c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("0-3m", "3-8m", "8m-Interface")) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Total de Transectos (1min) por localidade") +
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
                    labels = c("0-3m", "3-8m", "8m-Interface")) +
  geom_bar(position="stack", stat="identity") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("DPUE - Detecções/H/100m ") +
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

####

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
    na.label = "No data",
    opacity = 1
  ) %>%
  
  # Add scale bar
  addScaleBar(position = "bottomleft")

# 7. DISPLAY MAP
dpue_map





## Detection through the years ##############################

# First get localities with sampling in more than one year
multi_year_localities <- df_monit_effort_dpue %>%
  #filter(total_detections > 0) %>%
  mutate(year = year(data)) %>%
  distinct(localidade, year) %>%  # Get unique year-locality combinations
  group_by(localidade) %>%
  filter(n() > 1) %>%  # Keep only localities with >1 year
  ungroup() %>%
  pull(localidade) %>%
  unique()

# Filter the detection summary for only these localities
detection_summary_filtered <- df_monit_effort_dpue %>%
  filter(total_detections > 0,
         localidade %in% multi_year_localities) %>% 
  mutate(year = year(data)) %>%
  group_by(year, localidade) %>%
  summarise(
    total_detections = sum(total_detections, na.rm = TRUE),
    .groups = "drop"
  )

# Plot for filtered localities
ggplot(detection_summary_filtered, aes(x = year, y = total_detections, color = localidade)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Detections by Locality (Multi-Year Sampling)",
    subtitle = "Only showing localities with detections in >1 year",
    x = "Year", 
    y = "Number of Detections",
    color = "Locality"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(detection_summary_filtered$year)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8)) +  # Adjust legend text size
  guides(color = guide_legend(nrow = 3))  # Wrap legend into multiple rows if needed





############ !!!!!!!!!!!!!!!!!!!!!! ############################################
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
