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
library(scales)

#### Data ####

# monitoring
df_geo_pred = read_delim("data/predict_intensity.csv",
                      col_types = list(localidade = col_character(),
                                       model_pred = col_double(),
                                       regiao = col_character()
                      ))
df_geo_pred



df_geo_pred <- df_geo_pred %>% 
    mutate(localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
           regiao = str_to_upper(str_replace_all(regiao, "_", " ")),
           model_pred_rescale = scales::rescale(model_pred, to = c(0, 100)))
 
df_geo_pred

print(df_geo_pred, n= 40)

plot_pred_geo <- df_geo_pred %>% 
  filter(regiao != "ENTORNO") %>% 
  mutate(localidade = fct_reorder(localidade, model_pred_rescale)) %>%
  ggplot(aes(y=localidade, x=model_pred_rescale)) +
  geom_bar(position="stack", stat="identity", fill = "#536e99") +
  scale_x_continuous(position="top", n.breaks = 10, expand = c(0, 0)) +
  ggtitle("Predição IAR ~ Geo por Localidade - REBIO e Entorno Imediato") +
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


plot_pred_geo 
ggsave("plots/plot_pred_geo .png", width = 10, height = 10, dpi = 300)
