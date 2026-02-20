library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)


# Create density plot by year
# Compares the DAFOR between oldest and newest years
# Displays number of visual transects (n=) in each year label


# Create Sample data 
# saco_do_capim,
# saco_do_batismo,
# baia_das_tartarugas,
# engenho,
# farol  

df_guia <- df_monit |> 
  filter(localidade %in% c("saco_do_capim",
                           "saco_do_batismo",
                           "baia_das_tartarugas",
                           "engenho",
                           "farol"), 
         localidade_rebio!= "ENTORNO",
         obs != "estimado dos dados do ICMBio")







table(df_guia$localidade)

write_csv(df_guia, "guia/data_guia.csv")



##############################
### 1. Prepare and check data
##############################


data <- df_guia |> 
  mutate(
    localidade = str_to_upper(str_replace_all(localidade, "_", " ")),
    localidade_rebio = str_to_upper(str_replace_all(localidade_rebio, "_", " ")                           ),
    year = year(data)
  ) 

# Check if data exists

if(nrow(data) == 0) stop("No data found")

density_data <- data  |> 
  group_by(year) |> 
  mutate(
    n_trans_count = n(),
    total_dafor = sum(dafor, na.rm = TRUE)
  ) |> 
  ungroup()  |> 
  arrange(year) |> 
  mutate(
    year_label = paste0(year, " (n=", n_trans_count, ")"),
    year_label = factor(year_label, levels = unique(year_label))
  )

sum(is.na(density_data$dafor))






#############################################
### Stacked DAFOR by LOCALIDADE (ORDERED)
### effort = sum(n_trans_vis), bars stacked by DAFOR
#############################################



# 1) Prepare
data_loc <- density_data  |> 
  mutate(
    dafor_cat = case_when(
      dafor == 10 ~ "D",
      dafor == 8  ~ "A",
      dafor == 6  ~ "F",
      dafor == 4  ~ "O",
      dafor == 2  ~ "R",
      dafor == 0  ~ "Ausente",
      TRUE        ~ NA_character_
    ),
    n_trans_vis = dplyr::coalesce(n_trans_vis, 1)
  )

# 2) Category totals (stacked parts), weighted by n_trans_vis
cats_loc <- data_loc %>%
  filter(!is.na(dafor_cat)) %>%
  group_by(dafor_id, localidade, dafor_cat) %>%
  summarise(n_trans_eff = max(n_trans_vis, na.rm = TRUE), .groups = "drop") %>%
  distinct(dafor_id, localidade, dafor_cat, n_trans_eff) %>%
  group_by(localidade, dafor_cat) %>%
  summarise(count = sum(n_trans_eff, na.rm = TRUE), .groups = "drop") %>%
  complete(localidade, dafor_cat = c("D","A","F","O","R", "Ausente"), fill = list(count = 0)) %>%
  mutate(dafor_cat = factor(dafor_cat, levels = c("D","A","F","O","R", "Ausente")))

# 3) ORDER by total visual transects (sum of n_trans_vis)
loc_order <- cats_loc  |> 
  group_by(localidade)  |> 
  summarise(total = sum(count), .groups = "drop")  |> 
  arrange(total) |>  
  pull(localidade)

cats_loc <- cats_loc |> 
  mutate(localidade = factor(localidade, levels = loc_order))

# 4) Plot (horizontal)
stacked_dafor_localidade <- ggplot(cats_loc, 
                                   aes(x = localidade, 
                                       y = count, 
                                       fill = dafor_cat)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Esforço (soma de minutos de monitoramento)",
    fill = ""
  ) +
  scale_fill_viridis_d(option = "plasma", begin = 0.9, end = 0.1) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.size = unit(.9, "cm")
  )

stacked_dafor_localidade

ggsave("plots/stacked_dafor_localidade.png",
       stacked_dafor_localidade,
       width = 12, height = 6, dpi = 300)




