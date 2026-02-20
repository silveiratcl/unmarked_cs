library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)


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
    is_absence = is.na(dafor_cat)
    )# <- if you want only dafor==0, use: (dafor == 0)



# chart agregating  by localidade, n_trans_vis, stackinh dafor



#############################################
### Stacked DAFOR by LOCALIDADE (ORDERED)
### effort = sum(n_trans_vis), bars stacked by DAFOR
#############################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# 1) Prepare
data_loc <- density_data %>%
  mutate(
    dafor_cat = case_when(
      dafor == 10 ~ "D",
      dafor == 8  ~ "A",
      dafor == 6  ~ "F",
      dafor == 4  ~ "O",
      dafor == 2  ~ "R",
      TRUE        ~ NA_character_
    ),
    n_trans_vis = dplyr::coalesce(n_trans_vis, 1)
  )

# 2) Category totals (stacked parts), weighted by n_trans_vis
cats_loc <- data_loc %>%
  filter(!is.na(dafor_cat)) %>%
  group_by(localidade, dafor_cat) %>%
  summarise(count = sum(n_trans_vis, na.rm = TRUE), .groups = "drop") %>%
  complete(localidade, dafor_cat = c("D","A","F","O","R"), fill = list(count = 0)) %>%
  mutate(dafor_cat = factor(dafor_cat, levels = c("D","A","F","O","R")))

# 3) ORDER by total visual transects (sum of n_trans_vis)
loc_order <- cats_loc %>%
  group_by(localidade) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(total) %>%   # ascending so largest appears on top after coord_flip()
  pull(localidade)

cats_loc <- cats_loc %>%
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
    y = "Total Visual Transects (sum of n_trans_vis)",
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

