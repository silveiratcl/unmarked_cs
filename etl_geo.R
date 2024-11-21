
library(tidyverse)
library(readr)
library(tidyr)

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
problems(df_geo)
df_geo = df_geo[, 1:14]
df_geo


# localities

df_localidade = read_delim("data/localidade_rebio.csv", delim = ";", 
                           col_types = c("d","c","d"))
df_localidade
print(df_localidade, n = 41)



####
####
# Tasks
# Create a table id_dafor with the corresponding values iar_geo



### test table dafor

df_monit_test = df_monit %>% 
  filter(data > "2023-01-01",
         obs != "estimado dos dados do ICMBio",
         dafor > 0,
         faixa_bat != "entremare"
         ) %>% 
  mutate(dafor_id = as.integer(dafor_id),
         geo_id = as.integer(geo_id))

df_monit_test #ok

############################### resumo vic

df_monit_test_all = df_monit %>% 
  filter(data > "2023-01-01",
         obs != "estimado dos dados do ICMBio",
         #dafor > 0,
         faixa_bat != "entremare"
  ) %>% 
  mutate(dafor_id = as.integer(dafor_id),
         geo_id = as.integer(geo_id))

df_monit_test_all#ok

length(unique(df_monit_test_all$dafor_id))
# 69 transect

df_monit_test_all %>%
  group_by(dafor_id) %>% 
  filter(dafor > 0) %>% 
  summarise()


df_monit_test_all_sum= df_monit_test_all %>%
  group_by(dafor_id) %>% 
  #filter(dafor > 0) %>% 
  summarise(total_trans = max(n_trans_vis))

sum(df_monit_test_all_sum$total_trans)
# 3221

3222/60 #HORas

length(unique(df_monit_test_all$localidade))

####
df_monit_local = df_monit_test_all %>% 
  left_join(df_localidade, join_by(localidade)) %>%
  group_by(localidade) %>% 
  reframe(comp_m_local = sum(max(comp_m)/1000)) 
  
  
sum(df_monit_local$comp_m_local)/1000
  

df_monit_local$comp_m_local[1]

print(df_monit_local, n=200)
  ###########################
 

############################ resumo vic

df_geo_test =    df_geo %>%
  filter(data > "2023-01-01",
         faixa_bat != "entremare"
         ) %>% 
  mutate(geo_id = as.integer(geo_id))
  
df_geo_test  #ok

#leftjoin

df_monit_geo = df_monit_test %>% 
  left_join(df_geo_test, join_by(geo_id), relationship = "many-to-many")  %>% 
  select(localidade.x, 
         data.x,
         tempo_censo, 
         dafor, 
         n_trans_vis.x, 
         n_trans_pres, 
         dafor_id, 
         geo_id, 
         tempo_geo, 
         geo_cat, 
         iar_geo,
        )


print(df_monit_geo, n = 50)




# creating time in minutes to geo and investigating differences in time 

df_monit_geo = df_monit_geo %>% 
  group_by(geo_id, tempo_geo) %>% 
  mutate(tempo_geo_minute = (max(tempo_geo)*5)) %>% 
  ungroup() %>% 
  group_by(geo_id) %>% 
  mutate(tempo_geo_max = (max(tempo_geo)*5), 
         dif_t_dafor_geo = max(tempo_censo) - tempo_geo_max,
         )


df_monit_geo
print(df_monit_geo, n = 500)


# creating table


df_model <- df_monit_geo %>%
  group_by(dafor_id) %>%
  reframe(
    n_trans = max(n_trans_pres),
    tf_std = (iar_geo[geo_cat == "tf"] - mean(iar_geo[geo_cat == "tf"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    mp_std = (iar_geo[geo_cat == "mp"] - mean(iar_geo[geo_cat == "mp"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    gc_std = (iar_geo[geo_cat == "gc"] - mean(iar_geo[geo_cat == "gc"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    rpm_std = (iar_geo[geo_cat == "rpm"] - mean(iar_geo[geo_cat == "rpm"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    lg_std = (iar_geo[geo_cat == "lg"] - mean(iar_geo[geo_cat == "lg"], na.rm = TRUE)) / sd(iar_geo[geo_cat == "lg"], na.rm = TRUE)
  )

df_model
print(df_model, n = 148)  


#### AED

df_aed <- df_monit_geo %>%
  group_by(dafor_id) %>%
  reframe(
    n_trans = max(n_trans_pres),
    tf_avg = mean(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    tf_sd = sd(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    mp_avg = mean(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    mp_sd = sd(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    gc_avg = mean(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    gc_sd = sd(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    rpm_avg = mean(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    rpm_sd = sd(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    lg_avg = mean(iar_geo[geo_cat == "lg"], na.rm = TRUE),
    lg_sd = sd(iar_geo[geo_cat == "lg"], na.rm = TRUE)
  )

df_aed

#### com localidade

df_aed <- df_monit_geo %>%
  group_by(localidade.x, dafor_id) %>%
  reframe(
    n_trans = max(n_trans_pres),
    tf_avg = mean(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    tf_sd = sd(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    mp_avg = mean(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    mp_sd = sd(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    gc_avg = mean(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    gc_sd = sd(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    rpm_avg = mean(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    rpm_sd = sd(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    lg_avg = mean(iar_geo[geo_cat == "lg"], na.rm = TRUE),
    lg_sd = sd(iar_geo[geo_cat == "lg"], na.rm = TRUE)
  )

print(df_aed, n =12)



df_aed <- df_monit_geo %>%
  group_by(localidade.x) %>%
  reframe(
    n_trans = max(n_trans_pres),
    tf_avg = mean(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    tf_sd = sd(iar_geo[geo_cat == "tf"], na.rm = TRUE),
    mp_avg = mean(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    mp_sd = sd(iar_geo[geo_cat == "mp"], na.rm = TRUE),
    gc_avg = mean(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    gc_sd = sd(iar_geo[geo_cat == "gc"], na.rm = TRUE),
    rpm_avg = mean(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    rpm_sd = sd(iar_geo[geo_cat == "rpm"], na.rm = TRUE),
    lg_avg = mean(iar_geo[geo_cat == "lg"], na.rm = TRUE),
    lg_sd = sd(iar_geo[geo_cat == "lg"], na.rm = TRUE)
  )

print(df_aed, n =12)




# A tibble: 11 × 12
#dafor_id n_trans tf_avg tf_sd mp_avg mp_sd gc_avg gc_sd rpm_avg rpm_sd lg_avg lg_sd
#<int>   <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>   <dbl>  <dbl>  <dbl> <dbl>
#1       15       1   6    3.02    0.5  0.926  0     0       10     0       0.5  0.926
#2       21       2   6    0       0.4  0.843  0     0        8.4   0.843   0    0    
#3       22       4   6.75 1.41    4    2.03   1.75  1.59     6.75  2.68    0    0    
#4       44       5   3.75 1.58    4.5  2.82   0     0        0.5   0.877   0.5  0.877
#5       45       2   1.5  1.37    2.5  2.25   0     0        8.88  2.16    1.5  2    
#6       58       1   6    0.894   7.27 2.24   5.45  2.02     3.82  1.40    3.45 1.29 
#7       74       1   4.25 1.28    1.75 1.28   0.25  0.707    7.5   2.78    3.25 4.27 
#8       99       2   8.44 1.89    6.44 3.60   4.67  2.57     6.44  2.33    0    0    
#9      100      11   5.33 3.00    4.22 3.34   1.11  1.38     6.89  3.68    4.44 2.64 
#10      118       3   6.33 3.16    3.08 2.83   0.833 1.01     9.25  1.42    2.25 3.03 
#11      119       1   4    2.45    3.78 1.86   0     0        8.67  2.24    2.89 3.18 






mean(df_aed$n_trans)
#3
sd(df_aed$n_trans)
#2.966479

max(df_aed$n_trans)
# 11

cor(df_aed$n_trans, df_aed$tf_avg, method = "spearman")
cor(df_aed$n_trans, df_aed$mp_avg, method = "spearman")
cor(df_aed$n_trans, df_aed$gc_avg, method = "spearman")
cor(df_aed$n_trans, df_aed$rpm_avg, method = "spearman")
cor(df_aed$n_trans, df_aed$lg_avg, method = "spearman")

#> cor(df_aed$n_trans, df_aed$tf_avg, method = "spearman")
#[1] 0.07111002
#> cor(df_aed$n_trans, df_aed$mp_avg, method = "spearman")
#[1] 0.2818424
#> cor(df_aed$n_trans, df_aed$gc_avg, method = "spearman")
#[1] 0.1231662
#> cor(df_aed$n_trans, df_aed$rpm_avg, method = "spearman")
#[1] -0.314724
#> cor(df_aed$n_trans, df_aed$lg_avg, method = "spearman")
#[1] -0.163933






mean(df_aed$tf_avg)
sd(df_aed$tf_sd)
#> mean(df_aed$tf_avg)
#[1] 5.305556
#> sd(df_aed$tf_sd)
#[1] 0.9954703

mean(df_aed$mp_avg)
sd(df_aed$mp_sd)
#> mean(df_aed$mp_avg)
#[1] 3.4955
#> sd(df_aed$mp_sd)
#[1] 0.9220174


mean(df_aed$gc_avg)
sd(df_aed$gc_sd)
#> mean(df_aed$gc_avg)
#[1] 1.278696
#> sd(df_aed$gc_sd)
#[1] 0.9374563
#> 


mean(df_aed$rpm_avg)
sd(df_aed$rpm_sd)
#> mean(df_aed$rpm_avg)
#[1] 7.008471
#> sd(df_aed$rpm_sd)
#[1] 1.055789



mean(df_aed$lg_avg)
sd(df_aed$lg_sd)
#> mean(df_aed$lg_avg)
#[1] 1.707989
#> sd(df_aed$lg_sd)
#[1] 1.469996
 


#######
### Fig correlation with trendline


round(cor(df_aed$n_trans, df_aed$tf_avg, method = "spearman"), 2)
round(cor(df_aed$n_trans, df_aed$mp_avg, method = "spearman"), 2)
round(cor(df_aed$n_trans, df_aed$gc_avg, method = "spearman"), 2)
round(cor(df_aed$n_trans, df_aed$rpm_avg, method = "spearman"), 2)
round(cor(df_aed$n_trans, df_aed$lg_avg, method = "spearman"), 2)

geo_cat = c("tf", "mp", "gc", "rpm", "lg")
cor_spear = c(0.07, 0.28, 0.12, -0.31, -0.16)
cor_scores = tibble(geo_cat = geo_name, cor_spear = cor_spear)


detec_vs_geo <- detec_vs_geo %>%
  left_join(cor_scores, by = "geo_cat")

# Define positions for annotations
annotation_positions <- data.frame(
  geo_cat = geo_name,
  x_pos = c(2, 2, 2, 2, 2),
  y_pos = c(12, 13, 14, 15, 16)
)

summary_data <- summary_data %>%
  left_join(annotation_positions, by = "geo_cat")




detec_vs_geo %>% 
  ggplot(aes(x = mean_geo_value, y = detections, shape = geo_cat, color = geo_cat)) + 
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(data = summary_data, 
            aes(x = x_pos, y = y_pos, label = paste0("r = ", round(cor_spear, 2))), 
            hjust = 0, vjust = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")



#####

# alternative plot

detec_vs_geo %>% 
  ggplot(aes(x = mean_geo_value, y = detections, shape = geo_cat, color = geo_cat)) + 
  geom_point() +
  geom_jitter() +
  theme_minimal() +
  theme(legend.position = "bottom")





#####

#minuzzi  samuel


table(df_monit$localidade)

complete_dafor_cases <- c(0, 2, 4, 6, 8, 10)

df_monit_minuzzi <- df_monit %>%
  filter(localidade %in% c("engenho", "deserta_norte", "deserta_sul", "saco_dagua", "farol")) %>%
  group_by(localidade, dafor) %>%
  summarize(case_count = n(), .groups = 'drop'  ) %>%
  ungroup() %>%
  complete(localidade, dafor = complete_dafor_cases, fill = list(case_count = 0)) %>%
  pivot_wider(names_from = dafor, values_from = case_count, values_fill = list(case_count = 0))

df_monit_minuzzi 


df_monit_minuzzi <- df_monit %>%
  filter(localidade %in% c("engenho", "deserta_norte", "deserta_sul", "saco_dagua", "farol")) %>%
  group_by(localidade) %>%
  summarize(mean_dafor = mean(dafor), , .groups = 'drop') %>% 
  ungroup() 

df_monit_minuzzi 




#####

library(ggplot2)
library(reshape2)


# Create a data frame with a category column
dt <- data.frame(
  category = factor("A", "B", "C", "D", "E", "F"),  # Added a 'category' column
  A = rbinom(6, 100, 0.9),
  B = rbinom(6, 100, 0.5),
  C = rbinom(6, 100, 0.2),
  D = rbinom(6, 100, 0.05)
)

######
dt_melt <- melt(dt, id.vars = "category")

ggplot(dt_melt, aes(x = variable, y = category, fill = value)) +
  geom_tile(color = "black") +  # Add borders to the cells
  geom_text(aes(label = value), color = "white", size = 4) +  # Add text values
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Variable", y = "Category", fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

