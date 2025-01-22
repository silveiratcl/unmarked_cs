
library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)


# monitoring

df_monit <- read_delim("data/dados_monitoramento_cs_2024-03-22.csv",
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
df_monit
print(df_monit, n = 100)

## aplicando os filtros 
 
dfmonit_filt <- df_monit %>% 
  filter(data > "2022-01-01" &
         !(obs %in% c("Sem geo", "estimado dos dados do ICMBio"))) %>%
  arrange(data)
dfmonit_filt

## selecionando a localidade por data para filtrar o numero de transectos

monit <- dfmonit_filt[ ,c("localidade", "data", "n_trans_vis", "n_trans_pres")] %>%
  group_by(localidade, data) %>%
  reframe(visuals = max(n_trans_vis),
            detec = max(n_trans_pres)) %>%
  arrange(data)

print(monit, n = 44)
###número maximo de transectos por data e localidade

## obtendo o total de transectos vistos e detecções para cada localidade

monit2 <- monit %>%
  group_by(localidade) %>%
  summarise(vis = sum(visuals),
            det = sum(detec)) %>%
  arrange(desc(vis))

print(monit2, n = 35)

## detecções por unidade de tempo

dpue <- (monit2$vis/ 60)
dpue2 <- (monit2$det/dpue)
monit2$dpue2 <- dpue2
monit2

print(monit2, n = 35)


# geomorphology

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
df_geo = df_geo[, 1:14]
df_geo

## agrupando e obtendo o total do IAR para cada geomorfologia em cada localidade

geo <- df_geo[ ,c("localidade", "data", "geo_cat", "iar_geo")] %>%
  group_by(localidade, data, geo_cat) %>%
  reframe(iar_geo = max(iar_geo)) %>%
  arrange(data)

print(geo, n = 185)

geo2 <- geo[ ,c("localidade", "geo_cat", "iar_geo")] %>%
  group_by(localidade)

print(geo2, n = 185)

## unindo os df 

geomonit <- left_join(monit2, geo2)
print(geomonit, n = 182)

###colocar as geos como coluna e iargeo como linha dessa coluna
geomonit$iar_geo <- as.numeric(geomonit$iar_geo)
str(geomonit)

df_final <- geomonit %>%
  group_by(localidade) %>%
  pivot_wider(names_from = geo_cat, values_from = iar_geo, values_fn = list)
  
      







# standardazing the filtered data

pad_trans_pres_total <- as.matrix(monit$trans_pres_total - mean(as.matrix(monit$trans_pres_total)))/sd(as.matrix(monit$trans_pres_total))
pad_trans_vis_total <- as.matrix(monit$trans_vis_total - mean(as.matrix(monit$trans_vis_total)))/sd(as.matrix(monit$trans_vis_total))
pad_effort <- as.matrix(monit$effort - mean(as.matrix(monit$effort)))/sd(as.matrix(monit$effort))
pad_trans_pres_total
pad_trans_vis_total
pad_effort

#####
pad <- scale(monit[, 2:3]) #função que padroniza direto 
#variavel com media 0 e desvio padrao 1
#subtrai a média de cada coluna e divide pelo desvio padrão da mesma coluna


# associating to the filtered data frame

monit$pad_trans_pres_total <- pad_trans_pres_total
monit$pad_trans_vis_total <- pad_trans_vis_total
monit$pad_effort <- pad_effort
monit
print(monit, n = 37)


# geomorphology

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
df_geo = df_geo[, 1:14]
df_geo

# grouping by localities

dfgeo_filt <- df_geo[ ,c("localidade", "geo_cat", "iar_geo")]
dfgeo_filt

geomorf <- dfgeo_filt %>%
  group_by(geo_cat) %>%
  reframe(localidade = localidade,
    sum_iar_geo = sum(iar_geo))
  
  

geomorf 
  




  summarise(trans_pres_total = sum(n_trans_pres),
            trans_vis_total = sum(n_trans_vis),
            effort = sum(n_trans_pres/n_trans_vis)) %>%
  arrange(desc(trans_pres_total)) %>%
  drop_na()

# localities

df_localidade = read_delim("data/localidade_rebio.csv", delim = ";", 
                           col_types = c("d","c","d"))
df_localidade
print(df_localidade, n = 41)
#### o comprimento é baseado no mapa da localidade?

# Create a table id_dafor with the corresponding values iar_geo

## test table dafor

df_monit_test = df_monit %>% 
  filter(data > "2023-01-01",
         obs != "estimado dos dados do ICMBio",
         dafor > 0,
         faixa_bat != "entremare"
         ) %>% 
  mutate(dafor_id = as.integer(dafor_id),
         geo_id = as.integer(geo_id))

df_monit_test 
####vamos manter os filtros aplicados?

############################### resumo vic

df_monit_test_all = df_monit %>% 
  filter(data > "2023-01-01",
         obs != "estimado dos dados do ICMBio",
         #dafor > 0,
         faixa_bat != "entremare"
         ) %>% 
  mutate(dafor_id = as.integer(dafor_id),
         geo_id = as.integer(geo_id))

df_monit_test_all[, 1:17]

length(unique(df_monit_test_all$dafor_id))
####numero de valores diferentes na coluna do dafor id no df monit all

df_monit_test_all %>%
  group_by(dafor_id) %>% 
  filter(dafor > 0) %>% 
  summarise()


df_monit_test_all_sum= df_monit_test_all %>%
  group_by(dafor_id) %>% 
  #filter(dafor > 0) %>% 
  summarise(total_trans = max(n_trans_vis))

sum(df_monit_test_all_sum$total_trans)

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
  
df_geo_test

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

mean(df_aed$tf_avg)
sd(df_aed$tf_sd)
mean(df_aed$mp_avg)
sd(df_aed$mp_sd)
mean(df_aed$gc_avg)
sd(df_aed$gc_sd)
mean(df_aed$rpm_avg)
sd(df_aed$rpm_sd)
mean(df_aed$lg_avg)
sd(df_aed$lg_sd)


df_aed
print(df_aed, n = 148)  

## Teste de normalidade
shapiro.test(df_aed$n_trans) #fora da normalidade
shapiro.test(df_aed$tf_avg)
shapiro.test(df_aed$tf_sd)
shapiro.test(df_aed$mp_avg)
shapiro.test(df_aed$mp_sd)
shapiro.test(df_aed$gc_avg) #fora da normalidade 
shapiro.test(df_aed$gc_sd) #fora da normalidade
shapiro.test(df_aed$rpm_avg)
shapiro.test(df_aed$rpm_sd)
shapiro.test(df_aed$lg_avg)
shapiro.test(df_aed$lg_sd)


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


mean(df_aed$n_trans)
sd(df_aed$n_trans)
max(df_aed$n_trans)

## Analise de correlação
cor(df_aed$n_trans, df_aed$tf_avg, method = "spearman") 
cor(df_aed$n_trans, df_aed$mp_avg, method = "spearman") 
cor(df_aed$n_trans, df_aed$gc_avg, method = "spearman") 
cor(df_aed$n_trans, df_aed$rpm_avg, method = "spearman") 
cor(df_aed$n_trans, df_aed$lg_avg, method = "spearman") 
##p < 0,05, tem correlacao


## Grafico de dispersao
df_aed_mean <- df_aed %>%
  select(n_trans, tf_avg, mp_avg, gc_avg, rpm_avg, lg_avg)
str(df_aed_mean)

ggplot(df_aed_mean, aes(x = gc_avg, y = n_trans)) +
  geom_point(color = "black", fill = "seagreen", shape = 21, size = 3, alpha = .7)  +
  xlab("tf_avg") +          
  ylab("n_trans") +
  geom_smooth(method = "lm")
#geom_smooth(method="lm", se=FALSE, color="black")



##tentativa de unir tudo em relacao aos transectos, mas ainda nao rolou
df_long <- df_aed_mean %>%
  gather(key =  "variavel_x", value = "valor_x", -n_trans) %>%
  print(n = 110)


ggplot(df, aes(x = x1, y = y, color = factor(x2), shape = factor(x3))) +
  geom_point() +
  facet_wrap(~ factor(x4) + factor(x5), nrow = 1) +
  labs(x = "Variável Independente 1", y = "Variável Dependente") +
  scale_color_discrete(name = "Variável Independente 2") +
  scale_shape_discrete(name = "Variável Independente 3") +
  theme_minimal()

ggplot(df_long, aes(x = valor_x , y = n_trans, color = variavel_x)) +
  geom_point() +
  #facet_wrap(~ variavel_x, scales = "free_x") +
  labs(x = "Variáveis X", y = "n_trans") +
  theme_minimal()
=======
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

mean(df_aed$tf_avg)
sd(df_aed$tf_sd)

mean(df_aed$mp_avg)
sd(df_aed$mp_sd)

mean(df_aed$gc_avg)
sd(df_aed$gc_sd)

mean(df_aed$rpm_avg)
sd(df_aed$rpm_sd)

mean(df_aed$lg_avg)
sd(df_aed$lg_sd)


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

