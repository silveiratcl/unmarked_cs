
library(tidyverse)
library(readr)
library(tidyr)
library(ggplot2)

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

df_monit_test[ , 12:15]  #ok


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



mean(df_aed$n_trans)
sd(df_aed$n_trans)
max(df_aed$n_trans)

## Analise de correlação
cor(df_aed$n_trans, df_aed$tf_avg, method = "spearman") #sem correlacao
cor(df_aed$n_trans, df_aed$mp_avg, method = "spearman") #sem correlacao
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