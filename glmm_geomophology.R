# Glm

library("lme4")
load("unmarked_data.RData")

effort
detection
predictors


effort = effort %>%
  group_by(localidade) %>% 
  mutate(detec_all = sum(eff_raso, eff_entremare, eff_fundo)) %>% 
  ungroup() %>% 
  arrange(localidade)


effort %>% 
  left_join(effort, detection, by = "localidade") %>% 
  select(-ends_with(".y"))
  





# Padronização variáveis de esforco
# minutos positivos por estrato
# visibilidade por estrato
# comprimento da localidade


effort_df = as.matrix(effort[, 2:8 ])



esforco <- sqrt(effort[, 2:4])
range(esforco)
esforco_st <- as.matrix(esforco - mean(as.matrix(esforco)))/sd(as.matrix(esforco))# padronizar esforco
range(esforco_st)
str(esforco_st)

visib <- as.matrix(cbind(effort$visib_m, effort$visib_m, effort$visib_m ))
range(visib)
visib_st <- as.matrix(visib - mean(as.matrix(visib)))/sd(as.matrix(visib))# padronizar esforco
range(visib_st)
str(visib_st)

comp_local <- as.matrix(cbind(effort$locality_comp_m, effort$locality_comp_m, effort$locality_comp_m ))
range(comp_local)
comp_local_st <- as.matrix(comp_local - mean(as.matrix(comp_local)))/sd(as.matrix(comp_local))# padronizar esforco
range(comp_local_st)
str(visib_st)



## formatar os dados de acordo com os requerimentos do pacote
## y eh uma matriz, com os sítios nas linhas, dias de amostragem nas colunas
## siteCovs deve ser um dataframe, onde cada linha eh um si?tio e cada coluna uma covariavel
## obsCovs deve ser uma lista, onde cada linha eh um si?tio e cada elemento equivale a uma covariavel


cs_det = as.matrix(detection[, 2:4])
predictors = predictors[, 2:6] 


# site covariates
tf = as.matrix(predictors$tf)
mp = as.matrix(predictors$mp)
gc = as.matrix(predictors$gc)
rpm = as.matrix(predictors$rpm)
lg = as.matrix(predictors$lg)





unmarked_data <- unmarkedFrameOccu(y=cs_det,
                                   siteCovs = cbind(data.frame (tf = tf, 
                                                                mp = mp, #covs de site
                                                                gc = gc, 
                                                                rpm = rpm,
                                                                lg = lg)),
                                   obsCovs = list(esforco = esforco_st,
                                                  visib_m = visib_st,
                                                  comp_local = comp_local_st))


mod1 <- glm(esforco ~ 1, data = unmarked_data, family = poisson )

mod2 <- glm(esforco ~ tf, data = unmarked_data, family = poisson)

mod3 <- glm(esforco ~ mp, data = unmarked_data, family = poisson)

mod4 <- glm(esforco ~ gc, data = unmarked_data, family = poisson)

mod5 <- glm(esforco ~ rpm, data = unmarked_data, family = poisson)

mod6 <- glm(esforco ~ lg, data = unmarked_data, family = poisson)

mod7 <- glm(esforco ~ gc + mp, data = unmarked_data, family = poisson)

mod8 <- glm(esforco ~ lg + rpm, data = unmarked_data, family = poisson)

mod9 <- glm(esforco ~ mp * rpm, data = unmarked_data, family = poisson)