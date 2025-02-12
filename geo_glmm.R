# generalize linear mixed models

library(lme4)


#exemplo glm
glm01 <- glm(Species ~ Biomass + pH + Biomass:pH, family = poisson, data = arv)
#aqui nossa variavel resposta é a quantidade de especies em função da biomassa, do ph e da interação da biomassa com o ph
#posso colocar a função de ligação que eu quero usar, mas se nao especificar o r usa a padrão pra poisson que é log
#data é o df onde os dados estao
#daria pra ser mais simplificado, como:
glm01 <- glm(Species ~ Biomass * pH)
#o asterisco inclui o efeito sozinho e também suas interações
summary(glm01)
#vai mostrar os dados a serem analisados

modelo1 <- glm(det ~ gc_pad, family = poisson, data = geomonit)
summary(modelo1)

modelo2 <- glm(det ~ gc_pad + lg_pad + gc_pad:lg_pad, family = poisson, data = geomonit)
summary(modelo2)
#insignificante

modelo3 <- glm(det ~ lg_pad, family = poisson, data = geomonit)
summary(modelo3)
#insignificante

modelo4 <- glm(det ~ mp_pad, family = poisson, data = geomonit)
summary(modelo4)
#importancia

modelo5 <- glm(det ~ mp_pad + gc_pad, family = poisson, data = geomonit)
summary(modelo5)

modelo6 <- glm(det ~ mp_pad + gc_pad + mp_pad:gc_pad, family = poisson, data = geomonit)
summary(modelo6)
#significante



anova(modelo4, modelo5, modelo6, test = 'Chisq')
#modelo6 mais significante


#ir em busca do modelo minimo adequado (modelo cheio)

#comparar os modelos pela anova
anova(glm01, glm02, test='Chisq')
#deviance é o quanto o modelo ta explicando
#residual deviance é o quanto o modelo deixou de explicar
#o que ele deixou de explicar somado ao que explicou vai dar o resultado do quanto o outro modelo não tinha explciado
#mesmo que o modelo explique pouco, se ele for bastante significativo (p value) ainda sim é considerado
#anova da as comparações na ordem em que as variaveis sao colocadas na formatação do modelo

#contagem nunca vai ser abaixo de zero

#a equação do modelo é dada pelos coeficientes estimados do modelo

#distribuição poisson é definida so pelo parametro lambda (equivalente a media em uma distribuição normal)
#variancia da distribuição eh igual a media
#o quanto a variação dos meus dados é maior que a media dos meus dados
#calcula pegando o resíduo do modelo na escala de deviance e dividindo pelos graus de liberdade
##valor tem que ser proximo a 1, se nao for tem uma sobredispersão (mais dispersão do que a distribuição poisson da conta ou menos)
##precisa usar algo que de conta da sobre ou subdispersao dos dados
##familia associada é a quasipoisson, que calcula mais um parametro relacionando a media com a variancia



#exemplo glmm
m0 <- lm(Richness ~ fExposure * NAP, data = dados) #sem efeito aleat.
m1 <- lmer(Richness ~ fExposure * NAP + (1|Beach), data = dados)
## variações no intercepto entre praias ((1|Beach)) e  interação entre praia e NAP
m2 <- lmer(Richness ~ fExposure * NAP + (NAP|Beach), data = dados)
##variação de inclinação entre praias ((NAP|Beach))
##primeiro deve ser ajustado um modelo completo, com todos os efeitos fixos que estão sendo testados 
## noexemplo tínhamos apenas NAP, mas vamos incluir Exposure (como fator fixo) para exemplificar melhor, e vamos incluir a interação entre NAP e Exposure
##depois que monta o modelo completo, coloca os efeitos aleatorios

Richness ~ fExposure + NAP + fExposure:NAP + “efeito(s) aleatório(s)”

m0 <- lm(det ~ mp_paS * gc_pad, data = geomonit)
m1 <- lmer(det ~ mp_pad * gc_pad + (1|vis), data = geomonit)
m2 <- lmer(det ~ mp_pad * gc_pad + (gc_pad|vis), data = geomonit)
m3 <- lm(mp_pad ~ det, data = geomonit)
m4 <- lmer(mp_pad ~ det + (1|vis), data = geomonit)
m5 <- lmer(mp_pad ~ det + (1|min.div), data = geomonit)
summary(m3)
summary(m4)
summary(m5)

anova(m4, m5, refit = FALSE)
       