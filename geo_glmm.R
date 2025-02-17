
# generalize linear mixed models

library(lme4)

# gráfico das detecçoes em função das geomorfologias

plot(geomonit$det, geomonit$gc_pad, type = "p", col = "red", 
     main = "Geos em função das det", 
     xlab = "Detecções", ylab = "Geomorfologias")
lines(geomonit$det, geomonit$lg_pad, type = "p", col = "blue")
lines(geomonit$det, geomonit$mp_pad, type = "p", col = "green")
lines(geomonit$det, geomonit$rpm_pad, type = "p", col = "orange")
lines(geomonit$det, geomonit$tf_pad, type = "p", col = "brown")
legend("topright", legend = c("gc", "lg", "mp", "rpm", "tf"), 
       col = c("red", "blue", "green", "orange", "brown"), lty = 1)


# modelos glm 

modelo1 <- glm(det ~ gc_pad, family = poisson, data = geomonit)
plot(modelo1)
summary(modelo1)

modelo2 <- glm(det ~ gc_pad + lg_pad + gc_pad:lg_pad, family = poisson, data = geomonit)
summary(modelo2)


modelo3 <- glm(det ~ lg_pad, family = poisson, data = geomonit)
summary(modelo3)


modelo4 <- glm(det ~ mp_pad, family = poisson, data = geomonit)
summary(modelo4)


modelo5 <- glm(det ~ mp_pad + gc_pad, family = poisson, data = geomonit)
summary(modelo5)

modelo6 <- glm(det ~ mp_pad + gc_pad + mp_pad:gc_pad, family = poisson, data = geomonit)
summary(modelo6)

# comparando os modelos para ver qual é o mais significante

anova(modelo4, modelo5, modelo6, test = 'Chisq')


#deviance é o quanto o modelo ta explicando
#residual deviance é o quanto o modelo deixou de explicar
#o que ele deixou de explicar somado ao que explicou vai dar o resultado do quanto o outro modelo não tinha explciado
#mesmo que o modelo explique pouco, se ele for bastante significativo (p value) ainda sim é considerado


# modelos glmm
##primeiro ajustar um modelo completo, com todos os efeitos fixos que estão sendo testados 
##depois que monta o modelo completo, coloca os efeitos aleatorios


m0 <- lm(det ~ mp_pad, data = geomonit)
m1 <- lmer(det ~ mp_pad + (min.div|t_divers), data = geomonit)
summary(m1)

m2 <- lm(det ~ gc_pad, data = geomonit)
m3 <- lmer(det ~ gc_pad + (min.div|t_divers), data = geomonit)


anova(m1, m3, refit = FALSE)
