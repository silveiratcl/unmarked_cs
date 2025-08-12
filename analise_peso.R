library(readr)
data <- read_delim("data2.csv",
                   col_types = list(weight = col_double(),
                                    class = col_double(),
                                    n_polyps = col_double()),
                   delim = ";"
) 




head(data)


model1 <- lm(weight ~ n_polyps, data = data)

summary(model1)
plot(model1) 


model2 <- lm(weight ~ class, data = data)

summary(model2)
plot(model2) 



model3 <- glm(weight ~ class, data = data, family = Gamma(link = "inverse"))

summary(model3)
plot(model3) 

model3



