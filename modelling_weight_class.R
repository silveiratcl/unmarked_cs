library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(MASS)  # For mvrnorm
library(ggplot2)
library(ggpmisc)
library(viridis)



# 1. Import and prepare data (as before)
mass_data <- read_delim("data/data_mass_class.csv", delim = ";")
counting_data <- read_delim("data/data_counting_class.csv", delim = ";") %>%
  mutate(Data = dmy(Data))

# 2. Build model
lm_model <- lm(mass_g ~ factor(class), data = mass_data)

summary(lm_model)

# 3. Bootstrap prediction function
predict_mass_with_uncertainty <- function(counts_df, model, n_boot = 1000) {
  counts <- c(counts_df$class_1,
              counts_df$class_2,
              counts_df$class_3,
              counts_df$class_4,
              counts_df$class_5)
  counts[is.na(counts)] <- 0
  
  # Point estimate
  point_pred <- sum(predict(model, 
                            newdata = data.frame(class = rep(1:5, times = counts))))
  
  # Bootstrap CI
  coefs <- coef(model)
  vcov_mat <- vcov(model)
  sim_coefs <- MASS::mvrnorm(n_boot, coefs, vcov_mat)
  
  boot_preds <- sapply(1:n_boot, function(i) {
    means <- c(sim_coefs[i,1], 
               sim_coefs[i,1] + sim_coefs[i,2:5])
    sum(means * counts)
  })
  
  ci <- quantile(boot_preds, c(0.025, 0.975), na.rm = TRUE)
  
  tibble(
    pred_mass = point_pred,
    lower_ci = ci[1],
    upper_ci = ci[2],
    rel_uncertainty = (ci[2] - ci[1])/point_pred
  )
}

# 4. Apply to counting data
set.seed(123)  # For reproducibility
counting_data <- counting_data %>%
  rowwise() %>%
  mutate(predict_mass_with_uncertainty(
    data.frame(class_1, class_2, class_3, class_4, class_5),
    lm_model
  )) %>%
  ungroup()

# 5. View and analyze results
# Install required packages if needed
if (!require(ggpmisc)) install.packages("ggpmisc")
if (!require(viridis)) install.packages("viridis")


# Prepare class sample size annotations
class_n <- mass_data %>%
  count(class) %>%
  mutate(label = paste0("Class ", class, ": n=", n),
         x_pos = max(counting_data$total) * 0.7,
         y_pos = max(counting_data$pred_mass) * seq(0.8, 0.4, length.out = 5))

# Create the plot
ggplot(counting_data, aes(x = total, y = pred_mass)) +
  
  # Uncertainty and points
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0, alpha = 0.3, color = "gray50") +
  geom_point(aes(color = rel_uncertainty), size = 3) +
  
  # Regression line with equation
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "darkred", se = TRUE, fill = "pink", alpha = 0.2) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(eq.label), 
                                 after_stat(rr.label), sep = "~~~")),
               parse = TRUE,
               label.x = 0.1, label.y = 0.9,
               size = 4, color = "darkred") +
  
  # 1:1 reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  

  
  # Customization
  scale_color_viridis(name = "Incerteza\nRelativa", option = "plasma") +
  labs(title = "Predição de Massa de Coral-sol com Classes de Tamanho",
       subtitle = "Vermelho: Regressão Linear | Azul: 1:1 referencia | n = c1-30, c2-39, c3-46, c4-35, c5-10",
       x = "Contagem de Colônias",
       y = "Predição de Massa (g)",
       caption = "Barras indicam o intervalo de confiança de  95%") +
  #theme_minimal() +
  theme(legend.position = "right",
        panel.background = element_blank(),
        plot.title = element_text(size = 18, color ="#284b80"),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.line.x = element_line(colour = "grey",
                                   linewidth = 0.8, linetype = "solid"),
        axis.line.y = element_line(colour = "grey",
                                   linewidth = 0.8, linetype = "solid"))


ggsave("plots/counting_mass.png", width = 10, height = 5, dpi = 300)        
        
### Return Final data frame IN 
counting_data = counting_data %>% 
  mutate(pred_mass_kg = pred_mass/1000 )



write.csv(counting_data, file = "data/counting_data_pred.csv", row.names = FALSE)
