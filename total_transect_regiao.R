#### new fig spliting REBIO, Imediate surrounds and surrounding


df_ntrans_plit_regiao = df_monit_effort %>% 
  left_join(df_centroide_localidade, df_monit_effort, by = "localidade")
  mutate(regiao = str_to_upper(str_replace_all(regiao, "_", " "))) %>%

print(df_ntrans_plit_regiao, n= 86 )

df_ntrans_plit_regiao = df_ntrans_plit_regiao %>% 
  mutate(regiao = str_to_upper(str_replace_all(regiao, "_", " ")))
    
print(df_ntrans_plit_regiao, n= 86 )

  
  
plot_transec_strata_regiao <- df_ntrans_plit_regiao %>% 
  mutate(regiao = factor(regiao, levels = c("REBIO", "ENTORNO IMEDIATO", "ENTORNO")),
         localidade = factor(localidade)) %>%
  ggplot(aes(fill = factor(faixa_bat, levels = c("Entremaré", "Raso", "Fundo")), 
             y = reorder(localidade, max_trsct_vis, sum), 
             x = max_trsct_vis)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c('#db6d10', '#78bd49', '#536e99'),
                    labels = c("0-3m", "3-8m", "8m-Interface")) +
  scale_x_continuous(position = "top", n.breaks = 10, expand = c(0, 0)) +
  facet_wrap(~ regiao, ncol = 1, scales = "free_y") + # cats jump
  ggtitle("Total de Transectos (1min) por localidade") +
  theme(
    panel.background = element_blank(),
    axis.ticks.length.x = unit(0.2, "cm"), 
    axis.ticks.x = element_line(colour = "grey",
                                linewidth = 0.8, linetype = "solid"), 
    axis.line.x = element_line(colour = "grey",
                               linewidth = 0.8, linetype = "solid"),
    axis.ticks.y= element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, color ="#284b80" ),
    axis.title.y = element_blank(), 
    legend.text = element_text(size=15, color ="#284b80" ),
    legend.title = element_blank(),
    legend.key.size = unit(1.5, 'cm')
  )

plot_transec_strata_regiao
ggsave("plots/plot_transec_strata_regiao.png", width = 12, height = 7, dpi = 300)




