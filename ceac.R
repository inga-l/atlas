#CEAC
library(pacman)
p_load(tidyverse, readr, ggplot2, ggrepel, ggfortify, ggthemes, RColorBrewer, ggpubr, cowplot, "scales", EnvStats,hesim, ggbreak, wesanderson, geomtextpath, khroma)
theme_set(theme_few())

CI <- as.numeric(unlist(daly.ci[[1]][,1]))*1.09
MA<- as.numeric(unlist(daly.ma[[1]][,1]))*1.09
SE <- as.numeric(unlist(daly.se[[1]][,1]))*1.09
ceac_df <- data.frame(CI,MA,SE)

CI <- as.numeric(unlist(daly.ci[[2]][,1]))*1.09
MA <- as.numeric(unlist(daly.ma[[2]][,1]))*1.09
SE <- as.numeric(unlist(daly.se[[2]][,1]))*1.09
ceac_df.su <- data.frame(CI,MA,SE)

ceac_df <- pivot_longer(ceac_df,
                        cols = everything(),
                        names_to = "country",
                        values_to = "ICER") %>% add_column(scenario = "atlas")

ceac_df.su <- pivot_longer(ceac_df.su,
                           cols = everything(),
                           names_to = "country",
                           values_to = "ICER") %>% add_column(scenario = "su")
ceac_df <- bind_rows(ceac_df, ceac_df.su)

wtp <- seq(0, 770, by = 10)
n_wtp <- length(wtp)
df <- NULL
ci_a <- subset(ceac_df, country == "CI" & scenario == "atlas")
ci_s <- subset(ceac_df, country == "CI" & scenario == "su")
ma_a <- subset(ceac_df, country == "MA" & scenario == "atlas")
ma_s <- subset(ceac_df, country == "MA" & scenario == "su")
se_a <- subset(ceac_df, country == "SE" & scenario == "atlas")
se_s <- subset(ceac_df, country == "SE" & scenario == "su")  
for (i in 1:n_wtp) {
  df_i <- data.frame(country = c("CI", "CI", "MA", "MA", "SE", "SE"),
                     scenario = c("atlas", "su","atlas", "su","atlas", "su"),
                     wtp = wtp[i],
                     pce = c(sum(ci_a$ICER < wtp[i]) / nrow(ci_a),
                             sum(ci_s$ICER < wtp[i]) / nrow(ci_s),
                             sum(ma_a$ICER < wtp[i]) / nrow(ma_a),
                             sum(ma_s$ICER < wtp[i]) / nrow(ma_s),
                             sum(se_a$ICER < wtp[i]) / nrow(se_a),
                             sum(se_s$ICER < wtp[i]) / nrow(se_s) )*100)
  df <- rbind(df, df_i)
}


ceac.atlas <- ggplot(df, aes(x = wtp, y = pce/100, color = country, linetype = scenario)) +
  geom_line()+
  labs(linetype = "Scenario")+
  scale_linetype_manual(values = c ("solid", "dotted"), labels = c("ATLAS-only", "ATLAS Scale-Up"))+
  xlab("Willingness to Pay Threshold($USD 2022)") +
  ylab("Probability Cost-Effective") +
  scale_x_continuous(breaks = seq(0,750, 100), expand = c(0,0))+
  expand_limits(x= c(0, 770))+
  scale_y_continuous(breaks = seq(0,100,0.1), labels = percent, expand = c(0,0))+
  labs(color = "Country")+
  scale_color_manual(values = c("#004488", "#DDAA33", "#BB5566"),labels = c("Côte d’Ivoire", "Mali", "Senegal")) +
  geom_textvline(xintercept = 488, linetype = "dashed", label = "$488") +
  geom_textvline(xintercept = 155, linetype = "dashed",  label = "$155")

ceac.atlas
ggsave("plot_name.png", width = 6, height = 4, dpi = 600)

sum(ci_a$ICER < 155) / nrow(ci_a)
sum(ma_a$ICER < 155) / nrow(ma_a)
sum(se_a$ICER < 155) / nrow(se_a)

sum(ci_s$ICER < 488) / nrow(ci_s)
sum(ma_s$ICER < 488) / nrow(ma_s)
sum(se_s$ICER < 488) / nrow(se_s)
