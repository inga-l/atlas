#CEAC

CI <- as.numeric(unlist(daly.ci[[1]][,1]))
MA<- as.numeric(unlist(daly.ma[[1]][,1]))
SE <- as.numeric(unlist(daly.se[[1]][,1]))
ceac_df <- data.frame(CI,MA,SE)

ceac_df <- pivot_longer(ceac_df,
                        cols = everything(),
                        names_to = "country",
                        values_to = "ICER") %>% add_column(scenario = "atlas")


ceac.atlas <- ggplot(ceac_df, aes(ICER, color = country, linetype = scenario)) +
  labs(linetype = "Scenario")+
  scale_linetype_manual(values = c ("solid", "dotted"), labels = c("ATLAS-only", "ATLAS Scale-Up"))+
  stat_ecdf(geom = "step")+
  xlab("Willingness to Pay Threshold($USD 2020)") +
  ylab("Probability Cost-Effective")+
  scale_x_continuous(breaks = seq(0,800, 100),expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,1,0.1), labels = percent, expand = c(0,0))+
  labs(color = "Country")+
  scale_color_discrete(labels = c("Côte d’Ivoire", "Mali", "Senegal"))+
  geom_vline(xintercept = 200, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 400, linetype = "dashed", color = "grey") 

ceac.atlas

mean(daly.ci[[2]][,1]<400)
mean(daly.ma[[2]][,1]<400)
mean(daly.se[[2]][,1]<400)

# CI.su <- as.numeric(unlist(daly.ci[[2]][,1]))
# MA.su <- as.numeric(unlist(daly.ma[[2]][,1]))
# SE.su <- as.numeric(unlist(daly.se[[2]][,1]))
# ceac_df.su <- data.frame(CI.su,MA.su,SE.su)

CI <- as.numeric(unlist(daly.ci[[2]][,1]))
MA <- as.numeric(unlist(daly.ma[[2]][,1]))
SE <- as.numeric(unlist(daly.se[[2]][,1]))
ceac_df.su <- data.frame(CI,MA,SE)

ceac_df.su <- pivot_longer(ceac_df.su,
                           cols = everything(),
                           names_to = "country",
                           values_to = "ICER") %>% add_column(scenario = "su")
ceac_df <- bind_rows(ceac_df, ceac_df.su)


ceac.su <- ggplot(ceac_df.su, aes(ICER, color = country)) +
  stat_ecdf(geom = "step")+
  xlab("Willingness to Pay Threshold($USD 2020)") +
  ylab("Probability Cost-Effective")+
  scale_x_continuous(breaks = seq(0,800, 100),expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,1,0.1), labels = percent, expand = c(0,0))+
  labs(color = "Country")+
  scale_color_discrete(labels = c("Côte d’Ivoire", "Mali", "Senegal"))+ 
  geom_vline(xintercept = 200, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "grey")
ceac.su

ceac <- ggarrange(ceac.atlas, ceac.su,
                  title = "Cost-Effectiveness Accetability Curve",
                  labels = c("ATLAS Only", "ATLAS Scale-Up"),
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL),
                  common.legend = TRUE)
ceac
