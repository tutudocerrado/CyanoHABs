
# Adding new columns in the data ------------------------------------------

data <- data %>% mutate(mean_2040 = NA, mean_2060 = NA, mean_2080 = NA, mean_2100 = NA)

for (i in 1:nrow(data)) {
  data[i,80] <- sum(data[i,5],data[i,9],data[i,13],data[i,17],data[i,21],data[i,25])/6
  data[i,81] <- sum(data[i,6],data[i,10],data[i,14],data[i,18],data[i,22],data[i,26])/6
  data[i,82] <- sum(data[i,7],data[i,11],data[i,15],data[i,19],data[i,23],data[i,27])/6
  data[i,83] <- sum(data[i,8],data[i,12],data[i,16],data[i,20],data[i,24],data[i,28])/6
}

a <- data$mean_40
b <- data$Present

cor.test(data$mean_2040,data$Present, alternative = "greater", method = "spearman")

# Performing correlations -------------------------------------------------

# 2040

g_2040 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", colour = "red") +
  geom_point(data = data, 
             aes(x = Present, y = mean_2040, fill = cat_2040),
             shape = 21, size = 2) +
  theme_bw() +
  ggtitle("2040") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  xlab("Current suitability") +
  ylab("Future suitability") +
  coord_fixed() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  stat_cor(method="spearman", data = data, 
           aes(x = Present, y = mean_2040)) +
  geom_smooth(method = "loess",span = 2,se = FALSE,
              data = data, 
              aes(x = Present, y = mean_2040), 
              linetype = "dotted", colour = "black") +
  theme(axis.title = element_text(colour = "black", size = 12,face = "bold"),
        axis.text = element_text(colour = "black", size = 12),
        legend.title = element_blank(), 
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "none",
        panel.grid = element_blank(),
        legend.background = element_rect(colour = "black", linetype = "dotted", size = 0.5))

# 2060

g_2060 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", colour = "red") +
  geom_point(data = data, 
             aes(x = Present, y = mean_2060, fill = cat_2060),
             shape = 21, size = 2) +
  theme_bw() +
  ggtitle("2060") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  xlab("Current suitability") +
  ylab("Future suitability") +
  coord_fixed() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  stat_cor(method="spearman", data = data, 
           aes(x = Present, y = mean_2060)) +
  geom_smooth(method = "loess",span = 2,se = FALSE,
              data = data, 
              aes(x = Present, y = mean_2060), 
              linetype = "dotted", colour = "black") +
  theme(axis.title = element_text(colour = "black", size = 12,face = "bold"),
        axis.text = element_text(colour = "black", size = 12),
        legend.title = element_blank(), 
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "none",
        panel.grid = element_blank(),
        legend.background = element_rect(colour = "black", linetype = "dotted", size = 0.5))

# 2080

g_2080 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", colour = "red") +
  geom_point(data = data, 
             aes(x = Present, y = mean_2080, fill = cat_2080),
             shape = 21, size = 2) +
  theme_bw() +
  ggtitle("2080") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  xlab("Current suitability") +
  ylab("Future suitability") +
  coord_fixed() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  stat_cor(method="spearman", data = data, 
           aes(x = Present, y = mean_2080)) +
  geom_smooth(method = "loess",span = 2,se = FALSE,
              data = data, 
              aes(x = Present, y = mean_2080), 
              linetype = "dotted", colour = "black") +
  theme(axis.title = element_text(colour = "black", size = 12,face = "bold"),
        axis.text = element_text(colour = "black", size = 12),
        legend.title = element_blank(), 
        legend.text = element_text(colour = "black", size = 12),
        legend.position = "none",
        panel.grid = element_blank(),
        legend.background = element_rect(colour = "black", linetype = "dotted", size = 0.5))

# 2100

g_2100 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", colour = "red") +
  geom_point(data = data, 
             aes(x = Present, y = mean_2100, fill = cat_2100),
             shape = 21, size = 2) +
  theme_bw() +
  ggtitle("2100") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  xlab("Current suitability") +
  ylab("Future suitability") +
  coord_fixed() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  stat_cor(method="spearman", data = data, 
           aes(x = Present, y = mean_2100)) +
  geom_smooth(method = "loess",span = 2,se = FALSE,
              data = data, 
              aes(x = Present, y = mean_2100), 
              linetype = "dotted", colour = "black") +
  theme(axis.title = element_text(colour = "black", size = 12,face = "bold"),
        axis.text = element_text(colour = "black", size = 12),
        legend.title = element_blank(), 
        legend.text = element_text(colour = "black", size = 12),
        legend.position = c(0.75,0.2),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = "black", linetype = "dotted", size = 0.5))

# 2100 to use in graphical abstract

g_2100.2 <- ggplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", colour = "red") +
  geom_point(data = data, 
             aes(x = Present, y = mean_2100, fill = cat_2100),
             shape = 21, size = 6) +
  theme_bw() +
  #ggtitle("2100") +
  scale_fill_manual(values = c("blue", "white", "#f97d7d")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_fixed() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  # stat_cor(method="spearman", data = data, 
  #          aes(x = Present, y = mean_2100)) +
  geom_smooth(method = "loess",span = 2,se = FALSE,
              data = data, 
              aes(x = Present, y = mean_2100), 
              linetype = "dotted", colour = "black") +
  theme(axis.title = element_text(colour = "black", size = 12,face = "bold"),
        axis.text = element_text(colour = "black", size = 30),
        legend.title = element_blank(), 
        legend.text = element_text(colour = "black", size = 30),
        legend.position = c(0.75,0.2),
        panel.grid = element_blank(),
        legend.background = element_rect(colour = "black", linetype = "dotted", size = 0.5))

# Saving figure to graphical abstract -------------------------------------

ggsave("./suitability_2100.png", plot = g_2100.2, dpi = 300,width = 7.5)

# Saving regressions ------------------------------------------------------

suitability <- grid.arrange(g_2040,
                            g_2060,
                            g_2080,
                            g_2100, ncol = 2)

ggsave("suitability.tiff", 
       plot = suitability, 
       dpi = 300,
       width = 8, 
       height = 8)

rm(g_2040,g_2060,g_2080,g_2100,data_f)

