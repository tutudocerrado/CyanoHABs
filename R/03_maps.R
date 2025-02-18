
# Carrying the outline of Brasil to use in ggplot2 ------------------------

brasil3 <- borders(database = "world",regions = "Brazil", colour = "black", size = 1)

# Mapping CyanoHABs suitability for 2040

fig_2040 <- ggplot() +
  geom_tile(data = future_2040, aes(x = x, y = y, fill = Suitability)) +
  brasil3 +
  coord_fixed() +
  ggtitle("2040") +
  xlab(NULL) +
  ylab(NULL) +
  xlim(c(-75,-34)) +
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red2",
                       limits=c(0,1),
                       breaks=c(0,0.5,1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),axis.line = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12, colour = "black",face = "bold"),
        legend.position = c(0.2,0.3),
        legend.key.size = unit(0.5,"cm"),
        title = element_text(size = 12, colour = "black", face = "bold"))

# Mapping CyanoHABs suitability for 2060

fig_2060 <- ggplot() +
  geom_tile(data = future_2060, aes(x = x, y = y, fill = Suitability)) +
  brasil3 +
  coord_fixed() +
  ggtitle("2060") +
  xlab(NULL) +
  ylab(NULL) +
  xlim(c(-75,-34)) +
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red2",
                       limits=c(0,1),
                       breaks=c(0,0.5,1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),axis.line = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12, colour = "black",face = "bold"),
        legend.position = c(0.2,0.3),
        legend.key.size = unit(0.5,"cm"),
        title = element_text(size = 12, colour = "black", face = "bold"))

# Mapping CyanoHABs suitability for 2080

fig_2080 <- ggplot() +
  geom_tile(data = future_2080, aes(x = x, y = y, fill = Suitability)) +
  brasil3 +
  coord_fixed() +
  ggtitle("2080") +
  xlab(NULL) +
  ylab(NULL) +
  xlim(c(-75,-34)) +
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red2",
                       limits=c(0,1),
                       breaks=c(0,0.5,1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),axis.line = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12, colour = "black",face = "bold"),
        legend.position = c(0.2,0.3),
        legend.key.size = unit(0.5,"cm"),
        title = element_text(size = 12, colour = "black", face = "bold"))

# Mapping CyanoHABs suitability for 2100

fig_2100 <- ggplot() +
  geom_tile(data = future_2100, aes(x = x, y = y, fill = Suitability)) +
  brasil3 +
  coord_fixed() +
  ggtitle("2100") +
  xlab(NULL) +
  ylab(NULL) +
  xlim(c(-75,-34)) +
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red2",
                       limits=c(0,1),
                       breaks=c(0,0.5,1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),axis.line = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12, colour = "black",face = "bold"),
        legend.position = c(0.2,0.3),
        legend.key.size = unit(0.5,"cm"),
        title = element_text(size = 12, colour = "black", face = "bold"))

# Mapping CyanoHABs suitability for the current scenario

present_data <- as.data.frame(present_b, xy = TRUE)

present_fig <- ggplot() +
  geom_tile(data = present_data, aes(x = x, y = y, fill = ciano)) +
  brasil3 +
  coord_fixed() +
  ggtitle("Current") +
  xlab(NULL) +
  ylab(NULL) +
  xlim(c(-75,-34)) +
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red2",
                       limits=c(0,1),
                       breaks=c(0,0.5,1),
                       name = "Suitability") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),axis.line = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12, colour = "black",face = "bold"),
        legend.position = c(0.2,0.3),
        legend.key.size = unit(0.5,"cm"),
        title = element_text(size = 12, colour = "black", face = "bold"))

mapa <- grid.arrange(present_fig, present_fig, fig_2040, fig_2060, fig_2080, fig_2100, ncol = 2)

ggsave("mapa.tiff", 
       plot = mapa, 
       dpi = 800,
       width = 8, 
       height = 12)