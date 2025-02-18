
# Loading data of the water reservoir location ----------------------------

data <- readxl::read_excel("./data/reservoir_data.xlsx", sheet = 1)

# Preparing data ----------------------------------------------------------

data_suitability <- matrix(nrow = nrow(data), ncol = (4+2*length(files)))
data_suitability[,1] <- data$X
data_suitability[,2] <- data$Y

# Extracting values from the current --------------------------------------

p <- terra::extract(present, data[,1:2], cells = TRUE)
data_suitability[,3] <- p[,3] 
data_suitability[,4] <- p[,2]; rm(p)

# Extracting values from the future ---------------------------------------

for (i in 1:length(files)) {
  f <- terra::extract(future[[i]], data[,1:2])
  data_suitability[,i+4] <- f[,2]
}; rm(f,i)

data_suitability[,29] <- data_suitability[,5]-data_suitability[,4] #40
data_suitability[,30] <- data_suitability[,6]-data_suitability[,4] #60
data_suitability[,31] <- data_suitability[,7]-data_suitability[,4] #80
data_suitability[,32] <- data_suitability[,8]-data_suitability[,4] #100
data_suitability[,33] <- data_suitability[,9]-data_suitability[,4] #40
data_suitability[,34] <- data_suitability[,10]-data_suitability[,4] #60
data_suitability[,35] <- data_suitability[,11]-data_suitability[,4] #80
data_suitability[,36] <- data_suitability[,12]-data_suitability[,4] #100
data_suitability[,37] <- data_suitability[,13]-data_suitability[,4] #40
data_suitability[,38] <- data_suitability[,14]-data_suitability[,4] #60
data_suitability[,39] <- data_suitability[,15]-data_suitability[,4] #80
data_suitability[,40] <- data_suitability[,16]-data_suitability[,4] #100
data_suitability[,41] <- data_suitability[,17]-data_suitability[,4] #40
data_suitability[,42] <- data_suitability[,18]-data_suitability[,4] #60
data_suitability[,43] <- data_suitability[,19]-data_suitability[,4] #80
data_suitability[,44] <- data_suitability[,20]-data_suitability[,4] #100
data_suitability[,45] <- data_suitability[,21]-data_suitability[,4] #40
data_suitability[,46] <- data_suitability[,22]-data_suitability[,4] #60
data_suitability[,47] <- data_suitability[,23]-data_suitability[,4] #80
data_suitability[,48] <- data_suitability[,24]-data_suitability[,4] #100
data_suitability[,49] <- data_suitability[,25]-data_suitability[,4] #40
data_suitability[,50] <- data_suitability[,26]-data_suitability[,4] #60
data_suitability[,51] <- data_suitability[,27]-data_suitability[,4] #80
data_suitability[,52] <- data_suitability[,28]-data_suitability[,4] #100


# Naming columns ----------------------------------------------------------

colnames(data_suitability) <- c("X","Y","ID","Present","BCC-CSM2-MR-2040","BCC-CSM2-MR-2060",
                                "BCC-CSM2-MR-2080","BCC-CSM2-MR-2100","CNRM-CM6-1-2040",
                                "CNRM-CM6-1-2060","CNRM-CM6-1-2080","CNRM-CM6-1-2100",
                                "CNRM-ESM2-1-2040","CNRM-ESM2-1-2060","CNRM-ESM2-1-2080",
                                "CNRM-ESM2-1-2100","IPSL-CM6A-LR-2040","IPSL-CM6A-LR-2060",
                                "IPSL-CM6A-LR-2080","IPSL-CM6A-LR-2100","MIROC-ES2L-2040",
                                "MIROC-ES2L-2060","MIROC-ES2L-2080","MIROC-ES2L-2100",
                                "MIROC6-2040","MIROC6-2060","MIROC6-2080","MIROC6-2100",
                                "dif-BCC-CSM2-MR-2040","dif-BCC-CSM2-MR-2060","dif-BCC-CSM2-MR-2080",
                                "dif-BCC-CSM2-MR-2100","dif-CNRM-CM6-1-2040","dif-CNRM-CM6-1-2060",
                                "dif-CNRM-CM6-1-2080","dif-CNRM-CM6-1-2100","dif-CNRM-ESM2-1-2040",
                                "dif-CNRM-ESM2-1-2060","dif-CNRM-ESM2-1-2080","dif-CNRM-ESM2-1-2100",
                                "dif-IPSL-CM6A-LR-2040", "dif-IPSL-CM6A-LR-2060","dif-IPSL-CM6A-LR-2080",
                                "dif-IPSL-CM6A-LR-2100","dif-MIROC-ES2L-2040","dif-MIROC-ES2L-2060",
                                "dif-MIROC-ES2L-2080","dif-MIROC-ES2L-2100","dif-MIROC6-2040",
                                "dif-MIROC6-2060","dif-MIROC6-2080","dif-MIROC6-2100")

data <- as.data.frame(cbind(data_suitability,data[,3:ncol(data)])); rm(data_suitability)

# Function to calculate confidence interval -------------------------------

ic.m <- function(x, conf = 0.95){
  n <- length(x)
  media <- mean(x)
  variancia <- var(x)
  quantis <- qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
  ic <- media + quantis * sqrt(variancia/n)
  lo<- ic[1]
  up<-ic[2]
  med<- media
  ic<-cbind(med,lo,up)
  return(ic)
}

# Creating a new dataframe to save results of ic.m function ---------------

data_f <- matrix(nrow = nrow(data), ncol = 17)
data_f[,1] <- data[,3]

# Computing confidence interval -------------------------------------------

for (i in 1:nrow(data)) {
  m_2040 <- c(data[i,29],data[i,33],data[i,37],data[i,41],data[i,45],data[i,49])
  m_2060 <- c(data[i,30],data[i,34],data[i,38],data[i,42],data[i,46],data[i,50])
  m_2080 <- c(data[i,31],data[i,35],data[i,39],data[i,43],data[i,47],data[i,51])
  m_2100 <- c(data[i,32],data[i,36],data[i,40],data[i,44],data[i,48],data[i,52])
  data_f[i,2:4] <- ic.m(m_2040) 
  data_f[i,5:7] <- ic.m(m_2060)
  data_f[i,8:10] <- ic.m(m_2080)
  data_f[i,11:13] <- ic.m(m_2100)
}; rm(m_2040,m_2060,m_2080,m_2100,i)

data_f <- as.data.frame(data_f)

# Assigning class of ENM change to each water reservoir -------------------

for (i in 1:nrow(data_f)) {
  if(data_f[i,2] > 0 & data_f[i,3] > 0 & data_f[i,4] > 0){
    data_f[i,14] <- "Increases"
  } else{
    data_f[i,14] <- "Constant"
  }
  if(data_f[i,2] < 0 & data_f[i,3] < 0 & data_f[i,4] < 0){
    data_f[i,14] <- "Decreases"
  } 
  if(data_f[i,5] > 0 & data_f[i,6] > 0 & data_f[i,7] > 0){
    data_f[i,15] <- "Increases"
  } else{
    data_f[i,15] <- "Constant"
  }
  if(data_f[i,5] < 0 & data_f[i,6] < 0 & data_f[i,7] < 0){
    data_f[i,15] <- "Decreases"
  }
  if(data_f[i,8] > 0 & data_f[i,9] > 0 & data_f[i,10] > 0){
    data_f[i,16] <- "Increases"
  } else{
    data_f[i,16] <- "Constant"
  }
  if(data_f[i,8] < 0 & data_f[i,9] < 0 & data_f[i,10] < 0){
    data_f[i,16] <- "Decreases"
  }
  if(data_f[i,11] > 0 & data_f[i,12] > 0 & data_f[i,13] > 0){
    data_f[i,17] <- "Increases"
  } else{
    data_f[i,17] <- "Constant"
  }
  if(data_f[i,11] < 0 & data_f[i,12] < 0 & data_f[i,13] < 0){
    data_f[i,17] <- "Decreases"
  }
}

# Naming columns ----------------------------------------------------------

colnames(data_f) <- c("ID","med_2040","lo_2040","up_2040",
                      "med_2060","lo_2060","up_2060",
                      "med_2080","lo_2080","up_2080",
                      "med_2100","lo_2100","up_2100","cat_2040",
                      "cat_2060","cat_2080","cat_2100")

# Assigning levels in a factor --------------------------------------------

data_f$cat_2040 <- factor(data_f$cat_2040, levels = c("Decreases","Constant","Increases"))
data_f$cat_2060 <- factor(data_f$cat_2060, levels = c("Decreases","Constant","Increases"))
data_f$cat_2080 <- factor(data_f$cat_2080, levels = c("Decreases","Constant","Increases"))
data_f$cat_2100 <- factor(data_f$cat_2100, levels = c("Decreases","Constant","Increases"))

data <- cbind(data,data_f[,2:ncol(data_f)])

data_f$ID <- as.character(data_f$ID)
data_f[order(data_f$ID, decreasing=TRUE),]

# Forest plots ------------------------------------------------------------

# 2040

g_2040 <- ggplot(data = data_f, 
                 aes(med_2040, ID, xmax= up_2040, xmin= lo_2040, fill = cat_2040))+
  geom_vline(xintercept = 0.0, linetype="dotted")+
  geom_errorbarh(alpha=0.8, color="black",height = 0.1) +
  geom_point(data = data_f,aes(med_2040), size=2, shape = 22) +
  scale_size(range = c(2,5), guide=FALSE) +
  xlab(expression("Effect Size"~(~bar(X)[~Delta~Suit.~(Future~-~Present)]))) +
  theme_bw()+
  ggtitle("2040") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  theme(panel.grid= element_line(colour = "white"), 
        axis.text = element_text(colour = "black",size=12), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(colour = "black", size= 13),
        axis.title.y =element_text(colour = "white", size= 12),
        legend.position = "none")

# 2060

g_2060 <- ggplot(data = data_f, 
                 aes(med_2060, ID, xmax= up_2060, xmin= lo_2060, fill = cat_2060))+
  geom_vline(xintercept = 0.0, linetype="dotted")+
  geom_errorbarh(alpha=0.8, color="black",height = 0.1) +
  geom_point(data = data_f,aes(med_2060), size=2, shape = 22) +
  scale_size(range = c(2,5), guide=FALSE) +
  xlab(expression("Effect Size"~(~bar(X)[~Delta~Suit.~(Future~-~Present)]))) +
  theme_bw()+
  ggtitle("2060") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  theme(panel.grid= element_line(colour = "white"), 
        axis.text = element_text(colour = "black",size=12), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(colour = "black", size= 13),
        axis.title.y =element_text(colour = "white", size= 12),
        legend.position = "none")

# 2080

g_2080 <- ggplot(data = data_f, 
                 aes(med_2080, ID, xmax= up_2080, xmin= lo_2080, fill = cat_2080))+
  geom_vline(xintercept = 0.0, linetype="dotted")+
  geom_errorbarh(alpha=0.8, color="black",height = 0.1) +
  geom_point(data = data_f,aes(med_2080), size=2, shape = 22) +
  scale_size(range = c(2,5), guide=FALSE) +
  xlab(expression("Effect Size"~(~bar(X)[~Delta~Suit.~(Future~-~Present)]))) +
  theme_bw()+
  ggtitle("2080") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  theme(panel.grid= element_line(colour = "white"), 
        axis.text = element_text(colour = "black",size=12), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(colour = "black", size= 13),
        axis.title.y =element_text(colour = "white", size= 12),
        legend.position = "none")

# 2100

g_2100 <- ggplot(data = data_f, 
                 aes(med_2100, ID, xmax= up_2100, xmin= lo_2100, fill = cat_2100))+
  geom_vline(xintercept = 0.0, linetype="dotted")+
  geom_errorbarh(alpha=0.8, color="black",height = 0.1) +
  geom_point(data = data_f,aes(med_2100), size=2, shape = 22) +
  scale_size(range = c(2,5), guide=FALSE) +
  xlab(expression("Effect Size"~(~bar(X)[~Delta~Suit.~(Future~-~Present)]))) +
  theme_bw()+
  ggtitle("2100") +
  scale_fill_manual(values = c("blue", "white", "red")) +
  theme(panel.grid= element_line(colour = "white"), 
        axis.text = element_text(colour = "black",size=12), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(colour = "black", size= 13),
        axis.title.y =element_text(colour = "white", size= 12),
        legend.title = element_blank(),
        legend.position = c(0.73,0.7),
        legend.text = element_text(colour = "black", size= 12),
        legend.background = element_rect(colour = "black", linetype = "dotted", size = 0.5))


# Saving forest plot  -----------------------------------------------------

forest <- grid.arrange(g_2040,g_2060,g_2080,g_2100, ncol = 4)

ggsave("./forest2100.png",
       plot = g_2100,
       dpi = 300,
       width = 3,
       height = 12)

ggsave("./forest.tiff",
       plot = forest, 
       dpi = 300,
       width = 11.67,
       height = 9.58)

rm(g_2040,g_2060,g_2080,g_2100,forest,data_f,i,ic.m)