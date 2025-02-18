
# Loading ENM files -------------------------------------------------------

# Current

present <- rast("./Result/Ensemble/SUP/ciano.tif")

# Future

files <- list.files("./Result/Projection")
future <- list()

for (i in 1:length(files)) {
  future[[i]] <- rast(paste("./Result/Projection",
                            files[i],"/Ensemble/SUP/ciano.tif", sep = "/"))
}; rm(i)

# Cropping the ENM extent to the boundaries of Brazil ---------------------

# Loading Brazil's shapefile --

brasil <- vect("./data/brasil.shp")

# Cropping the current scenario

present_b <- mask(crop(present,brasil),brasil)

# Cropping the ENM projection for 2040

future_2040 <- list()
for (i in seq(1,length(files),4)) {
  for (j in 1:6) {
    future_2040[[j]] <- mask(crop(future[[i]], 
                                  brasil), 
                             brasil)
  }
};rm(i,j)

# Cropping the ENM projection for 2060

future_2060 <- list()
for (i in seq(2,length(files),4)) {
  for (j in 1:6) {
    future_2060[[j]] <- mask(crop(future[[i]], 
                                  brasil), 
                             brasil)
  }
};rm(i,j)

# Cropping the ENM projection for 2080

future_2080 <- list()
for (i in seq(3,length(files),4)) {
  for (j in 1:6) {
    future_2080[[j]] <- mask(crop(future[[i]], 
                                  brasil), 
                             brasil)
  }
};rm(i,j)

# Cropping the ENM projection for 2100

future_2100 <- list()
for (i in seq(4,length(files),4)) {
  for (j in 1:6) {
    future_2100[[j]] <- mask(crop(future[[i]], 
                                  brasil), 
                             brasil)
  }
};rm(i,j)

# Transforming ENM projections into dataframe -----------------------------

# 2040

future_2040 <- cbind(as.data.frame(future_2040[[1]], xy = TRUE),
                     as.data.frame(future_2040[[2]]),
                     as.data.frame(future_2040[[3]]),
                     as.data.frame(future_2040[[4]]),
                     as.data.frame(future_2040[[5]]),
                     as.data.frame(future_2040[[6]]))

# Naming dataframe colunms 

colnames(future_2040) <- c("x","y","model1","model2","model3","model4","model5","model6")

# Performing the mean of the six ENM projections for 2040

future_2040 <- future_2040 %>% mutate(Suitability = rowMeans(future_2040[,3:8]))

# 2060

future_2060 <- cbind(as.data.frame(future_2060[[1]], xy = TRUE),
                     as.data.frame(future_2060[[2]]),
                     as.data.frame(future_2060[[3]]),
                     as.data.frame(future_2060[[4]]),
                     as.data.frame(future_2060[[5]]),
                     as.data.frame(future_2060[[6]]))

# Naming dataframe colunms 

colnames(future_2060) <- c("x","y","model1","model2","model3","model4","model5","model6")

# Performing the mean of the six ENM projections for 2060

future_2060 <- future_2060 %>% mutate(Suitability = rowMeans(future_2060[,3:8]))

# 2080

future_2080 <- cbind(as.data.frame(future_2080[[1]], xy = TRUE),
                     as.data.frame(future_2080[[2]]),
                     as.data.frame(future_2080[[3]]),
                     as.data.frame(future_2080[[4]]),
                     as.data.frame(future_2080[[5]]),
                     as.data.frame(future_2080[[6]]))

# Naming dataframe colunms 

colnames(future_2080) <- c("x","y","model1","model2","model3","model4","model5","model6")

# Performing the mean of the six ENM projections for 2080

future_2080 <- future_2080 %>% mutate(Suitability = rowMeans(future_2080[,3:8]))

# 2100

future_2100 <- cbind(as.data.frame(future_2100[[1]], xy = TRUE),
                     as.data.frame(future_2100[[2]]),
                     as.data.frame(future_2100[[3]]),
                     as.data.frame(future_2100[[4]]),
                     as.data.frame(future_2100[[5]]),
                     as.data.frame(future_2100[[6]]))

# Naming dataframe colunms 

colnames(future_2100) <- c("x","y","model1","model2","model3","model4","model5","model6")

# Performing the mean of the six ENM projections for 2100

future_2100 <- future_2100 %>% mutate(Suitability = rowMeans(future_2100[,3:8]))



