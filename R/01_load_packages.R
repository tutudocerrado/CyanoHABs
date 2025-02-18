# Load packages -----------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("maps",
              "scales",
              "terra",
              "ggplot2",
              "readxl",
              "gridExtra", 
              "tidyverse",
              "ggpubr",
              "ggmap")

ipak(packages); rm(packages,ipak)
