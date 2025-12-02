# Load libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(janitor)
library(ggplot2)
library(scales)
library(ggpubr)
library(sf)       
library(naniar)

# Data load
df <- read_csv(file = "/Users/annamatzen/Desktop/Skrivebord MacBook Pro tilhørende Anna/MSc Business Analytics/ETSEIB/Data science for Industry/Project/Accidents.csv") %>% clean_names()

# Data inspection
summary(df)
str(df)
head(df)
head(df$hor,5)