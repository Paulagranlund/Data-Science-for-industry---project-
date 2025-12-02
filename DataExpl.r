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
df <- read_csv(file = "data/accidents_catalunya_english.xlsb.csv")

# Data inspection
summary(df)
str(df)
head(df)