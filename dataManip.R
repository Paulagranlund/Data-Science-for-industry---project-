library(tidyverse)
library(ggplot2)
library(dplyr)
library(leaps)

df <- read.csv("accidents_catalunya_english.xlsb.csv")

# Dataformating and creating beforetoll column
df$dat <- as.Date(df$dat, format = "%d/%m/%Y")
dateToll <- as.Date("01/09/2021", format = "%d/%m/%Y")
df$IS_after_toll <- ifelse(df$dat >= dateToll, 1, 0) # TRUE if after 01/09/21
# 8202 after the toll, 16276 before the toll

# Roads that changed the toll rules (regardless of before/after):
tollFreeRoads <- c("AP-7", "AP-2", "C-32", "C-33")
df$IS_on_toll_road <- ifelse(df$via %in% tollFreeRoads, 1, 0)
# 844 on toll road, 23634 not on (not considering time)

# IF on tollRoad after the toll changes
df$IS_on_tollfree_after <- df$IS_after_toll == 1 & df$IS_on_toll_road == 1 
# 276 on tollfree after change, 24202 not

# which month 
df$month <- as.numeric(format(df$dat, "%m"))

# changing "No" and "Si" to 0 and 1 in D_INFL's
for (i in 31:40){
  df[i] <- ifelse(df[i] == "No", 0,1)
}
# Fatal or serious accident
df$IS_fatal <- ifelse(df$D_SEVERITY == "FatalAcc", 0,1)
df$D_SEVERITY <- NULL

# changing possible columns to 0,1 in binary queries 

df$IS_special_road_sit <- ifelse(df$D_SPESIAL_ROAD_SITUATION == "Si", 1, 0)
df$workday <- ifelse(df$workday == "Weekday", 1, 0)
df$D_HIT_A_RUN <- ifelse(df$D_HIT_A_RUN == "Si", 1 ,0 )
df$D_FOG <- ifelse(df$D_FOG == "Si", 1, 0)
df$D_LIMIT_VELOCITY <- ifelse(df$D_LIMIT_VELOCITY == "Generic road", 1, 0)
df$IS_dry <- ifelse(df$D_SURFACE == "Dry and clean", 1,0)

# checking missing data points
NA_tresh <- 0.3
missing_pts <- sapply(df, function(x) mean(is.na(x)))
colsMissing <- missing_pts[missing_pts > NA_tresh]
print(colsMissing)

# Many missing data points (NA) > 30%
df$D_ROAD_LAYOUT  <- NULL # 57%
df$D_TERRAIN_CHARACTER <- NULL # 38%
df$D_PRIORITY_REGULATION  <- NULL # 71%
df$D_SUBTYPE_STREET <- NULL # 67%
df$D_ROAD_OWNERSHIP  <- NULL # 51 %

# Creating smaller df, removing possible usless info
miniDf <- df
miniDf$nomMun <- NULL
miniDf$nomCom <- NULL
miniDf$D_SPESIAL_ROAD_SITUATION <- NULL
miniDf$D_LIMIT_VELOCITY <- NULL
miniDf$D_ROAD_OWNERSHIP <- NULL
miniDf$groupHour <- NULL
miniDf$tipDia <- NULL
miniDf$D_SUBTYPE_ACCIDENT <- NULL
miniDf$D_SUBTYPE_STREET <- NULL
miniDf$D_SUBZONE <- NULL
miniDf$D_AT_INTERSECTION <- NULL
miniDf$D_SURFACE <- NULL
miniDf$D_ROAD_LAYOUT <- NULL
miniDf$D_PRIORITY_REGULATION <- NULL
##mby
miniDf$D_TRAFFIC_DIRRECTIONS <- NULL
miniDf$D_ROAD_TYPE <- NULL
miniDf$tipAcc <- NULL
##mby