### inc
library(plyr)
library(ggplot2)
library(gridExtra)

library(data.table)

### read data
load("data/processed/grid2.RData")

df <- fread("data/processed/train.csv")

df <- subset(df, !is.na(month) & !is.na(Shift) & !is.na(GridID2))

### gf
weekdays <- na.omit(unique(df$weekday))
months <- na.omit(unique(df$month))
years <- na.omit(unique(df$year))

gf <- expand.grid(weekday = weekdays,
  month = months, year = years,
  Shift = na.omit(unique(df$Shift)), 
  GridID2 = na.omit(unique(grid2$GridID2)), stringsAsFactors = FALSE)

train3 <- data.frame(stringsAsFactors = FALSE)

for(y in years) {
  for(m in months) {
    dfa <- subset(df, month == m & year == y)
    sfa <- ddply(dfa, c("month", "Shift", "weekday",  "GridID2"), summarise, Accidents = length(Date))

    sfna <- expand.grid(weekday = weekdays,
      month = m, year = y,
      Shift = na.omit(unique(df$Shift)), 
      GridID2 = na.omit(unique(grid2$GridID2)), stringsAsFactors = FALSE)

    sf <- join(sfna, sfa)
    sf <- within(sf,
      Accidents[is.na(Accidents)] <- 0)
      
    train3 <- rbind(train3, sf)
  }
}

### save
save(train3, file = "train3.RData")
