### inc
library(plyr)
library(ggplot2)
library(gridExtra)

library(data.table)

### read data
grid <- fread("~/git/variani/acndatathon/data/city-grid.csv")

df <- fread("data/processed/train.csv")

df <- subset(df, !is.na(month) & !is.na(Shift) & !is.na(GridID))

### gf
weekdays <- na.omit(unique(df$weekday))
months <- na.omit(unique(df$month))
years <- na.omit(unique(df$year))

gf <- expand.grid(weekday = weekdays,
  month = months, year = years,
  Shift = na.omit(unique(df$Shift)), 
  GridID = na.omit(unique(grid$GridID)), stringsAsFactors = FALSE)

train2 <- data.frame(stringsAsFactors = FALSE)

for(y in years) {
  for(m in months) {
    dfa <- subset(df, month == m & year == y)
    sfa <- ddply(dfa, c("month", "Shift", "weekday",  "GridID"), summarise, Accidents = length(Date))

    sfna <- expand.grid(weekday = weekdays,
      month = m, year = y,
      Shift = na.omit(unique(df$Shift)), 
      GridID = na.omit(unique(grid$GridID)), stringsAsFactors = FALSE)

    sf <- join(sfna, sfa)
    sf <- within(sf,
      Accidents[is.na(Accidents)] <- 0)
      
    train2 <- rbind(train2, sf)
  }
}

### save
save(train2, file = "train2.RData")
