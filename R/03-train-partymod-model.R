# Intercept varying among g1 and g2 within g1
# (1 | g1/g2) 
# (1 | g1)+(1 | g1:g2) 

### inc
library(plyr)
library(ggplot2)
library(gridExtra)

library(party)

library(PredictABEL)

library(data.table)

### read data
load("data/processed/train2.RData")

test <- fread("data/processed/test.csv", data.table = F)

### prepare data
df <- mutate(train2,
  AccidentsBin = as.numeric(Accidents > 0),
  weekdayNum = as.numeric(factor(weekday, ordered = TRUE, 
    levels = c("Sunday", "Saturday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))),
  weekendNum = as.numeric(weekday %in% c("Sunday", "Saturday")),
  weekendFac = as.factor(weekendNum),
  yearNum = as.numeric(year) - 2012,
  ShiftNum = as.numeric(factor(Shift, levels = c("Night", "Morning", "Afternoon"), ordered = TRUE))
)  

test <- mutate(test,
  weekdayNum = as.numeric(factor(weekday, ordered = TRUE, 
    levels = c("Sunday", "Saturday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))),
  weekendNum = as.numeric(weekday %in% c("Sunday", "Saturday")),    
  weekendFac = as.factor(weekendNum),  
  yearNum = as.numeric(year) - 2012,
  ShiftNum = as.numeric(factor(Shift, levels = c("Night", "Morning", "Afternoon"), ordered = TRUE))
)  
  
### train model
mod4 <- mob(I(Accidents > 0) ~ yearNum + weekdayNum + weekendNum + ShiftNum + weekendFac:ShiftNum | month + GridID, df, model = glinearModel, family = binomial())
  
