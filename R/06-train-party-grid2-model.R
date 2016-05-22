### inc
library(plyr)
library(ggplot2)
library(gridExtra)

library(party)

library(PredictABEL)

library(data.table)

### read data
load("data/processed/train3.RData")

test <- fread("data/processed/test.csv", data.table = F)

### prepare data
df <- mutate(train3,
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

df <- within(df, {
  GridID2 <- factor(GridID2, levels = na.omit(unique(test$GridID2)))
  weekendFac <- factor(weekendFac, levels = na.omit(unique(test$weekendFac)))
  month <- factor(month, levels = na.omit(unique(test$month)))  
})

test <- within(test, {
  GridID2 <- factor(GridID2, levels = na.omit(unique(test$GridID2)))
  weekendFac <- factor(weekendFac, levels = na.omit(unique(test$weekendFac)))
  month <- factor(month, levels = na.omit(unique(test$month)))  
})
  
### model 1
mod1 <- ctree(AccidentsBin ~ yearNum + weekdayNum + weekendNum + ShiftNum + weekendFac:ShiftNum + month + GridID2,
  data = df)

stop()

N <- 1e4  

ypt <- predict(mod1, head(df, N))
yt <- head(df$AccidentsBin, N)

#plotROC(data.frame(outcome = yt), 1, ypt)
#AUC [95% CI] for the model 1 :  0.779 [ 0.77  -  0.788 ] 

# submission for `mod1`
yp <- predict(mod1, test)

test$AccidentLikelihood <- yp
sub <- subset(test, select = c("Date", "Shift", "GridID", "AccidentLikelihood"))
sub <- rename(sub, c(Date = "date"))
write.csv(sub, file = "submission-party-mod1.csv", row.names = F)

### model 1
mod2 <- cforest(AccidentsBin ~ yearNum + weekdayNum + weekendNum + ShiftNum + weekendFac:ShiftNum + month + GridID2,
  data = df, control = cforest_unbiased(ntree = 20))

N <- 1e4  

ypt <- predict(mod1, head(df, N))
yt <- head(df$AccidentsBin, N)

plotROC(data.frame(outcome = yt), 1, ypt)
  
             
