# Intercept varying among g1 and g2 within g1
# (1 | g1/g2) 
# (1 | g1)+(1 | g1:g2) 

### inc
library(plyr)
library(ggplot2)
library(gridExtra)

library(lme4)

library(PredictABEL)

library(data.table)

### read data
load("data/processed/train2.RData")

test <- fread("data/processed/test.csv", data.table = F)

### prepare data
df <- mutate(train2,
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

df <- within(df,
  GridID <- factor(GridID, levels = na.omit(unique(test$GridID)))
  
### model 1
mod1 <- lmer(Accidents ~ yearNum + weekdayNum + ShiftNum + (1|month) + (1|GridID), df)
mod1@frame <- head(mod1@frame)

ypt <- predict(mod1, df, re.form = NULL)
ypt[ypt < 0] <- 0

yt <- as.numeric(df$Accidents > 0)

N <- 1e4
#plotROC(data.frame(outcome = head(yt, N)), 1, head(ypt, N))
#AUC [95% CI] for the model 1 :  0.823 [ 0.794  -  0.852 ] 

yp1 <- predict(mod1, test, re.form = NULL)

# submission for `mod1`
yp <- predict(mod1, test, re.form = NULL)
yp[yp < 0] <- 0

test$AccidentLikelihood <- yp
sub <- subset(test, select = c("Date", "Shift", "GridID", "AccidentLikelihood"))
sub <- rename(sub, c(Date = "date"))
write.csv(sub, file = "submission-mm-mod1.csv", row.names = F)

### model 2: weekdayNum:ShiftNum
mod2 <- lmer(Accidents ~ yearNum + weekdayNum + ShiftNum + weekdayNum:ShiftNum + (1|month) + (1|GridID), df)
mod2@frame <- head(mod2@frame)

ypt <- predict(mod2, df, re.form = NULL)
ypt[ypt < 0] <- 0

yt <- as.numeric(df$Accidents > 0)

N <- 1e4
#plotROC(data.frame(outcome = head(yt, N)), 1, head(ypt, N))
#AUC [95% CI] for the model 1 :  0.822 [ 0.793  -  0.85 ] 

# submission for `mod2`
yp <- predict(mod2, test, re.form = NULL)
yp[yp < 0] <- 0

test$AccidentLikelihood <- yp
sub <- subset(test, select = c("Date", "Shift", "GridID", "AccidentLikelihood"))
sub <- rename(sub, c(Date = "date"))
write.csv(sub, file = "submission-mm-mod2.csv", row.names = F)

### model 3:  + (1|GridID:month) 
mod3 <- lmer(Accidents ~ yearNum + weekdayNum + weekendNum + ShiftNum + weekendFac:ShiftNum + (yearNum||month) + (1|GridID), df)

ypt <- predict(mod3, df, re.form = NULL)
ypt[ypt < 0] <- 0

yt <- as.numeric(df$Accidents > 0)

N <- 1e4
#plotROC(data.frame(outcome = head(yt, N)), 1, head(ypt, N))

# submission for `mod3`
yp <- predict(mod3, test, re.form = NULL)
yp[yp < 0] <- 0

test$AccidentLikelihood <- yp
sub <- subset(test, select = c("Date", "Shift", "GridID", "AccidentLikelihood"))
sub <- rename(sub, c(Date = "date"))
write.csv(sub, file = "submission-mm-mod3.csv", row.names = F)

### model 4:  fixef as in model3, randef smple
mod4 <- lmer(Accidents ~ yearNum + weekdayNum + weekendNum + ShiftNum + weekendFac:ShiftNum + (1|month) + (1|GridID), df)
mod4@frame <- head(mod4@frame)

N <- 1e4
ypt <- predict(mod4, head(df, N), re.form = NULL)
ypt[ypt < 0] <- 0

yt <- as.numeric(head(df$Accidents, N) > 0)

plotROC(data.frame(outcome = yt), 1, ypt)
#AUC [95% CI] for the model 1 :  0.827 [ 0.799  -  0.855 ] 

# submission for `mod4`
yp <- predict(mod4, test, re.form = NULL)
yp[yp < 0] <- 0

test$AccidentLikelihood <- yp
sub <- subset(test, select = c("Date", "Shift", "GridID", "AccidentLikelihood"))
sub <- rename(sub, c(Date = "date"))
write.csv(sub, file = "submission-mm-mod4.csv", row.names = F)

### local functions

mod2roc(mod4, df, 1e4)
#mod2sub(mod4, test, 

mod2roc <- function(mod, df, N)
{ 
  if(missing(N)) {
    ind <- 1:nrow(df)
  } else {
    ind <- sample(1:nrow(df), N)
  }
  
  yp <- predict(mod, test[ind, ], re.form = NULL)
  yp[yp < 0] <- 0
  yp[yp > 1] <- 1
  
  yt <- as.numeric(df[ind, "Accidents"] > 0)

  plotROC(data.frame(outcome = yt), 1, yp)
}

mod2sub <- function(mod, df, file)
{
  yp <- predict(mod, test, re.form = NULL)
  yp[yp < 0] <- 0
  yp[yp > 1] <- 1
  
  yt <- as.numeric(df$Accidents > 0)

  df$AccidentLikelihood <- yp
  sub <- subset(df, select = c("Date", "Shift", "GridID", "AccidentLikelihood"))
  sub <- rename(sub, c(Date = "date"))
  
  write.csv(sub, file = file, row.names = F)
}

### save
#save(mod1, mod2, file = "mixedmodels.RData")
