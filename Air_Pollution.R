# Edmond Cheung
# Econometrics
# Predicting air pollution with a multivariate linear regression
# and an autoregressive model

# Note: throughout the data, Guangzhou is used as the test case for the code.
# After Guangzhou, the same steps are taken for the other four cities.

# Setting the CD
# setwd("")

# Library import
library(tidyverse)
library(lubridate)
library(TSA)
library(forecast)

# Reading in files
guangzhou <- read_csv('GuangzhouPM20100101_20151231.csv')

beijing <- read_csv('BeijingPM20100101_20151231.csv')
chengdu <- read_csv('ChengduPM20100101_20151231.csv')
shanghai <- read_csv('ShanghaiPM20100101_20151231.csv')
shenyang <- read_csv('ShenyangPM20100101_20151231.csv') 

# Removing spaces in the PM variables and removing NA values----
# There are three PM for Guangzhou
summary(guangzhou)
guangzhou_no_spaces <- guangzhou %>% rename(PM_City_Station = `PM_City Station`,
                                  PM_5th_Middle_School = `PM_5th Middle School`,
                                  PM_US_Post = `PM_US Post`)
summary(guangzhou_no_spaces)

# Removing missing values in Guangzhou, notably for regression
guangzhou_no_NA <- guangzhou_no_spaces %>% drop_na()
summary(guangzhou_no_NA)

# There are four PM for Beijing
beijing_no_spaces <- beijing %>% rename(PM_US_Post = `PM_US Post`)
summary(beijing_no_spaces)
beijing_no_NA <- beijing_no_spaces %>% drop_na()
summary(beijing_no_NA)

# There are three PM for Chengdu
chengdu_no_spaces <- chengdu %>% rename(PM_US_Post = `PM_US Post`)
summary(chengdu_no_spaces)
chengdu_no_NA <- chengdu_no_spaces %>% drop_na()
summary(chengdu_no_NA)

# There are three PM for Shanghai
shanghai_no_spaces <- shanghai %>% rename(PM_US_Post = `PM_US Post`)
summary(shanghai_no_spaces)
shanghai_no_NA <- shanghai_no_spaces %>% drop_na()
summary(shanghai_no_NA)

# There are three PM for Shenyang
shenyang_no_spaces <- shenyang %>% rename(PM_US_Post = `PM_US Post`)
summary(shenyang_no_spaces)
shenyang_no_NA <- shenyang_no_spaces %>% drop_na()
summary(shenyang_no_NA)

# Model training with ZERO assumptions about the data as baseline----
# Model selection
fullmodel<- lm(PM_City_Station ~ . - No - PM_5th_Middle_School - PM_US_Post,
               data = guangzhou_no_NA)
nullmodel<- lm(PM_City_Station ~ 1 - No - PM_5th_Middle_School - PM_US_Post,
               data = guangzhou_no_NA)
step(fullmodel, nullmodel, direction = "both")
step(fullmodel, nullmodel, direction = "forward")
step(fullmodel, nullmodel, direction = "backward")

# The suggested model
lm1 <- lm(formula = PM_City_Station ~ year + month + day + hour + season + 
            DEWP + HUMI + PRES + TEMP + cbwd + Iws + Iprec, 
          data = guangzhou_no_NA)
summary(lm1)

# The suggested model, but with factor variables as is proper
lm2 <- lm(formula = PM_City_Station ~ as.factor(year) + as.factor(month) +
            as.factor(day) + as.factor(hour) - as.factor(season) + 
            DEWP + HUMI + PRES + TEMP + cbwd + Iws + Iprec,
          data = guangzhou_no_NA
            )
summary(lm2)

# Model selection again, but factor variables as is proper 
fullmodel1<- lm(PM_City_Station ~ . - No - PM_5th_Middle_School - PM_US_Post - 
                  year - month - day - hour - as.factor(season) + 
                  as.factor(year) + as.factor(month) + as.factor(day) + 
                  as.factor(hour),
               data = guangzhou_no_NA)
nullmodel<- lm(PM_City_Station ~ 1 - No - PM_5th_Middle_School - PM_US_Post,
               data = guangzhou_no_NA)
step(fullmodel1, nullmodel, direction = "both")
step(fullmodel1, nullmodel, direction = "forward")
step(fullmodel1, nullmodel, direction = "backward")

# Good news: it's the same model as above, even when things are factored

summary(lm2)

# Checking to see whether the years 2010 to 2012 even have data----
guangzhou_no_spaces %>% group_by(year) %>%
  summarize (sum_PM_City = sum(PM_City_Station, na.rm = TRUE),
             sum_PM_5th = sum(PM_5th_Middle_School, na.rm = TRUE),
             sum_PM_US = sum(PM_US_Post, na.rm = TRUE))

guangzhou_no_NA %>% group_by(year) %>%
  summarize (sum_PM_City = sum(PM_City_Station, na.rm = TRUE),
             sum_PM_5th = sum(PM_5th_Middle_School, na.rm = TRUE),
             sum_PM_US = sum(PM_US_Post, na.rm = TRUE))

# note: guangzhou_no_NA also removes PM measurements if one of them is missing

# Step 1 to fixing this issue is calculating an avg value for it----
guangzhou_step1 <- guangzhou_no_spaces %>% 
  mutate(PM_Avg_Reading = ifelse(
    rowSums(
      is.na(
        cbind(PM_City_Station, PM_5th_Middle_School, PM_US_Post))) == 3,
    NA,
    rowMeans(
      cbind(PM_City_Station, PM_5th_Middle_School, PM_US_Post), na.rm = TRUE)
      )
  )
View(guangzhou_step1)
summary(guangzhou_step1)

beijing_step1 <- beijing_no_spaces %>% 
  mutate(PM_Avg_Reading = ifelse(
    rowSums(
      is.na(
        cbind(PM_Dongsi,	PM_Dongsihuan, PM_Nongzhanguan, PM_US_Post))) == 4,
    NA,
    rowMeans(
      cbind(PM_Dongsi,	PM_Dongsihuan, PM_Nongzhanguan, PM_US_Post),
      na.rm = TRUE)
  )
  )

chengdu_step1 <- chengdu_no_spaces %>% 
  mutate(PM_Avg_Reading = ifelse(
    rowSums(
      is.na(
        cbind(PM_Caotangsi, PM_Shahepu, PM_US_Post))) == 3,
    NA,
    rowMeans(
      cbind(PM_Caotangsi, PM_Shahepu, PM_US_Post),
      na.rm = TRUE)
  )
  )

shanghai_step1 <- shanghai_no_spaces %>% 
  mutate(PM_Avg_Reading = ifelse(
    rowSums(
      is.na(
        cbind(PM_Jingan, PM_Xuhui, PM_US_Post))) == 3,
    NA,
    rowMeans(
      cbind(PM_Jingan, PM_US_Post, PM_Xuhui),
      na.rm = TRUE)
  )
  )

shenyang_step1 <- shenyang_no_spaces %>% 
  mutate(PM_Avg_Reading = ifelse(
    rowSums(
      is.na(
        cbind(PM_Taiyuanjie, PM_US_Post, PM_Xiaoheyan))) == 3,
    NA,
    rowMeans(
      cbind(PM_Taiyuanjie, PM_US_Post, PM_Xiaoheyan),
      na.rm = TRUE)
  )
  )

# Credit to https://community.rstudio.com/t/average-across-columns-with-missing-values/79942/3
# Okay, I now have an average that != NA so long as there's at least one value
# Now that I've verified the averages work, time for step 2

# Step 2, I gotta drop those three columns----
# First with Guangzhou
guangzhou_step2 <- 
  guangzhou_step1 %>%
  select(-PM_City_Station, -PM_5th_Middle_School, -PM_US_Post)
str(guangzhou_step2)
summary(guangzhou_step2)
View(guangzhou_step2)

# Every other column only has 1 NA at most
# 19,211 hours without any PM2.5 measurement. Time to delete those rows
# After that, I should be ready to repeat the stepwise and regression above

# Repeating for the other cities
beijing_step2 <- 
  beijing_step1 %>%
  select(-PM_Dongsi, -PM_Dongsihuan, -PM_Nongzhanguan, -PM_US_Post)

chengdu_step2 <- 
  chengdu_step1 %>%
  select(-PM_Caotangsi, -PM_Shahepu, -PM_US_Post)

shanghai_step2 <- 
  shanghai_step1 %>%
  select(-PM_Jingan, -PM_Xuhui, -PM_US_Post)

shenyang_step2 <- 
  shenyang_step1 %>%
  select(-PM_Taiyuanjie, -PM_US_Post, -PM_Xiaoheyan)

# Step 3: No more NA values AND grouped by day----

# There are -9,999 values in DEWP and HUMI, so those values are removed
# There are several ideas for step3, denoted by step3, step32, and step33

# Log of Iws is taken in "step3"

guangzhou_step3 <- guangzhou_step2 %>%
  drop_na() %>%
  filter(DEWP > -100 & HUMI > -5) %>%
  group_by(year, month, day) %>%
  mutate(Date = as.Date(paste(year, "/", month, "/", day, sep = "")),
         Daily_PM = mean(PM_Avg_Reading),
         DAILY_DEWP = mean(DEWP),
         DAILY_HUMI = mean(HUMI),
         DAILY_PRES = mean(PRES),
         DAILY_TEMP = mean(TEMP),
         DAILY_cbwd = mode(cbwd),
         DAILY_Iws = mean(ifelse(Iws == 0, 0, log(Iws))), # It's an I
         DAILY_precipitation = mean(precipitation),
         DAILY_Iprec = mean(Iprec)
         )
# Log is a bad idea because of the difficulty of explaining negative log values
# - to a ley audience
summary(guangzhou_step3)

# "step32" is a square-root instead of a log, so no negative values
guangzhou_step32 <- guangzhou_step2 %>%
  drop_na() %>%
  filter(DEWP > -100 & HUMI > -5) %>%
  group_by(year, month, day) %>%
  mutate(Date = as.Date(paste(year, "/", month, "/", day, sep = "")),
         Daily_PM = mean(PM_Avg_Reading),
         DAILY_DEWP = mean(DEWP),
         DAILY_HUMI = mean(HUMI),
         DAILY_PRES = mean(PRES),
         DAILY_TEMP = mean(TEMP),
         DAILY_cbwd = mode(cbwd), # This gotta be the mode
         DAILY_Iws = mean(sqrt(Iws)), # It's an I
         DAILY_precipitation = mean(precipitation),
         DAILY_Iprec = mean(Iprec)
  ) # 
summary(guangzhou_step32)

# Adapted from StackOverflow to calculate statistical modes
# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
ActualMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# "step33" where no log or sqrt of Iws is taken
guangzhou_step33 <- guangzhou_step2 %>%
  drop_na() %>%
  filter(DEWP > -100 & HUMI > -5) %>%
  group_by(year, month, day) %>%
  mutate(Date = as.Date(paste(year, "/", month, "/", day, sep = "")),
         Daily_PM = mean(PM_Avg_Reading),
         Daily_DEWP = mean(DEWP),
         Daily_HUMI = mean(HUMI),
         Daily_PRES = mean(PRES),
         Daily_TEMP = mean(TEMP),
         Daily_cbwd = ActualMode(cbwd),
         Daily_Iws = mean(Iws), # It's an I
         Daily_precipitation = mean(precipitation),
         Daily_Iprec = mean(Iprec)
  ) 

head(guangzhou_step33)
View(guangzhou_step33)
str(guangzhou_step33)
summary(guangzhou_step33)

# Comparing step3, step32, and step33
str(guangzhou_step3)
summary(guangzhou_step3)
View(guangzhou_step3)

# Will proceed with step33.

beijing_step33 <- beijing_step2 %>%
  drop_na() %>%
  filter(DEWP > -100 & HUMI > -5) %>%
  group_by(year, month, day) %>%
  mutate(Date = as.Date(paste(year, "/", month, "/", day, sep = "")),
         Daily_PM = mean(PM_Avg_Reading),
         Daily_DEWP = mean(DEWP),
         Daily_HUMI = mean(HUMI),
         Daily_PRES = mean(PRES),
         Daily_TEMP = mean(TEMP),
         Daily_cbwd = ActualMode(cbwd),
         Daily_Iws = mean(Iws), # It's an I
         Daily_precipitation = mean(precipitation),
         Daily_Iprec = mean(Iprec)
  ) 

chengdu_step33 <- chengdu_step2 %>%
  drop_na() %>%
  filter(DEWP > -100 & HUMI > -5) %>%
  group_by(year, month, day) %>%
  mutate(Date = as.Date(paste(year, "/", month, "/", day, sep = "")),
         Daily_PM = mean(PM_Avg_Reading),
         Daily_DEWP = mean(DEWP),
         Daily_HUMI = mean(HUMI),
         Daily_PRES = mean(PRES),
         Daily_TEMP = mean(TEMP),
         Daily_cbwd = ActualMode(cbwd),
         Daily_Iws = mean(Iws), # It's an I
         Daily_precipitation = mean(precipitation),
         Daily_Iprec = mean(Iprec)
  ) 

shanghai_step33 <- shanghai_step2 %>%
  drop_na() %>%
  filter(DEWP > -100 & HUMI > -5) %>%
  group_by(year, month, day) %>%
  mutate(Date = as.Date(paste(year, "/", month, "/", day, sep = "")),
         Daily_PM = mean(PM_Avg_Reading),
         Daily_DEWP = mean(DEWP),
         Daily_HUMI = mean(HUMI),
         Daily_PRES = mean(PRES),
         Daily_TEMP = mean(TEMP),
         Daily_cbwd = ActualMode(cbwd),
         Daily_Iws = mean(Iws), # It's an I
         Daily_precipitation = mean(precipitation),
         Daily_Iprec = mean(Iprec)
  ) 

shenyang_step33 <- shenyang_step2 %>%
  drop_na() %>%
  filter(DEWP > -100 & HUMI > -5) %>%
  group_by(year, month, day) %>%
  mutate(Date = as.Date(paste(year, "/", month, "/", day, sep = "")),
         Daily_PM = mean(PM_Avg_Reading),
         Daily_DEWP = mean(DEWP),
         Daily_HUMI = mean(HUMI),
         Daily_PRES = mean(PRES),
         Daily_TEMP = mean(TEMP),
         Daily_cbwd = ActualMode(cbwd),
         Daily_Iws = mean(Iws), # It's an I
         Daily_precipitation = mean(precipitation),
         Daily_Iprec = mean(Iprec)
  ) 

# Step 4 is when I trim the list so that there's only one entry per day----
# All non-daily columns (e.g., without an aggregation) are removed
guangzhou_step4 <- guangzhou_step33 %>% select(No,
                                               Date,
                                               season,
                                               Daily_PM,
                                               Daily_DEWP,
                                               Daily_HUMI,
                                               Daily_PRES,
                                               Daily_TEMP,
                                               Daily_cbwd,
                                               Daily_Iws,
                                               Daily_precipitation,
                                               Daily_Iprec)

str(guangzhou_step4)
summary(guangzhou_step4)
head(guangzhou_step4)
View(guangzhou_step4)

beijing_step4 <- beijing_step33 %>% select(No,
                                               Date,
                                               season,
                                               Daily_PM,
                                               Daily_DEWP,
                                               Daily_HUMI,
                                               Daily_PRES,
                                               Daily_TEMP,
                                               Daily_cbwd,
                                               Daily_Iws,
                                               Daily_precipitation,
                                               Daily_Iprec)

chengdu_step4 <- chengdu_step33 %>% select(No,
                                           Date,
                                           season,
                                           Daily_PM,
                                           Daily_DEWP,
                                           Daily_HUMI,
                                           Daily_PRES,
                                           Daily_TEMP,
                                           Daily_cbwd,
                                           Daily_Iws,
                                           Daily_precipitation,
                                           Daily_Iprec)

shanghai_step4 <- shanghai_step33 %>% select(No,
                                           Date,
                                           season,
                                           Daily_PM,
                                           Daily_DEWP,
                                           Daily_HUMI,
                                           Daily_PRES,
                                           Daily_TEMP,
                                           Daily_cbwd,
                                           Daily_Iws,
                                           Daily_precipitation,
                                           Daily_Iprec)

shenyang_step4 <- shenyang_step33 %>% select(No,
                                           Date,
                                           season,
                                           Daily_PM,
                                           Daily_DEWP,
                                           Daily_HUMI,
                                           Daily_PRES,
                                           Daily_TEMP,
                                           Daily_cbwd,
                                           Daily_Iws,
                                           Daily_precipitation,
                                           Daily_Iprec)




# Step 5: Removing duplicate values----

guangzhou_step5 <- guangzhou_step4 %>% distinct(Date, .keep_all = TRUE)
str(guangzhou_step5)
summary(guangzhou_step5)
head(guangzhou_step5)
View(guangzhou_step5)

beijing_step5 <- beijing_step4 %>% distinct(Date, .keep_all = TRUE)
View(beijing_step5)

chengdu_step5 <- chengdu_step4 %>% distinct(Date, .keep_all = TRUE)
View(chengdu_step5)

shanghai_step5 <- shanghai_step4 %>% distinct(Date, .keep_all = TRUE)
View(shanghai_step5)

shenyang_step5 <- shenyang_step4 %>% distinct(Date, .keep_all = TRUE)
View(shenyang_step5)

# March 15, 2012 was when data consistently began appearing.
# Step 6 will be to remove data before then----
guangzhou_step6 <- guangzhou_step5 %>% filter(Date >= "2012-03-15")
str(guangzhou_step6)
summary(guangzhou_step6)
head(guangzhou_step6)
View(guangzhou_step6)

beijing_step6 <- beijing_step5 %>% filter(Date >= "2012-03-15")
chengdu_step6 <- chengdu_step5 %>% filter(Date >= "2012-03-15")
shanghai_step6 <- shanghai_step5 %>% filter(Date >= "2012-03-15")
shenyang_step6 <- shenyang_step5 %>% filter(Date >= "2012-03-15")

everything_step6 <- bind_rows(beijing_step6,
                              chengdu_step6,
                              guangzhou_step6,
                              shanghai_step6,
                              shenyang_step6)
# Note: the data is unlabeled

# Step 7: Labeling the data----
guangzhou_step7 <- guangzhou_step6 %>% mutate(City = "Guangzhou")

View(guangzhou_step7)

beijing_step7 <- beijing_step6 %>% mutate(City = "Beijing")
chengdu_step7 <- chengdu_step6 %>% mutate(City = "Chengdu")
shanghai_step7 <- shanghai_step6 %>% mutate(City = "Shanghai")
shenyang_step7 <- shenyang_step6 %>% mutate(City = "Shenyang")

everything_step7 <- bind_rows(beijing_step7,
                              chengdu_step7,
                              guangzhou_step7,
                              shanghai_step7,
                              shenyang_step7)


# 80-20 break for time
guangzhou_step6_train <- guangzhou_step5 %>% filter(Date >= "2012-03-15" &
                                                      Date < "2015-01-01")
guangzhou_train_TS <- guangzhou_step6_train$Daily_PM
  
guangzhou_step6_test <- guangzhou_step5 %>% filter(Date >= "2015-01-01" &
  Date <= "2015-12-31")
guangzhou_test_TS <- guangzhou_step6_test$Daily_PM

# Time series for Guangzhou----
guangzhou_TS <- guangzhou_step6$Daily_PM

beijing_TS <- beijing_step6$Daily_PM
chengdu_TS <- chengdu_step6$Daily_PM
shanghai_TS <- shanghai_step6$Daily_PM
shenyang_TS <- shenyang_step6$Daily_PM


# Better Stepwise and Regression----
# Using Step 6 data
str(guangzhou_step6)
summary(guangzhou_step6)
head(guangzhou_step6)

# Break into training and test (?)
nullmodel2 <- lm(Daily_PM ~ 1,
                 data = guangzhou_step6)

# Some issue with season, so I'm not including it in the "full" model
fullmodel2 <- lm(Daily_PM ~ as.factor(year) + as.factor(month) +
                   as.factor(day) + Daily_PM + Daily_DEWP +
                   Daily_HUMI + Daily_PRES + Daily_TEMP +
                   as.factor(Daily_cbwd) + Daily_Iws + Daily_precipitation +
                   Daily_Iprec,
                 data = guangzhou_step6)


nullmodel3 <- lm(Daily_PM ~ 1,
                 data = everything_step6)
fullmodel3 <- lm(Daily_PM ~ as.factor(year) + as.factor(month) +
                   as.factor(day) + Daily_DEWP +
                   Daily_HUMI + Daily_PRES + Daily_TEMP +
                   as.factor(Daily_cbwd) + Daily_Iws + Daily_precipitation +
                   Daily_Iprec,
                 data = everything_step6)

step(fullmodel3, nullmodel3, direction = "both")
# Drop as.factor(day)
# Drop Daily Dew Point
# Drop Daily Iprec

lm3 <- lm(Daily_PM ~ as.factor(year) + as.factor(month) +
                   Daily_HUMI + Daily_PRES + Daily_TEMP +
                   as.factor(Daily_cbwd) + Daily_Iws + Daily_precipitation,
                 data = everything_step6)
summary(lm3)


nullmodel4 <- lm(Daily_PM ~ 1,
                 data = everything_step7)
fullmodel4 <- lm(Daily_PM ~ as.factor(year) + as.factor(month) +
                   as.factor(day) + Daily_DEWP + as.factor(City) +
                   Daily_HUMI + Daily_PRES + Daily_TEMP +
                   as.factor(Daily_cbwd) + Daily_Iws + Daily_precipitation +
                   Daily_Iprec,
                 data = everything_step7)

step(fullmodel4, nullmodel4, direction = "both")
# Drop as.factor(day), then daily humidity

lm4 <- lm(Daily_PM ~ as.factor(year) + as.factor(month) + 
            Daily_DEWP + as.factor(City) + Daily_PRES + Daily_TEMP +
            as.factor(Daily_cbwd) + Daily_Iws + Daily_precipitation +
            Daily_Iprec,
          data = everything_step7)
summary(lm4)

# Recommends dropping day category and precipitation: for model 2?

# AR Models----
guangzhou_ar1 <- arima(x = guangzhou_TS, order = c(1, 0, 0))
summary(guangzhou_ar1)

guangzhou_ar2 <- arima(x = guangzhou_TS, order = c(2, 0, 0))
summary(guangzhou_ar2)

guangzhou_ar3 <- arima(x = guangzhou_TS, order = c(3, 0, 0))
summary(guangzhou_ar3)

guangzhou_ar7 <- arima(x = guangzhou_TS, order = c(7, 0, 0))
summary(guangzhou_ar7)

# This one with differences
guangzhou_ar1_diff <- arima(x = diff(guangzhou_TS), order = c(1, 0, 0))
summary(guangzhou_ar1_diff)

guangzhou_ar2_diff <- arima(x = diff(guangzhou_TS), order = c(2, 0, 0))
summary(guangzhou_ar2_diff)

guangzhou_ar3_diff <- arima(x = diff(guangzhou_TS), order = c(3, 0, 0))
summary(guangzhou_ar3_diff)

guangzhou_ar7_diff <- arima(x = diff(guangzhou_TS), order = c(7, 0, 0))
summary(guangzhou_ar7_diff)

# For Beijing
beijing_ar1 <- arima(x = beijing_TS, order = c(1, 0, 0))
summary(beijing_ar1)

beijing_ar2 <- arima(x = beijing_TS, order = c(2, 0, 0))
summary(beijing_ar2)

beijing_ar3 <- arima(x = beijing_TS, order = c(3, 0, 0))
summary(beijing_ar3)

beijing_ar7 <- arima(x = beijing_TS, order = c(7, 0, 0))
summary(beijing_ar7)

# This one with differences
beijing_ar1_diff <- arima(x = diff(beijing_TS), order = c(1, 0, 0))
summary(beijing_ar1_diff)

beijing_ar2_diff <- arima(x = diff(beijing_TS), order = c(2, 0, 0))
summary(beijing_ar2_diff)

beijing_ar3_diff <- arima(x = diff(beijing_TS), order = c(3, 0, 0))
summary(beijing_ar3_diff)

beijing_ar7_diff <- arima(x = diff(beijing_TS), order = c(7, 0, 0))
summary(beijing_ar7_diff)

# For Chengdu
chengdu_ar1 <- arima(x = chengdu_TS, order = c(1, 0, 0))
summary(chengdu_ar1)

chengdu_ar2 <- arima(x = chengdu_TS, order = c(2, 0, 0))
summary(chengdu_ar2)

chengdu_ar3 <- arima(x = chengdu_TS, order = c(3, 0, 0))
summary(chengdu_ar3)

chengdu_ar7 <- arima(x = chengdu_TS, order = c(7, 0, 0))
summary(chengdu_ar7)

# This one with differences
chengdu_ar1_diff <- arima(x = diff(chengdu_TS), order = c(1, 0, 0))
summary(chengdu_ar1_diff)

chengdu_ar2_diff <- arima(x = diff(chengdu_TS), order = c(2, 0, 0))
summary(chengdu_ar2_diff)

chengdu_ar3_diff <- arima(x = diff(chengdu_TS), order = c(3, 0, 0))
summary(chengdu_ar3_diff)

chengdu_ar7_diff <- arima(x = diff(chengdu_TS), order = c(7, 0, 0))
summary(chengdu_ar7_diff)

# For Shanghai
shanghai_ar1 <- arima(x = shanghai_TS, order = c(1, 0, 0))
summary(shanghai_ar1)

shanghai_ar2 <- arima(x = shanghai_TS, order = c(2, 0, 0))
summary(shanghai_ar2)

shanghai_ar3 <- arima(x = shanghai_TS, order = c(3, 0, 0))
summary(shanghai_ar3)

shanghai_ar7 <- arima(x = shanghai_TS, order = c(7, 0, 0))
summary(shanghai_ar7)

# This one with differences
shanghai_ar1_diff <- arima(x = diff(shanghai_TS), order = c(1, 0, 0))
summary(shanghai_ar1_diff)

shanghai_ar2_diff <- arima(x = diff(shanghai_TS), order = c(2, 0, 0))
summary(shanghai_ar2_diff)

shanghai_ar3_diff <- arima(x = diff(shanghai_TS), order = c(3, 0, 0))
summary(shanghai_ar3_diff)

shanghai_ar7_diff <- arima(x = diff(shanghai_TS), order = c(7, 0, 0))
summary(shanghai_ar7_diff)

# For Shenyang
shenyang_ar1 <- arima(x = shenyang_TS, order = c(1, 0, 0))
summary(shenyang_ar1)

shenyang_ar2 <- arima(x = shenyang_TS, order = c(2, 0, 0))
summary(shenyang_ar2)

shenyang_ar3 <- arima(x = shenyang_TS, order = c(3, 0, 0))
summary(shenyang_ar3)

shenyang_ar7 <- arima(x = shenyang_TS, order = c(7, 0, 0))
summary(shenyang_ar7)

# This one with differences
shenyang_ar1_diff <- arima(x = diff(shenyang_TS), order = c(1, 0, 0))
summary(shenyang_ar1_diff)

shenyang_ar2_diff <- arima(x = diff(shenyang_TS), order = c(2, 0, 0))
summary(shenyang_ar2_diff)

shenyang_ar3_diff <- arima(x = diff(shenyang_TS), order = c(3, 0, 0))
summary(shenyang_ar3_diff)

shenyang_ar7_diff <- arima(x = diff(shenyang_TS), order = c(7, 0, 0))
summary(shenyang_ar7_diff)

# Plotting----
# Note: plot() will occasionally return the unit circle plot rather than TS plot
# plot(guangzhou_ar)
# plot(guangzhou_ar, n.ahead = 365)

# plot(guangzhou_ar_ts)
# plot(guangzhou_ar_ts, n.ahead = 365)

# Time series plots
plot(guangzhou_TS, type = 'o')
plot(diff(guangzhou_TS), type = 'o')

# Unused prediction code----
# forecast(guangzhou_test_TS, model = guangzhou_ar_train)
## predict(guangzhou_ar_train, guangzhou_test_TS)

# plot(forecast(guangzhou_test_TS, model = guangzhou_ar_train))
