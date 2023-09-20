# ---------------     Capstone Project-2: Fitbit_Bellabeat    -----------------

# You will imagine you are working for Bellabeat, a high-tech manufacturer of health-focused products for women,
# and meet different characters and team members.
# In order to answer the key business questions, you will follow the steps
# of the data analysis process: ask, prepare, process, analyze, share, and act.

# These questions will guide your analysis:
#   1. What are some trends in smart device usage?
#   2. How could these trends apply to Bellabeat customers?
#   3. How could these trends help influence Bellabeat marketing strategy?
  
# -----------------    Load packages:-    --------------------

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidydr)

# -----------------    Import Data:-    --------------------

daily_activity <- read.csv("C:\\Users\\amita\\Downloads\\R_Basic_Google\\Capstone_FitBit\\Fitabase Data 4.12.16-5.12.16\\dailyActivity_merged.csv")
hourly_calories <- read.csv("C:\\Users\\amita\\Downloads\\R_Basic_Google\\Capstone_FitBit\\Fitabase Data 4.12.16-5.12.16\\hourlyCalories_merged.csv")
hourly_Intensities <- read.csv("C:\\Users\\amita\\Downloads\\R_Basic_Google\\Capstone_FitBit\\Fitabase Data 4.12.16-5.12.16\\hourlyIntensities_merged.csv")
day_sleep <- read.csv("C:\\Users\\amita\\Downloads\\R_Basic_Google\\Capstone_FitBit\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv")
weight <- read.csv("C:\\Users\\amita\\Downloads\\R_Basic_Google\\Capstone_FitBit\\Fitabase Data 4.12.16-5.12.16\\weightLogInfo_merged.csv")

head(daily_activity)
head(hourly_calories)
head(hourly_Intensities)
head(day_sleep)
head(weight)

    # here, we checked all tables and I spotted the problem in time stamp.
    # I will convert that column into date time format and...
    # will split it in date and time with two different columns.

# -----------------    Formatting:-    --------------------

# ====    daily_activity   ====
# ---------------   "as.POSIXct()" function   ----------------
# it will use to Convert a variety of date-time classes to POSIXlt and POSIXct

daily_activity$ActivityDate = as.POSIXct(daily_activity$ActivityDate, 
                                         format = "%m/%d/%Y", tz=Sys.timezone())
daily_activity$date <- format(daily_activity$ActivityDate, format = "%m/%d/%y")

head(daily_activity)

# ====    hourly_calories   ====
# Here we will use....   
# ---------------   "as.POSIXct()" function   ----------------
# it will use to Convert a variety of date-time classes to POSIXlt and POSIXct

hourly_calories$ActivityHour = as.POSIXct(hourly_calories$ActivityHour, 
                                          format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_calories$time <- format(hourly_calories$ActivityHour, format = "%H:%M:%S")
hourly_calories$date <- format(hourly_calories$ActivityHour, format = "%m/%d/%y")
head(hourly_calories)  
#  ====    hourly_Intensities   ====
 
hourly_Intensities$ActivityHour = as.POSIXct(hourly_Intensities$ActivityHour, 
                                             format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_Intensities$time <- format(hourly_Intensities$ActivityHour, format = "%H:%M:%S")
hourly_Intensities$date <- format(hourly_Intensities$ActivityHour, format = "%M/%d/%y")
head(hourly_Intensities)
# ====    day_sleep   ====

day_sleep$SleepDay = as.POSIXct(day_sleep$SleepDay, 
                                format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
day_sleep$date <- format(day_sleep$SleepDay, format = "%m/%d/%y")
head(day_sleep)


# -----------------    DATA EXPLORSTION:-    --------------------

# let's check unique Id from each table:-

n_distinct(daily_activity$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_Intensities$Id)
n_distinct(day_sleep$Id)
n_distinct(weight$Id)

    # --> This information tells us about number of participation ion each data sets.
    # --> There are 33 participants in the daily_activity, calories and intensities.
    # --> 24 participants in day_sleep,
    # --> and only 8 participants in the weight dataset, which is not significant to make any recommendations and conclusions.

# -----------------    STATISTIC SUMMARY:-    -------------------

# ====    daily_activity   ====

daily_activity %>% 
  select(TotalSteps,TotalDistance, SedentaryMinutes, Calories) %>% 
  summary()

# ------------- explore number of active minutes per category:-

daily_activity %>% 
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>% 
  summary()

# ====    hourly_calories   ====

hourly_calories %>% 
  select(Calories) %>% 
  summary()

# ====    day_sleep   ====

day_sleep %>% 
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()


# ====    weight   ====

weight %>% 
  select(WeightKg, BMI) %>% 
  summary()

    # ------->    CONCLUSION     <---------
    # Here, we found average sedentary time is 991 minutes or 16.5 hours ion a day.
    # in daily_activity majority of participants are Lightly active.
    # average sleep time in a day is 419.5 minutes or 7 hours.
    # as we check our average steps in a day are 7638 steps...
    # which a little bit less for having healthy benefits according to the CDC research.
  

# -----------------   MREGE DATA:-    -------------------
# before visualizations I would like to merge my data.
# I will merge daily_activity and day_sleep dataaset using common columns id & date.

merge_sleep_activity <- merge(day_sleep, daily_activity, by=c('Id', 'date'))
head(merge_sleep_activity)



# -----------------   VISUALIZATION:-    -------------------

# Total Steps VS. Calories:-

ggplot(data = daily_activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title = "Total Steps VS. Calories")

  # ------->    CONCLUSION     <---------
  # Here, I see the positive correlation between Total Steps and calories...
  # which means, more activities, the more calories burn.

# Total minutes sleep VS. Toatl time in bed:-

ggplot(data = day_sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) +
  geom_point() + labs(title = "Total Minutes a Sleep VS. Total tim in Bed")

  # ------->    CONCLUSION     <---------
  # as we check our visualization it looks like linear relationship.
  # that mean, The relationship between Total Minutes sleep and Total Time in Bed looks Linear.
  # So, if the Bellabeta users wants to improve their sleep, we should notify them to go to sleep.


# let's look hourly_Intestines data over time.

Intenstine_new <- hourly_Intensities %>% 
  group_by(time) %>% 
  drop_na() %>% 
  summarise(mean_total_Intensty = mean(TotalIntensity))
View(Intenstine_new)


ggplot(data=Intenstine_new, aes(x=time, y=mean_total_Intensty)) + 
  geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")

  # ------->    CONCLUSION     <---------
  # after Visualization of hourly Intensity, I found that...
  # people are more active between 5 AM to 10 PM.
  # as I can see there are high Intensity between 5 Pm to 7 Pm...
  # where people are preferring go to gym, walk after finishing their routine activity,
  # so, BellaBeta app can use this time to remind and motivate users to go for a run or walk or gym.


# Minute a Sleep VS. Sedentary minutes.

ggplot(data = merge_sleep_activity, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) +
  geom_point(color = 'red') + geom_smooth() +
  labs(title = "Minutes Asleep VS. Sedentary Minutes")

  # ------->    CONCLUSION     <---------
# So, after visualization of Total minutes sleep and sedentary minutes.....
# it clearly shows us negative correlation. 
# As a solution of the BellaBeta App: if users wants to improve their sleep,....
# app can recommend reducing the sedentary time.




#############################   SUMMARY   ######################################

# ------ Recommendation for the BellaBeta App Business---------------------

# # As we already know, collecting data on activity, sleep, stress,......
# and reproductive health has allowed Bellabeat to empower women with knowledge about their own health and habits. 
# Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.
# # 
# # After analyzing FitBit Fitness Tracker Data,.... 
# I found some insights that would help influence Bellabeat marketing strategy.
# 


# Target audience
# 
# Women who work full-time jobs (according to the hourly intensity data) and spend a lot of time at the computer/in a meeting/ focused on work they are doing
# (according to the sedentary time data).
# 
# These women do some light activity to stay healthy (according to the activity type analysis).
# Even though they need to improve their everyday activity to have health benefits.
# They might need some knowledge about developing healthy habits or motivation to keep going.
# 
# As there is no gender information about the participants, 
# I assumed that all genders were presented and balanced in this data set.
# The key message for the Bellabeat online campaign
# 
# The Bellabeat app is not just another fitness activity app.
# It's a guide (a friend) who empowers women to balance full personal and professional life and healthy habits and routines by educating and motivating them through daily app recommendations.


# Ideas for the Bellabeat app:-
# 
# Average total steps per day are 7638 which a little bit less for having health benefits for according to the CDC research. 
# They found that taking 8,000 steps per day was associated with a 51% lower risk for all-cause mortality (or death from all causes). 
# Taking 12,000 steps per day was associated with a 65% lower risk compared with taking 4,000 steps. 
# Bellabeat can encourage people to take at least 8 000 explaining the benefits for their health.
# 
# If users want to lose weight, it's probably a good idea to control daily calorie consumption. 
# Bellabeat can suggest some ideas for low-calorie lunch and dinner.
# 
# If users want to improve their sleep, Bellabeat should consider using app notifications to go to bed.
# 
# Most activity happens between 5 pm and 7 pm - I suppose, that people go to a gym or for a walk after finishing work.
# Bellabeat can use this time to remind and motivate users to go for a run or walk.
# 
# As an idea: if users want to improve their sleep, the Bellabeat app can recommend reducing sedentary time.






































































