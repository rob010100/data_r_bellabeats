#install packages
install.packages("tidyverse")
library(tidyverse)


#STEP 1 Save datasets as data frames

#weight log
weight_log <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv")

#daily activities
daily_activity <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")

#heart rate in seconds
heart_rate_seconds <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")

#number of calories burned per hour
hourly_calories <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")

#workout intensity by the hour
hourly_intensity <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/hourlyIntensities_merged.csv")

#number of steps per hour
hourly_steps <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/hourlySteps_merged.csv")

#number of calories burned per minute
minute_calories <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv")

#workout intensity by the minute
minute_intensity <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv")

#workout METs by the minute-this dataset is unclear what it actually measures-need to follow up with customer or disregard data
minute_mets <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/minuteMETsNarrow_merged.csv")

#sleep by the minute
minute_sleep <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")

#steps by the minute
minute_steps <- read.csv("C:/Users/rober/OneDrive/Documents/Rob School/Google Certs/Data Analytics/Capstone Project/Data/Fitabase Data 3.12.16-4.11.16/minuteStepsNarrow_merged.csv")

#STEP 2 Verify Integrity of Data

#STEP 2A Check for blank cells within each data set

sum(is.na(daily_activity))

sum(is.na(weight_log))

sum(is.na(heart_rate_seconds))

sum(is.na(hourly_calories))

sum(is.na(hourly_intensity))

sum(is.na(hourly_steps))

sum(is.na(minute_calories))

sum(is.na(minute_intensity))

sum(is.na(minute_sleep))

sum(is.na(minute_steps))

sum(is.na(minute_mets))


#Address weight_log, it returns 31 blank cells in that dataset "[1] 31", all others are "[1] 0"

View(weight_log)

#results show only 2 of 33 rows contain data within column labeled "Fat" so delete "Fat" column 

weight_log_v2 <- weight_log %>%
  select(-Fat)

View(weight_log_v2)
#Looks great!

#STEP 2B Check for any duplicate rows within each dataset

duplicates <- duplicated(daily_activity)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(weight_log)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(heart_rate_seconds)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(hourly_calories)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(hourly_intensity)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(hourly_steps)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(minute_calories)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(minute_intensity)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(minute_sleep)
number_duplicates <- sum(duplicates)
print(number_duplicates)

duplicates <- duplicated(minute_steps)
number_duplicates <- sum(duplicates)
print(number_duplicates)

#All datasets return zero duplicate rows except minute_sleep dataset which returns "[1] 525"

#BTW My kiddo wants to tell you, "A group of baby bunnies is known as a 'fluffle'!"
#This is left here on purpose. I'm Rob, a human, not AI. 

View(minute_sleep)
#The data measured in minutes in the value column needs to be converted to hours to be more useful

#First remove the duplicated rows in minute_sleep dataset

minute_sleep_v2 <- minute_sleep[!duplicated(minute_sleep), ]

#check for duplicates within the version 2 minute_sleep data

duplicates <- duplicated(minute_sleep_v2)
number_duplicates <- sum(duplicates)
print(number_duplicates)

#minute_sleep_v2 = no duplicates "[1] 0"

#Next convert each sleep session to hours instead of minutes
#Datetime could be easier to work with, mutate that first

minute_sleep_v3 <- minute_sleep_v2 %>%
  mutate(date = mdy_hms(date),
  date_only = as.Date(date))

minute_sleep_v3 <- minute_sleep_v3 %>%
  mutate(date_only = as.Date(date))

#Now minute_sleep_v3 has only dates, no times, on the date column
#Aggregate minutes together by date

minute_sleep_v3 <- minute_sleep_v3 %>%
  group_by(Id, date_only) %>%
  summarize(total_sleep_minutes = sum(value, na.rm = TRUE), .groups = 'drop')

#Convert minutes to hours

minute_sleep_v3 <- minute_sleep_v3 %>%
  mutate(total_sleep_hours = total_sleep_minutes / 60)

#Sleep minutes have been aggregated, converted to hours, and grouped by date

View(minute_sleep_v3)

#Looks great! But...
#Some outliers = too little sleep per day, as in < than 2 hrs per day
#Record was incomplete, incorrect, or flawed

#Filter out entries of <2 hr per day sleep

minute_sleep_v4 <- minute_sleep_v3 %>%
  filter(total_sleep_hours >= 2)

View(minute_sleep_v4)
#Looks great! 

#STEP 2C Add obtainable information to weight_log dataset

#you have everything you need to calculate and add their height
#Add HeightInches column, calculate it with BMI formula from NIH.gov website
#BMI Formula is: weight (lb) / [height (in)]^2 x 703 = BMI

weight_log_v2$HeightInches <- sqrt((weight_log_v2$WeightPounds * 703) / weight_log_v2$BMI)

#Updated version number
weight_log_v3 <- weight_log_v2

View(weight_log_v3)
#Looks Great! But...
#The date looks inconsistent from minute_sleep_v4 and daily_activity's date columns

#STEP 2D Check each dataset for datetimestamp and consistency
#No timestamps unless they're necessary

#Date record change, alter the Column name in minute_sleep_v4
minute_sleep_v4 <- minute_sleep_v4 %>%
  rename(Date = date_only)

#update version number
minute_sleep_v5 <- minute_sleep_v4

#Date record change, alter the date format in daily_activity
daily_activity <- daily_activity %>%
  mutate(ActivityDate = mdy(ActivityDate))

#update version number
daily_activity_v2 <- daily_activity

#Date record change, alter the date format and data value in weight_log_v3
weight_log_v3 <- weight_log_v3 %>%
  mutate(Date = as.Date(mdy_hms(Date)))

#update version number
weight_log_v4 <- weight_log_v3

#All other datasets needs timestamps with the date to remain relevant
#FYI all those other datasets have datetimestamps in chr data value
#They're in "MM/DD/YYYY 12:00:00 AM" format

#STEP 2E Trim the data from each dataset and update version numbers

daily_activity_v3 <- daily_activity_v2 %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

heart_rate_seconds_v2 <- heart_rate_seconds %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

hourly_calories_v2 <- hourly_calories %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

hourly_intensity_v2 <- hourly_intensity %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

hourly_steps_v2 <- hourly_steps %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

#minute_calories <- minute_calories %>%
# mutate(across(everything(), ~ trimws(as.character(.))))
#Weird. Calories all revert to the same number, need to follow up on this

#For now, store v1 in v2 to keep version control consistent for future work
#Keep in mind for now this dataset is not trimmed

minute_calories_v2 <- minute_calories

#minute_intensity_v2 <- minute_intensity %>%
#  mutate(across(everything(), ~ trimws(as.character(.))))
#Same deal as Weird thing above with minute_calories dataset
#made all values in intensity column turn to zero

#For now, store v1 in v2 to keep version control consistent for future work
#Keep in mind for now this dataset is not trimmed

minute_intensity_v2 <- minute_intensity

minute_sleep_v6 <- minute_sleep_v5 %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

minute_steps_v2 <- minute_steps %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

weight_log_v5 <- weight_log_v4 %>%
  mutate(across(everything(), ~ trimws(as.character(.))))

#STEP 2F format columns and data consistently across datasets

#Format columns 
#Column name formatting ItShouldBeInThisFormat

minute_sleep_v7 <- minute_sleep_v6 %>%
  rename(
    TotalSleepMinutes = total_sleep_minutes,
    TotalSleepHours = total_sleep_hours
  )

#Change Column TotalIntensity to HourlyIntensity on hourly_intensity dataset

hourly_intensity_v3 <- hourly_intensity_v2 %>%
  rename(
    HourlyIntensity = TotalIntensity
  )

#Format data types
#Changing Id column data type to match other dataset's Id column

minute_calories_v3 <- minute_calories_v2 %>%
  mutate(Id = as.character(Id))

minute_calories_v4 <- minute_calories_v3 %>%
  mutate(Calories = as.character(Calories))

minute_intensity_v3 <- minute_intensity_v2 %>%
  mutate(Id = as.character(Id))

minute_intensity_v4 <- minute_intensity_v3 %>%
  mutate(Intensity = as.character(Intensity))

#STEP 3 Store cleaned data in new data frames and merge any related tables

#STEP 3A clean data frames

daily_activity_clean <- daily_activity_v3

weight_log_clean <- weight_log_v5

heart_rate_seconds_clean <- heart_rate_seconds_v2

hourly_calories_clean <- hourly_calories_v2

hourly_intensity_clean <-hourly_intensity_v3

hourly_steps_clean <- hourly_steps_v2

minute_calories_clean <- minute_calories_v4

minute_intensity_clean <- minute_intensity_v4

minute_steps_clean <- minute_steps_v2

total_sleep_clean <- minute_sleep_v7
#this one gets a slight name change to reflect the min/hour conversion and aggregate values

#STEP 3B Merge dataframes together for plotting
#Merge daily_activity and weight_log, format BMI, height,time, etc for plotting

merged_activity_weight <- daily_activity_clean %>%
  inner_join(weight_log_clean, by = "Id")

merged_activity_weight <- merged_activity_weight%>%
  mutate(SedentaryMinutes = as.numeric(SedentaryMinutes))

merged_activity_weight <- merged_activity_weight %>%
  mutate(SedentaryHours = SedentaryMinutes / 60)

merged_activity_weight <- merged_activity_weight %>%
  mutate(BMI = as.numeric(BMI)) %>%
  mutate(BMI = round(BMI, 1))

merged_activity_weight <- merged_activity_weight %>%
  mutate(HeightInches = as.numeric(HeightInches)) %>%
  mutate(HeightInches = round(HeightInches, 1))

#Analyze Merged Data
#Scatterplot

ggplot(merged_activity_weight, aes(x = SedentaryHours, y = BMI)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Correlation between Sedentary Hours and BMI",
       x = "Sedentary Hours",
       y = "BMI") +
  theme_minimal()


#Scatterplot with smooth curve noting trends
ggplot(merged_activity_weight, aes(x = SedentaryHours, y = BMI)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Correlation between Sedentary Hours and BMI",
       x = "Sedentary Hours",
       y = "BMI") +
  theme_minimal()

#Aside from sedentary time vs BMI, lets look at when users are active
#Time of day needs to be in proper date format

# Convert the date column to datetime format
minute_sleep_v3 <- minute_sleep_v3 %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"))

# Extract hour from the date and categorize into time of day
minute_sleep_v3 <- minute_sleep_v3 %>%
  mutate(Hour = hour(date),
         TimeOfDay = case_when(
           Hour >= 5 & Hour < 12 ~ "Morning",
           Hour >= 12 & Hour < 17 ~ "Afternoon",
           Hour >= 17 & Hour < 21 ~ "Evening",
           TRUE ~ "Night"
         ))

























