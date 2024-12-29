
#Install all the necesary librarys for the project
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
library(janitor)
install.packages('ggbeeswarm')
install.packages('ggridges')
install.packages('data.table')
library(ggbeeswarm)
library(ggridges)
library(data.table)


#Export the next data sets for the analysis

heartrate_seconds<- read_csv("C:/Users/Moi/Desktop/Certificados/Google Certificate/Capstones Projects/Bellabeat Proyect/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")
View(heartrate_seconds)

hourlyCalories<- read_csv("C:/Users/Moi/Desktop/Certificados/Google Certificate/Capstones Projects/Bellabeat Proyect/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
View(hourlyCalories)

hourlySteps<- read_csv("C:/Users/Moi/Desktop/Certificados/Google Certificate/Capstones Projects/Bellabeat Proyect/Fitabase Data 3.12.16-4.11.16/hourlySteps_merged.csv")
View(hourlySteps)

sleepDay<- read_csv("C:/Users/Moi/Desktop/Certificados/Google Certificate/Capstones Projects/Bellabeat Proyect/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
View(sleepDay)


#Quick review of the datasets

str(heartrate_seconds)
summary(heartrate_seconds)
head(heartrate_seconds)

str(hourlyCalories)
summary(hourlyCalories)
head(hourlyCalories)

str(hourlySteps)
summary(hourlySteps)
head(hourlySteps)

str(sleepDay)
summary(sleepDay)
head(sleepDay)


#As a quick takeaway, we need to change the format of the dates from chr to date


#Take a look if we found some NA's

colSums(is.na(heartrate_seconds))
colSums(is.na(hourlyCalories))
colSums(is.na(hourlySteps))
colSums(is.na(sleepDay))

#No NA's

#Let's review if there are any duplicated values

summary(duplicated(heartrate_seconds))
summary(duplicated(hourlyCalories))
summary(duplicated(hourlySteps))
summary(duplicated((sleepDay)))

#We found 3 duplicated rows in the sleepDay dataset
#Lets remove them

sleepDay<-sleepDay[!duplicated(sleepDay),]


#Knowing that the Calories and Steps datasets have the same number of rows and 
#columns, we are going to merge both of them considering the Id and the 
#ActivityHour which are the same

hourly_calories_steps<-merge(hourlyCalories, hourlySteps,by=c("Id","ActivityHour"),all=TRUE)



#We are going to change the dates from chr to date format

heartrate_seconds$Time <- mdy_hms(heartrate_seconds$Time)
hourlyCalories$ActivityHour<-mdy_hms(hourlyCalories$ActivityHour)
hourlySteps$ActivityHour<-mdy_hms(hourlySteps$ActivityHour)
hourly_calories_steps$ActivityHour<-mdy_hms(hourly_calories_steps$ActivityHour)
sleepDay$SleepDay<-as.Date(sleepDay$SleepDay, format = '%m/%d/%Y')


#Now lets add weekdays to each data set for a better daily analysis

sleepDay$Day_week<-weekdays(sleepDay$SleepDay)
hourlyCalories$Day_week<-weekdays(hourlyCalories$ActivityHour)
hourlySteps$Day_week<-weekdays(hourlySteps$ActivityHour)
hourly_calories_steps$Day_week<-weekdays(hourly_calories_steps$ActivityHour)


#For a more accurate analysis with the heartrate, steps and calories dataset, 
#we are goint to divide every hour, minute and second just in case

heartrate_seconds$Hour<-hour(heartrate_seconds$Time)
heartrate_seconds$Minute<-minute(heartrate_seconds$Time)
heartrate_seconds$Seconds<-second(heartrate_seconds$Time)
hourlyCalories$Hour<-hour(hourlyCalories$ActivityHour)
hourlySteps$Hour<-hour(hourlySteps$ActivityHour)
hourly_calories_steps$Hour<-hour(hourly_calories_steps$ActivityHour)

#####HEARTRATES####
#We are going to mesure the heartrate by every hour to analyse during which period
#of time, the heart is more active

ggplot(heartrate_seconds, aes(x = Value)) +
  geom_density() +
  facet_wrap(~ Hour) + 
  labs(x = "Heart Rate", y = "Density", title = "Heart Rate Density by Hour") +
  theme_bw()


#"The following graphs depict periods of lower heart rate variability, typically 
#between 2:00 AM and 6:00 AM. Lower heart rate variability during these hours often 
#indicates deeper sleep and a more relaxed state, as the heart's rhythm remains 
#relatively consistent. By the other hand, increased heart rate variability observed 
#between 10:00 AM and 8:00 PM likely reflects heightened activity levels during the day. 
#The flatter appearance of the graphs during these hours suggests a wider range of
#heart rates, indicative of increased physiological activity and engagement in daily 
#routines."



#####SLEEPING####



#The sleepDay dataset is going to be distributed by the quality of the sleepers
#This division is going to be distributed by the number of minutes 

#0-200 minutes<-4 hours (Bad sleeper)
#0-420 minutes<-7 hours (Regular sleeper)
#0-500 minutes<-8.3 hours (Good sleeper)
#>500 minutes<- Oversleeping


sleepDay$sleep_quality <- cut(sleepDay$TotalMinutesAsleep, 
                              breaks = c(-Inf, 240, 420, 500, Inf), 
                              labels = c("Bad sleeper", "Regular sleeper", "Good sleeper", "Oversleeping"))



#We are going to ilustrate percentage of this distribution for better understanding
#of the records

sleep_quantity_count<-sleepDay %>%
  group_by(sleep_quality) %>%
  summarize(count_sleepers = sum(sleep_quality %in% 
                                   c("Bad sleeper", "Regular sleeper",'Good sleeper','Oversleeping')))

total_sleepers<-sum(sleep_quantity_count$count_sleepers)

sleep_quantity_counts_percentage <- sleep_quantity_count %>%
  mutate(percentage = (count_sleepers / total_sleepers) * 100)


ggplot(sleep_quantity_counts_percentage, aes(x = "", y = percentage, fill = sleep_quality)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + 
  labs(title = "Distribution of Sleep Quality", fill = "Sleep Quality") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage, 1), "% (", count_sleepers, ")")), 
            position = position_stack(vjust = 0.5))




#Boxplot
ggplot(sleepDay, aes(x = factor(Day_week, 
levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
y = TotalMinutesAsleep, 
color = sleep_quality)) + 
geom_boxplot()+labs(x = "Days of the Week", y = "Total Minutes Asleep")





#The next boxplot graph shows us the sleep quality by the days of the week 
#with some intresting takeaways
#The day where people decide to sleep more time is Saturday with a bigger box/rectangle
#The day with the worst amount of minutes to sleep is Thursday. This can suggest us 
#that on Thurdsays, people tend to be a little bit overload with daily responsabilities
#until they finally get to friday which is the second worst day of sleeping but also 
#one of the days where people can sleep a little bit more considering the purple plot 
#at the begining of the graph



#####CALORIES AND STEPS####


#Now we are going to divide the types of users related with the calories to determine
#if the users are in any type of activity. The more calories they are burning,
#the body is more involved in energetic situation

#0-100 calories (LowActivity)
#100-200 calories(RegularActivity)
#200-400 calories(HighlyActivity)
#>400 calories(VeryActive)

hourly_calories_steps$type_of_user<-cut(hourly_calories_steps$Calories, 
breaks = c(-Inf, 100, 200, 400, Inf), 
labels = c("LowActivity", "RegularActivity", "HighlyActive", "VeryActive"))


users_quantity<-hourly_calories_steps %>%
group_by(type_of_user) %>%
summarize(count_users = sum(type_of_user %in% 
c("LowActivity", "RegularActivity",'HighlyActive','VeryActive')))

total_users<-sum(users_quantity$count_users)


users_quantity_counts_percentage <- users_quantity %>%
  mutate(percentage = (count_users / total_users) * 100)


ggplot(users_quantity_counts_percentage, aes(x = type_of_user, y = percentage, fill = type_of_user)) +
  geom_col() + 
  labs(x = "Type of User", y = "Percentage", fill = "Type of User") +
  geom_text(aes(label = paste0(round(percentage, 1), "% (", count_users, ")")), 
  position = position_stack(vjust = 0.7))

#Correlation of users by steps and calories

#Correlation= 0.825
cor(hourly_calories_steps$Calories,hourly_calories_steps$StepTotal)

ggplot(hourly_calories_steps)+geom_smooth(mapping = aes(x=StepTotal,
y=Calories))+geom_point(mapping=aes(x=StepTotal, y=Calories, color=type_of_user))

#With a correlation of 0.825, we can say that the calories are well corralated 
#with the total amount of steps by user. The tend is completly positive and the 
#types of users are more concentrated in the HighlyActive group with some 
#distributed purple point which are from the group of the very active users.
#As a conclusion, we can say that most of the people tend to be more proactive as 
#expected



#Distribution of steps and calories by hour 
ggplot(hourly_calories_steps, aes(x = factor(Hour))) + 
  stat_summary(fun = "mean", geom = "bar", aes(y = Calories), fill='red') + 
  labs(x = "Hour", y = "Mean Calories")


ggplot(hourly_calories_steps, aes(x = factor(Hour))) + 
  stat_summary(fun = "mean", geom = "bar", aes(y = StepTotal), fill='orange') + 
  labs(x = "Hour", y = "Mean Steps")



#Distribution of calories and steps by day of the week and type of user
calories <- hourly_calories_steps %>% 
  group_by(Day_week, type_of_user)%>%
  summarize(mean_calories = mean(Calories))


steps <- hourly_calories_steps %>% 
  group_by(Day_week, type_of_user)%>%
  summarize(mean_steps = mean(StepTotal))


ggplot(calories, aes(x = factor(Day_week,levels = c("Monday", "Tuesday", 
"Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = mean_calories, 
fill = type_of_user)) + geom_col(position = "dodge") + 
labs(x = "Day of the Week", y = "Mean Calories", fill = "User Type")+ 
facet_wrap(~type_of_user)+geom_text(aes(label = round(mean_calories, 1)), 
position = position_dodge(width = 0.9), vjust = -0.5, size = 3)


ggplot(steps, aes(x = factor(Day_week,levels = c("Monday", "Tuesday", 
"Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = mean_steps, 
fill = type_of_user)) + geom_col(position = "dodge") + 
labs(x = "Day of the Week", y = "Mean Steps", fill = "User Type")+
facet_wrap(~type_of_user)+geom_text(aes(label = round(mean_steps, 1)), 
position = position_dodge(width = 0.9), vjust = -0.5, size = 3)


