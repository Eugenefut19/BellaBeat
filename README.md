---
title: "Bellabeat"
author: "Eugene Ohba"
date: "2023-07-22"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
```

#Reads in the CSV filews

```{r}
daily_activity= read.csv("dailyActivity_merged.csv")
sleep= read.csv("minuteSleep_merged.csv")
sleep_day= read.csv("sleepDay_merged.csv")
weight= read.csv("weightLogInfo_merged.csv")
heart_rate= read.csv("heartrate_seconds_merged.csv")
```

#Remove NAs

```{r}
daily_activity <- daily_activity %>%
  drop_na()

sleep_day <-sleep_day %>%
  drop_na()

sleep <- sleep %>%
  drop_na()

heart_rate= heart_rate %>% 
  drop_na()
```

#Distinct ID's

```{r}
#Want to show distinctness in the ID's
unique(daily_activity$Id)
unique(sleep_day$Id)# Has only 17 unique ID's
unique(sleep$Id) #Has only 17 unique ID's
unique(heart_rate$Id)
unique(weight$Id)
```

**We will remove weight as it only has 8 unique Id's which is not enough of a sample size for our project**

#Summary Tables

##sleep summary and coverts to data frame

```{r}
sl=sleep_day %>% 
  select(TotalMinutesAsleep, TotalTimeInBed)
st=data.frame(unclass(summary(sl)), 
           check.names = F)

boxplot(sl$TotalMinutesAsleep, ylab="Total Minutes")
boxplot(sl$TotalTimeInBed, ylab="Totoal Minutes")
#write.csv(st, file="sleep_summary.csv")
summary(sl)
```

##Daily Activity Summary and coverts to data frame

```{r}
p=daily_activity %>% 
  select(TotalSteps, TotalDistance, SedentaryMinutes, Calories)

df_a=data.frame(unclass(summary(p)),
           check.names = F)

boxplot(p)
summary(p)

#write.csv(df_a, file="Daily_Activity_Summary.csv")
```

```{r}
boxplot(p$TotalSteps, ylab="Total Amount of Steps")
boxplot(p$TotalDistance, ylab="Total Distance")
boxplot(p$SedentaryMinutes, ylab="Sedentary Minutes")
boxplot(p$Calories, ylab="Calories")
```













#Graphs Calories vs TotalSteps

```{r}
daily_activity %>% 
  ggplot(aes(x= TotalSteps, y= Calories))+
  geom_point()+
  geom_smooth()+
  ggtitle("Calories Burned vs TotalSteps")+
  xlab("Total Steps")+
  ylab("Calories Burned")
  
```
![image](https://github.com/Eugenefut19/BellaBeat/assets/134546229/f2b19d53-28e1-4b1d-8680-5b36a5615863)


**Shows that more step, more calories burned**

#Calculates means of daily activity table

```{r}
daily_activity_mean=daily_activity %>% 
  group_by(Id) %>%  
  summarise(mean_steps=mean(TotalSteps),
            mean_distance= mean(TotalDistance),
            mean_very_active_distance= mean(VeryActiveDistance),
            mean_fairly_active_distance= mean(FairlyActiveMinutes),
            mean_lighlty_active_distance= mean(LightActiveDistance),
            mean_calories= mean(Calories),
            mean_sedentary_min= mean(SedentaryMinutes))
daily_activity_mean
```

#Combine heartrate with daily

```{r}
h=heart_rate %>% 
  group_by(Id) %>% 
  summarise(mean_heart_value= mean(Value))

heart_daily=inner_join(daily_activity_mean, h, by="Id")
heart_daily
```

#mean_heart vs mean_seden

```{r}
heart_daily %>% 
  ggplot(aes(y= mean_heart_value, x= mean_sedentary_min))+
  geom_point()+
  geom_smooth()+
  ggtitle("Mean Heart Rate vs Mean Time of Sedentary Minutes")+
  xlab("Mean Time of Sendatary in Min")+
  ylab("Mean Heart Rate")
```
![image](https://github.com/Eugenefut19/BellaBeat/assets/134546229/80e58c18-d541-4095-af3b-7b5875e3bcce)

#Joins Daily activity with weight

```{r}
joined_weight=right_join(daily_activity, weight, by= "Id")


joined_weight %>% 
  ggplot(aes(x=TotalSteps, y=BMI))+
  geom_point()+
  geom_smooth()
```
![image](https://github.com/Eugenefut19/BellaBeat/assets/134546229/ce0017d3-5f1f-409b-a119-72d205c0007c)

**Shows that people who had more steps had less fat.** \*\*Comparing kg and pound to total steps is not ideal as muscle weighs more than fat.\*

```{r}

joined_weight %>% 
  ggplot(aes(x=SedentaryMinutes, y=VeryActiveMinutes))+
  geom_point()
```
![image](https://github.com/Eugenefut19/BellaBeat/assets/134546229/7309a3a7-a624-43b8-9d5b-d669071a70e3)

#Changes Date Format

```{r}
daily_activity=daily_activity %>% 
  mutate(ActivityDate=mdy(ActivityDate),
         month= month(ActivityDate),
         year= year(ActivityDate)) 
```

##Summary 2016 4, 5

```{r}
t=daily_activity %>% 
  group_by(Id, year, month) %>% 
  summarise(mean_total_steps=mean(TotalSteps),
            mean_total_distance=mean(TotalDistance),
            mean_calories=mean(Calories))
z=t %>% 
  group_by(Id) %>% 
  summarise(mean_step_difference= mean_total_steps[month==5]- mean_total_steps[month==4],
            mean_distance_difference= mean_total_distance[month==5]- mean_total_distance[month==4],
            mean_calorie_difference= mean_calories[month==5]- mean_calories[month==4])


z$Id <- as.numeric(factor(z$Id))
z
```
![image](https://github.com/Eugenefut19/BellaBeat/assets/134546229/29ba6cbd-a864-4d7e-a09b-384d961be588)

```{r}
z %>% 
  mutate(color_var = ifelse(mean_step_difference < 0, "Negative", "Positive")) %>%
  ggplot(aes(x = Id, y = mean_step_difference, fill=color_var)) +
  geom_col() +
  ggtitle("Difference of Total steps from April 2016 to May 2016") +
  xlab("Number of Unique Individuals Counted") +
  ylab("Mean Step Difference")

mean(z$mean_step_difference)
```
![image](https://github.com/Eugenefut19/BellaBeat/assets/134546229/3021553b-2f68-4c2b-b853-8df88a60ba72)

```{r}
z %>% 
  mutate(color_var = ifelse(mean_distance_difference < 0, "Negative", "Positive")) %>%
  ggplot(aes(x = Id, y = mean_distance_difference, fill=color_var)) +
  geom_col() +
  ggtitle("Difference of Total Distance in Miles from April 2016 to May 2016") +
  xlab("Number of Unique Individuals Counted") +
  ylab("Mean Distance Difference in Miles")
mean(z$mean_distance_difference)
```
![image](https://github.com/Eugenefut19/BellaBeat/assets/134546229/408ec1af-12bb-4768-a7f7-39a855975a9e)

```{r}
z %>% 
  mutate(color_var = ifelse(mean_calorie_difference < 0, "Negative", "Positive")) %>%
  ggplot(aes(x = Id, y = mean_calorie_difference, fill=color_var)) +
  geom_col() +
  ggtitle("Difference of Total Calories from April 2016 to May 2016") +
  xlab("Number of Unique Individuals Counted") +
  ylab("Mean Calorie Difference")
mean(z$mean_calorie_difference)
```
