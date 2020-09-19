### The goal of this project is to explore a DateTime series data set involving energy use and sub-meters. 

### Loading packages
library(RMySQL)
library(lubridate)
library(tidyverse)
library(openxlsx)
library(knitr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(scales)
library(imputeTS)


### Using SQL to pull data and creating a database connection
con = dbConnect(MySQL(), user = 'deepAnalytics',
                password='Sqltask1234!', dbname='dataanalytics2018',
                host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

### List the tables contained in the database
dbListTables(con)

### List attributes contained in 'yr_2006' table
dbListFields(con, 'yr_2006')

### Pulling specific attributes needed from each data frame for analysis
yr_2006 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006')
yr_2007 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007')
yr_2008 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008')
yr_2009 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009')
yr_2010 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010')

### Investigating yr_2006, contains 2 weeks of data
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)

### Investigating yr_2007, contains 1 full year
str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

### Investigating yr_2008, contains 1 full year
str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

### Investigating yr_2009, contains 1 full year
str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

### Investigating yr_2010, contains 11 months
str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

### Combining tables into a primary data frame, only including df's that contain a whole year
subMeters <- bind_rows(yr_2007, yr_2008, yr_2009)

### Investigating subMeters, confirming dates are in the correct order
str(subMeters)
summary(subMeters)
head(subMeters)
tail(subMeters)

## Preprocessing
### Combining Date and Time attributes into a new attribute column
subMeters <- cbind(subMeters, paste(subMeters$Date, subMeters$Time), stringsAsFactors = FALSE)
str(subMeters)
colnames(subMeters)[6] <- 'DateTime'

### Moving DateTime closer to front of data frame ###Correct
subMeters <- subMeters %>% relocate(DateTime, .before = Sub_metering_1)
head(subMeters)
tail(subMeters)

### Now we have to convert DateTime from character to POSIXct, which stores date/time values as number of seconds since January 1, 1970.
subMeters$DateTime <- as.POSIXct(subMeters$DateTime, '%Y/%m/%d %H:%M:%S')

### Adding time zone, data description indicates data set is from France
attr(subMeters$DateTime, 'tzone') <- 'Europe/Paris'

### Checking structure, we have our correct DateTime accounting for timezone. 
str(subMeters)

### Deleting old Date and Time columns, as we will create new Date and Time columns with correct time zone from 'DateTime'
subMeters$Date <- NULL
subMeters$Time <- NULL

### Creating new Date column with correct time zone
subMeters$Date <- date(subMeters$DateTime)
subMeters$Time <- format(subMeters$DateTime, '%H:%M:%S')

### Moving Date and Time to more strategic location ###Correct
subMeters <- subMeters %>% relocate(Date, .before = Sub_metering_1)
subMeters <- subMeters %>% relocate(Time, .before = Sub_metering_1)
head(subMeters)
tail(subMeters)

### Checking structure again
str(subMeters)

### Changing name of certain columns
subMeters <- subMeters %>% rename(sub1 = Sub_metering_1)
subMeters <- subMeters %>% rename(sub2 = Sub_metering_2)
subMeters <- subMeters %>% rename(sub3 = Sub_metering_3)

### Using lubridate to create new attributes from 'DateTime' for analysis
subMeters$year <- year(subMeters$DateTime)
subMeters$quarter <- quarter(subMeters$DateTime)
subMeters$month <- month(subMeters$DateTime)
subMeters$week <- isoweek(subMeters$DateTime)
subMeters$wday <- wday(subMeters$DateTime)
subMeters$day <- day(subMeters$DateTime)
subMeters$hour <- hour(subMeters$DateTime)
subMeters$minute <- minute(subMeters$DateTime)

### Moving Date and Time to more strategic location ###Correct
subMeters <- subMeters %>% relocate(sub1, .after = minute)
subMeters <- subMeters %>% relocate(sub2, .after = sub1)
subMeters <- subMeters %>% relocate(sub3, .after = sub2)

### Checking for missing data
### Group by date, obtain the count, and turn into data frame
missing_datetime <- subMeters %>% count(Date)
incomplete_data <- data.frame(table(missing_datetime$n))

### Now filtering for all days that do not have 1440 hours
### Reveals 53 days with missing minutes in time series
missing_time <- missing_datetime %>% filter(n !=1440)

### Filtering a few dates so we can ensure time zones is correct
Apr29_30 <- subMeters %>% filter(Date >= as.Date('2007-04-28') & Date <= as.Date('2007-05-01'))


### Initial EDA
### Viewing summary statistics
summary(subMeters)
sd(subMeters$sub1)
sd(subMeters$sub2)
sd(subMeters$sub3)
var(subMeters$sub1)
var(subMeters$sub2)
var(subMeters$sub3)
sum(subMeters$sub1)
sum(subMeters$sub2)
sum(subMeters$sub3)

# Changing wday and month from numbers to labels
subMeters$wday <- wday(subMeters$DateTime, label=TRUE)
subMeters$month <- month(subMeters$DateTime, label=FALSE)
subMeters$day <- wday(subMeters$DateTime, label=TRUE) 

## Observations:
### Submeter 1 (Kitchen: dishwasher, oven, microwave):
#+ Uses the least energy, with mean of 1.16 watts per hour
#+ Largest energy range, from 0 to 82 watts per hour
#+ SD=6.29
#+ Variance=39.54
#+ Sum in 3 years=1,819,989
### Submeter 2 (Laundry: washing machine, drier, fridge, light):
#+ Uses 1.34 watts per hour on average
#+ Energy range is 0 to 78
#+ SD=5.97
#+ Variance=35.67
#+ Sum in 3 years=2,208,410
### Submeter 3 (AC, Electric water heater):
#+ Uses the most energy with mean of 6.26 watts used per hour
#+ Smallest range, from 0 to 31
#+ Largest Standard deviation at 8.34. ACs and water heaters can be on and off for prolonged periods.
#+ Variance=69.6
#+ Sum in 3 years=9,758,843
#+ Often at 0, but will run at 15-18 for about 54 minutes at a time


### If we could add more information to the data set, what kinds of attributes would you add?
### --I would break the power usage by sub-meters down into yearly, monthly, weekly, weekday, daily, and hourly usage.
### What would be important to understanding the power usage in this home?
### --Looking at it on various granular levels will enable us to investigate trends and detect regular and atypical patterns
### in many different ways. For example, we can look at how power usage varies by month for each year. Or, we 
### could note how power usage varies from season to season in each year, or daily and weekly usage patterns.

# exporting final df to csv for next task
write.csv(subMeters, file = 'subMeters.csv', row.names = FALSE)

# Submeter usage
YearlyAvg <- subMeters %>% 
  group_by(year) %>% 
  summarise(across(starts_with('sub'), mean))

###### YearlySum Regular and Gathered
YearlySum <- subMeters %>%
  group_by(DateTime, Date, year) %>% 
  summarise(across(starts_with('sub'), sum))

YearlySumGather <- gather(YearlySum, 'sub1', 'sub2', 'sub3',
                          key = 'submeter', value = 'amount')

###### QtrlySum Regular and Gathered
QtrlySum <- subMeters %>%
  group_by(year, quarter) %>% 
  summarise(across(starts_with('sub'), sum))

QtrlySumGather <- gather(QtrlySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')

###### MonthlySum Regular and Gathered
MonthlySum <- subMeters %>%
  group_by(year, month) %>% 
  summarise(across(starts_with('sub'), sum))

MonthSumGather <- gather(MonthlySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')

###### WdaySum Regular and Gathered
WdaySum <- subMeters %>%
  group_by(year, wday) %>% 
  summarise(across(starts_with('sub'), sum))

WdaySumGather <- gather(WdaySum, 'sub1', 'sub2', 'sub3',
                        key = 'submeter', value = 'amount')

WeekSum <- subMeters %>% 
  group_by(year, month, week) %>% 
  summarise(across(starts_with('sub'), sum))

WeekSumGather <- gather(WeekSum, 'sub1', 'sub2', 'sub3',
                        key = 'submeter', value = 'amount')


###### DailySum Regular and Gathered
DailySum <- subMeters %>%
  group_by(Date, year, quarter, day) %>% 
  summarise(across(starts_with('sub'), sum))

DailySumGather <- gather(DailySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')


###### HourlySum Regular and Gathered
HourlySum <- subMeters %>% 
  group_by(Date, month, week, day, hour) %>% 
  summarise(across(starts_with('sub'), sum))

HourlySumGather <- gather(HourlySum, 'sub1', 'sub2', 'sub3',
                          key = 'submeter', value = 'amount')

###### AllSum Regular and Gathered
AllSum <- subMeters %>% 
  group_by(DateTime, year, month, week, day, wday, hour) %>% 
  summarise(across(starts_with('sub'), sum))

AllSumGathered <- gather(subMeters, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')


###### Export AllSum DFs to csv
# Exporting correlation to excel
write.csv(AllSumGathered, file = "AllSumGathered.csv", row.names = FALSE)
write.csv(subMeters, file = 'subMeters.csv', row.names = FALSE)

# Plot all of submeter 1

houseWeek <- data.frame(filter(subMeters, year==2008 & week==2))

plot(houseWeek$sub1)


## Plotly
# Subset 9th day of January 2008 - All observations
houseDay <- data.frame(filter(subMeters, year==2008 & month=='Jan' & day==9))

houseDay <- subMeters %>% filter(year==2008 & month==1 & day==9)

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$sub1, type = 'scatter', mode = 'lines')

DailySumGather %>% 
  filter(month=='Jun') %>% 
  ggplot(aes(day, sub3)) +
  geom_area(aes(fill=factor(year)), size=1) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('June') +
  ylab('Submeter 3 usage/day') +
  labs(fill='Year') +
  ggtitle('Comparison of Submeter 3 Daily Use in June by Year')

DailySum %>% 
  filter(month=='Jul') %>% 
  ggplot(aes(day, sub3)) +
  geom_area(aes(fill=factor(year)), size=1) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('July') +
  ylab('Submeter 3 usage/day') +
  labs(fill='Year') +
  ggtitle('Comparison of Submeter 3 Daily Use in July by Year')

DailySum %>% 
  filter(month=='Aug') %>% 
  ggplot(aes(day, sub3)) +
  geom_area(aes(fill=factor(year)), size=1) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('August') +
  ylab('Submeter 3 usage/day') +
  labs(fill='Year') +
  ggtitle('Comparison of Submeter 3 Daily Use in August by Year')

DailySum %>% 
  filter(month=='Sep') %>% 
  ggplot(aes(day, sub3)) +
  geom_area(aes(fill=factor(year)), size=1) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('September') +
  ylab('Submeter 3 usage/day') +
  labs(fill='Year') +
  ggtitle('Comparison of Submeter 3 Daily Use in September by Year')

QtrlySum %>%
  filter(year!=2010) %>% 
  ggplot(aes(quarter, sub1)) +
  geom_area(aes(color=factor(year)), size=1) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('Quarter') +
  ylab('Submeter 1 usage/quarter') +
  labs(color='Year') +
  ggtitle('Comparison of Submeter 1 Quarterly Use by Year')

QtrlySum %>%
  filter(year!=2010) %>% 
  ggplot(aes(quarter, sub2)) +
  geom_line(aes(color=factor(year)), size=1) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('Quarter') +
  ylab('Submeter 2 usage/quarter') +
  labs(color='Year') +
  ggtitle('Comparison of Submeter 3 Quarterly Use by Year')

QtrlySum %>%
  filter(year!=2010) %>% 
  ggplot(aes(quarter, sub3)) +
  geom_line(aes(color=factor(year)), size=1) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('Quarter') +
  ylab('Submeter 3 usage/quarter') +
  labs(color='Year') +
  ggtitle('Comparison of Submeter 3 Quarterly Use by Year')

QtrlySum %>%
  filter(year!=2010) %>% 
  ggplot(aes(quarter, sub1)) +
  geom_bar(stat = 'identity', aes(fill=factor(year)), width = 0.5) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('Year') +
  ylab('Submeter 1 usage/quarter') +
  labs(fill='Year') +
  ggtitle('Comparison of Submeter 1 Quarterly Use by Year')

QtrlySum %>%
  filter(year!=2010) %>% 
  ggplot(aes(quarter, sub2)) +
  geom_bar(stat = 'identity', aes(fill=factor(year)), width = 0.5) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('Quarter') +
  ylab('Submeter 2 usage/quarter') +
  labs(fill='Year') +
  ggtitle('Comparison of Submeter 2 Quarterly Use by Year')

QtrlySum %>%
  filter(year!=2010) %>% 
  ggplot(aes(quarter, sub3)) +
  geom_bar(stat = 'identity', aes(fill=factor(year)), width = 0.5) +
  facet_grid(year~.) +
  theme_bw() +
  xlab('Year') +
  ylab('Submeter 3 usage/quarter') +
  labs(fill='Quarter') +
  ggtitle('Comparison of Submeter 3 Quarterly Use by Year')

###################################################################################################

HourlySum %>%
  filter(month=='Aug' & week==32) %>% 
  ggplot(aes(hour, sub1)) +
  geom_bar(stat = 'identity', aes(fill=factor(day)), width = 0.5) +
  facet_grid(day~.) +
  theme_bw() +
  xlab('Hourly Usage') +
  ylab('Submeter 1 use/hour') +
  labs(fill='Day') +
  ggtitle('Submeter 1 Hourly Use by Day: August Week 2')

HourlySum %>%
  filter(month=='Aug' & week==32) %>% 
  ggplot(aes(hour, sub2)) +
  geom_bar(stat = 'identity', aes(fill=factor(day)), width = 0.5) +
  facet_grid(day~.) +
  theme_bw() +
  xlab('Hourly Usage') +
  ylab('Submeter 2 use/hour') +
  labs(fill='Day') +
  ggtitle('Submeter 2 Hourly Use by Day: August Week 2')

HourlySum %>%
  filter(month=='Aug' & week==32) %>% 
  ggplot(aes(hour, sub3)) +
  geom_bar(stat = 'identity', aes(fill=factor(day)), width = 0.5) +
  facet_grid(day~.) +
  theme_bw() +
  xlab('Hourly Usage') +
  ylab('Submeter 3 use/hour') +
  labs(fill='Day') +
  ggtitle('Submeter 3 Hourly Use by Day: August Week 2')

subMeters %>%
  ggplot(aes(minute, sub3)) +
  geom_point() +
  theme_bw() +
  xlab('Hourly Usage') +
  ylab('Submeter 3 use/hour') +
  ggtitle('Submeter 3 Use: August Week 2')

houseWeek31 <- filter(subMeters, year==2008 & week==31)
houseWeek32 <- filter(subMeters, year==2008 & week==32)
houseWeek33 <- filter(subMeters, year==2008 & week==33)


# Sub 1, Week 31
ggplot(sample_frac(houseWeek31, 1)) +
  geom_area(aes(DateTime, sub1))

# Sub 1, Week 32
ggplot(sample_frac(houseWeek32, 1)) +
  geom_area(aes(DateTime, sub1))

# Sub 1, Week 33
ggplot(sample_frac(houseWeek33, 1)) +
  geom_area(aes(DateTime, sub1))


# Sub 2, Week 31
ggplot(sample_frac(houseWeek31, .25)) +
  geom_area(aes(DateTime, sub2))

# Sub 2, Week 32
ggplot(sample_frac(houseWeek32, .25)) +
  geom_area(aes(DateTime, sub2))

# Sub 2, Week 33
ggplot(sample_frac(houseWeek33, 0.25)) +
  geom_area(aes(DateTime, sub2))


# Sub 1, Week 33
ggplot(sample_frac(houseWeek31, 0.25)) +
  geom_area(aes(DateTime, sub3))

# Sub 3, Week 33
ggplot(sample_frac(houseWeek32, 0.25)) +
  geom_area(aes(DateTime, sub3))

# Week 33, Sub 3
ggplot(sample_frac(houseWeek33, 0.25)) +
  geom_area(aes(DateTime, sub3))







