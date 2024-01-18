# Processing FluServ Influenza Hospitalization data
# January 2024

# URL: https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html

# downloaded data from 2019 - 2023 flu seasons
# downloaded data from all FluSurv networks and for CT, NY (close to MA)
# deleted the top two rows in excel

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(tidyverse)

##########################################################
### import and clean CDC FluSurv data ###
##########################################################


df = read.csv("C:\\Users\\emily\\OneDrive\\Desktop\\ERDC\\EPH\\Military Healthcare\\Paper 2\\Data\\FluSurv\\CDC_FluSurv_All_CT_NY_2019_2023.csv")
# 16352 obs of 12 variables

# delete rows: AGE.ADJUSTED.CUMULATIVE.RATE, AGE.ADJUSTED.WEEKLY.RATE
# no data for these columns
df = select(df, -AGE.ADJUSTED.CUMULATIVE.RATE, -AGE.ADJUSTED.WEEKLY.RATE)
# now 10 variables

# the raw data includes age, sex, race distributions
# here, we are primarily interested in age (pediatric versus adult)
# so, keep rows where SEX.CATEGORY == "overall" and RACE.CATEGORY == "Overal"
# noting that it might be interesting to include this sex/race data for future iterations of this work
df2 = filter(df, SEX.CATEGORY == "Overall") # now, 15072 obs
df2 = filter(df2, RACE.CATEGORY == "Overall") # now, 11932 obs


# for AGE.CATEGORY, keep only == "< 18" | ">= 18" | "Overall"
table(df2$AGE.CATEGORY)
str(df2$AGE.CATEGORY) # chr
df2 = filter(df2, AGE.CATEGORY == "Overall" | AGE.CATEGORY =="< 18" | AGE.CATEGORY == ">= 18") # now, 1884 obs


# pivot from long to wide for AGE.CATEGORY for CUMULATIVE.RATE and WEEKLY.RATE values
df3 = df2 %>%
  pivot_wider(
    names_from = AGE.CATEGORY,
    values_from = c(CUMULATIVE.RATE, WEEKLY.RATE)
  )

# rename variables to avoid issues with spaces in column name
# include "Pediatric" and "Adult" rather than < 18, >= 18
# delete columns with old variable names
df3$WEEKLY.RATE.PEDIATRIC = df3$`WEEKLY.RATE_< 18`
df3$WEEKLY.RATE.ADULT = df3$`WEEKLY.RATE_>= 18`
df3$WEEKLY.RATE.OVERALL = df3$WEEKLY.RATE_Overall
df3$CUMULATIVE.RATE.PEDIATRIC = df3$`CUMULATIVE.RATE_< 18`
df3$CUMULATIVE.RATE.ADULT = df3$`CUMULATIVE.RATE_>= 18`
df3$CUMULATIVE.RATE.OVERALL = df3$CUMULATIVE.RATE_Overall
df3 = select(df3, -`WEEKLY.RATE_< 18`, -`WEEKLY.RATE_>= 18`, -WEEKLY.RATE_Overall, -`CUMULATIVE.RATE_< 18`, -`CUMULATIVE.RATE_>= 18`, -CUMULATIVE.RATE_Overall)
# now, 13 vars


# set columns 8:13 to numeric
str(df3$WEEKLY.RATE.PEDIATRIC)
print(df3[,8:13])
df3[, 8:13] <- lapply(df3[, 8:13], as.numeric)


# save the dataframe as cdc
cdc = df3

rm(df)
rm(df2)
rm(df3)


#### set years, weeks to calendar date
cdc$date = ISOdate(cdc$MMWR.YEAR, 1, 1) + weeks(cdc$MMWR.WEEK -1) + days(1)
cdc$date = as.Date(cdc$date)
print(cdc$date)


# Here, include only at full flusurv network data
# rather than NY, CT FluSurv data
cdc2 = filter(cdc, CATCHMENT == "Entire Network") # now, 157 obs
cdc2$date
diff(cdc2$date)
# differences of 7 days (sometimes 8) or 161 days for annual gap in data during non reporting period



# Fill in missing dates in CDC data with zero
# CDC FluSurveillance only include flu season; no reports from week 18 - 39 for each year
# need to create rows for weeks 18 - 39 and fill missing data with 0

# Beth -- I am stuck here. Do you mind taking a look at creating new rows
# for missing weeks and filling with zero?
# I found examples using padr and tidyverse libraries below but can't get through errors

#Creating a blank data frame with weeks 1-52 filling data with 0
template <- data.frame(MMWR.WEEK = rep(1:52, times = 6),   ###repeating sequence 1-52 6 times
                       MMWR.YEAR = rep(2019:2024, each =52)) ###repeating each year 52 times before going to the next year
template$WEEKLY.RATE.PEDIATRIC <-  0.0  
template$WEEKLY.RATE.ADULT <-  0.0
template$WEEKLY.RATE.OVERALL <-  0.0
template$CUMULATIVE.RATE.PEDIATRIC <-  0.0
template$CUMULATIVE.RATE.ADULT <-  0.0
template$CUMULATIVE.RATE.OVERALL <-  0.0

# Filter the template data frame to include only weeks 18-39
template <- template%>%
  filter(MMWR.WEEK<40 , MMWR.WEEK >17)

# Merge the new template data frame with cdc2 
result <- cdc2%>%
  full_join(template)

##Order by weeks and year 
result <- result[order(result$MMWR.YEAR, result$MMWR.WEEK), ]

##Fill in the rest of the missing variables
result$date = ISOdate(result$MMWR.YEAR, 1, 1) + weeks(result$MMWR.WEEK -1) + days(1)
result$date = as.Date(result$date)

##Checking the new difference after adding in all of the missing dates
diff(result$date) ##cells 158 - 169 are repeated? og data (cdc2) has week 18-23. 
### we could just remove the created data for these rows, just interesting why there is data here but for no other year
duplicated_result <- result[!duplicated(result$date), ]

result_filled <- duplicated_result %>%
  fill(CATCHMENT, .direction = "downup") ## this will fill the categorical variables using the values and filling up and down 

result_filled <- result_filled %>%
  fill(NETWORK, .direction = "downup")

result_filled <- result_filled %>%
  fill(SEX.CATEGORY, .direction = "downup")

result_filled <- result_filled %>%
  fill(RACE.CATEGORY, .direction = "downup")

###checking the weeks again
diff(result_filled$date) ##5 weeks are either 2 or 8 days apart

# same result_filled as cdc2
cdc2 = result_filled

rm(result)
rm(result_filled)
rm(duplicated_result)
rm(template)

# export the cleaned CDC FluSurveillance csv file
#write.csv(cdc, .....)


##statistical distributions for CDC data
# pediatric rate over time
p = ggplot(cdc2, aes(x=date, y = WEEKLY.RATE.PEDIATRIC)) + 
  geom_bar(stat = "identity") + 
  theme_apa()
p

p = ggplot(cdc2, aes(x=date, y = WEEKLY.RATE.ADULT)) + 
  geom_bar(stat = "identity") + 
  theme_apa()
p

##seems to be missing data for 2020 into 2021...
# grouped boxplot
ggplot(cdc2, aes(x=as.factor(MMWR.YEAR), y=WEEKLY.RATE.PEDIATRIC)) + 
  geom_boxplot() +
  theme_apa()
ggplot(cdc2, aes(x=as.factor(MMWR.YEAR), y=WEEKLY.RATE.ADULT)) + 
  geom_boxplot() +
  theme_apa()

####################################################
### import HHS Protect data ###
####################################################

# import HHS Protect data for MA
hhs = read.csv("C:\\Users\\emily\\OneDrive\\Desktop\\ERDC\\EPH\\Military Healthcare\\Paper 2\\Data\\HHS_COVID_Influenza\\COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility_20231228.csv")
# 953,973 obs of 128 variables

# filter for MA only
hhs = filter(hhs, state == "MA") # now, 13836 obs
# earliest collection week is now 03/22/2020 (COVID)

# number of distinct facilities per hospital_subtype
hhs %>%
  group_by(hospital_subtype) %>%
  summarise(n_distinct(hospital_name))


# select only necessary variables for now
# facility identifiers and 
hhs2 = hhs %>% select(1:11, previous_day_admission_influenza_confirmed_7_day_sum, 
                      previous_day_admission_pediatric_covid_confirmed_7_day_sum, 
                      previous_day_admission_adult_covid_confirmed_7_day_sum)

# set values of -99999 to median of 2 (values of -99999 range from 1 to 3 cases)
hhs2 = hhs2 %>% 
  mutate(previous_day_admission_influenza_confirmed_7_day_sum = ifelse(previous_day_admission_influenza_confirmed_7_day_sum == '-999999',2,previous_day_admission_influenza_confirmed_7_day_sum), 
         previous_day_admission_pediatric_covid_confirmed_7_day_sum= ifelse(previous_day_admission_pediatric_covid_confirmed_7_day_sum == '-999999',2,previous_day_admission_pediatric_covid_confirmed_7_day_sum),
         previous_day_admission_adult_covid_confirmed_7_day_sum= ifelse(previous_day_admission_adult_covid_confirmed_7_day_sum == '-999999',2,previous_day_admission_adult_covid_confirmed_7_day_sum))

# set collection_week to date
str(hhs2$collection_week)
hhs2$date = as.Date(as.character(hhs2$collection_week), tryFormats = "%Y/%m/%d")
str(hhs2$date)

# create year column
hhs2$year <- as.numeric(format(hhs2$date,'%Y'))

# arrange by date
hhs2 = hhs2 %>% arrange(date)

# how many missing values are there?
summary(hhs2$previous_day_admission_influenza_confirmed_7_day_sum) # 4121 NAs
summary(hhs2$previous_day_admission_pediatric_covid_confirmed_7_day_sum) # 1027 NAs
summary(hhs2$previous_day_admission_adult_covid_confirmed_7_day_sum) # 949 NAs

# fill in missing facility data
# step 1: fill in NAs by median values of same hospital_subtype
table(hhs$hospital_subtype)
hhs2 = hhs2 %>% 
  group_by(hospital_subtype, date) %>% 
  mutate(year = year) %>%
  mutate_at(vars(previous_day_admission_influenza_confirmed_7_day_sum,
                 previous_day_admission_pediatric_covid_confirmed_7_day_sum,
                 previous_day_admission_adult_covid_confirmed_7_day_sum), 
            ~replace_na(., 
                        median(., na.rm = TRUE)))

# how many missing values are there after filling in weekly median by hospital_subtype?
summary(hhs2$previous_day_admission_influenza_confirmed_7_day_sum) # 3157 NAs
summary(hhs2$previous_day_admission_pediatric_covid_confirmed_7_day_sum) # 933 NAs
summary(hhs2$previous_day_admission_adult_covid_confirmed_7_day_sum) # 875 NAs

# step 2: forward fill remaining missing values by hospital facility
hhs2 = hhs2 %>%
  dplyr::group_by(hospital_name) %>%
  fill(previous_day_admission_influenza_confirmed_7_day_sum, .direction = "downup") %>%
  fill(previous_day_admission_pediatric_covid_confirmed_7_day_sum, .direction = "downup") %>% 
  fill(previous_day_admission_adult_covid_confirmed_7_day_sum, .direction = "downup") %>%
  mutate(year = year) %>%
  dplyr::ungroup()


# how many missing values are there after filling in weekly mean by hospital_subtype?
summary(hhs2$previous_day_admission_influenza_confirmed_7_day_sum) # 0 NAs
summary(hhs2$previous_day_admission_pediatric_covid_confirmed_7_day_sum) # 0 NAs
summary(hhs2$previous_day_admission_adult_covid_confirmed_7_day_sum) # 0 NAs


# group by week to estimate state-level influenza, covid admissions
hhs3 = hhs2 %>% 
  group_by(date) %>%
  summarise(previous_day_admission_influenza_confirmed_7_day_sum_MA = sum(previous_day_admission_influenza_confirmed_7_day_sum), 
            previous_day_admission_pediatric_covid_confirmed_7_day_sum_MA = sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum),
            previous_day_admission_adult_covid_confirmed_7_day_sum_MA = sum(previous_day_admission_adult_covid_confirmed_7_day_sum))
# 195 obs of 4 variables

# how many days between
daysbetween = diff(hhs3$date) # 7 for all
print(daysbetween)

# create year column
hhs3$year <- as.numeric(format(hhs3$date,'%Y'))
str(hhs3$year)


# group by week and hospital_subtype to estimate state-level influenza, covid admissions
hhs4 = hhs2 %>% 
  group_by(date, hospital_subtype) %>% 
  summarise(previous_day_admission_influenza_confirmed_7_day_sum_MA = sum(previous_day_admission_influenza_confirmed_7_day_sum), 
            previous_day_admission_pediatric_covid_confirmed_7_day_sum_MA = sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum),
            previous_day_admission_adult_covid_confirmed_7_day_sum_MA = sum(previous_day_admission_adult_covid_confirmed_7_day_sum))
# 777 obs of 5 variables

# create year column
hhs4$year <- as.numeric(format(hhs4$date,'%Y'))
str(hhs4$year)



# grouped bar graph by hospital subtype 
# Stacked
library(ggplot2)
library(jtools)

# influenza hosp over time
p = ggplot(hhs4, aes(fill=hospital_subtype, y=previous_day_admission_influenza_confirmed_7_day_sum_MA, x=date)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_apa()
p
# grouped boxplot
p= ggplot(hhs4, aes(x=date, y=previous_day_admission_influenza_confirmed_7_day_sum_MA, fill=as.factor(year))) + 
  geom_boxplot() +
  facet_wrap(~hospital_subtype, scale="free") +
  theme_apa()
p

# pediatric covid hosp over time
p = ggplot(hhs4, aes(fill=hospital_subtype, y=previous_day_admission_pediatric_covid_confirmed_7_day_sum_MA , x=date)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_apa()
p
# grouped boxplot
p= ggplot(hhs4, aes(x=date, y=previous_day_admission_pediatric_covid_confirmed_7_day_sum_MA, fill=as.factor(year))) + 
  geom_boxplot() +
  facet_wrap(~hospital_subtype, scale="free") +
  theme_apa()
p


#########################################################################################
# join cdc flusurv data to hhs3 (grouped by week) for entire FluSurv catchment area
#########################################################################################

# Join CDC and HHS by nearest date
# https://stackoverflow.com/questions/23342647/how-to-match-by-nearest-date-from-two-data-frames 
library(data.table)

str(hhs3)
hhs5 = setDT(hhs3) ## convert to data.table by reference
str(hhs5)
cdc3 = setDT(cdc2)

hhs5[, date1 := date]  ## create a duplicate of 'date1'
cdc3[, date1 := date]  ## create a duplicate of 'date1'

setkey(hhs5, date1)    ## set the column to perform the join on
setkey(cdc3, date1)    ## same as above

# join CDC FluSurv to HHS facility data by closest dates rather than exact dates
hhs_cdc = cdc3[hhs5, roll=Inf] ## perform rolling join

summary(hhs_cdc$WEEKLY.RATE.PEDIATRIC) # 30 NAs
summary(hhs_cdc$previous_day_admission_influenza_confirmed_7_day_sum_MA) # 0 NAs



# double check days between weeks
testing = diff(hhs_cdc$date)


#########################################################################################
### estimates for pediatric flu hosp based on FluSurv ###
########################################################################################

### Approach December 28, 2023

# version 1
# estimated hosp > 18 = total HHS confirmed hosp * (adult case rate/100000)
# estimated hosp < 18 = total HHS confirmed hosp * (ped case rate/100000)
# scaling factor = (sum of estimated hosp / total HHS hospitalizations)


hhs_cdc$adult_hosp_flu_cases = (hhs_cdc$previous_day_admission_influenza_confirmed_7_day_sum_MA * (hhs_cdc$WEEKLY.RATE.ADULT/100000)) + 0.00001 # added 0.00001 to avoid issue of zeros for now
summary(hhs_cdc$adult_hosp_flu_cases)
hhs_cdc$pediatric_hosp_flu_cases = hhs_cdc$previous_day_admission_influenza_confirmed_7_day_sum_MA * (hhs_cdc$WEEKLY.RATE.PEDIATRIC/100000) + 0.00001 # added 0.00001 to avoid issue of zeros for now

# scale: distribution of adult and pediatric flu hospitalizatons per week in MA
hhs_cdc$adult_hosp_flu_scale = (hhs_cdc$adult_hosp_flu_cases) / (hhs_cdc$adult_hosp_flu_cases + hhs_cdc$pediatric_hosp_flu_cases)
hhs_cdc$pediatric_hosp_flu_scale = (hhs_cdc$pediatric_hosp_flu_cases) / (hhs_cdc$adult_hosp_flu_cases + hhs_cdc$pediatric_hosp_flu_cases)

# derived estimates of adult and pediatric flu hospitalizations per week in MA
# note: pediatric and adult estimates should sum to previous_day_admission_influenza_confirmed_7_day_sum_MA
hhs_cdc$adult_hosp_flu_est = hhs_cdc$adult_hosp_flu_scale * hhs_cdc$previous_day_admission_influenza_confirmed_7_day_sum_MA
hhs_cdc$pediatric_hosp_flu_est = hhs_cdc$pediatric_hosp_flu_scale * hhs_cdc$previous_day_admission_influenza_confirmed_7_day_sum_MA

summary(hhs_cdc$pediatric_hosp_flu_est) # 30 NAs
hist(hhs_cdc$pediatric_hosp_flu_est) # max = 300
hist(hhs_cdc$previous_day_admission_influenza_confirmed_7_day_sum_MA) # max = 800


plot(hhs_cdc$date, hhs_cdc$pediatric_hosp_flu_cases)
plot(hhs_cdc$date, hhs_cdc$pediatric_hosp_flu_scale)
plot(hhs_cdc$date, hhs_cdc$pediatric_hosp_flu_est)

##01/02/24 somethings seems to be up with 2021


# export outcome CSV files


# Version 2 from beth (similar to dinesh)
# only using CDC FluSurv, not HHS data
# prefer version 1 because version 2 assumes that FluSurv rates from other states apply to MA
# cannot be as reliably scaled to state, facility levels. 
# Differing vaccination rates between states, for instance, could led to skewed estimates using only FluSurv.
# can always come back to this approach...
# (pediatric pop < 18 in MA) * case rate, where... 
# case rate = number of pos cases < 18 / (population/100,000)
# (adult pop > 18 in MA) * case rate, where... 
# case rate = number of pos cases > 18 / (population/100,000)
# sum of adult and pediatric = total hospitalization cases in MA
# then, scale estimates such that total pos confirmed MA / total estimated MA




##################################################################
### Exogenous variables ###
##################################################################

# import outcome csv file or build from df's

#########################################################################################
### estimates for pediatric flu hosp based on FluSurv ###
### Approach from week of December 18. 2023 ###
### OLD but gets same results as version from December 28,2023
########################################################################################



# Calculate the total population needed for the given hospitalization rates: 
# Find the population that would contribute X hospitalizations based on the combined rates:
# Total population = 765 hospitalizations / (14.5 peds + 12.6 adult hospitalizations per 100,000 population)
# = (765 / 27.1) = 28.28 hospitalizations per 100,000 population 
# ??? 28,232 people

#hhs_cdc$WEEKLY.RATE.PEDIATRIC.ADULT = (hhs_cdc$WEEKLY.RATE.PEDIATRIC + hhs_cdc$WEEKLY.RATE.ADULT)
#summary(hhs_cdc$WEEKLY.RATE.PEDIATRIC.ADULT) # 52 NAs prior to filling in CDC weeks 18 - 39


#hhs_cdc$total_population_est = (hhs_cdc$previous_day_admission_influenza_confirmed_7_day_sum_MA / hhs_cdc$WEEKLY.RATE.PEDIATRIC.ADULT)
#summary(hhs_cdc$total_population_est) # 78 NAs

# Calculate the number of pediatric and adult hospitalizations based on the population and rates: 
# Using the calculated population, we can find 
# the expected number of hospitalizations for each age group:
# Pediatric hospitalizations: 28,232 people * 14.5 hospitalizations per 100,000 population ??? 408
# Adult hospitalizations: 28,232 people * 12.6 hospitalizations per 100,000 population ??? 355

#hhs_cdc$pediatric_hosp_flu_est = (hhs_cdc$total_population_est) *  (hhs_cdc$WEEKLY.RATE.PEDIATRIC)
#summary(hhs_cdc$pediatric_hosp_flu_est)

#hhs_cdc$adult_hosp_flu_est = (hhs_cdc$total_population_est) *  (hhs_cdc$WEEKLY.RATE.ADULT)
#summary(hhs_cdc$adult_hosp_flu_est)

#hhs_cdc$total_hosp_flu_est = hhs_cdc$pediatric_hosp_flu_est + hhs_cdc$adult_hosp_flu_est
#summary(hhs_cdc$total_hosp_flu_est)



