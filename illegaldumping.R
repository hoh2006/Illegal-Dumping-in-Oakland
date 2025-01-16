library(tidyverse)
library(janitor)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


illegaldump <- read_csv("illegal_dumping.csv")

cleanillegaldump <- clean_names(illegaldump)

#To give an overview of my project, I first moved my data into a readable format, then I looked at the total number of requests each year and noted them.
#I then selected the columns I wanted to view and used by group by and summarize to find the average length of time for source and description across the four year time period.
#I conducted several joins so that I could see all the data next to one another in the dataframes (for all requests, for sources, and for descriptions)
#I then pivoted the source data to put it in long format to read it into ggplot and produce a line chart

#creating dates in a readable format (POSIXct) in both columns, and then finding length of time between when cases were opened and when they were closed
illegal_dump_clean_dates <- cleanillegaldump %>% 
  mutate(datetimeinit = mdy_hms(datetimeinit),
         datetimeclosed = mdy_hms(datetimeclosed),
         length = datetimeclosed - datetimeinit) 

#2023
#finding the number of cases this calendar year that had start and end dates in 2023, marked as CLOSED
requests2023 <- illegal_dump_clean_dates %>% 
  filter(datetimeinit >= as.Date("2023-01-01"), datetimeclosed <= as.Date("2023-10-09"))

#15,336 requests that had start and end dates in 2023 that were CLOSED

#selecting the columns I want to look at in 2023 
selected_columns_2023 <- requests2023 %>% 
  select(requestid, datetimeinit, source, description, status, datetimeclosed, length) 

#obtaining the mean length of time from request opened to request closed for all requests in 2023  
summarized_2023 <- selected_columns_2023 %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by source (where illegal dumping request was submitted) and obtaining the mean length of time in 2023
average_source_times2023 <- selected_columns_2023 %>% 
  group_by(source) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by description (illegal dumping reported activity) and obtaining the mean length of time based on description in 2023
description2023_average <- selected_columns_2023 %>% 
  group_by(description) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#2022
#finding the number of cases this calendar year that had start and end dates in 2022, marked as CLOSED
requests2022 <- illegal_dump_clean_dates %>% 
  filter(datetimeinit >= as.Date("2022-01-01"), datetimeclosed <= as.Date("2022-12-31"))

#23,844 requests that had start and end dates in 2022 that were CLOSED

#selecting the columns I want to look at in 2022
selected_columns_2022 <- requests2022 %>% 
  select(requestid, datetimeinit, source, description, status, datetimeclosed, length)

#obtaining the mean length of time from the request opened to request closed for all requests in 2022  
summarized_2022 <- selected_columns_2022 %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by source (where illegal dumping request was submitted) and obtaining the mean length of time in 2022
average_source_times2022 <- selected_columns_2022 %>% 
  group_by(source) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by description (illegal dumping reported activity) and obtaining the mean length of time based on description in 2022
description2022_average <- selected_columns_2022 %>% 
  group_by(description) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#2021
#finding the number of cases this calendar year that had start and end dates in 2021
requests2021 <- illegal_dump_clean_dates %>% 
  filter(datetimeinit >= as.Date("2021-01-01"), datetimeclosed <= as.Date("2021-12-31"))

#29,991 requests that had start and end dates in 2021

#selecting the columns I want to look at in 2021
selected_columns_2021 <- requests2021 %>% 
  select(requestid, datetimeinit, source, description, status, datetimeclosed, length)

#obtaining the mean length of time from the request opened to request closed for all requests in 2021  
summarized_2021 <- selected_columns_2021 %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by source (where illegal dumping request was submitted) and obtaining the mean length of time based on that source in 2021
average_source_times2021 <- selected_columns_2021 %>% 
  group_by(source) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by description (illegal dumping reported activity) and obtaining the mean length of time based on description in 2021
description2021_average <- selected_columns_2021 %>% 
  group_by(description) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#2020
#finding the number of cases this calendar year that had start and end dates in 2020
requests2020 <- illegal_dump_clean_dates %>% 
  filter(datetimeinit >= as.Date("2020-01-01"), datetimeclosed <= as.Date("2020-12-31"))

#35,023 requests that had start and end dates in 2020

#selecting the columns I want to look at in 2020
selected_columns_2020 <- requests2020 %>% 
  select(requestid, datetimeinit, source, description, status, datetimeclosed, length)

#summarizing the mean length of time from the request opened to request closed for all requests in 2020  
summarized_2020 <- selected_columns_2020 %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by source (where illegal dumping request was submitted) and obtaining the mean length of time based on that source in 2020
average_source_times2020 <- selected_columns_2020 %>% 
  group_by(source) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#grouping by description (illegal dumping reported activity) and obtaining the mean length of time based on description in 2020
description2020_average <- selected_columns_2020 %>% 
  group_by(description) %>% 
  summarize(average_time = (mean(length, na.rm = TRUE)))

#Now that I have all my average times calculated for each year, I need to join those average times to see them in one dataframe side by side

#joining average time by source over the four year period
average_source_2020_and_2021 <- 
  left_join(average_source_times2020, average_source_times2021, by = "source")

average_source_2020_2021_and_2022 <-
  left_join(average_source_2020_and_2021, average_source_times2022, by = "source")

#renaming columns to reflect average time over four year period by source
average_source_join_four_year_period <-
  left_join(average_source_2020_2021_and_2022, average_source_times2023, by = "source") %>% 
  rename(average_time_2020 = average_time.x, average_time_2021 = average_time.y, average_time_2022 = average_time.x.x, average_time_2023 = average_time.y.y)

#joining average time across all requests via summarized dataframe
total_request_time_2020_2021 <-
  full_join(summarized_2020, summarized_2021)

total_request_2020_2021_2022 <-
  full_join(total_request_time_2020_2021, summarized_2022)

total_request_join_four_year_period <-
  full_join(total_request_2020_2021_2022, summarized_2023) 

#joining requests based on description of request (illegal dumping materials, green waste, etc.) to see average time over four year period
description_request_2020_2021 <-
  inner_join(description2020_average, description2021_average, by = "description")
  
description_request_2020_2021_2022 <-
  inner_join(description_request_2020_2021, description2022_average, by = "description")

#renaming columns to reflect average time over four year period by description
description_request_four_year_period <-
  inner_join(description_request_2020_2021_2022, description2023_average, by = "description") %>% 
  rename(average_time_2020 = average_time.x, average_time_2021 = average_time.y, average_time_2022 = average_time.x.x, average_time_2023 = average_time.y.y)

#pivoting average source join from wide to long format and changing year column to a numeric value, while also taking the data in seconds and converting it to days
average_source_long <- average_source_join_four_year_period %>% 
  pivot_longer(
    starts_with("average"),
    names_to = "year",
    values_to = "time"
  ) %>% 
  mutate(year_numeric = case_when(
    year == "average_time_2020" ~ 2020,
    year == "average_time_2021" ~ 2021,
    year == "average_time_2022" ~ 2022,
    year == "average_time_2023" ~ 2023
  ),
  days = as.numeric(time)/86400 )

#setting up ggplot for sources over time
source_data_over_time_graph <- ggplot(average_source_long) +
  geom_line(aes(x = year_numeric, y = days, color = source)) +
  scale_y_continuous(
    limits = c(0, 8)) +
  theme_minimal()

source_data_over_time_graph

#I conducted descriptions over time analysis in Google Sheets to create a table summarizing results from four different types of illegal dumping requests: Vehicle Cleanout