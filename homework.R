# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below: Priscilla H. Whang

# clear console, environment
cat('\014')
rm(list = ls())

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

library(here)

# create function
read_weather <- function(station) {
  file_name <- paste0(station, '.csv')
  file_path <- file.path('us-weather-history', file_name)
  file <- read_csv(file_path)
  
  file <- file %>% 
    mutate(station = station,
           date = as.Date(date))
  
  return(file)
}

# check
read_weather('KCLT')
glimpse(read_weather('KCLT'))

# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, 
#> and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

?bind_rows

# iterate read_weather through stations
ds <- map(stations, read_weather)
ds # list of 10

# bind resulting rows
ds <- bind_rows(ds)
ds # 3650 obs. 14 vars.

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

?factor
?fct_count

# make 'city' factor
ds <- ds %>% 
  mutate(city = factor(station, 
                       levels = stations, 
                       labels = cities))

# now 15 vars.

# check
fct_count(ds$city)

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

# view data
ds %>% head(20)
names(ds)

# function to convert from F to C
convert_to_c <- function(f) {
  c <- (f - 32) * 5/9
  c.rounded <- round(c, 1)
  
  return(c.rounded)
}

# convert temps in ds
ds <- ds %>% 
  mutate(across(ends_with('temp'), ~ convert_to_c(.)))

### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

compiled_data <- read_csv('data-clean/compiled_data.csv')

view(compiled_data)
view(ds) # looks same :-)

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

# function for counting days of extreme temp
count_extreme_days <- function(data) {
  data %>% 
    group_by(city) %>% 
    summarize(extreme_days = sum(actual_min_temp == record_min_temp |
                                 actual_max_temp == record_max_temp),
              .groups = 'drop') %>% 
    arrange(desc(extreme_days))
}

# (not sure where/how i can use the '.' pip function?)

# check (without saving)
count_extreme_days(ds) # e.g., 20 extreme days in Seattle

# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

?lubridate

library(lubridate)

# make 'month' col
ds <- ds %>% 
  mutate(month = factor(month(date, label = TRUE, abbr = TRUE),
                        levels = month.abb))

# check
glimpse(ds$month)
table(ds$month)

?split 

# split into lists
months <- split(ds, ds$month)
months

# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

?cor

names(ds)

for (month in names(months)) {
  month_data <- months[[month]]
    
  # precipitation
  print(paste(toupper(month),
              '-- Correlation between actual and average precipitation is:', 
              round(cor(month_data$actual_precipitation, 
                        month_data$average_precipitation), 2)))
  
  # min
  print(paste(toupper(month),
              '-- Correlation between actual and average minimum temperature is:',
              round(cor(month_data$actual_min_temp, 
                        month_data$average_min_temp), 2)))
  
  # max
  print(paste(toupper(month),
              '-- Correlation between actual and average maximum temperature is:',
              round(cor(month_data$actual_max_temp, 
                        month_data$average_max_temp), 2)))
}

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

library(DataExplorer)

glimpse(ds)
?plot_boxplot
?plot_correlation

# boxplots
plot_boxplot(ds, by = 'city')
plot_boxplot(ds, by = 'month')

# corrs
plot_correlation(ds, type = 'continuous')

# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

?facet_wrap

# generate plot
ggplot(ds, aes(x = date,
               y = actual_mean_temp,
               color = month)) +
  geom_point() +
  facet_wrap(facets = vars(city),
             ncol = 3)

# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month

# still doing actual mean temp here?

?ggtitle

plot_by_month <- function(data, month) {
  plot <- ggplot(data, aes(y = actual_mean_temp,
                           x = date,
                           color = city)) +
    geom_point() +
    geom_line() +
    labs(title = month)

  ggsave(paste0('eda/', month, '.png'))
}

# call function
for (month in names(months)) {
  month_data <- months[[month]]
  
  plot_by_month(month_data, month)
}

# end of code! that was fun.