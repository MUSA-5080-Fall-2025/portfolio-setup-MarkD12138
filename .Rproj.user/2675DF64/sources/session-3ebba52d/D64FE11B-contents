#load library
library(tidyverse)
car_data <- read_csv("data/car_sales_data.csv") # reads in as tibble. vs read.csv which reads in as a data frame
class(car_data)
car_data

car_df <- as.data.frame(car_data)   # Regular data frame - prints all rows
car_df

head(car_data,10)
names(car_data)
glimpse(car_data)
# Rule 1: Data frame first & Rule 2: Column names without quotes
select1 <- select(car_data, Manufacturer, Price)
filter(car_data, Price > 20000)
mutate(car_data, price_k = Price / 1000)

# Rule 3: Always returns a new data frame
new_data <- select(car_data, Manufacturer, Price)
# car_data is unchanged, new_data contains selected columns

glimpse(car_data)

names(car_data)

#rename a column
car_data<- rename(car_data, year = `Year of manufacture`)
#why did I need those funky quotes above?
names(car_data)

#remove a column
names(new_data)
new_data <- select(new_data, -Price)

#create a new column
car_data<- mutate(car_data, age = 2025-year)

#create another new column, fancier this time
car_data <- mutate(car_data, 
                   oldcars = case_when(age>20~"old", TRUE ~ "not old"))

# filter (select rows)
old_cars <- filter(car_data, oldcars == 'old')
young_cars <- filter(car_data, oldcars != 'old')

#logical operators.
young__or_diesel <- filter(car_data, oldcars == 'not old' | 
                         `Fuel type` == 'Diesel')
# What happens with 'Or' instead? (|)

#group_by and summarize. Like peanut butter & jelly
avg_manufacturer <- car_data %>% 
  group_by(Manufacturer)%>%summarize(ave_price = mean(Price))
avg_manufacturer

#summarize categorical data
old_car_summary <- car_data %>% 
  group_by(oldcars) %>% 
  summarize (n=n()) %>% 
  mutate (freq = n/sum(n))
old_car_summary
