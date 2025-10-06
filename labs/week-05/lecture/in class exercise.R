#| include: false
options(scipen = 999)
library(tidyverse)
library(tidycensus)
library(broom)
library(scales)

census_api_key("cd859f2a38103dda55948d92b3679de845f42f0f")

challenge_data <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_income = "B19013_001",   # Median household income
    median_age = "B01002_001",      # Median age
    percent_college = "B15003_022", # Bachelor's degree or higher
    median_rent = "B25058_001",     # Median rent
    poverty_rate = "B17001_002"     # Population in poverty
  ),
  year = 2022,
  output = "wide"
)

set.seed(123)
n <- nrow(pa_data)

# 70% training, 30% testing data set split
n <- nrow(challenge_data)

train_indices <- sample(1:n, size = 0.7 * n)
train_data <- challenge_data[train_indices, ]
test_data <- challenge_data[-train_indices, ]


#| echo: true
#| eval: true
model1 <- lm(home_valueE ~
               log(total_popE) +
               log(median_incomeE) + 
               median_ageE + 
               log(percent_collegeE) + 
               log(median_rentE) + 
               poverty_rateE, 
             data = train_data)
summary(model1)

test_predictions <- predict(model1, newdata = test_data)

rmse_test <- sqrt(mean((test_data$home_valueE - test_predictions)^2))

cat("Test RMSE:", round(rmse_test, 0), "\n")



ggplot(data = challenge_data, aes(x = poverty_rateE)) +
  geom_histogram()


## Multicollinearity plot
ggplot(challenge_data, aes(x = home_valueE, y = median_rentE)) +
  geom_point(alpha = 0.6, size = 3)