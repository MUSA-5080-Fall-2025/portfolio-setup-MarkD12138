#load libraries
library(tidycensus)
library(tidyverse)
library(knitr)
# Set API key (you'll get yours for Lab 1)
census_api_key("yourkeyhere")

# Get state-level population data
state_pop <- get_acs(
  geography = "state",
  variables = "B01003_001",  # Total population
  year = 2022,
  survey = "acs5"
)

glimpse(state_pop)

# Get income and population for Pennsylvania counties
pa_data <- get_acs(
  geography = "county",
  variables = c(
    total_pop = "B01003_001",
    median_income = "B19013_001"
  ),
  state = "PA",
  year = 2022,
  output = "wide"  # Makes analysis easier
)

head(pa_data)

pa_clean <- pa_data %>%
  mutate(
    # Remove state name from county names
    county_name = str_remove(NAME, ", Pennsylvania"),
    # Remove "County" word
    county_name = str_remove(county_name, " County")
  )

# Compare before and after
select(pa_clean, NAME, county_name)

pa_reliability <- pa_clean %>%
  mutate(
    # Calculate MOE as percentage of estimate
    moe_percentage = round((median_incomeM / median_incomeE) * 100, 2),
    
    # Create reliability categories
    reliability = case_when(
      moe_percentage < 5 ~ "High Confidence",
      moe_percentage >= 5 & moe_percentage <= 10 ~ "Moderate",
      moe_percentage > 10 ~ "Low Confidence"
    )
  )

count(pa_reliability, reliability)

# Find counties with highest uncertainty
high_uncertainty <- pa_reliability %>%
  filter(moe_percentage > 8) %>%
  arrange(desc(moe_percentage)) %>%
  select(county_name, median_incomeE, moe_percentage)

# Summary by reliability category  
reliability_summary <- pa_reliability %>%
  group_by(reliability) %>%
  summarize(
    counties = n(),
    avg_income = round(mean(median_incomeE, na.rm = TRUE), 0)
  )

# Create formatted table
kable(high_uncertainty,
      col.names = c("County", "Median Income", "MOE %"),
      caption = "Counties with Highest Income Data Uncertainty",
      format.args = list(big.mark = ","))



