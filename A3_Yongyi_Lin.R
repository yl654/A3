library(plm)
library(tidyverse)
library(xts)

# Exercise 1 
population <- read.csv('population.csv')
crime_long <- read.csv('crime_long.csv')
officers <- read.csv('officers.csv')

# Exercise 2
# Calculate total crime per month
crime_monthly <- crime_long %>%
  group_by(crime_month) %>%
  summarize(crime_by_month = sum(crimes))
# Plot time series of crime
crime_ts <- ts(crime_monthly$crime_by_month, frequency = 12, start = c(2002, 1))
plot.ts(crime_ts, xlab = "Year", ylab = "Total Crime", xaxt = "n")
axis(side = 1, at = 2002:2019)
# Construct panel data
crime_poplong <- merge(population, crime_long, by.x = c("month", "district"), by.y = c("crime_month", "district"), all.y = TRUE)
crime_panel <- crime_poplong %>%
  mutate(property_crimes = case_when(crime_type == "property" ~ crimes, TRUE ~ 0L), violent_crimes = case_when(crime_type == "violent" ~ crimes, TRUE ~ 0L)) %>%
  group_by(month, district) %>%
  summarize(
    total_crimes = sum(crimes),
    violent_crimes = sum(violent_crimes),
    property_crimes = sum(property_crimes),
    median_income = p50_inc,
    share_black = tot_black/tot_pop,
    share_hisp = tot_hisp/tot_pop,
    share_white = tot_white/tot_pop
  ) %>%
  distinct()
head(crime_panel)

# Exercise 3
panel_officers <- merge(officers, crime_panel, by.y = c("month", "district"), by.x = c("month", "unit"), all.x = TRUE)
lm_ex3 <- lm(formula = arrest ~ tenure + total_crimes + median_income + share_black + share_hisp + share_white - 1, data = panel_officers)
lm_ex3$coefficients

# Exercise 4
fe <- lm(formula = arrest ~ tenure + total_crimes + median_income + share_black + share_hisp + share_white + factor(unit) + factor(month) - 1, data = panel_officers)
fe$coefficients

# Exercise 5
# within
fe2_within <- plm(formula = arrest ~ tenure + total_crimes + median_income + share_black + share_hisp + share_white + factor(unit) - 1, effect = "twoway", data = panel_officers, model = "within")
# between
fe2_between <- plm(formula = arrest ~ tenure + total_crimes + median_income + share_black + share_hisp + share_white + factor(unit) + factor(month), effect = "individual", data = panel_officers, model = "between")
# first difference
fe4 <- plm(formula = arrest ~ tenure + total_crimes + median_income + share_black + share_hisp + share_white + factor(unit) + factor(month), effect = "individual", data = panel_officers, model = "fd")