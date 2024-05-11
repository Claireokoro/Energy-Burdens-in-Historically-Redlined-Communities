library(tidyverse)
library(ggplot2)
library(dplyr)
library(modelsummary)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)

rm(list = ls())

# 1 LOAD THE DATA 2020 ---------
data<-read_csv("/Users/maryclaireokoro/Downloads/recs2020_public_v7.csv")

# I WOULD BE WORKING WITH MICHIGAN
Mich_data <- data %>% dplyr::filter(state_postal == "MI")

# 2  GETTING THE PERCENTAGE OF HOMEOWNERS AND RENTERS------------ 
# Summarize the data to count the number of owners and renters
ownership_counts <- Mich_data %>%
  group_by(KOWNRENT) %>%
  summarise(count = n(), .groups = 'drop')

# Calculate total respondents for normalization
total_respondents <- sum(ownership_counts$count)

# Add a column for percentage
ownership_counts <- ownership_counts %>%
  mutate(percentage = (count / total_respondents) * 100)

library(dplyr)

# Assuming ownership_counts has already been created and contains the kownrent, count, and percentage columns

# Map the ownership codes to more descriptive labels
ownership_counts <- ownership_counts %>%
  mutate(ownership_type = case_when(
    KOWNRENT == 1 ~ "Owners",
    KOWNRENT == 2 ~ "Renters",
    KOWNRENT== 3 ~ "Occupants",  # Adding a label for the new category
    TRUE ~ "Unknown"  # This handles any unexpected values
  ))

# 3 ANALYZING THE SHARE OF HOUSEOWNERSHIP AND RENTERS WITH RACE AND ETHNICITY -----------

# Map the descriptive labels
Mich_data <- Mich_data %>%
  mutate(
    ownership_status = case_when(
      KOWNRENT == 1 ~ "Owners",
      KOWNRENT == 2 ~ "Renters",
      KOWNRENT == 3 ~ "Occupants",
      TRUE ~ "Unknown"
    ),
    hispanic_latino = case_when(
      SDESCENT == 1 ~ "Yes",
      SDESCENT == 0 ~ "No",
      TRUE ~ "Unknown"
    ),
    race = case_when(
      HOUSEHOLDER_RACE == 1 ~ "White Alone",
      HOUSEHOLDER_RACE == 2 ~ "Black or African American Alone",
      HOUSEHOLDER_RACE == 3 ~ "American Indian or Alaska Native Alone",
      HOUSEHOLDER_RACE == 4 ~ "Asian Alone",
      HOUSEHOLDER_RACE == 5 ~ "Native Hawaiian or Other Pacific Islander Alone",
      HOUSEHOLDER_RACE == 6 ~ "Two or More Races",
      TRUE ~ "Unknown"
    )
  )

# Calculate counts and percentages
race_ownership_summary <- Mich_data %>%
  group_by(race, ownership_status) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

# USING BARCHART
ggplot(race_ownership_summary, aes(x = race, y = percentage, fill = ownership_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Ownership Status by Race",
       x = "Race",
       y = "Percentage",
       fill = "Ownership Status") +
  theme_minimal()

# 4 ANALYSING INCOMES ACROSS RACES 
# Map the descriptive labels for income and race
Mich_data<- Mich_data %>%
  mutate(
    income_range = case_when(
      MONEYPY == 1 ~ "Less than $5,000",
      MONEYPY == 2 ~ "$5,000 - $7,499",
      MONEYPY == 3 ~ "$7,500 - $9,999",
      MONEYPY == 4 ~ "$10,000 - $12,499",
      MONEYPY == 5 ~ "$12,500 - $14,999",
      MONEYPY == 6 ~ "$15,000 - $19,999",
      MONEYPY == 7 ~ "$20,000 - $24,999",
      MONEYPY == 8 ~ "$25,000 - $29,999",
      MONEYPY == 9 ~ "$30,000 - $34,999",
      MONEYPY == 10 ~ "$35,000 - $39,999",
      MONEYPY == 11 ~ "$40,000 - $49,999",
      MONEYPY == 12 ~ "$50,000 - $59,999",
      MONEYPY == 13 ~ "$60,000 - $74,999",
      MONEYPY == 14 ~ "$75,000 - $99,999",
      MONEYPY == 15 ~ "$100,000 - $149,999",
      MONEYPY == 16 ~ "$150,000 or more",
      TRUE ~ "Unknown"
    ),
    race = case_when(
      HOUSEHOLDER_RACE == 1 ~ "White Alone",
      HOUSEHOLDER_RACE == 2 ~ "Black or African American Alone",
      HOUSEHOLDER_RACE == 3 ~ "American Indian or Alaska Native Alone",
      HOUSEHOLDER_RACE == 4 ~ "Asian Alone",
      HOUSEHOLDER_RACE == 5 ~ "Native Hawaiian or Other Pacific Islander Alone",
      HOUSEHOLDER_RACE == 6 ~ "Two or More Races",
      TRUE ~ "Unknown"
    )
  )


# Summarize data by race and income range
income_summary_by_race <- Mich_data %>%
  group_by(race, income_range) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create a bar plot to visualize the income distribution by race
ggplot(income_summary_by_race, aes(x = income_range, y = percentage, fill = race)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Income Distribution by Race",
       x = "Income Range",
       y = "Percentage",
       fill = "Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity


# 5 ANALYZING WHICH RACIAL GROUP FORGO BASIC NESSECITIES DUE TO HOME ENERGY BILLS 
# Map the descriptive labels for frequency of reducing basic necessities and race
Mich_data <- Mich_data %>%
  mutate(
    frequency = case_when(
      SCALEB == 1 ~ "Almost every month",
      SCALEB == 2 ~ "Some months",
      SCALEB == 3 ~ "1 or 2 months",
      SCALEB == 0 ~ "Never",
      TRUE ~ "Unknown"
    ),
    race = case_when(
      HOUSEHOLDER_RACE == 1 ~ "White Alone",
      HOUSEHOLDER_RACE == 2 ~ "Black or African American Alone",
      HOUSEHOLDER_RACE == 3 ~ "American Indian or Alaska Native Alone",
      HOUSEHOLDER_RACE == 4 ~ "Asian Alone",
      HOUSEHOLDER_RACE == 5 ~ "Native Hawaiian or Other Pacific Islander Alone",
      HOUSEHOLDER_RACE == 6 ~ "Two or More Races",
      TRUE ~ "Unknown"
    )
  )
# Calculate counts and percentages
frequency_summary_by_race <- Mich_data %>%
  group_by(race, frequency) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create a bar chart for frequency of reducing necessities by race
ggplot(frequency_summary_by_race, aes(x = race, y = percentage, fill = frequency)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Frequency of Reducing Necessities Due to Energy Bills by Race",
       x = "Race",
       y = "Percentage",
       fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#. 6 housing unit type, construction year, race and energy usage

# Map descriptive labels for housing type, construction year, energy usage, and race
Mich_data <- Mich_data %>%
  mutate(
    house_type = case_when(
      TYPEHUQ == 1 ~ "Mobile home",
      TYPEHUQ == 2 ~ "Single-family detached",
      TYPEHUQ == 3 ~ "Single-family attached",
      TYPEHUQ == 4 ~ "Apartment in a building with 2 to 4 units",
      TYPEHUQ == 5 ~ "Apartment in a building with 5 or more units",
      TRUE ~ "Unknown"
    ),
    year_built_range = case_when(
      YEARMADERANGE == 1 ~ "Before 1950",
      YEARMADERANGE == 2 ~ "1950 to 1959",
      YEARMADERANGE == 3 ~ "1960 to 1969",
      YEARMADERANGE == 4 ~ "1970 to 1979",
      YEARMADERANGE == 5 ~ "1980 to 1989",
      YEARMADERANGE == 6 ~ "1990 to 1999",
      YEARMADERANGE == 7 ~ "2000 to 2009",
      YEARMADERANGE == 8 ~ "2010 to 2015",
      YEARMADERANGE == 9 ~ "2016 to 2020",
      TRUE ~ "Unknown"
    ),
    electricity_only = if_else(ALLELEC == 1, "Yes", "No"),
    race = case_when(
      HOUSEHOLDER_RACE == 1 ~ "White Alone",
      HOUSEHOLDER_RACE == 2 ~ "Black or African American Alone",
      HOUSEHOLDER_RACE == 3 ~ "American Indian or Alaska Native Alone",
      HOUSEHOLDER_RACE == 4 ~ "Asian Alone",
      HOUSEHOLDER_RACE == 5 ~ "Native Hawaiian or Other Pacific Islander Alone",
      HOUSEHOLDER_RACE == 6 ~ "Two or More Races",
      TRUE ~ "Unknown"
    )
  )

# Analyze house type distribution by race
house_type_by_race <- Mich_data %>%
  group_by(race, house_type) %>%
  summarise(count = n(), .groups = 'drop')

# Analyze construction year distribution by race
construction_by_race <- Mich_data %>%
  group_by(race, year_built_range) %>%
  summarise(count = n(), .groups = 'drop')

# Analyze electricity-only usage by race
electricity_usage_by_race <- Mich_data %>%
  group_by(race, electricity_only) %>%
  summarise(count = n(), .groups = 'drop')

# Visualize house type distribution by race
ggplot(house_type_by_race, aes(x = race, y = count, fill = house_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "House Type Distribution by Race", x = "Race", y = "Count", fill = "House Type")

# Visualize construction year range distribution by race
ggplot(construction_by_race, aes(x = race, y = count, fill = year_built_range)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Construction Year Distribution by Race", x = "Race", y = "Count", fill = "Year Built Range")

# Visualize electricity usage distribution by race
ggplot(electricity_usage_by_race, aes(x = race, y = count, fill = electricity_only)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Electricity-Only Usage by Race", x = "Race", y = "Count", fill = "Electricity Only")

# 7 CORRELATION BETWEEN INCOME AND ENERGY COSTS 
# Calculate Pearson correlation
# Assuming 'df' is your dataframe and 'MONEYPY' holds the income range codes
Mich_data$income_factor <- factor(Mich_data$MONEYPY, 
                           levels = 1:16, 
                           labels = c("Less than $5,000", "$5,000 - $7,499", "$7,500 - $9,999", 
                                      "$10,000 - $12,499", "$12,500 - $14,999", "$15,000 - $19,999", 
                                      "$20,000 - $24,999", "$25,000 - $29,999", "$30,000 - $34,999", 
                                      "$35,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", 
                                      "$60,000 - $74,999", "$75,000 - $99,999", "$100,000 - $149,999", 
                                      "$150,000 or more"), 
                           ordered = TRUE)


ggplot(Mich_data, aes(x = income_factor, y = DOLLAREL)) +
  geom_boxplot() +
  labs(title = "Energy Costs Across Income Ranges", x = "Income Range", y = "Energy Costs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8 ENERGY INSECURITY INDICATORS AGAINST RACE
# Install the packages if you haven't already

# Summary table for all indicators
summary_table <- Mich_data %>%
  group_by(HOUSEHOLDER_RACE) %>%
  summarise(
    Total = n(),  # Total respondents per race
    Heat_Medical_Attention = sum(HOTMA == "Yes", na.rm = TRUE),
    Cold_Medical_Attention = sum(COLDMA == "Yes", na.rm = TRUE),
    Disconnect_Notice = sum(SCALEE == "1", na.rm = TRUE),  # Assuming '1' is 'Almost every month'
    Broken_Heating = sum(NOHEATBROKE == "Yes", na.rm = TRUE),
    Heating_Disconnected = sum(NOHEATEL == "Yes", na.rm = TRUE),
    AC_Broken = sum(NOACBROKE == "Yes", na.rm = TRUE),
    AC_Disconnected = sum(NOACEL == "Yes", na.rm = TRUE)
  ) %>%
  mutate(
    Heat_Medical_Attention_Prop = Heat_Medical_Attention / Total,
    Cold_Medical_Attention_Prop = Cold_Medical_Attention / Total,
    Disconnect_Notice_Prop = Disconnect_Notice / Total,
    Broken_Heating_Prop = Broken_Heating / Total,
    Heating_Disconnected_Prop = Heating_Disconnected / Total,
    AC_Broken_Prop = AC_Broken / Total,
    AC_Disconnected_Prop = AC_Disconnected / Total
  )

# View the summary table
print(summary_table)

# 9 energy assitance
# Assuming ENERGY_ASSISTANCE is a binary variable where 1 = received assistance, 0 = did not receive
# Convert ENERGYASST to a factor
Mich_data$ENERGYASST <- factor(Mich_data$ENERGYASST, levels = c(0, 1), labels = c("No", "Yes"))

# Load necessary packages
library(dplyr)

# Create a summary table
# Load necessary packages
library(dplyr)

# Assuming ENERGYASST and HOUSEHOLDER_RACE might have NA values
# Convert ENERGYASST to a factor with correct labels if not already done
Mich_data$ENERGYASST <- factor(Mich_data$ENERGYASST, levels = c(0, 1), labels = c("No", "Yes"))

# Ensure HOUSEHOLDER_RACE has meaningful labels and exclude NA values
Mich_data$HOUSEHOLDER_RACE <- factor(Mich_data$HOUSEHOLDER_RACE, levels = 1:6,
                                     labels = c("White Alone", "Black or African American Alone", "American Indian or Alaska Native Alone",
                                                "Asian Alone", "Native Hawaiian or Other Pacific Islander Alone", "2 or More Races Selected"))

# Filtering out NA values before summarizing
energy_assist_summary <- Mich_data %>%
  filter(!is.na(HOUSEHOLDER_RACE), !is.na(ENERGYASST)) %>%  # Exclude rows where RACE or ENERGYASST is NA
  group_by(HOUSEHOLDER_RACE) %>%
  summarise(
    Total = n(),  # Total number of valid respondents per race
    Assistance_Received = sum(ENERGYASST == "Yes", na.rm = TRUE),
    Proportion = Assistance_Received / Total
  ) %>%
  arrange(desc(Proportion))

# Print the summary table
print(energy_assist_summary)


library(dplyr)
library(tidyr)

# Assuming your data is in a dataframe called 'your_data'
# Assuming the columns for race, state, and ownership status are named accordingly

ownership_summary1 <- data %>%
  group_by(state_name, HOUSEHOLDER_RACE, KOWNRENT) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(ownership_type = case_when(
    KOWNRENT == 1 ~ "Owner",
    KOWNRENT == 2 ~ "Renter",
    KOWNRENT == 3 ~ "Occupant"
  )) %>%
  pivot_wider(names_from = ownership_type, values_from = count, values_fill = list(count = 0)) %>%
  mutate(Total = Owner + Renter + Occupant)

# Adjust according to the column names in your dataset
print(ownership_summary)

library(dplyr)
library(tidyr)

# Map race codes to descriptive names
race_descriptions <- c("1" = "White Alone", 
                       "2" = "Black or African American Alone", 
                       "3" = "American Indian or Alaska Native Alone", 
                       "4" = "Asian Alone", 
                       "5" = "Native Hawaiian or Other Pacific Islander Alone", 
                       "6" = "Two or More Races")

# Assuming your data is in a dataframe called 'your_data'
ownership_summary2 <- data %>%
  mutate(HOUSEHOLDER_RACE = factor(HOUSEHOLDER_RACE, labels = race_descriptions)) %>%
  group_by(state_name, HOUSEHOLDER_RACE, KOWNRENT) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(ownership_type = case_when(
    KOWNRENT == 1 ~ "Owner",
    KOWNRENT == 2 ~ "Renter",
    KOWNRENT == 3 ~ "Occupant"
  )) %>%
  pivot_wider(names_from = ownership_type, values_from = count, values_fill = list(count = 0)) %>%
  mutate(Total = Owner + Renter + Occupant)

# Print the modified data frame
print(ownership_summary)




