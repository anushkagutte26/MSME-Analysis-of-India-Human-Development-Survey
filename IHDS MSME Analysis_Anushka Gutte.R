
setwd("/Users/apple/Desktop/Data Analysis/R/IHDS/IHDS II Data")

# ============================================================
# IHDS-II: MSMEs, Household Welfare, and Social Capital
# Simple R Analysis

# This analysis uses IHDS-II data to examine the relationship between
# household enterprise activity (proxied as MSMEs), household welfare,
# social capital and gender outcomes in India.

# First, I conduct household-level analysis to study whether households
# engaged in non-farm business activities have higher levels of welfare,
# measured using per-capita consumption.

# Second, I construct a social capital index using household membership
# in economic, community, and civic groups, and examine how social
# networks are associated with welfare and MSME participation.

# Third, I merge household data with individual-level data to analyse
# labour market outcomes, focusing on gender differences in employment
# and participation in business activities.

# All results are descriptive and associational, and should not be
# interpreted as causal.


# ============================================================

install.packages("tidyverse", "janitor", "haven", "stargazer")

library(tidyverse)
library(janitor)
library(haven)
library(stargazer)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------

load("36151-0002-Data.rda")
load("ihds_individual.rda")

# Assign it to hh
hh <- da36151.0002
ind <- da36151.0001

hh <- hh %>%
  clean_names()

ind <- ind %>%
  clean_names()

# Check data
dim(hh)
names(hh)

# ------------------------------------------------------------
# Rename important variables
# ------------------------------------------------------------
hh <- hh %>%
  rename(
    household_id = idhh,
    state = stateid,
    distrist = distid,
    household_weight = wt,
    rural_urban = urban2011,
    caste_code = id13,
    principal_income_source = id14,
    household_size = npersons,
    
    nonfarm_business = nf1,
    number_of_businesses = nf1n,
    business_receipts = nf3,
    business_cost_rent = nf4a,
    business_cost_utilities_transport = nf4b,
    business_cost_labour = nf4c,
    business_cost_materials = nf4d,
    business_cost_loan_interest = nf4e,
    business_cost_other = nf4f,
    business_total_cost = nf4g,
    business_income = nf5,
    business_investment = nf6,
    business_location = nf7,
    business_family_worker = nf8,
    business_decision_maker = nf15,
    
    loan_from_bank_or_government = db1a,
    loan_from_microfinance_or_shg = db1b,
    loan_from_moneylender = db1c,
    loan_from_employer = db1d,
    loan_from_friends_or_relatives = db1e,
    loan_from_other_source = db1f,
    largest_loan_amount = db2b,
    largest_loan_purpose = db2c,
    largest_loan_source = db2d
  )

ind <- ind %>%
  rename(
    household_id = idhh,
    person_id = idperson
  )

ind <- ind %>%
  mutate(across(c(ro3, ro7), as.numeric))

ind <- ind %>%
  mutate(
    # Gender
    female = case_when(
      ro3 == 2 ~ 1,
      ro3 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Age
    age = ro5,
    
    # Working age restriction
    working_age = case_when(
      ro5 >= 15 & ro5 <= 59 ~ 1,
      TRUE ~ 0
    ),
    
    # Employment status
    employed = case_when(
      ro7 %in% c(1:9) ~ 1,   # working categories
      ro7 %in% c(10,11, 12, 13, 14, 15) ~ 0,
      TRUE ~ NA_real_
    ))
    

# ------------------------------------------------------------
# Create MSME / household enterprise variables
# ------------------------------------------------------------

# IHDS asks whether anyone in the household runs their own business,
# however big or small, including selling goods, providing services,
# or producing for contractors. I use this as a proxy for MSME activity.

hh <- hh %>%
  mutate(
    nonfarm_business_num = as.numeric(nonfarm_business),
    principal_income_source = as.numeric(principal_income_source)
  )
hh <- hh %>%
  mutate(
    msme_household = case_when(
      nonfarm_business_num == 2 ~ 1,
      nonfarm_business_num == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )


table(hh$principal_income_source)
hh <- hh %>%
  mutate(
    msme_income_source = case_when(
      principal_income_source %in% c(5, 6, 7, 9) ~ 1,
      principal_income_source %in% c(1, 2, 3, 4, 8, 10, 11) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Business income and investment variables

hh <- hh %>%
  mutate(
    business_income_clean = as.numeric(business_income),
    business_investment_clean = as.numeric(business_investment),
    business_receipts_clean = as.numeric(business_receipts)
  )

# ------------------------------------------------------------
# Creating household background variables
# ------------------------------------------------------------

# Rural / urban variable

hh <- hh %>%
  mutate(rural_urban = as.numeric(rural_urban))

table(hh$caste_code)

hh <- hh %>%
  mutate(
    urban = case_when(
      rural_urban == 2 ~ 1,
      rural_urban == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Household size

hh <- hh %>%
  mutate(
    household_size_clean = as.numeric(household_size)
  )

# ------------------------------------------------------------
# Creating household welfare variables
# ------------------------------------------------------------


hh <- hh %>%
  mutate(
    total_consumption = as.numeric(cototal),
    per_capita_consumption = as.numeric(copc),
    log_total_consumption = log(total_consumption + 1),
    log_per_capita_consumption = log(per_capita_consumption + 1)
  )


summary(hh$total_consumption)
summary(hh$per_capita_consumption)
summary(hh$log_per_capita_consumption)

# ------------------------------------------------------------
# Creating credit access variables
# ------------------------------------------------------------

table(hh$has_friend_or_relative_loan)
hh <- hh %>%
  mutate(
    loan_from_bank_or_government = as.numeric(loan_from_bank_or_government),
    loan_from_microfinance_or_shg = as.numeric(loan_from_microfinance_or_shg),
    loan_from_moneylender = as.numeric(loan_from_moneylender),
    loan_from_friends_or_relatives = as.numeric(loan_from_friends_or_relatives),
    largest_loan_purpose = as.numeric(largest_loan_purpose)
  )

hh <- hh %>%
  mutate(
    has_bank_or_government_loan = case_when(
      loan_from_bank_or_government == 2 ~ 1,
      loan_from_bank_or_government == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    has_microfinance_or_shg_loan = case_when(
      loan_from_microfinance_or_shg == 2 ~ 1,
      loan_from_microfinance_or_shg == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    has_moneylender_loan = case_when(
      loan_from_moneylender == 2 ~ 1,
      loan_from_moneylender == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    has_friend_or_relative_loan = case_when(
      loan_from_friends_or_relatives == 2 ~ 1,
      loan_from_friends_or_relatives == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    business_loan = case_when(
      largest_loan_purpose == 5 ~ 1,
      !is.na(largest_loan_purpose) ~ 0,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------
#  Creating social capital variables manually
# ------------------------------------------------------------

# I create each social capital variable separately.

# The IHDS social capital module asks whether anyone in the household
# belongs to different groups or organisations.


hh <- hh %>% mutate(across(c(me1, me2, me3, me4, me5, me6, me7, me8, me9, me10,
                             me11, me12, me13, me14, me14a
), as.numeric))

hh <- hh %>%
  mutate(
    mahila_mandal_member = case_when(
      me1 == 2 ~ 1,
      me1 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    youth_club_member = case_when(
      me2 == 2 ~ 1,
      me2 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    employee_business_group_member = case_when(
      me3 == 2 ~ 1,
      me3 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    self_help_group_member = case_when(
      me4 == 2 ~ 1,
      me4 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    credit_savings_group_member = case_when(
      me5 == 2 ~ 1,
      me5 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    religious_group_member = case_when(
      me6 == 2 ~ 1,
      me6 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    social_festival_group_member = case_when(
      me7 == 2 ~ 1,
      me7 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    caste_association_member = case_when(
      me8 == 2 ~ 1,
      me8 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    ngo_development_group_member = case_when(
      me9 == 2 ~ 1,
      me9 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    cooperative_member = case_when(
      me10 == 2 ~ 1,
      me10 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    political_party_member = case_when(
      me11 == 2 ~ 1,
      me11 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    rotary_lions_club_member = case_when(
      me12 == 2 ~ 1,
      me12 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    attended_public_meeting = case_when(
      me13 == 2 ~ 1,
      me13 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    panchayat_or_ward_member = case_when(
      me14 == 2 ~ 1,
      me14 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    close_to_panchayat_or_ward_member = case_when(
      me14a == 2 ~ 1,
      me14a == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------
# Building sub-indices manually
# ------------------------------------------------------------

# 1. Economic / business network index
# This captures memberships that may directly help with enterprise activity,
# credit, markets, employment, or production.

hh <- hh %>%
  mutate(
    economic_network_index =
      employee_business_group_member +
      self_help_group_member +
      credit_savings_group_member +
      cooperative_member
  )

# 2. Community membership index
# This captures broader local embeddedness and participation in community life.

hh <- hh %>%
  mutate(
    community_membership_index =
      mahila_mandal_member +
      youth_club_member +
      religious_group_member +
      social_festival_group_member +
      caste_association_member +
      ngo_development_group_member +
      rotary_lions_club_member
  )

# 3. Political / civic network index
# This captures links to local institutions and public participation.

hh <- hh %>%
  mutate(
    civic_network_index =
      attended_public_meeting +
      panchayat_or_ward_member +
      close_to_panchayat_or_ward_member +
      political_party_member
  )

# 4. Overall social capital index
# This combines economic, community, and civic network participation.

hh <- hh %>%
  mutate(
    social_capital_index =
      economic_network_index +
      community_membership_index +
      civic_network_index
  )

# 5. High social capital indicator

hh <- hh %>%
  mutate(
    high_social_capital = case_when(
      social_capital_index > median(social_capital_index, na.rm = TRUE) ~ 1,
      social_capital_index <= median(social_capital_index, na.rm = TRUE) ~ 0,
      TRUE ~ NA_real_
    )
  )


summary(hh$economic_network_index)
summary(hh$community_membership_index)
summary(hh$civic_network_index)
summary(hh$social_capital_index)

table(hh$high_social_capital, useNA = "ifany")

# ------------------------------------------------------------
# Descriptive statistics
# ------------------------------------------------------------

# Overall MSME share

hh %>%
  summarise(
    number_of_households = n(),
    msme_share = mean(msme_household, na.rm = TRUE)
  )

# Household welfare by MSME status

hh %>%
  group_by(msme_household) %>%
  summarise(
    number_of_households = n(),
    average_total_consumption = weighted.mean(total_consumption, household_weight, na.rm = TRUE),
    average_per_capita_consumption = weighted.mean(per_capita_consumption, household_weight, na.rm = TRUE),
    average_business_income = weighted.mean(business_income_clean, household_weight, na.rm = TRUE),
    average_social_capital = weighted.mean(social_capital_index, household_weight, na.rm = TRUE)
  )
# MSME participation by rural / urban location

hh %>%
  group_by(urban) %>%
  summarise(
    number_of_households = n(),
    msme_share = weighted.mean(msme_household, household_weight, na.rm = TRUE),
    average_per_capita_consumption = weighted.mean(per_capita_consumption, household_weight, na.rm = TRUE)
  )

# MSME participation by caste group

hh %>%
  group_by(caste_code) %>%
  summarise(
    number_of_households = n(),
    msme_share = weighted.mean(msme_household, household_weight, na.rm = TRUE),
    average_per_capita_consumption = weighted.mean(per_capita_consumption, household_weight, na.rm = TRUE)
  )

# MSME participation by social capital group

hh %>%
  group_by(high_social_capital) %>%
  summarise(
    number_of_households = n(),
    msme_share = weighted.mean(msme_household, household_weight, na.rm = TRUE),
    average_per_capita_consumption = weighted.mean(per_capita_consumption, household_weight, na.rm = TRUE)
  )

# ------------------------------------------------------------
# Simple graphs
# ------------------------------------------------------------

# Graph 1: household welfare by MSME status

hh %>%
  group_by(msme_household) %>%
  summarise(
    average_per_capita_consumption = weighted.mean(per_capita_consumption, household_weight, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = factor(msme_household), y = average_per_capita_consumption)) +
  geom_col() +
  labs(
    title = "Household welfare by MSME status",
    x = "MSME household",
    y = "Average per-capita consumption"
  ) +
  theme_minimal()

# Graph 2: MSME participation by caste

hh %>%
  group_by(caste_code) %>%
  drop_na(caste_code) %>% 
  summarise(
    msme_share = weighted.mean(msme_household, household_weight, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = caste_code, y = msme_share)) +
  geom_col() +
  labs(
    title = "MSME participation by caste group",
    x = "Caste group",
    y = "Share of households with MSME"
  ) +
  theme_minimal()

# Graph 3: MSME participation by social capital

hh %>%
  group_by(high_social_capital) %>%
  drop_na(high_social_capital) %>% 
  summarise(
    msme_share = weighted.mean(msme_household, household_weight, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = factor(high_social_capital), y = msme_share)) +
  geom_col() +
  labs(
    title = "MSME participation by social capital",
    x = "High social capital household",
    y = "Share of households with MSME"
  ) +
  theme_minimal()




table(hh$business_decision_maker)

# ------------------------------------------------------------
# Regression analysis for MSME and household welfare (consumption)
# ------------------------------------------------------------

# Model 1:
# Simple relationship between MSME participation and household welfare

model1 <- lm(
  log_per_capita_consumption ~ msme_household,
  data = hh,
  weights = household_weight
)

summary(model1)

# Model 2:
# Add household size, rural/urban location, and caste

model2 <- lm(
  log_per_capita_consumption ~ msme_household +
    household_size_clean +
    urban +
    caste_code,
  data = hh,
  weights = household_weight
)

summary(model2)

# Model 3:
# Add social capital index

model3 <- lm(
  log_per_capita_consumption ~ msme_household +
    social_capital_index +
    household_size_clean +
    urban +
    caste_code,
  data = hh,
  weights = household_weight
)

summary(model3)

# Model 4:
# Interaction model:
# This checks whether the association between MSME participation
# and household welfare is different for households with more social capital.

model4 <- lm(
  log_per_capita_consumption ~ msme_household +
    social_capital_index +
    msme_household:social_capital_index +
    household_size_clean +
    urban +
    caste_code,
  data = hh,
  weights = household_weight
)

summary(model4)

# Model 5:
# Alternative MSME definition using principal income source

model5 <- lm(
  log_per_capita_consumption ~ msme_income_source +
    social_capital_index +
    household_size_clean +
    urban +
    caste_code,
  data = hh,
  weights = household_weight
)

summary(model5)

# ------------------------------------------------------------
# Saving regression table
# ------------------------------------------------------------

stargazer(
  model1, model2, model3, model4, model5,
  type = "text",
  title = "MSMEs, Social Capital, and Household Welfare",
  dep.var.labels = "Log per-capita household consumption",
  covariate.labels = c(
    "MSME household",
    "Social capital index",
    "MSME x social capital",
    "Household size",
    "Urban",
    "Alternative MSME proxy",
    "Caste group controls"
  )
)

# ------------------------------------------------------------
# Merging with Individual data to analyse female participation in MSME
# ------------------------------------------------------------


ind <- ind %>%
  filter(working_age == 1)

ind <- ind %>%
  mutate(
    female_business_activity = case_when(
      ro7 %in% c(5, 6, 7, 9) ~ 1,
      ro7 %in% c(1,2,3,4,8) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Merge (many individuals to one household)
data <- ind %>%
  left_join(hh, by = "household_id")

# Check merge worked
dim(data)
summary(data$msme_household)

data %>%
  filter(female == 1) %>%
  summarise(
    share_business = mean(female_business_activity, na.rm = TRUE)
  )

data %>%
  filter(female == 1) %>%
  group_by(msme_household) %>%
  summarise(
    business_participation = mean(female_business_activity, na.rm = TRUE)
  )

data %>%
  group_by(msme_household, female) %>%
  summarise(
    employment_rate = weighted.mean(employed, household_weight, na.rm = TRUE),
    avg_consumption = weighted.mean(per_capita_consumption, household_weight, na.rm = TRUE)
  )


data %>%
  filter(female == 1) %>%
  group_by(msme_household) %>%
  summarise(
    female_employment_rate = weighted.mean(employed, household_weight, na.rm = TRUE)
  )


model_emp1 <- lm(
  employed ~ msme_household + female + age + urban + caste_code,
  data = data,
  weights = household_weight
)

summary(model_emp1)


model_emp2 <- lm(
  employed ~ msme_household +
    female +
    msme_household:female +
    age + urban + caste_code,
  data = data,
  weights = household_weight
)

summary(model_emp2)


model_emp3 <- lm(
  employed ~ msme_household +
    female +
    social_capital_index +
    msme_household:female +
    msme_household:social_capital_index +
    age + urban + caste_code,
  data = data,
  weights = household_weight
)

summary(model_emp3)


model_female_business <- lm(
  female_business_activity ~ msme_household +
    social_capital_index +
    age + urban + caste_code,
  data = data,
  weights = household_weight
)

summary(model_female_business)

write.csv(hh, "ihds_msme_welfare_social_capital_clean.csv", row.names = FALSE)
