install.packages("tidyverse")
# Bring in library

library(tidyverse)
library(haven)
library(dplyr)

# Read Data
health_survey <- read_dta("Data/IAHR52FL.dta")


# First Data Frame
df1 <- select(health_survey, hhid:hv208, hv270)
view(df1)

# Second Data Frame
df2 <- select(health_survey, hhid, starts_with("hvidx"), starts_with("hv108"))
view(df2)

# Anthropometric data for females (x can be 0:6)
anthro_female <- select(health_survey, hhid, starts_with("ha0_"):starts_with("ha6_"))
view(anthro_female)

# Anthropometric data for males
anthro_male <- select(health_survey, hhid, starts_with("hb0_"), starts_with("hb1_"),
                      starts_with("hb2_"), starts_with("hb3_"), starts_with("hb4_"),
                      starts_with("hb5_"), starts_with("hb6_"))
view(anthro_male)

# 6 Tyding up data frame 2
# Step 1: Use Gather
# Step 2: Use Separate
# Step 3: Use Spread
df2_tidy <- df2 %>%
  gather(variable_name, variable_value, -hhid) %>%
  separate(variable_name, into = c("Var.", "Number"), sep = "_") %>%
  spread(key = Var., value = variable_value) %>%
  filter(!is.na(hvidx)) %>%
  select(-Number) %>%
  rename(Education = hv108, Line_Number = hvidx)
view(df2_tidy)

# 7 Tyding up female sample
# Step 1: Use Gather
# Step 2: Use Separate
# Step 3: Use Spread
anthro_female_tidy <- anthro_female %>%
  gather(variable_name, variable_value, -hhid) %>%
  separate(variable_name, into = c("Var.", "Number"), sep = "_") %>%
  spread(key = Var., value = variable_value) %>%
  filter(!is.na(ha0), (ha1), (ha2), (ha3), (ha4), (ha5), (ha6)) %>%
  select(-Number,-ha0, -ha4, -ha5, -ha6) %>%
  rename(Age = ha1, Weight = ha2, Height = ha3) %>%
  mutate(female = TRUE)
view(anthro_female_tidy)

# 8 Tyding up male sample
# Step 1: Use Gather
# Step 2: Use Separate
# Step 3: Use Spread
anthro_male_tidy <- anthro_male %>%
  gather(variable_name, variable_value, -hhid) %>%
  separate(variable_name, into = c("Var.", "Number"), sep = "_") %>%
  spread(key = Var., value = variable_value) %>%
  filter(!is.na(hb0), (hb1), (hb2), (hb3), (hb4), (hb5), (hb6)) %>%
  select(-Number,-hb0, -hb4, -hb5, -hb6) %>%
  rename(Age = hb1, Weight = hb2, Height = hb3) %>%
  mutate(female = FALSE)
view(anthro_male_tidy)

# 9 Combining the data frames
stack <- bind_rows(anthro_female_tidy, anthro_male_tidy)
merge_table <- inner_join(stack, df2_tidy)
final_table <- inner_join(merge_table, df1)

# Couldn't figure out to get the right amount of obs.

female_med <- final_table %>%
  group_by(female) %>%
  summarise(median(Age))

