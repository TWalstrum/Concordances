#———————————————————————————————————————————————————————————————————————————————
# Description ------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
# API Discovery tool
# https://api.census.gov/data.html

# Data dictionary: 
# https://www.census.gov/programs-surveys/economic-census/year
#        /2022/technical-documentation/data-dictionary.html

# 2012 to 2017 bridge
# https://data.census.gov/table?q=EC1700BRIDGE2
# Variables https://api.census.gov/data/2017/ecnbridge2/variables.html
#———————————————————————————————————————————————————————————————————————————————
# Setup ------------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
library(tidyverse)
library(httr2)
library(tidyr)
library(janitor)
#———————————————————————————————————————————————————————————————————————————————
# Functions --------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
#————————————————————————————
# Calculate employment ratios
#————————————————————————————
naics_ratio <- function(naics_lvl, data, base_var, new_var) {
  base_var_str <- as_label(enquo(base_var))
  new_var_str <- as_label(enquo(new_var))
  data %>%
  mutate(
      "{{base_var}}" := str_sub({{base_var}}, 1, naics_lvl),
      "{{new_var}}"  := str_sub( {{new_var}}, 1, naics_lvl)) %>%
  summarize(emp = sum(emp), .by = c({{base_var}}, {{new_var}})) %>%
  reframe(
    emp = emp,
    "{{new_var}}" := {{new_var}},
    emp_sum = sum(emp),
    .by = c({{base_var}}))  %>%
  mutate(ratio = emp / emp_sum) %>%
  select({{base_var}}, {{new_var}}, ratio) %>%
  filter(!({{base_var}} == {{new_var}})) %>%
  write_csv(
    str_glue("naics/data/refined/{base_var_str}_{new_var_str}_{naics_lvl}.csv"))
}
#———————————————————————————————————————————————————————————————————————————————
# Flags for suppressed employment ----------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
emp_flags_17 <- read_csv("naics/data/raw/econ_census_emp_flags_2017.csv")
#———————————————————————————————————————————————————————————————————————————————
# Import and clean bridge tables -----------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
bridge_12_17 <- 
  str_c(
    "https://api.census.gov/data/2017/ecnbridge2?",
    "get=NAICS2012,NAICS2017BDG,EMP,EMP_F",
    "&for=us:*",
    "&key=c39028888a47a13f4a2423db2a0edb0c455c7d8f") %>%
  request() %>% 
  req_perform() %>% 
  resp_body_json(simplifyVector = TRUE) %>%
  as_tibble(.name_repair = make_clean_names) %>%
  row_to_names(1) %>%
  rename(
    naics12 = NAICS2012,
    naics17 = NAICS2017BDG,
    emp = EMP,
    emp_flag = EMP_F
  ) %>%
  mutate(emp = as.integer(emp))
# Use midpoints for the flagged variables.
emp_estimated_12_17 <-
  bridge_12_17 %>%
  left_join(emp_flags_17) %>%
  mutate(emp = if_else(is.na(emp_flag), emp, midpoint)) %>%
  select(naics12, naics17, emp) %>%
  filter(naics17 != '00000000') %>%
  mutate(naics17 = str_sub(naics17, 1, 6)) 
#———————————————————————————————————————————————————————————————————————————————
# Ratios by NAICS level --------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
naics_12_17 <- map(2:6, naics_ratio, emp_estimated_12_17, naics12, naics17)
naics_17_12 <- map(2:6, naics_ratio, emp_estimated_12_17, naics17, naics12)








