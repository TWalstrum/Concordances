#———————————————————————————————————————————————————————————————————————————————
# Description ------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
# Builds a crosswalk between the 2010 and 2018 Census occupation codes.
# Conversion rates for 2018 to 2010 are not available.
# 
# Source:
# https://www.census.gov/library/publications/2020/demo/acs-tp78.html
#———————————————————————————————————————————————————————————————————————————————
# Setup ------------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
library(tidyverse)
library(janitor)
library(readxl)
#———————————————————————————————————————————————————————————————————————————————
# Clean  --------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
# The Excel file is a slightly cleaned version of references/table-h1_h2.xlsx,
# sheet Template_Back_DO NOT EDIT
b24124_2017_occ10 <-
  read_excel(
    "census_occ/data/raw/table-h1_h2.xlsx",
    sheet = "Example 2017",
    range = "a14:c540") |>
  clean_names() |>
  rename(
    occ10_nm = x2010_occupation_description,
    occ10 = x2010_occupation_code,
    emp17 = estimate_from_table) |>
  drop_na() |>
  mutate(
    occ10 = str_replace_all(occ10, ", ", "_"),
    emp17 = str_replace_all(emp17, ",", ""),
    emp17 = as.integer(emp17))
b24124_2017_occ18 <-
  read_excel(
    "census_occ/data/raw/table-h1_h2.xlsx",
    sheet = "Example 2017",
    range = "f14:h580") |>
  clean_names() |>
  rename(
    occ18_nm = x2018_occupation_description,
    occ18 = x2018_occupation_code,
    emp17 = converted_estimate) |>
  drop_na() |>
  mutate(
    occ18 = str_replace_all(occ18, ", ", "_"),
    # In the B24124, occ code 1860 is combined with occ code 1830.
    occ18 = str_replace(occ18, "1860", "1830_1860"),
    # In the B24124, occ code 3235 is combined with occ code 3245.
    occ18 = str_replace(occ18, "3245", "3235_3245"))
concord_base <-
  read_excel(
    "census_occ/data/raw/table-h1_h2.xlsx",
    sheet = "Template_Back_DO NOT EDIT",
    range = "b4:g693") |>
  clean_names() |>
  rename(
    occ10 = x2010_census_code,
    type = x3,
    occ18 = x2018_census_code
  ) 
code_changes <-
  concord_base |>
  select(occ10, type) |>
  drop_na()
concordance <-
  concord_base |>
  select(occ10, occ18) |>
  fill(occ10) |>
  drop_na() |>
  left_join(code_changes) |>
  left_join(b24124_2017_occ10)  
