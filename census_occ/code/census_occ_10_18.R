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
    occ10    = x2010_occupation_code,
    emp17_10 = estimate_from_table) |>
  drop_na() |>
  mutate(
    occ10    = str_replace_all(occ10, ", ", "_"),
    emp17_10 = str_replace_all(emp17_10, ",", ""),
    emp17_10 = as.integer(emp17_10))
b24124_2017_occ18 <-
  read_excel(
    "census_occ/data/raw/table-h1_h2.xlsx",
    sheet = "Example 2017",
    range = "f14:h580") |>
  clean_names() |>
  rename(
    occ18_nm = x2018_occupation_description,
    occ18    = x2018_occupation_code,
    emp17_18 = converted_estimate) |>
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
    occ18 = x2018_census_code) 
code_changes <-
  concord_base |>
  select(occ10, type) |>
  drop_na()
concordance <-
  concord_base |>
  select(occ10, occ18) |>
  fill(occ10) |>
  drop_na() |>
  #Combine codes to match the employment data.
  mutate(
    occ10 = str_replace(occ10, "1830|1860", "1830_1860"),
    occ10 = str_replace(occ10, "2900|2960", "2900_2960"),
    occ10 = str_replace(occ10, "3235|3245", "3235_3245"),
    occ10 = str_replace(occ10, "6100|6110", "6100_6110"),
    occ10 = str_replace(occ10, "6310|6320", "6310_6320"),
    occ10 = str_replace(occ10, "6540|6765", "6540_6765"),
    occ10 = str_replace(occ10, "7440|7630", "7440_7630"),
    occ10 = str_replace(occ10, "8255|8256", "8255_8256"),
    occ10 = str_replace(occ10, "8430|8460", "8430_8460"),
    occ10 = str_replace(occ10, "8520|8550", "8520_8550"),
    occ18 = str_replace(occ18, "1830|1860", "1830_1860"),
    occ18 = str_replace(occ18, "2905|2970", "2905_2970"),
    occ18 = str_replace(occ18, "3235|3245", "3235_3245"),
    occ18 = str_replace(occ18, "6765|6540", "6765_6540"),
    occ18 = str_replace(occ18, "7440|7640", "7440_7640"),
    occ18 = str_replace(occ18, "8255|8256", "8255_8256"))|>
  left_join(code_changes) |>
  left_join(b24124_2017_occ10) |>
  left_join(b24124_2017_occ18) |>
  drop_na()
