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
    range = "a14:d540") 

|>
  mutate(
    occ10 = as.integer(occ10),
    occ18 = as.integer(occ18)
  ) |>
  fill(soc10, occ10, soc10_nm) |>
  select(!type) |>
  drop_na() |>
  write_csv(str_glue("census_occ/data/clean/occ_10_18.csv"))
