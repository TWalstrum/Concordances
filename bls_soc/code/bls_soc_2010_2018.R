#———————————————————————————————————————————————————————————————————————————————
# Description ------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
# Builds a crosswalk between the 2010 and 2018 BLS occupation codes.
# Weights are not available.
#
# Source: https://www.bls.gov/soc/2018/crosswalks.htm
#———————————————————————————————————————————————————————————————————————————————
# Setup ------------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
library(tidyverse)
library(janitor)
library(readxl)
#———————————————————————————————————————————————————————————————————————————————
# Clean  -----------------------------------------------------------------------
#———————————————————————————————————————————————————————————————————————————————
soc_2010_2018_base <-
  read_excel(
    "bls_soc/data/raw/soc_2010_to_2018_crosswalk.xlsx",
    sheet = "Sorted by 2010",
    range = "a9:d909") |>
  clean_names() |>
  rename(
    soc10    = x2010_soc_code,
    soc10_nm = x2010_soc_title,
    soc18    = x2018_soc_code,
    soc18_nm = x2018_soc_title) 
soc_2010_2018 <-
  soc_2010_2018_base |>
  add_count(soc10, name = "soc10_n") |>
  mutate(ratio = 1 / soc10_n) |>
  select(!"soc10_n") |>
  write_csv("bls_soc/data/refined/bls_soc_2010_2018.csv")
soc_2018_2010 <-
  soc_2010_2018_base |>
  add_count(soc18, name = "soc18_n") |>
  mutate(ratio = 1 / soc18_n) |>
  select(!"soc18_n") |>
  relocate(soc18, soc18_nm) |>
  arrange(soc18) |>
  write_csv("bls_soc/data/refined/bls_soc_2018_2010.csv")