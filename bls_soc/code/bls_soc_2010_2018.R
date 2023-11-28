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
    soc18_nm = x2018_soc_title) |>
  add_count(soc10, name = "soc10_n") |>
  add_count(soc18, name = "soc18_n")

|>
#   drop_na() |>
#   mutate(
#     occ10    = str_replace_all(occ10, ", ", "_"),
#     emp17_10 = str_replace_all(emp17_10, ",", ""),
#     emp17_10 = as.integer(emp17_10))
# b24124_2017_occ18 <-
#   read_excel(
#     "census_occ/data/raw/table-h1_h2.xlsx",
#     sheet = "Example 2017",
#     range = "f14:h580") |>
#   clean_names() |>
#   rename(
#     occ18_nm = x2018_occupation_description,
#     occ18    = x2018_occupation_code,
#     emp17_18 = converted_estimate) |>
#   drop_na() |>
#   mutate(
#     occ18 = str_replace_all(occ18, ", ", "_"),
#     # In B24124, occ code 1860 is combined with occ code 1830.
#     occ18 = str_replace(occ18, "1860", "1830_1860"),
#     # In B24124, occ code 3235 is combined with occ code 3245.
#     occ18 = str_replace(occ18, "3245", "3235_3245"),
#     # In B24124, the code numbers are reversed for the combo of 6540 and 6765.)
#     occ18 = str_replace(occ18, "6765_6540", "6540_6765"))
# concord_base <-
#   read_excel(
#     "census_occ/data/raw/table-h1_h2.xlsx",
#     sheet = "Template_Back_DO NOT EDIT",
#     range = "b4:h693") |>
#   clean_names() |>
#   remove_empty() |>
#   select(!x2018_soc_code) |>
#   rename(
#     occ10 = x2010_census_code,
#     occ10_nm = x2010_soc_title,
#     type = x3,
#     ratio_10_18 = total_conversion_rate,
#     occ18 = x2018_census_code,
#     occ18_nm = x2018_soc_title)
# occ10_nm <-
#   concord_base |>
#   select(occ10, occ10_nm) |>
#   distinct() |>
#   drop_na()
# occ18_nm <-
#   concord_base |>
#   select(occ18, occ18_nm) |>
#   distinct() |>
#   drop_na()
# changes <-
#   concord_base |>
#   select(occ10, type) |>
#   remove_empty() |>
#   mutate(type = replace_na(type, "None"))
# concordance <-
#   concord_base |>
#   select(occ10, occ18, ratio_10_18) |>
#   fill(occ10) |>
#   drop_na() |>
#   left_join(changes) |>
#   # Combine codes to match the employment data.
#   mutate(
#     occ10 = str_replace(occ10, "1830|1860", "1830_1860"),
#     occ10 = str_replace(occ10, "3235|3245", "3235_3245"),
#     occ10 = str_replace(occ10, "6100|6110", "6100_6110"),
#     occ10 = str_replace(occ10, "6310|6320", "6310_6320"),
#     occ10 = str_replace(occ10, "6540|6765", "6540_6765"),
#     occ10 = str_replace(occ10, "7440|7630", "7440_7630"),
#     # 7440 isn't labeled Agg because it is a 1-1 match in the ratio table.
#     type  = if_else(occ10 == "7440_7630", "Agg", type),
#     occ10 = str_replace(occ10, "8255|8256", "8255_8256"),
#     occ10 = str_replace(occ10, "8430|8460", "8430_8460"),
#     occ10 = str_replace(occ10, "8520|8550", "8520_8550"),
#     occ18 = str_replace(occ18, "1830|1860", "1830_1860"),
#     occ18 = str_replace(occ18, "3235|3245", "3235_3245"),
#     occ18 = str_replace(occ18, "6540|6765", "6540_6765"),
#     occ18 = str_replace(occ18, "7440|7640", "7440_7640"),
#     occ18 = str_replace(occ18, "8255|8256", "8255_8256")) |>
#   # In the employment data, occ10 codes 2900 and 2960 are combined. The ratios
#   # provided are not for this combined group and it is necessary to make an 
#   # assumption about what share of the combined employment is in 2900 and 
#   # what is in 2960. Since 2960 is a catchall, I assume it is the bigger group
#   # and assign it 75% of the combined employment. To do this, I adjust the 
#   # relationships for occ10 code 2900 and drop the relationships for occ10
#   # code 2960.
#   mutate(
#     ratio_10_18 = 
#       if_else(
#         occ10 == "2900" & occ18 == "2905", 
#         0.75 + 0.25 * ratio_10_18, 
#         ratio_10_18),
#     ratio_10_18 = 
#       if_else(
#         occ10 == "2900" & occ18 == "5040", 
#         0.25 * ratio_10_18, 
#         ratio_10_18),
#     occ10 = str_replace(occ10, "2900", "2900_2960"),
#     occ18 = str_replace(occ18, "2905", "2905_2970")) |>
#   filter(!(occ10 == "2960")) |>
#   # The weights don't always sum to 1.000 for some occ codes. I adjust
#   # the largest weight to make the sum 1.
#   mutate(
#     ratio_10_18 = 
#       if_else(
#         occ10 == "2630" & occ18 == "2640", 
#         ratio_10_18 - 0.0001, 
#         ratio_10_18),
#     ratio_10_18 = 
#       if_else(
#         occ10 == "3320" & occ18 == "3323", 
#         ratio_10_18 - 0.0001, 
#         ratio_10_18),
#     ratio_10_18 = 
#       if_else(
#         occ10 == "3420" & occ18 == "3424", 
#         ratio_10_18 + 0.0001, 
#         ratio_10_18),
#     ratio_10_18 = 
#       if_else(
#         occ10 == "4250" & occ18 == "4251", 
#         ratio_10_18 + 0.0001, 
#         ratio_10_18),
#     ratio_10_18 = 
#       if_else(
#         occ10 == "4520" & occ18 == "4521", 
#         ratio_10_18 + 0.0001, 
#         ratio_10_18),
#     ratio_10_18 = 
#       if_else(
#         occ10 == "9120" & occ18 == "9121", 
#         ratio_10_18 + 0.0001, 
#         ratio_10_18)) |>
#   # Join with the employment data.
#   distinct() |>
#   full_join(b24124_2017_occ10) |>
#   # Drop military occupations (9800 and higher).
#   drop_na() |>
#   mutate(emp = ratio_10_18 * emp17_10) |>
#   select(occ10, occ18, emp)
# # Employment by occ code
# occ10 <-
#   concordance |>
#   summarize(emp10 = sum(emp), .by = "occ10")
# occ18 <-
#   concordance |>
#   summarize(emp18 = sum(emp), .by = "occ18")
# # Validate employment sums. Because of adjustments made to the ratios so that 
# # the occ10 check is correct, the occ18 check will be off because those 
# # adjustments are not made in the example table in the reference document.
# occ10_check <-
#   occ10 |>
#   full_join(b24124_2017_occ10) |>
#   mutate(diff = emp10 - emp17_10)
# summary(occ10_check)
# occ18_check <-
#   occ18 |>
#   full_join(b24124_2017_occ18) |>
#   mutate(diff = emp18 - emp17_18)
# summary(occ18_check)
# # Calculate final weights.
# occ10_occ18 <-
#   concordance |>
#   full_join(occ10) |>
#   mutate(ratio = emp / emp10) |>
#   select(occ10, occ18, ratio) |>
#   write_csv("census_occ/data/refined/occ10_occ18.csv")
# occ18_occ10 <-
#   concordance |>
#   full_join(occ18) |>
#   mutate(ratio = emp / emp18) |>
#   select(occ10, occ18, ratio) |>
#   relocate(occ18, occ10, ratio) |>
#   write_csv("census_occ/data/refined/occ18_occ10.csv")
# 
# 
# 
# 
# 
# 
# 
