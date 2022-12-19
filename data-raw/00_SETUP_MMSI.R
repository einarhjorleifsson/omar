# MMSI related things ----------------------------------------------------------
SAVE <- FALSE
library(rvest)
library(countrycode)
library(tidyverse)

## MMSI country code -----------------------------------------------------------
url <- "https://en.wikipedia.org/wiki/Maritime_identification_digits"
mid <-
  url %>%
  read_html() %>%
  #html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table()
mid <-
  mid[[1]] %>%
  as_tibble() %>%
  rename(country = Country) %>%
  separate(col = "Codes", into = paste0("c", 1:20)) %>%
  gather(dummy, mid, -country) %>%
  drop_na() %>%
  select(-dummy)
mmsi_MID <-
  mid %>%
  mutate(country = case_when(country == "Alaska (State of)" ~ "United States",
                             country == "Ascension Island" ~ "Great Britain",
                             country == "Azores (Portuguese isles of)" ~ "Portugal",
                             country == "Bonaire, Sint Eustatius and Saba - Netherlands (Kingdom of the)" ~ "Netherlands",
                             country == "Crozet Archipelago" ~ "France",
                             country == "Curaçao - Netherlands (Kingdom of the)" ~ "Netherlands",
                             country == "Guiana (French Department of)" ~ "France",
                             country == "Kerguelen Islands" ~ "France",
                             country == "Madeira (Portuguese isles of)" ~ "Portugal",
                             country == "Rwandese Republic" ~ "Rwanda",
                             country == "Saint Paul and Amsterdam Islands" ~ "France",
                             TRUE ~ country)) %>% 
  mutate(flag = countrycode(country, "country.name", "iso3c")) %>%
  select(MID = mid, FLAG = flag)

if(SAVE) {
  DBI::dbWriteTable(con, name = "MMSI_MID", value = mmsi_MID, overwrite = TRUE)
}

## Icelandic MMSI registry -----------------------------------------------------

### Most recent ----------------------------------------------------------------
# 2022-12-02
# https://www.fjarskiptastofa.is/english/telecom-affairs/maritime-communications
# "In the following registers information can be found about numbers that have 
#  been allocated to Icelandic ships: MMSI (DSC and 406 MHz emergency beacons), 
#  INMARSAT Standard A, Standard B, Standard C and Standard M and 
#  Selcall (Radiotelex).
#   Updated November 17th 2022."
pth <- "https://www.fjarskiptastofa.is/library?itemid=1e46976b-5fed-4431-94dc-7c15f69fb54c"
download.file(pth, destfile = "tmp.xlsx")
v_mmsi <-
  readxl::read_excel("tmp.xlsx") %>%
  janitor::clean_names() %>%
  dplyr::rename(ACTIVE = x5) |> 
  dplyr::mutate(ACTIVE = ifelse(ACTIVE == "0", "No", "Yes")) |> 
  dplyr::select(SKNR = sknr,
                NAME = skip,
                CS = kallm,
                MMSI = mmsi_nr,
                STDC = standard_c,
                ACTIVE,
                dplyr::everything()) %>%
  dplyr::mutate(VID = dplyr::case_when(stringr::str_sub(MMSI, 1, 3) == "251" ~ as.integer(SKNR),
                                       TRUE ~ NA_integer_),
                VID2 = dplyr::case_when(stringr::str_sub(MMSI, 1, 3) != "251" ~ as.integer(SKNR),
                                        TRUE ~ NA_integer_)) %>%
  dplyr::select(SKNR, VID, VID2, NAME, MMSI, CS, dplyr::everything()) %>%
  dplyr::arrange(VID)
colnames(v_mmsi) <-toupper(colnames(v_mmsi))
if(SAVE) {
  con <- omar::connect_mar()
  DBI::dbWriteTable(con, name = "MMSI_ICELANDIC_REGISTRY", value = v_mmsi, overwrite = TRUE)
}

### Historical stuff -----------------------------------------------------------
# pth <- "https://www.pfs.is/library/Skrar/Tidnir-og-taekni/Numeramal/MMSI/NUMER%20Query270619.xlsx"
# download.file(pth, destfile = "data-raw/ss-270619_mmsi.xlsx")
# v_mmsi <-
#   readxl::read_excel("data-raw/ss-270619_mmsi.xlsx") %>%
#   janitor::clean_names() %>%
#   dplyr::select(SKNR = sknr,
#                 NAME = skip,
#                 CS = kallm,
#                 MMSI = mmsi_nr,
#                 STDC = standard_c) %>%
#   dplyr::mutate(VID = case_when(str_sub(MMSI, 1, 3) == "251" ~ as.integer(SKNR),
#                                 TRUE ~ NA_integer_),
#                 VID2 = case_when(str_sub(MMSI, 1, 3) != "251" ~ as.integer(SKNR),
#                                  TRUE ~ NA_integer_)) %>%
#   dplyr::select(SKNR, VID, VID2, NAME, MMSI, CS) %>%
#   dplyr::arrange(VID)
# dbWriteTable(con, name = "VESSEL_MMSI_20190627", value = v_mmsi, overwrite = TRUE)

# # updated table 2021-01-06
# pth <- "https://www.pfs.is/library/Skrar/Tidnir-og-taekni/Numeramal/MMSI/NUMER%20Query151220.xlsx"
# download.file(pth, destfile = "tmp.xlsx")
# v_mmsi <-
#   readxl::read_excel("tmp.xlsx") %>%
#   janitor::clean_names() %>%
#   dplyr::select(SKNR = sknr,
#                 NAME = skip,
#                 CS = kallm,
#                 MMSI = mmsi_nr,
#                 STDC = standard_c) %>%
#   dplyr::mutate(VID = dplyr::case_when(stringr::str_sub(MMSI, 1, 3) == "251" ~ as.integer(SKNR),
#                                        TRUE ~ NA_integer_),
#                 VID2 = dplyr::case_when(stringr::str_sub(MMSI, 1, 3) != "251" ~ as.integer(SKNR),
#                                         TRUE ~ NA_integer_)) %>%
#   dplyr::select(SKNR, VID, VID2, NAME, MMSI, CS) %>%
#   dplyr::arrange(VID)
# DBI::dbWriteTable(con, name = "VESSEL_MMSI_20201215", value = v_mmsi)


