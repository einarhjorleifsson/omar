# All vessel related things ----------------------------------------------------
#  For Icelandic MMSI registry see data-raw/00_SETUP_MMSI.R

SAVE <- FALSE
library(tidyverse)
library(omar)
con <- connect_mar()


## Umdæmisbókstafir íslenskra skipa --------------------------------------------
ust <-
  tribble(~UST, ~STADUR,
          "AK", "Akranes",
          "NS", "Norður-Múlasýsla og Seyðisfjörður",
          "ÁR", "Árnessýsla",
          "ÓF", "Ólafsfjörður",
          "BA", "Barðastrandarsýsla",
          "RE", "Reykjavík",
          "DA", "Dalasýsla",
          "SF", "Austur-Skaftafellssýsla",
          "EA", "Eyjafjarðarsýsla og Akureyri",
          "SH", "Snæfellsness-og Hnappadalssýsla",
          "GK", "Gullbringusýsla",
          "SI", "Siglufjörður",
          "HF", "Kjósarsýsla og Hafnarfjörður",
          "SK", "Skagafjarðarsýsla og Sauðárkrókur",
          "HU", "Húnavatnssýsla",
          "ST", "Strandasýsla",
          "ÍS", "Ísafjarðarsýsla",
          "SU", "Suður-Múlasýsla",
          "KE", "Keflavík",
          "VE", "Vestmannaeyjar",
          "KÓ", "Kópavogur",
          "VS", "Vestur-Skaftafellssýsla",
          "MB", "Mýra-og Borgarfjarðarsýsla",
          "ÞH", "Þingeyjarsýslur",
          "NK", "Neskaupstaður")
if(SAVE) {
  dbWriteTable(con, name = "VESSEL_UMDAEMISBOKSTAFIR", value = ust, overwrite = TRUE)
}

## Vessel class - get the proper one from siglo --------------------------------
vclass <-
  tribble(~code, ~flokkur, ~class,
          0L,      "unspecified", "unspecified",
          33L,     "FISKISKIP",   "fishing",
          35L,     "SKUTTOGARI", "fishing",
          36L,     "NÓTAVEIÐI, SKUTTOGARI", "fishing",
          37L,     "HVALVEIÐISKIP",    "whaler",
          38L,     "unspecified", "unspecified",
          39L,     "vöruflutningaskip", "cargo",
          40L,     "unspecified", "unspecified",
          41L,     "FARÞEGASKIP", "passenger",
          42L,     "VARÐSKIP", "coast guard",
          43L,     "SKÓLASKIP", "school ship",
          44L,     "RANNSÓKNARSKIP", "research",
          45L,     "SJÓMÆLINGASKIP", "research",
          46L,     "BJÖRGUNARSKIP", "sar",
          48L,     "OLÍUFLUTNINGASKIP", "tanker",
          49L,     "olíuskip", "tanker",
          50L,     "DRÁTTARSKIP", "tug boat",
          51L,     "unspecified", "unspecified",
          53L,     "LÓÐSSKIP", "pilot vessel",
          54L,     "VINNUSKIP", "utility vessel",
          55L,     "DÝPK. OG SANDSKIP", "hopper dredger",
          56L,     "DÝPKUNARSKIP", "dredger",
          57L,     "PRAMMI", "barge",
          58L,     "FLOTBRYGGJA", "flotbryggja",
          59L,     "FLOTKVÍ", "flotkví",
          60L,     "SEGLSKIP", "sailing vessel",
          61L,     "VÍKINGASKIP", "longboat",
          62L,     "SKEMMTISKIP", "passenger?",
          63L,     "AFSKRÁÐUR", "Decomissioned",
          64L,     "FISKI, FARÞEGASKIP", "turist fisher",
          65L,     "HAFNSÖGU, DRÁTTARSKIP", "pilot/tugboat",
          66L,     "ÞANGSKURÐARPRAMMI", "kelp vessel",
          67L,     "unspecified", "unspecified",
          68L,     "FRÍSTUNDAFISKISKIP", "pleasure vessel",
          69L,     "EFTIRLITS- OG BJÖRGUNARSKIP", "unspecified",
          70L,     "unspecified", "unspecified",
          73L,     "FARÞEGABÁTUR", "passenger",
          74L,     "FISKI, FARÞEGABÁTUR", "turist fisher",
          75L,     "SJÓKVÍA VINNUSKIP", "utility vessel",
          NA_integer_, NA_character_, NA_character_) %>%
  select_all(toupper)
vclass %>% count(CODE) %>% filter(n > 1)
vclass %>% count(FLOKKUR) %>% filter(n > 1)
vclass %>% count(CLASS) %>% filter(n > 1)
if(SAVE) {
  dbWriteTable(con, name = "VESSEL_CLASS", value = vclass, overwrite = TRUE)
}

## Vessel call signs - ITU prefixes --------------------------------------------
# library(rvest)
# library(countrycode)
# library(tidyverse)
#url <- "https://en.wikipedia.org/wiki/ITU_prefix#Allocation_table"
# need to check Swaziland and Fiji
# x <-
#   rio::import("../ITU_prefix.csv", setclass = "tibble") %>%
#   janitor::clean_names() %>%
#   rename(cs = call_sign_series, cntr = allocated_to) %>%
#   mutate(cntr = str_replace(cntr, "\\[Note 1\\]", ""),
#          cntr = str_replace(cntr, "\\[Note 2\\]", ""),
#          cntr = str_replace(cntr, "\\[Note 4\\]", ""),
#          cntr = ifelse(str_starts(cntr, "France"), "France", cntr),
#          cntr = ifelse(str_starts(cntr, "United Kingdom"), "United Kingdom", cntr),
#          cntr = ifelse(str_starts(cntr, "Canada"), "Canada", cntr),
#          cntr = ifelse(str_starts(cntr, "Hong Kong"), "Hong Kong", cntr),
#          cntr = ifelse(str_starts(cntr, "Macao"), "Macao", cntr),
#          cntr = ifelse(str_starts(cntr, "Netherlands"), "Netherlands", cntr),
#          cntr = ifelse(str_detect(cntr, "Bosnia and Herzegovina"), "Bosnia and Herzegovina", cntr)) %>%
#   filter(!cntr %in% c("", "Republic of China (Taiwan)",
#                       "Liechtenstein (uses prefixes allocated to Switzerland)",
#                       "Swaziland", "Fiji")) %>%
#   add_row(cs = "BM-BQ", cntr = "Republic of China (Taiwan)") %>%
#   add_row(cs = "BU-BX", cntr = "Republic of China (Taiwan)") %>%
#   add_row(cs = c("HB0", "HB3Y", "HBL"), cntr = rep("Liechtenstein", 3)) %>%
#   separate(cs, c("from", "to", "to2"), remove = FALSE) %>%
#   mutate(to = ifelse(!is.na(to), to, from),
#          first = str_sub(from, 1, 1),
#          t1 = str_sub(from, 2, 2),
#          t2 = str_sub(to,   2, 2))
#
# # Poor mans loop
# n <- length(c(1:9, LETTERS))
# ltrs <- 1:n
# names(ltrs) <- c(1:9, LETTERS)
# # NOTE: Needs further work
# res <- list()
# for(i in 1:nrow(x)) {
#   print(i)
#   if(x$cs[[i]] %>% nchar() == 1) {
#     res[[i]] <- tibble(cs = x$cs[[i]], cntr = x$cntr[[i]])
#   } else {
#     res[[i]] <-
#       tibble(cs = paste0(x$first[[i]], names(ltrs[ltrs[[x$t1[[i]]]]:ltrs[[x$t2[[i]]]]])),
#              cntr = x$cntr[[i]])
#   }
# }
# ITU_prefix <-
#   bind_rows(res) %>%
#   mutate(iso2 = countrycode(cntr, "country.name", "iso2c"),
#          iso3 = countrycode(cntr, "country.name", "iso3c")) %>%
#   rename(CS_PREFIX = cs, COUNTRY = cntr, ISO2 = iso2)
#
# dbWriteTable(con, name = "VESSEL_CS_ITU_PREFIX", value = ITU_prefix, overwrite = TRUE)

