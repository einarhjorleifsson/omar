# Change log
# 2022-02-02:
#   * Moved scripts from ~/stasi/oracle_sjor
# 2022-07-03:
#   * changed script name

# Notes
# 

# Libraries --------------------------------------------------------------------
library(data.table)
library(readxl)
library(pdftools)
library(tidyverse)
library(lubridate)
library(knitr)
library(omar)
con <- connect_mar()


# GEAR CORRECTION --------------------------------------------------------------

# Logbooks ---------------------------------------------------------------------
YEARS <- 1993:2022
## 0. Functions ----------------------------------------------------------------
match_nearest_date <- function(lb, ln) {
  
  lb.dt <-
    lb %>%
    select(vid, datel) %>%
    distinct() %>%
    setDT()
  
  ln.dt <-
    ln %>%
    select(vid, datel) %>%
    distinct() %>%
    mutate(dummy = datel) %>%
    setDT()
  
  res <-
    lb.dt[, date.ln := ln.dt[lb.dt, dummy, on = c("vid", "datel"), roll = "nearest"]] %>%
    as_tibble()
  
  lb %>%
    left_join(res,
              by = c("vid", "datel")) %>%
    left_join(ln %>% select(ID, vid, date.ln = datel, gid.ln, sid.target.ln),
              by = c("vid", "date.ln"))
  
}
# end: 0. Functions

## 1 get and prepare data ------------------------------------------------------

lb <-
  omar:::lb_base(con) %>%
  filter(year %in% YEARS) %>%
  select(vid, visir, date, datel, gid.lb = gid, lon, lat) %>%
  left_join(omar::lb_catch(con),
            by = "visir") %>%
  collect(n = Inf) %>%
  arrange(visir, sid) %>%
  group_by(vid, visir, datel) %>%
  mutate(date = as_date(date),
         datel = as_date(datel),
         total = sum(catch, na.rm = TRUE),
         p = catch / total,
         n.sid = n()) %>%
  arrange(visir, date,  desc(p), sid) %>%
  group_by(visir, date) %>%
  slice(1) %>%
  ungroup() %>%
  # this may not always be true, e.g. in nephrops fisheries that species may
  #  not be the most dominant species in the catch.
  rename(sid.target = sid)
ln <-
  omar:::ln_catch(con) %>%
  filter(!is.na(vid), !is.na(date)) %>%
  mutate(year = year(date)) %>%
  filter(year %in% YEARS) %>%
  collect(n = Inf) %>%
  mutate(date = as_date(date)) %>%
  group_by(vid, ID, date, sid) %>%
  mutate(total = sum(catch, na.rm = TRUE),
         p = catch / total,
         n.sid = n()) %>%
  arrange(ID, date,  desc(p), sid) %>%
  group_by(ID, date) %>%
  slice(1) %>%
  ungroup() %>%
  select(vid, ID, gid.ln = gid, datel = date, sid.target.ln = sid)

## 2. nearest date match -------------------------------------------------------
d <-
  lb %>%
  match_nearest_date(ln) %>%
  # just take the first match, i.e. ignore any second landings match
  distinct(visir, .keep_all = TRUE)

## 3. Gear corrections ---------------------------------------------------------
gears <- 
  gid_orri_plus(con) %>% 
  collect(n = Inf) %>% 
  mutate(gid = as.integer(gid),
         gid2 = as.integer(gid2))


# Steps here were collated kind of interactively

### Part A - first check if gear class the same --------------------------------

d <-
  d %>%
  left_join(gears %>% select(gid.lb = gid, gc.lb = gid2), by = "gid.lb") %>%
  left_join(gears %>% select(gid.ln = gid, gc.ln = gid2), by = "gid.ln") %>%
  select(visir:gid.ln, gc.lb, gc.ln, everything()) %>%
  mutate(gid = NA_integer_,
         gid.source = NA_character_) %>%
  # 1. gears and gear class the same
  mutate(i = gid.lb == gid.ln & gc.lb == gc.ln,
         gid = ifelse(i, gid.ln, gid),
         gid.source = ifelse(i, "gid.lb=gid.ln & gc.lb= gc.ln", gid.source),
         step = ifelse(i, 1L, NA_integer_)) %>%
  # 2. class gillnet, use ln gid
  mutate(i = gc.ln == 2 & gc.lb == 2,
         gid = ifelse(i, gid.ln, gid),
         gid.source = ifelse(i, "gc.lb=gc.ln=2   -> gid.ln", gid.source),
         step = ifelse(i, 2L, step)) %>%
  # 2. class purse seine, use ln gid except if ln.gid == 4
  mutate(i = gc.ln == 4 & gc.lb == 4,
         gid = case_when(i & gid.ln == 4L ~ gid.lb,
                         i ~ gid.ln,
                         TRUE ~ gid),
         gid.source = ifelse(i, "gc.lb=gc.ln=4   -> gid.ln", gid.source),
         step = ifelse(i, 2L, step)) %>%
  # 3. danish seine class, use ln gid
  mutate(i = gc.ln == 5 & gc.lb == 5,
         gid = ifelse(i, gid.ln, gid),
         gid.source = ifelse(i, "gc.lb=gc.ln=5   -> gid.ln", gid.source),
         step = ifelse(i, 3L, step)) %>%
  # 4. pelagic class, use ln gid
  mutate(i = gc.ln == 7 & gc.lb == 7,
         gid = ifelse(i, gid.ln, gid),
         gid.source = ifelse(i, "gc.lb=gc.ln=7   -> gid.ln", gid.source),
         step = ifelse(i, 4L, step)) %>%
  # 5. nephrops use gid 9
  mutate(i = gc.ln == 9 & gc.lb == 9,
         gid = ifelse(i, 9L, gid),
         gid.source = ifelse(i, "gc.lb=gc.ln=9   -> 9", gid.source),
         step = ifelse(i, 5L, step)) %>%
  # 6. shrimp use gid 14
  mutate(i = gc.ln == 14 & gc.lb == 14,
         gid = ifelse(i, 14L, gid),
         gid.source = ifelse(i, "gc.lb=gc.ln=14   -> 14", gid.source),
         step = ifelse(i, 6L, step))

### Part B steps done chronologically and recursivly by drilling through the most
#  prevalent mismatches, like:
d %>%
  filter(is.na(step)) %>%
  count(gid.ln, gid.lb, sid.target) %>%
  arrange(-n) %>%
  slice(1:20)
#  But exclude the dredges here, done further downstream

# useful downstream??
# vids <-
#   omar:::vid_registry(con) %>%
#   select(vid, length = length) %>%
#   collect(n = Inf)

# Cases where gid.lb, gid.ln and sid.target records are > 1000
d <-
  d %>%
  mutate(gid = case_when(gid.ln == 6L & gid.lb == 14L & sid.target == 41L ~ 14L,
                         # next two lines are redfish - check with KK
                         gid.ln == 6L & gid.lb == 7L & sid.target == 11L ~ 7L,
                         gid.ln == 6L & gid.lb == 7L & sid.target == 61L ~ 6L,
                         # largely based on vessel class check:
                         gid.ln == 1L & gid.lb == 3L ~ gid.lb,
                         # checked by sid.target and sid.target.ln
                         gid.ln == 6L & gid.lb == 7L & sid.target == 1 ~ gid.ln,
                         gid.ln == 7L & gid.lb == 6L & sid.target == 1 ~ gid.lb,
                         gid.ln == 7L & gid.lb == 6L & sid.target == 22 ~ gid.lb,
                         # not sure what to do here
                         #gid.ln == 3L & gid.lb == 1L & sid.target == 1 ~ XXXX,
                         gid.ln == 6L & gid.lb == 9L & sid.target == 40 ~ gid.lb,
                         gid.ln == 7L & gid.lb == 6L & sid.target == 5  ~ gid.lb,
                         gid.ln == 14L & gid.lb == 6L & sid.target == 22 ~ gid.ln,
                         # dubious what to do with the next two
                         #gid.ln == 2L & gid.lb == 5L & sid.target == 1 ~ XXX,
                         #gid.ln == 1L & gid.lb == 2L & sid.target == 1 ~ XXX,
                         gid.ln == 7L & gid.lb == 6L & sid.target == 2 ~ gid.lb,
                         gid.ln == 6L & gid.lb == 14L & sid.target == 1 ~ gid.lb,
                         gid.ln == 14L & gid.lb == 6L & sid.target == 1 ~ gid.lb,
                         gid.ln == 7L & gid.lb == 6L & sid.target == 3 ~ gid.lb,
                         gid.ln == 6L & gid.lb == 5L & sid.target == 23 ~ gid.lb,
                         gid.ln == 6L & gid.lb == 7L & sid.target == 5 ~ gid.ln,
                         TRUE ~ gid))

# if gid.lb is on target species
d <-
  d %>%
  #                      flatfish
  mutate(gid = case_when(gid.lb %in% c(5, 26, 27, 35, 55, 80) & sid.target %in% c(23:29) ~ gid.lb,
                         # nephrops
                         gid.lb == 9L & sid.target == 40 ~ gid.lb,
                         # shrimp
                         gid.lb == 14L & sid.target == 41 ~ gid.lb,
                         TRUE ~ gid))

sids <- omar:::sid_orri(con) %>% collect() %>% select(sid, tegund)

d <-
  d %>%
  #                      sæbjúga
  mutate(gid = case_when(sid.target.ln == 199L & sid.target == 199L ~ -199L,
                         # ígulker
                         sid.target.ln == 191L & sid.target == 191L ~ 40L,
                         # kúffiskur
                         sid.target.ln == 46L & sid.target == 46L ~ 38L,
                         # hörpudiskur
                         sid.target.ln == 43L & sid.target == 43L ~ 15L,
                         # beitukóngur
                         sid.target.ln == 45L & sid.target == 45L ~ 39L,
                         TRUE ~ gid)) %>%
  # sæbjúgu again
  mutate(gid = case_when(gid.ln == 15L & sid.target.ln == 199L ~ -199L,
                         # ígulker
                         gid.ln == 40L & sid.target.ln == 191L ~ 40L,
                         gid.lb == 40L & sid.target.ln == 191L ~ 40L,
                         TRUE ~ gid))

# 2021-07-02: added refinement on gill nets
d <- 
  d %>% 
  #                      # grálúðunet
  mutate(gid = case_when(gid == 2 & sid.target == 22 ~ 92L,
                         # skötuselsnet
                         gid == 2 & sid.target == 14 ~ 91L,
                         TRUE ~ gid))

# just assume that gid.lb is correct for the rest, would though be
#  nice to go over the pelagics again. and then the lesser species/gears
d <-
  d %>%
  mutate(gid = ifelse(is.na(gid), gid.lb, gid)) %>%
  mutate(gid = ifelse(is.na(gid), gid.ln, gid)) %>%
  # loose two records
  filter(!is.na(gid))

d %>%
  mutate(same = ifelse(gid == gid.lb, TRUE, FALSE)) %>%
  count(same) %>%
  mutate(p = n / sum(n))

d.final <-
  d %>%
  select(VISIR = visir, ID, GID = gid)
DBI::dbWriteTable(con, name = "LB_GEAR_CORRECTION", value = d.final, overwrite = TRUE)

# On gear code -----------------------------------------------------------------

# remotes::install_github("ices-tools-prod/icesVocab", force = TRUE)
library(pdftools)
library(tidyverse)
library(icesVocab)
library(omar)
con <- connect_mar()

## FAO: Revised International Standard Classification of Fishing Gears  --------
if(FALSE) {
  tmpfile <- tempfile()
  download.file("http://www.fao.org/fishery/docs/DOCUMENT/cwp/handbook/annex/AnnexM2fishinggear.pdf",
                destfile = tmpfile)
  pdftools::pdf_text(tmpfile) %>% writeLines(tmpfile)
  readLines(tmpfile)
  rename(gear = x1,
         group = x2,
         isscfg = x3)
  d <- 
    readLines(tmpfile)[c(12:52, 62:92)] %>% 
    as_tibble() %>% 
    separate(value, into = c("gear_fao", "gid_fao", "isscfg"), sep = c(52, 80)) %>% 
    mutate(gear_fao = str_trim(gear_fao),
           gid_fao = str_trim(gid_fao),
           isscfg = str_trim(isscfg)) %>% 
    mutate(gear_fao = ifelse(gear_fao == "Hand implements (Wrenching gear, Clamps, Tongs, ",
                             "Hand implements (Wrenching gear, Clamps, Tongs, Rakes, Spears)",
                             gear_fao)) %>% 
    filter(gear_fao != "Rakes, Spears)")
  d %>% write_csv("inst/csv/gear_fao_isscfg.csv")
}
gear_fao <- 
  read_csv("inst/csv/gear_fao_isscfg.csv",
           show_col_types = FALSE)
gear_fao <- 
  gear_fao %>% 
  filter(!is.na(gid_fao)) %>% 
  mutate(isscfg_prefix = str_sub(isscfg, 1, 2)) %>% 
  left_join(gear_fao %>% 
              filter(is.na(gid_fao)) %>% 
              select(gear_class_fao = gear_fao,
                     isscfg_prefix = isscfg))
colnames(gear_fao) <- gear_fao %>% colnames() %>% toupper()
DBI::dbWriteTable(con, name = "GEAR_FAO", value = gear_fao, overwrite = TRUE)

## Mapping orri gear to fao gear -----------------------------------------------
if(FALSE) {
  tbl_mar(con, "orri.veidarfaeri") %>% 
    select(veidarfaeri:lods_veidarfaeri, lysing_enska) %>% 
    collect(n = Inf) %>% 
    arrange(veidarfaeri) %>% 
    select(gid = veidarfaeri,
           gid_fi = fi_veidarfaeri,
           gid_lods = lods_veidarfaeri,
           veiðarfæri = lysing,
           gear = lysing_enska) %>% 
    write_csv("inst/csv/gear_orri.csv")
}
gear_orri <- 
  read_csv("inst/csv/gear_orri.csv", show_col_types = FALSE)




## Other stuff -----------------------------------------------------------------
# fao <- omar::gid_isscfg_category(con) %>% collect()
# fao2 <- omar::gid_isscfg(con) %>% collect()
bthe_gear_type <- 
  tbl_mar(con, 'ops$bthe."gear_mapping"') %>% 
  collect() %>% 
  arrange(veidarfaeri)
IC_gear_type <- getCodeList("IC_GearType")

