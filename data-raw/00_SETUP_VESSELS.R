# VESSEL REGISTRY --------------------------------------------------------------
#
# This is an attempt to create the most complete up-to-date vessel registry of
#  Icelandic vessels or foreign vessels that are allowed to fish/land in 
#  Iceland/Icelandic waters
# It is constructed because, and possible despite, the current lack of up-to-date
#  vessel registration in the MRI institute.
# The approach used here is that information ...  describe here what source
#  supersedes other sources ...
#
# The output: ops$einarhj.vessels_vessels
# 2023-04-06: Update, include last change in management class
# 2023-01-09: First generation

library(omar)
library(tidyverse)
con <- connect_mar()
lh <- function(d) {
  d |> 
    mutate(vessel = str_replace(vessel, "\\.", ""),
           vessel = str_squish(vessel),
           vessel = str_to_title(vessel),
           vessel = case_when(str_ends(vessel, "Iii") ~ str_replace(vessel, "Iii", "III"),
                              str_ends(vessel, "Ii") ~  str_replace(vessel, "Ii", "II"),
                              str_ends(vessel, "Lll") ~  str_replace(vessel, "Lll", "III"),
                              str_ends(vessel, "Ll") ~   str_replace(vessel, "Ll", "II"),
                              TRUE ~ vessel)) |> 
    mutate(vessel = case_when(vessel == "Hamrasvanurii" ~"Hamrasvanur II",
                              vessel == "Guðmundur Bþorláksson" ~ "Guðmundur B Þorláksson",
                              TRUE ~ vessel))
}
lh_replace_0 <- function(x) {
  ifelse(x == 0, NA, x)
}
# Official vessel registry (Samgongustofa) -------------------------------------
siglo <- 
  vessel_registry(con) |> 
  collect(n = Inf) |> 
  arrange(vid) |> 
  lh()

# Official registry - scraped from web -----------------------------------------
scrape <-
  read_rds("~/stasi/skipaskra/data/siglo-scraped_2022-12-28.rds") |> 
  arrange(vid) |> 
  lh() |> 
  rename(gv = brl, gt = brt, skipaflokkur = type) |> 
  separate(uid, into = c("uid", "dummy"), sep = "\\(") |> 
  select(-dummy) |> 
  separate(uid, into = c("uid", "uno"), sep = "-") |> 
  mutate(uid = str_squish(uid),
         uid = ifelse(uid == "", NA, uid),
         uno = str_squish(uno),
         uno = ifelse(uno %in% c("", "."), NA, uno),
         uno = as.integer(uno),
         length = ifelse(length <= 1, NA, length),
         gv = ifelse(gv == 0, NA, gv),
         gt = ifelse(gt == 0, NA, gt)) |> 
  select(vid, vessel, uid, uno, length:gt, skipaflokkur)
# Directory of fisheries (Fiskistofa) registry ---------------------------------
saga <- 
  vessel_history(con) |>
  collect(n = Inf) |> 
  arrange(vid, desc(histno)) |> 
  # Foreign vessels have callsign in 
  separate(vessel, into = c("vessel", "cs"), sep = "\\(") |>
  mutate(vessel = str_squish(vessel),
         cs = str_replace(cs, "\\)", ""),
         country = case_when(foreign == 1 ~ countrycode::countrycode(uid, origin = "iso2c", destination = "country.name"),
                             TRUE ~ "Iceland"),
         # revert back to standard code
         country = countrycode::countrycode(country, origin = "country.name", destination = "iso2c"),
         uid = ifelse(foreign == 1, NA, uid)) |> 
  group_by(vid) |> 
  filter(histno == min(histno)) |> 
  ungroup() |> 
  mutate(vessel = ifelse(vessel == "Óskráður", NA, vessel))
# mbl scraped ------------------------------------------------------------------
an <- as.numeric
ai <- as.integer

# only one or none "."
lh_remove_first_dot <- function(x) {
  orgiginal <- nchar(x)
  stripped <- str_remove_all(x, "\\.") |> nchar()
  cond <- ifelse(orgiginal-stripped <= 1, TRUE, FALSE)
  ifelse(cond, x, str_remove(x, "\\."))
}

fil <- dir("/net/hafkaldi.hafro.is/export/home/haf/einarhj/stasi/skipaskra/data-raw/mbl", full.names = TRUE)
mbl <- 
  fil |> 
  map(read_csv, show_col_types = FALSE, col_types = "cci")

mbl <- 
  mbl |> 
  bind_rows() |> 
  select(variable, value, vid) |> 
  filter(!is.na(variable)) |> 
  # get rid of "empty" vessels
  group_by(vid) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  filter(n > 3) |> 
  select(-n) |> 
  spread(variable, value) |> 
  janitor::clean_names() |> 
  filter(vid > 5) |> 
  rename(width_reg = breidd,
         gv = bruttorumlestir,
         gt = bruttotonn,
         depth_reg = dypt,
         hull = efni_i_bol,
         vessel_old = fyrra_nafn,
         harbour = heimahofn,
         hp = hestofl,
         cs = kallmerki,
         length = mesta_lengd,
         vessel = nafn,
         nt = nettotonn,
         phone = simi,
         length_reg = skrad_lengd,
         year_built = smidaar,
         place_built = smidastadur,
         factory_built = smidastod,
         vclass = tegund,
         operator = utgerd,
         mclass = utgerdarflokkur,
         engine = vel,
         url = heimasida) |> 
  select(-skipanr) |> 
  mutate(width_reg = str_remove(width_reg, " m") |> an(),
         gv = lh_remove_first_dot(gv) |> an(),
         gt = str_remove(gt, " t") |> lh_remove_first_dot() |> an(),
         depth_reg = str_remove(depth_reg, " m") |> an(),
         hp = lh_remove_first_dot(hp) |> an(),
         imo = str_remove(imo, "IMO") |> ai(),
         length = str_remove(length, " m") |> lh_remove_first_dot() |> an(),
         mmsi = ai(mmsi),
         nt = lh_remove_first_dot(nt) |> an(),
         length_reg = str_remove(length_reg, " m") |> lh_remove_first_dot() |> an(),
         year_built = ai(year_built),
         loc = str_locate(vessel, "\\-")[,1] - 2,
         uid = str_sub(vessel, loc) |> str_trim(),
         vessel = ifelse(is.na(loc), vessel, str_sub(vessel, 1, loc - 1)),
         vessel = str_trim(vessel),
         vessel = case_when(str_ends(vessel, "Iii") ~ str_replace(vessel, "Iii", "III"),
                            str_ends(vessel, "Ii")  ~ str_replace(vessel, "Ii", "II"),
                            TRUE ~ vessel)) |> 
  select(-loc) |> 
  separate(uid, into = c("uid", "uno"), sep = "-") |> 
  mutate(uno = ifelse(uno == "", NA, uno),
         uno = ai(uno)) 

mbl <- 
  mbl |> 
  select(vid, vessel, uid, uno, length, length_reg, gv, gt, vclass, 
         width_reg,
         depth_reg,
         cs,
         imo,
         mmsi,
         harbour, 
         everything())
mbl <- 
  mbl |> 
  mutate(cs = str_remove(cs, "-"))
# mmsi -------------------------------------------------------------------------
mmsi <- 
  omar::mmsi_icelandic_registry(con) |> 
  collect(n = Inf) |> 
  filter(is.na(vid2)) |> 
  filter(!is.na(vid)) |> 
  select(vid, vessel = name, mmsi, cs) |> 
  mutate(loc = str_locate(vessel, "-")[,1] - 3,
         uid = ifelse(!is.na(loc), str_sub(vessel, loc), NA),
         uid = str_trim(uid),
         vessel = ifelse(!is.na(loc), str_sub(vessel, 1, loc), vessel),
         vessel = str_trim(vessel),
         vessel = str_to_title(vessel),
         vessel = case_when(str_ends(vessel, "Iii") ~ str_replace(vessel, "Iii", "III"),
                            str_ends(vessel, "Ii")  ~ str_replace(vessel, "Ii", "II"),
                            TRUE ~ vessel)) |> 
  separate(uid, into = c("uid", "uno"), sep = "-") |> 
  mutate(uno = ifelse(uno == "", NA, uno),
         uno = as.numeric(uno),
         mmsi = as.integer(mmsi)) |> 
  select(-loc)
channel <-
  vessel_channel(con) |> 
  collect(n = Inf)
combo <- 
  siglo |> 
  select(vid) |> mutate(siglo = TRUE) |> 
  full_join(scrape |> select(vid) |> mutate(scrape = TRUE)) |> 
  full_join(saga |> select(vid) |> mutate(saga = TRUE)) |> 
  full_join(mbl  |> select(vid) |> mutate(mbl = TRUE)) |>
  full_join(mmsi |> select(vid) |> mutate(mmsi = TRUE)) |> 
  full_join(channel |> select(vid) |> mutate(channel = TRUE)) |> 
  arrange(vid) |>  
  filter(!vid %in% c(0, 1, 3, 4, 5))
combo |> 
  gather(var, val, -vid) |> 
  mutate(val = replace_na(val, FALSE)) |> 
  count(var, val) |> 
  spread(val, n) |> 
  arrange(desc(`TRUE`))

# Note: If variables x1 and x2 are not NA, then x2 supersedes x1
lh_replace <- function(x1, x2) {
  case_when( is.na(x1) & !is.na(x2) ~ x2,
             !is.na(x2) &  is.na(x2) ~ x1,
             x1 != x2 ~ x2,
             TRUE ~ x1)
}

# join siglo and scrape --------------------------------------------------------
#  here the scrape has the more recent information, thus variables get
#  overwritten
d <- 
  combo |> 
  select(vid) |> 
  left_join(siglo) |> 
  left_join(scrape |> select(vid, v2 = vessel, uid2 = uid, uno2 = uno, l2 = length, gv2 = gv, gt2 = gt)) |> 
  mutate(vessel = lh_replace(vessel, v2),
         uid = lh_replace(uid, uid2),
         uno = lh_replace(uno, uno2),
         length_reg =  lh_replace(length_reg, l2),
         gv = lh_replace(gv, gv2),
         gt = lh_replace(gt, gt2)) |> 
  select(-c(v2, uid2, uno2, l2, gv2, gt2))
# add foreign info from saga ---------------------------------------------------
#  also here we get the management class (mclass) and time of last change (t1)
d <- 
  d |> 
  left_join(saga |> 
              select(vid, v2 = vessel, uid2 = uid, uno2 = uno, l2 = length, 
                     lr2 = length_reg, w2 = width_reg, z2 = depth_reg,
                     gv2 = gv, gt2 = gt, cs2 = cs, cntr = country, mclass, t1)) |> 
  mutate(foreign = ifelse(between(vid, 3700, 4999), TRUE, FALSE),
         country = ifelse(foreign, cntr, "IS"), 
         vessel = ifelse(is.na(vessel), v2, vessel),
         uid = ifelse(is.na(uid), uid2, uid),
         cs = ifelse(is.na(cs), cs2, cs),
         length = ifelse(is.na(length), l2, length),
         gt = ifelse(is.na(gt), gt2, gt),
         gv = ifelse(is.na(gv), gv2, gv),
         length_reg = ifelse(is.na(length_reg), lr2, length_reg),
         width_reg = ifelse(is.na(width_reg), w2, width_reg),
         depth_reg = ifelse(is.na(depth_reg), z2, depth_reg)) |> 
  select(-c(v2:cntr))

### get MMSI from mmsi, also check the cs
d <- 
  d |> 
  left_join(mmsi |> 
              select(vid, 
                     cs2 = cs,
                     mmsi)) |> 
  mutate(cs = lh_replace(cs, cs2)) |> 
  select(-cs2)
         
# check the mbl ----------------------------------------------------------------
#  various checks indicate that it does not add much value
d <- 
  d |> 
  # select(-t1) |> 
  arrange(vid) |> 
  select(vid:length, mclass, date_mclass = t1, vclass, cs, imo, mmsi, everything())

names(d) <- toupper(names(d))
d <- d |> arrange(VID)
# problem with setting time zone, thus:
d <- d |> mutate(DATE_MCLASS = as.character(DATE_MCLASS))
if(SAVE) {
  DBI::dbWriteTable(con, name = "VESSELS", value = d, overwrite = TRUE)
}

