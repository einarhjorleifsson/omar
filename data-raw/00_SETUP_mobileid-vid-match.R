library(tidyverse)
library(omar)
con <- connect_mar()


# HISTORY ----------------------------------------------------------------------
# 2024-02-12: Use the new vessel database
# 2023-05-09: Added 
#          Ghandi (mid = 103015, vid == 2702)
#          Haberg (mid = 101083, vid = 2654)
#          Birtingur (mid = 101115, vid = 1807)
#          Skafti (mid == 102965, vid == 1337)
# 2023-05-05: Corrected mid == 100109 in manual match (used 101109, which is Arni Fridriksson)
# 2023-04-25: Add T1 and T2 to reflect match of mobileid and vessel id
# 2023-04-08: Seeding
#

# NOTES: mid 

## Make a copy of older oracle data-file ---------------------------------------
d <- 
  tbl_mar(con, "ops$einarhj.mobile_vid") |> 
  collect()
names(d) <- toupper(names(d))
DBI::dbWriteTable(con, name = "MOBILE_VID_20240212", value = d, overwrite = FALSE)

## Vessels ---------------------------------------------------------------------
#   User the mother of all vessel information
vid <-
  omar::vessel(con) |>
  collect(n = Inf) |>
  filter(!vid %in% c(9999)) |>
  # ignore foreign vessels
  # filter(!between(vid, 3700, 4999)) |>
  # umdæmisnúmer never greater than 999 (could be wrong)
  mutate(uno_c = case_when(uno > 0 ~ str_pad(uno, 3, pad = "0"),
                           .default = NA),
         uid = case_when(!is.na(uid) & !is.na(uno_c) ~ paste0(uid, uno_c),
                         .default = NA)) |>
  select(vid:uid, cs, mclass, imo, mmsi, length) |>
  # this should be fixed upstream
  mutate(cs = case_when(vid ==  2730 ~ "TFCR",
                        .default = cs)) |>
  arrange(vid)

## stk.summary -----------------------------------------------------------------
### stk.summary from stk.stk_trail ---------------------------------------------
stk_summary <-
  tbl(con, dbplyr::in_schema("STK", "TRAIL")) |>
  mutate(YEAR = year(POSDATE)) |>
  filter(YEAR >= 2000) |>
  group_by(MOBILEID) |>
  summarise(pings = n(),
            n_years = n_distinct(YEAR),
            t1_org = min(POSDATE, na.rm = TRUE),
            t2_org = max(POSDATE, na.rm = TRUE),
            #cv_lon = sd(POSLON, na.rm = TRUE) / mean(POSLON, na.rm = TRUE),
            #cv_lat = sd(POSLAT, na.rm = TRUE) / mean(POSLAT, na.rm = TRUE),
            .groups = "drop") |>
  collect(n = Inf) |>
  rename(mid = MOBILEID) |>
  mutate(t1_org = as_date(t1_org),
         t2_org = as_date(t2_org)) |>
  arrange(mid) |> 
  # 2023-04-25: Variables to be used as timefilter downstream
  mutate(T1 = ymd("2001-01-01"),
         T2 = ymd("2029-12-31"))

### stk.summary from stk.stk_vms_v ---------------------------------------------
# just testing if stk.stk_vms_v gives the same
if(FALSE) {
  stk_summary2 <-
    tbl(con, dbplyr::in_schema("STK", "STK_VMS_V")) |>
    mutate(YEAR = year(POSDATE)) |>
    filter(YEAR >= 2000) |>
    group_by(MOBILEID, SKIP_NR) |>
    summarise(pings = n(),
              n_years = n_distinct(YEAR),
              t1 = min(POSDATE, na.rm = TRUE),
              t2 = max(POSDATE, na.rm = TRUE),
              #cv_lon = sd(POSLON, na.rm = TRUE) / mean(POSLON, na.rm = TRUE),
              #cv_lat = sd(POSLAT, na.rm = TRUE) / mean(POSLAT, na.rm = TRUE),
              .groups = "drop") |>
    collect(n = Inf) |>
    rename(mid = MOBILEID,
           vid_vms = SKIP_NR) |>
    mutate(t1 = as_date(t1),
           t2 = as_date(t2)) |>
    arrange(mid)
  identical(stk_summary$mid, stk_summary2$mid)
  identical(stk_summary$pings, stk_summary2$pings) # difference just due to timing
  identical(stk_summary$n_years, stk_summary2$n_years)
  identical(stk_summary$t1_org, stk_summary2$t1)
  identical(stk_summary$t2_org, stk_summary2$t2)
}
## Mapping vessels by localid and globalid -------------------------------------
### mapping of stk.mobile ------------------------------------------------------
mob <-
  tbl_mar(con, "stk.mobile") %>%
  select(mid = mobileid,
         loid = localid,
         glid = globalid) |>
  collect(n = Inf) |>
  arrange(mid) |>
  #full_join(stk_summary) |>
  arrange(mid) |>
  mutate(lid_what = case_when(as.integer(loid) %in% vid$vid ~ "vid",
                              (nchar(loid) == 4 & str_starts(loid, "TF")) |
                                loid %in% vid$cs[!is.na(vid$cs)] ~ "cs",
                              loid %in% vid$uid[!is.na(vid$uid)] ~ "uid",
                              .default = NA),
         gid_what = case_when(as.integer(glid) %in% vid$vid ~ "vid",
                              (nchar(glid) == 4 & str_starts(glid, "TF")) |
                                glid %in% vid$cs[!is.na(vid$cs)] ~ "cs",
                              glid %in% vid$uid[!is.na(vid$uid)] ~ "uid",
                              .default = NA),
         link = case_when(!is.na(lid_what) & !is.na(gid_what) ~ paste0(lid_what, "-", gid_what),
                          .default = NA_character_))
VID <- vid$vid
mob_vid_vid <-
  mob |>
  filter(link == "vid-vid") |>
  mutate(vid = as.integer(loid)) |> 
  filter(vid %in% VID)
mob_vid_cs <-
  mob |>
  filter(link == "vid-cs") |>
  mutate(vid = as.integer(loid),
         cs = glid) |>
  inner_join(vid |> select(vid, cs)) |> 
  select(-cs)
mob_uid_cs <-
  mob |>
  filter(link == "uid-cs") |>
  mutate(uid = loid,
         cs = glid) |>
  inner_join(vid |> select(vid, uid, cs)) |> 
  select(-c(uid, cs))

### mapping of stk.mobile_skip_v -----------------------------------------------
mob_mobile_skip_v <-
  tbl_mar(con, "stk.mobile_skip_v") %>%
  select(mid = mobileid,
         loid = localid,
         glid = globalid,
         vid = skip_nr) |>
  collect(n = Inf) |> 
  filter(!vid %in% c(0, 9999)) |> 
  filter(loid != vid) |> 
  mutate(vid = as.integer(vid),
         link = "mobile_skip_v") |> 
  arrange(mid)
### Merge the mapping ----------------------------------------------------------
mid.match.so.far <- 
  c(mob_vid_vid$mid, mob_vid_cs$mid, mob_uid_cs$mid)
mob_final <-
  bind_rows(mob |> filter(!mid %in% mid.match.so.far),
            mob_vid_vid,
            mob_vid_cs,
            mob_uid_cs) |> 
  mutate(midvid = ifelse(!is.na(vid), paste0(as.character(as.integer(mid)), "-", vid), NA)) |> 
  arrange(mid)
mob_final <- 
  mob_final |> 
  bind_rows(mob_mobile_skip_v |> 
              mutate(midvid = ifelse(!is.na(vid), paste0(as.character(as.integer(mid)), "-", vid), NA)) |> 
              filter(!midvid %in% mob_final$midvid[!is.na(mob_final$midvid)])) |> 
  arrange(mid, desc(link)) |> 
  select(-midvid)
## Add auxillary informations --------------------------------------------------
lnd_last <-
  omar::ln_catch(con) |>
  filter(catch > 0) |>
  filter(vid > 0) |>
  group_by(vid) |>
  filter(date == max(date, na.rm = TRUE)) |>
  ungroup() |>
  select(vid, datel = date) |>
  distinct() |>
  collect(n = Inf) |>
  mutate(datel = as_date(datel))
MOBILE_VID <-
  mob_final |>
  left_join(stk_summary) |>
  left_join(lnd_last) |>
  select(mid:glid, vid, pings, n_years, datel, link, everything()) |>
  arrange(mid, vid, desc(link)) |> 
  #  vessels still not in registry 
  mutate(vid = case_when(mid == 146524 & is.na(vid) ~ 3038,
                         mid == 145278 & is.na(vid) ~ 3007,
                         mid == 132918 & is.na(vid) ~ 2969,
                         mid == 121166 & is.na(vid) ~ 2906,
                         mid == 121682 & is.na(vid) ~ 2871,
                         mid == 109624 & is.na(vid) ~ 2809,
                         mid == 141610 & is.na(vid) ~ 2983,
                         mid == 137136 & is.na(vid) ~ 7839,
                         mid == 137501 & is.na(vid) ~ 7837,
                         mid == 121845 & is.na(vid) ~ 7788,
                         mid == 118744 & is.na(vid) ~ 7787,
                         mid == 109644 & is.na(vid) ~ 7763,
                         mid == 124115 & is.na(vid) ~ 7744,
                         mid == 102216 & is.na(vid) ~ 6717,
                         mid == 103015 & is.na(vid) ~ 2702, # Ghandi, but only for years 2010-2014
                                                            #   taken care of below
                         mid == 101083 & is.na(vid) ~ 2654, # Háberg, but only for years 2009-2011
                                                            #   taken care of below
                         mid == 101115 & is.na(vid) ~ 1807, # Birtingur, loid is vid but glidi is cs as TMP_TFDP
                         mid == 102965 & is.na(vid) ~ 1337, # Skafti
                         mid == 102869 & vid == 6339 ~ 6155,
                         
                         
                         .default = vid))
# Mobileid takeover ------------------------------------------------------------
# NOTE: datel (last landing date) not included here
#  The "no" = 1 refers to the automatically derived match that is being
#   split up. This matters in the step below: "replace.vid" where the
#   records where no = 1 is replace by the split record.
manual <- 
  tribble(~vid, ~mid, ~T1, ~T2, ~no,
           100, 100619, "2001-01-01", "2014-03-12", 1,
          2930, 100619, "2016-01-01", "2029-12-31", 2,
          
           124, 101548, "2001-01-01", "2009-01-01", 2,
          2929, 101548, "2016-01-01", "2029-12-31", 1,
          
           183, 101106, "2001-01-01", "2013-12-31", 2,
          2883, 101106, "2014-01-01", "2029-12-31", 1,
          
           219, 100056, "2001-01-01", "2014-12-31", 1,
          7749, 100056, "2020-01-01", "2029-12-31", 2,
          
           220, 101099, "2001-01-01", "2014-12-31", 1,   # here we have two matching
          2978, 101099, "2021-01-01", "2029-12-31", 1,   #  "vid-cs" and "mobile_skip_v"
          # Hence both matches get removed
           239, 100171, "2001-01-01", "2015-12-31", 1,
          3014, 100171, "2021-01-01", "2029-12-31", 2,
          
           243, 101030, "2001-01-01", "2008-12-31", 2,
          2952, 101030, "2019-01-01", "2029-12-31", 1,
          
           256, 101075, "2001-01-01", "2018-12-31", 2,
          2944, 101075, "2019-01-01", "2029-12-31", 1,
          
           733, 100340, "2001-01-01", "2013-12-31", 1,
          2938, 100340, "2014-01-01", "2029-12-31", 2,
          
           962, 103067, "2001-01-01", "2017-06-13", 2, # check these dates
          2936, 103067, "2017-06-14", "2029-12-31", 1,
          
           968, 101148, "2001-01-01", "2020-12-31", 1,
          3017, 101148, "2021-01-01", "2029-12-31", 2,
          
           971, 101223, "2001-01-01", "2015-12-31", 1,
          2718, 101223, "2016-01-01", "2029-12-31", 2,
          
           975, 101092, "2001-01-01", "2018-12-31", 1,
            NA, 101092, "2019-01-01", "2029-12-31", 2,
          
          1010, 101408, "2001-01-01", "2020-12-01", 1,
          2991, 101408, "2021-01-01", "2029-12-31", 2,
          
          1013, 101205, "2001-01-01", "2008-12-31", 1,
          1453, 101205, "2015-01-01", "2029-12-31", 1,
          
          1014, 100343, "2001-01-01", "2018-12-31", 1,
          2992, 100343, "2019-01-01", "2029-12-31", 2,
          
          1031, 101086, "2001-01-01", "2015-01-01", 1,
          2955, 101086, "2016-01-01", "2029-12-31", 2,
          
          1060, 101545, "2001-01-01", "2010-12-31", 1,
          2894, 101545, "2017-01-01", "2029-12-31", 1,
          
          1100, 102555, "2001-01-01", "2008-12-31", 2,
          2822, 102555, "2009-01-01", "2029-12-31", 1,
          
          1204, 100652, "2001-01-01", "2015-12-31", 1,
          3013, 100652, "2021-01-01", "2029-12-31", 2,
          
          1236, 100579, "2001-01-01", "2016-12-31", 1,
          7183, 100579, "2020-01-01", "2029-12-31", 2,  # Dubious match, not catch reported
          
          1244, 100716, "2001-01-01", "2010-12-31", 1,
          2999, 100716, "2021-01-01", "2029-12-31", 2,
          
          1270, 101079, "2001-01-01", "2017-12-31", 1,
            NA, 101079, "2021-01-01", "2029-12-31", 2,  # This is a fishing vessel (dredge??)
          
          1272, 102100, "2001-01-01", "2021-01-13", 1,
          2995, 102100, "2021-11-20", "2029-12-31", 2,
          
          1275, 101069, "2001-01-01", "2016-06-01", 2,
          1850, 101069, "2016-06-02", "2029-12-31", 1,
          
          1279, 101142, "2001-01-01", "2015-12-31", 1,
          3004, 101142, "2016-01-01", "2029-12-31", 2,
          
          1291, 101072, "2001-01-01", "2008-12-31", 1,
            NA, 101072, "2019-01-01", "2029-12-31", 2,  # útlendingur, cs skráning vitlaus
          
          1395, 101227, "2001-01-01", "2019-12-31", 1,
          3899, 101227, "2020-01-01", "2029-12-31", 2, # kalbakur flutningaskip?
          
          1401, 101119, "2001-01-01", "2022-04-30", 1,
          2730, 101119, "2022-05-01", "2029-12-31", 2, # vantar 2007-2013
          
          1612, 101065, "2001-01-01", "2012-12-31", 2,
          2888, 101065, "2014-01-01", "2029-12-31", 1,
          
          1562, 100109, "2001-01-01", "2008-12-31", 2,  # Jón á Hofi
          1890, 100109, "2009-01-01", "2029-12-31", 1,  # as dual mobileid
          
          1622, 100120, "2001-01-01", "2018-12-31", 1,
          2997, 100120, "2019-01-01", "2029-12-31", 2,
          
          1639, 100163, "2001-01-01", "2014-12-31", 1,
          2967, 100163, "2019-01-01", "2029-12-31", 2,
          
          1674, 101144, "2001-01-01", "2020-12-31", 1,
          3022, 101144, "2021-01-01", "2029-12-31", 2,
          
          2061, 102284, "2001-01-01", "2008-01-01", 2,
          2917, 102284, "2017-01-01", "2029-12-31", 1,

          2020, 101074, "2001-01-01", "2018-12-31", 1,
          3016, 101074, "2021-01-01", "2029-12-31", 2,
          
          2067, 101081, "2001-01-01", "2012-12-31", 1,
            NA, 101081, "2013-01-01", "2029-12-31", 2,
          
          2154, 101161, "2001-01-01", "2013-12-31", 2,
          2861, 101161, "2018-01-01", "2029-12-31", 1,
          
          2212, 101084, "2001-01-01", "2008-12-31", 2,
          2903, 101084, "2015-01-01", "2021-12-31", 1,
          2212, 101084, "2022-01-01", "2029-12-31", 2,

          2345, 101104, "2001-01-01", "2017-12-31", 1,
          3035, 101104, "2022-01-01", "2029-12-31", 2,
          
          2395, 100907, "2001-01-01", "2011-12-31", 1,
            NA, 100907, "2016-01-01", "2029-12-31", 2,
          
          2410, 101402, "2001-01-01", "2019-12-31", 2,
          2982, 101402, "2020-01-01", "2029-12-31", 1,
          
          2500, 103173, "2016-10-01", "2029-12-31", 1,  # check these corrections
          2549, 103173, "2001-01-01", "2014-01-01", 2,  # carefully
          
          2642, 102515, "2001-01-01", "2018-12-31", 1,
          3030, 102515, "2022-01-01", "2029-12-31", 2,
          
          2645, 102967, "2001-01-01", "2016-05-31", 1,
            NA, 102967, "2015-06-01", "2029-12-31", 2,
          
          2750, 103872, "2001-01-01", "2017-12-31", 1,
          3015, 103872, "2021-12-31", "2029-12-31", 2,
          
          2772, 104362, "2001-01-01", "2019-12-31", 1,
          3000, 104362, "2020-01-01", "2029-12-31", 2,
          
          2702, 103015, "2010-01-01", "2014-12-31", 1,    # Ghandi, see above
            NA, 103015, "2019-01-01", "2029-12-31", 2,
          
          2654, 101083, "2001-01-01", "2010-12-31", 1,    # Háberg, see above
            NA, 101083, "2011-01-01", "2029-12-31", 2     # This is a longliner
          
          
          
          
          )
# check again 1109, ...
# vessels with problem stk
# 1136  missing stk in the start of the series


MOBILE_VID <-
  MOBILE_VID |> 
  mutate(datel = as.character(datel),
         t1_org = as.character(t1_org),
         t2_org = as.character(t2_org),
         T1 = as.character(T1),
         T2 = as.character(T2))

replace.vid <- manual |> filter(no == 1) |> pull(vid)

MOBILE_VID <- 
  bind_rows(MOBILE_VID |> filter(!vid %in% replace.vid),
            manual |> mutate(link = "manual"))

MOBILE_VID |> glimpse()



# to oracle --------------------------------------------------------------------
if(FALSE) {
  names(MOBILE_VID) <- toupper(names(MOBILE_VID))
  DBI::dbWriteTable(con, name = "MOBILE_VID", value = MOBILE_VID, overwrite = FALSE)
  # DBI::dbExecute(con, 'GRANT select on "MOBILE_VID" to public')
  tbl_mar(con, "ops$einarhj.mobile_vid") |> glimpse()
}
