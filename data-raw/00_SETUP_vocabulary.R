
# channel ----------------------------------------------------------------------

voc_channel <- 
  #              note order
  dplyr::tribble(~omar, ~mar,
                 # ch_station_v
                 ".stid", "stod_id",
                 "vid",   "skip_nr",
                 "date",  "dags",
                 "year",  "ar",
                 "sq",    "reitur",
                 "ssq",   "smareitur",
                 "lon",   "kastad_lengd",
                 "lat",   "kastad_breidd",
                 "lon2",  "hift_lengd",
                 "lat2",  "hift_breidd",
                 "z1",    "botndypi_kastad",
                 "z2",    "botndypi_hift",
                 "st",    "yfirbordshiti",
                 "bt",    "botnhiti",
                 # ch_syni_v
                 ".id",       "synis_id",
                 "gid",       "veidarfaeri",
                 "sclass",    "synaflokkur_nr",
                 "tow_id",    "tog_nr",
                 "t1",        "togbyrjun",
                 "t2",        "togendir",
                 "tid",       "tog_nr",
                 "speed",     "toghradi",
                 "towlength", "toglengd",
                 # ch_sample_class
                 "sclass",       "sample_category_no",
                 "sample_class", "eng_descr",
                 "synaflokkur",  "descr")

# biota ------------------------------------------------------------------------
voc_biota <- 
  #              note order
  dplyr::tribble(~omar, ~mar,
                 ".id",    "synis_id",
                 "sid",    "tegund_nr",
                 "sid",    "species",
                 "length", "lengd",
                 "sex",    "kyn_nr",
                 "mat",    "kynthroski_nr",
                 "age",   "aldur",
                 "n",     "fjoldi",
                 "rn",     "r_talid",
                 ".mid",   "maeling_id",
                 "knr",    "kvarna_nr",
                 "wt",     "thyngd",
                 "gwt",    "slaegt",
                 "stomach","magi",
                 "liver",  "lifur",
                 "gonads", "kynfaeri",
                 "rate",   "hlutfall")
                 

# vessel -----------------------------------------------------------------------
voc_vessel <- 
  dplyr::tribble(~mar, ~omar,
                 "registration_no", "vid",
                 "name", "vessel",
                 "imo_no", "imo")

# create data ------------------------------------------------------------------

vocabulary <-
  bind_rows(voc_channel,
            voc_biota,
            voc_vessel) |> 
  distinct()
vocabulary <- 
  stats::setNames(object = vocabulary$mar, 
                  nm = vocabulary$omar)

usethis::use_data(vocabulary, overwrite = TRUE)
