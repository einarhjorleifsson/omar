dictionary <- 
  tibble::tribble(~new, ~old,
                  ".sid", "visir",
                  "vid", "skipnr",
                  "gid", "veidarf",
                  "crew_n", "ahofn",
                  "year", "ar",
                  "month", "veman",
                  "date", "vedags",
                  "lon1", "lengd",
                  "lat1", "breidd",
                  "lon2", "lengd_lok",
                  "lat2", "breidd_lok",
                  "sq", "reitur",
                  "ssq", "smareitur",
                  "z1", "dypi",
                  "z2", "dypi_lok",
                  "winddirection", "vindatt",
                  "beaufort", "vindstig",
                  "m_sec", "m_sek",
                  "distance", "toglengd",   # Derived measure
                  "D2", "ldags",            # Landing date
                  "hid2", "lhofn",          # Harbour id landings took place
                  "sid", "tegund",
                  "catch", "afli",
                  "towtime", "togtimi",
                  "on.bottom", "ibotni",
                  "mesh", "moskvi",
                  "mesh_min", "moskvi_minnsti",
                  "doors", "hlerar",              # in kilograms
                  "headline", "hoflina",
                  "sweeps", "grandarar",          # in meters ???
                  "plow_width", "pl_breidd",
                  "tempb1", "botnhiti",           # bottom temperature
                  "tempb2", "botnhiti_lok",
                  "temps1",  "uppsj_hiti",         # surface temperature
                  "temps2", "uppsj_hiti_lok",
                  "height", "haed",           # gillnets
                  "mean_gillnet_length", "medal_lengd_neta",
                  "bait", "beita",
                  "temps1", "uppsjavarhiti",  # surface temperature
                  "t0", "logn_hefst",         # time setting starts
                  "t1", "drattur_hefst",      # time gear hauling starts
                  "t2", "drattur_lykur",      # time gear hauling ends
                  "n_units", "gildrur",
                  "hours", "klst"
  )
usethis::use_data(dictionary, overwrite = TRUE)

# omar::tbl_mar(omar::connect_mar(), "afli.stofn") |> 
#   translate_names(dictionary_logbooks) |> 
#   dplyr::glimpse()
