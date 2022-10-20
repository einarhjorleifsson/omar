# why is strata stuff in biota, would think channel more appropriate?
# note, not a 1:1 link of strata here and in utbrteg
strata_stations <- function (con) {
  tbl_mar(con, "biota.strata_stations") %>% 
    dplyr::rename(sclass = synaflokkur,
                  index = station)
}

strata_attributes <- function(con) {
  tbl_mar(con, "biota.strata_attributes")
}

# not sure what this is vs above
strata_areas <- function(con) {
  tbl_mar(con, "ops$bthe.\"strata_areas\"")
}

