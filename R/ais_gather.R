# TODO:
#      Whacky tracks?
#      Time interval filter?
#      No error checks yet, e.g. if vessel not in database


#' Gather all AIS data
#'
#' @param con oracle connection
#' @param VID Vessel id
#' @param trim.harbour exclude points in harbour (default TRUE)
#'
#' @return a tibble
#' @export
#'
ais_gather <- function(con, VID, trim.harbour = TRUE) {
  
  # PAME -----------------------------------------------------------------------
  pam <- 
    mmsi_vessel(con) |> 
    dplyr::select(vid, mmsi) |> 
    dplyr::filter(vid == VID) |> 
    dplyr::left_join(pame_trail(con),
                     by = "mmsi") |> 
    dplyr::select(vid, 
                  time,
                  lon,
                  lat) %>% 
    dplyr::collect(n = Inf) %>% 
    dplyr::arrange(time) %>% 
    dplyr::mutate(source = "pam",
                  speed = traipse::track_speed(lon, lat, time),
                  speed = ramb::rb_ms2kn(speed)) %>% 
    # first record has speed as NA
    tidyr::fill(speed, .direction = "up") |> 
    dplyr::distinct(time, .keep_all = TRUE)
  # stk ------------------------------------------------------------------------
  stk <- 
    tbl_mar(con, "ops$einarhj.MID_VID_ICELANDIC_20220418") |> 
    dplyr::filter(vid == VID) |> 
    dplyr::select(mid, vid) |> 
    dplyr::left_join(omar::stk_trail(con),
                     by = "mid") %>%
    dplyr::select(vid, time, lon, lat, speed, heading) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::mutate(source = "stk") %>% 
    dplyr::arrange(time) %>% 
    dplyr::distinct(time, .keep_all = TRUE)
  # lgs ------------------------------------------------------------------------
  lgs <- 
    lb_trail(con) |> 
    dplyr::filter(vid == VID) |> 
    dplyr::collect(n = Inf) %>% 
    dplyr::mutate(lon = NA_real_,
                  lat = NA_real_,
                  source = "lgs") |> 
    dplyr::arrange(time) %>% 
    dplyr::select(-visir) |> 
    dplyr::distinct(time, .keep_all = TRUE)
  # ais ------------------------------------------------------------------------
  ais <-
    dplyr::bind_rows(stk,
                     lgs,
                     pam) %>% 
    dplyr::arrange(time) %>% 
    dplyr::group_by(vid) %>% 
    # NAs (the lgs-trails) approximated, note not operating on a sphere, should 
    #      not matter because distance small
    dplyr::mutate(lon = stats::approx(time, lon, time, method = "linear", rule = 1, f = 0, ties = mean)$y,
                  lat = stats::approx(time, lat, time, method = "linear", rule = 1, f = 0, ties = mean)$y)
  # asign a trip (here called .cid (aka: cruise id)
  hb <- 
    ramb::read_is_harbours() %>%
    dplyr::filter(iso2a == "IS",
                  # filter out some smaller harbours, cause a nuisance downstream
                  !hid %in% c("HRI", "ASS", "HAU", "GRE", "MJH", "MJO", "AED", "HJA"))
  
  ais <- 
    ais |> 
    dplyr::filter(!is.na(lon)) |> 
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = 4326,
                 remove = FALSE) |> 
    sf::st_join(hb %>% dplyr::select(hid)) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(vid) %>%
    # cruise id (aka tripid)
    dplyr::mutate(.cid = ramb::rb_trip(!is.na(hid))) %>% 
    dplyr::ungroup()
  
  if(trim.harbour) {
    ais <- 
      ais |> 
      dplyr::filter(is.na(hid)) |> 
      dplyr::select(-hid)
  }
  
  ais <-
    ais |> 
    dplyr::distinct(time, .keep_all = TRUE)
  
  return(ais)
  
}
