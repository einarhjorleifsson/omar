TOW_METRICS <- 
  tibble::tibble(sclass = c(30, 35, 31, 37, 40, 34, 19), 
               min_towlength = c(2, 2, 0.5, 0.5, 0.5, 0.5, 0.5), 
               max_towlength = c(8, 8, 4, 4, 4, 0.5, 2.5), 
               std_towlength = c(4, 4, 1, 1, 2, 0.5, 2), 
               std_width = c(17/1852, 17/1852, 17/1852, 27.595/1.852^2/1000, 4/1852, 50, 8/1852),
               sur = c("smb", "smh", "smr", "smri", "smm", "smn", "smg")) %>% 
  mutate(min_towlength = min_towlength * 1852,
         max_towlength = max_towlength * 1852,
         std_towlength = std_towlength * 1852,
         std_width = std_width * 1852)
colnames(TOW_METRICS) <- toupper(colnames(TOW_METRICS))
DBI::dbWriteTable(con, name = "TOW_METRICS", value = TOW_METRICS, overwrite = TRUE)
