library(tidyverse)
# tac up to and including qyear 2021/2022
tac <-
  read_csv("~/cronjobs/landanir/data/tac.csv") |> 
  select(sid:tac) |> 
  drop_na()
q20222023 <- 
  tribble(~sid, ~tac,
           1, 208846,  
           2,  62219,
           3,  71300,
           4,   1091,
           5,  25545,
           6,   6098,
           7,    259,
           8,   4464,
           9,   8107,
          12,   1105,
          13,    334,
          14,    258,
          19,  11520,
          22,  25710,
          23,   7663,
          24,   1137,
          25,   1230,
          26,    132,
          27,    301,
          30,  66195,
          60,    585) |> 
  mutate(qyear = "2022/2023")
d <- bind_rows(tac, q20222023)
names(d) <- toupper(names(d))
DBI::dbWriteTable(con, name = "TAC", value = d, overwrite = TRUE)
          
