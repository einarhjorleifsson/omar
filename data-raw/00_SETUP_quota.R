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
          60,    585,
          61,   6336) |> 
  mutate(qyear = "2022/2023")
q20232024 <- 
  tribble(~sid, ~tac,
          1, 211309,  
          2,  76415,
          3,  66533,
          4,   1309,
          5,  41286,
          6,   6566,
          7,    259,
          8,   5139,
          9,   8344,
          12,    822,
          13,    296,
          14,    188,
          19,  12080,
          22,  21541,
          23,   7830,
          24,    971,
          25,   1476,
          26,     92,
          27,    361,
          30,  92634,
          60,    569,
          61,      0) |> 
  mutate(qyear = "2023/2024")
d <- bind_rows(tac, q20222023, q20232024)
names(d) <- toupper(names(d))
con <- mar::connect_mar()
DBI::dbWriteTable(con, name = "TAC", value = d, overwrite = TRUE)
          
