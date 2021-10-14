cb_speed_projs <- read_csv(url("https://raw.githubusercontent.com/tejseth/cb-speed/master/cb_speed_projs.csv"))

cb_speed_select <- cb_speed_projs %>%
  dplyr::select()