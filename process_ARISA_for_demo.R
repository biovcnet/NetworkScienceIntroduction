library(tidyverse)

data <- read_csv("SPOT/arisa_latlon_sort_vess_bio.csv", na = "nd")

data2 <- data %>%
  # only keep cases where cruise id exists, otherwise I get a duplicate in
  # october 2006
  filter(!is.na(cruise_id)) %>%
  # only keep surface
  filter(depth_n == "5") %>%
  # only relevant columns
  select(date_local, arisa_frag, rel_abund)

write_csv(data2, "SPOT/spot_arisa_surface.csv")

zip("SPOT/spot_arisa_surface.zip", "SPOT/spot_arisa_surface.csv")

#data %>% filter(year == 2006, month == 10, arisa_frag == "ARISA_400.5", is.na(cruise_id)) -> dataLook
