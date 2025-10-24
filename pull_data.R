library(rStrava)
library(tidyverse)

source("key.R")

activs_raw <- get_activity_list(stoken = my_token)
activs <- compile_activities(activs_raw)

write_csv(activs, "activs.csv")

head(activs)
names(activs)

activs |> 
  filter(sport_type == "Ride") |>
  ggplot(aes(x = distance*0.62137119)) +
  geom_histogram() +
  scale_y_continuous(n.breaks = 20)
