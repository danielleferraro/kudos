library(rStrava)
library(tidyverse)

source("key.R")

activs_raw <- get_activity_list(stoken = my_token)
activs <- compile_activities(activs_raw)

write_csv(activs, "activs.csv")
