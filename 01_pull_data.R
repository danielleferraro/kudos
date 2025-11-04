#' ::::::::::::::::::::::::::::::::::::::::::
#' Script name: pull_strava_data.R
#' Author: Danielle Ferraro
#' Date: 2025-10-24
#' Purpose:
#' Pull activity data with Strava API and save to CSV
#' ::::::::::::::::::::::::::::::::::::::::::
#' Notes:
#' Requires Strava API token, which can be generated
#' following Sam's helpful instructions here: 
#' https://github.com/samanthacsik/strava-dashboard/wiki/Creating-a-Strava-API-Application-&-authentication
#' ::::::::::::::::::::::::::::::::::::::::::

# Load package and my API token
library(rStrava)
source("key.R")

# Pull data and convert to dataframe
activs_raw <- get_activity_list(stoken = my_token)
activs <- compile_activities(activs_raw) 

# Save
write.csv(activs, "activs.csv", row.names = FALSE)
