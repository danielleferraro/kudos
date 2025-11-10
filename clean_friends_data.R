#' ::::::::::::::::::::::::::::::::::::::::::
#' Script name: clean_friends_data.R
#' Author: Danielle Ferraro
#' Date: 2025-11-07
#' Purpose:
#' Harmonize friends' Strava data and calculate their E over time
#' ::::::::::::::::::::::::::::::::::::::::::
#' Notes:
#' Input Strava data (in data/raw) is not tracked on git.
#' Output is a csv (data/clean_eddington_data.csv) with columns: name, date, distance, and E =
#' ::::::::::::::::::::::::::::::::::::::::::

#### Setup ####

# Load packages
library(here)
library(tidyverse)

# Load function
source(here("get_eddington_summary.R"))

#### Load data ####

matt <- read_csv(here("data", "raw", "activs_matt.csv")) |> 
  mutate(name = "matt")
danielle <- read_csv(here("data", "raw", "activs.csv")) |> 
  mutate(name = "danielle")
sean <- read_csv(here("data", "raw", "activs_sean.csv")) |> 
  mutate(name = "sean")
jon <- read_csv(here("data", "raw", "activs_jon.csv")) |> 
  mutate(name = "jon")

# Tidy data. All we really need is the date and ride distance (in miles) on that day

sean_clean <- sean |> 
  janitor::clean_names() |> 
  filter(activity_type == "Ride") |> 
  select(name, activity_date, distance_miles) |> 
  # Convert activity_date string to a date format and extract day
  mutate(date = floor_date(mdy_hms(activity_date), unit = "day"), .keep = "unused") |> 
  group_by(name, date) |> 
  summarize(distance = sum(distance_miles, na.rm = TRUE)) |> 
  ungroup()

jon_clean <- jon |> 
  janitor::clean_names() |> 
  filter(activity_type == "Ride") |> 
  select(name, activity_date, distance_7) |> 
  # Convert activity_date string to a date format and extract day
  mutate(date = floor_date(mdy_hms(activity_date), unit = "day"), .keep = "unused",
        distance = distance_7 * 0.62137119) |> 
  group_by(name, date) |> 
  summarize(distance = sum(distance, na.rm = TRUE)) |> 
  ungroup()

matt_clean <- matt |>
  filter(sport_type == "Ride") |>
  mutate(date = floor_date(start_date_local, unit = "day"),
         distance = distance * 0.62137119) |> 
  group_by(name, date) |>
  summarize(distance = sum(distance, na.rm = TRUE)) |>
  ungroup()

danielle_clean <- danielle |>
  filter(sport_type == "Ride") |>
  mutate(date = floor_date(start_date_local, unit = "day"),
         distance = distance * 0.62137119) |>   group_by(name, date) |>
  summarize(distance = sum(distance, na.rm = TRUE)) |>
  ungroup()

#### Append E data and combine ####

clean_e_data <- map(
  .x = list(danielle_clean, sean_clean, matt_clean, jon_clean),
  .f = ~ {
    .x |>
      arrange(date) |>
      mutate(
        E = map_dbl(
          .x = seq_along(distance),
          .f = ~ get_eddington_summary(distance[1:.x])$E
        )
      )
  }
) |> 
  bind_rows()

# Save
write.csv(clean_e_data, here("data", "clean_eddington_data.csv"), row.names = FALSE)
