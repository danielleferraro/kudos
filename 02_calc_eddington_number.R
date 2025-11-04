#' ::::::::::::::::::::::::::::::::::::::::::
#' Script name: calc_eddington_number.R
#' Author: Danielle Ferraro
#' Date: 2025-10-24
#' Purpose:
#' Calculate my Eddington number, defined as
#' the largest integer E, where you have cycled
#' at least E miles on at least E days.
#' ::::::::::::::::::::::::::::::::::::::::::
#' Notes:
#'
#' ::::::::::::::::::::::::::::::::::::::::::

#### Setup ####

library(here)
library(tidyverse)

#### Load data ####

# source(here("pull_data.R")) # Pull latest data & overwrite file
activs <- read_csv(here("activs.csv"))

# Distances are in km, convert to miles
hist(activs$distance[activs$sport_type == "Ride"], breaks = 20)
activs$distance <- activs$distance * 0.62137119

#### Calculate E ####

# Summarize mileage per day because E is based on # days, not # rides
daily_mileage <- activs |>
  filter(sport_type == "Ride") |>
  mutate(date = floor_date(start_date_local, unit = "day")) |>
  group_by(date) |>
  summarize(distance = sum(distance, na.rm = TRUE)) |>
  ungroup()

# Current E (rounding down)
desc_dist <- sort(daily_mileage$distance, decreasing = TRUE)
current_E <- max(which(desc_dist >= seq_along(desc_dist)))

# E over time: given a vector of distances, calculate current E 
# and how many more days @ goal E mileage needed to reach goal E
eddington_summary <- function(distances, goal_E = current_E + 1) {
  
  # Current E
  desc_dist <- sort(distances, decreasing = TRUE)
  current_E <- max(which(desc_dist >= seq_along(desc_dist)))

  # How many more rides to reach goal E
  out <- tibble(
    E = current_E,
    next_target = goal_E,
    rides_over_next = sum(distances >= goal_E),
    needed_for_next = goal_E - rides_over_next
  )
  
  # Return dataframe
  return(out)
}

# My current stats
eddington_summary(daily_mileage$distance)

# Append E over time to mileage dataframe
daily_mileage_E <- daily_mileage |>
  arrange(date) |>
  mutate(
    E = map_dbl(
      .x = seq_along(distance),
      .f = ~eddington_summary(distance[1:.x])$E
    )
  )

#### Plot ####

col <- "aquamarine4"

# E over time
ggplot(data = daily_mileage_E, aes(x = date, y = E)) +
  geom_line(color = col) +
  geom_hline(yintercept = current_E, linetype = "dashed") +
  annotate(
    geom = "text",
    x = mean(daily_mileage_E$date),
    y = current_E + 2,
    label = paste0("Current E = ", floor(current_E))
  ) +
  labs(y = "Eddington number", x = "Date") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 7, expand = expansion(mult = c(0, 0.1))) +
  cowplot::theme_minimal_hgrid()

# Min number of days @ mileage per E, vs. my data
freq_data <- map(.x = 1:50, ~ eddington_summary(daily_mileage$distance, goal_E = .x)) |>
  bind_rows()

ggplot(freq_data, aes(x = next_target, y = rides_over_next)) +
  geom_col(fill = "darkred") +
  gghighlight::gghighlight(next_target == floor(current_E), unhighlighted_params = list(fill = col)) +
  geom_abline(slope = 1, intercept = 0, color = "darkred") +
  labs(x = "Eddington number", y = "Number of days") +
  scale_y_continuous(n.breaks = 7, expand = expansion(mult = c(0, 0.1))) +
  cowplot::theme_minimal_hgrid()

