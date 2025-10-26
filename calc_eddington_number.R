#' ::::::::::::::::::::::::::::::::::::::::::
#' Script name: eddington_number.R
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

head(activs)
names(activs)

# Distances are in km, convert to miles
hist(activs$distance[activs$sport_type == "Ride"], breaks = 20)
activs$distance <- activs$distance * 0.62137119

#### Calculate E ####

rides <- subset(activs, sport_type == "Ride")

# Current
desc_dist <- sort(rides$distance, decreasing = TRUE)
current_E <- max(desc_dist[seq_along(desc_dist) > desc_dist])

# Over time

eddington_summary <- function(distances, goal_E = E + 1) {
  distances <- sort(distances, decreasing = TRUE)
  n <- length(distances)

  # Calculate E
  E <- sum(distances >= seq_len(n))

  # How many more rides to reach goal E
  next_target <- goal_E
  rides_over_next <- sum(distances >= next_target)
  needed_for_next <- next_target - rides_over_next

  # Return dataframe
  out <- tibble(
    E = E,
    next_target = next_target,
    rides_over_next = rides_over_next,
    needed_for_next = needed_for_next
  )

  return(out)
}

# Only 33 :(
eddington_summary(rides$distance)

rides_E <- rides |>
  arrange(start_date_local) |>
  mutate(
    E = map(
      .x = seq_along(distance),
      .f = ~ eddington_summary(distance[1:.x])$E
    )
  )

#### Plot ####

col <- "aquamarine4"

# E over time
ggplot(data = rides_E, aes(x = start_date_local, y = as.numeric(E))) +
  geom_line(color = col) +
  geom_hline(yintercept = current_E, linetype = "dashed") +
  annotate(
    geom = "text",
    x = mean(rides_E$start_date_local),
    y = current_E + 2,
    label = paste0("Current E = ", floor(current_E))
  ) +
  labs(y = "Eddington number", x = "Date") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 7, expand = expansion(mult = c(0, 0.1))) +
  cowplot::theme_minimal_hgrid()

# Frequency 
freq_data <- map(.x = 1:50, ~ eddington_summary(rides$distance, goal_E = .x)) |>
  bind_rows()

ggplot(freq_data, aes(x = next_target, y = rides_over_next)) +
  geom_col(fill = "darkred") +
  gghighlight::gghighlight(next_target == floor(current_E), unhighlighted_params = list(fill = col)) +
  geom_abline(slope = 1, intercept = 0, color = "darkred") +
  labs(x = "Eddington number", y = "Number of rides") +
  scale_y_continuous(n.breaks = 7, expand = expansion(mult = c(0, 0.1))) +
  cowplot::theme_minimal_hgrid()

