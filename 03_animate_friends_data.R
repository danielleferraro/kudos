#' ::::::::::::::::::::::::::::::::::::::::::
#' Script name: 03_animate_friends_data.R
#' Author: Danielle Ferraro
#' Date: 2025-11-09
#' Purpose:
#' Make a fun animation of friends' data over time
#' ::::::::::::::::::::::::::::::::::::::::::
#' Notes:
#' Saves .gif out to figures/eddington_animation.gif
#' ::::::::::::::::::::::::::::::::::::::::::

#### Setup ####

library(here)
library(tidyverse)
library(gganimate)

#### Load data ####

# Convert date and get each person's current E as a column
data <- read_csv(here("data", "clean_eddington_data.csv")) |>
  mutate(date = as.POSIXct(date)) |>
  group_by(name) |>
  mutate(current_E = max(E)) |>
  ungroup() |>
  mutate(name = fct_reorder(str_to_sentence(name), current_E, .desc = TRUE))


#### Plot ####

# Static plot
(static <- ggplot(data = data, aes(x = date, y = E, color = name)) +
  geom_line() +
  labs(y = "Eddington number", x = "Date") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 7, expand = expansion(mult = c(0, 0.1))) +
  scale_color_brewer(palette = "Set2") +
  labs(x = NULL, color = NULL) +
  cowplot::theme_minimal_hgrid(font_size = 14))

animated <- ggplot(data = data, aes(x = date, y = E, color = name)) +
  geom_line() +
  geom_text(
    aes(label = paste0(name, ": ", E)),
    hjust = -0.2, # Move text slightly right of the line tip
    size = 6.5
  ) +
  labs(y = "Eddington number", x = "Date") +
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0, 0.2))
  ) +
  scale_y_continuous(n.breaks = 7, expand = expansion(mult = c(0, 0.1))) +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(clip = 'off') +
  labs(x = NULL, color = NULL) +
  cowplot::theme_minimal_hgrid(font_size = 20) +
  theme(legend.position = "none") + # Hide legend
  # Animate
  transition_reveal(date)

# Render
animate(
  animated,
  fps = 10,
  duration = 10,
  end_pause = 35,
  width = 800,
  height = 500,
  render = gifski_renderer()
)

# Save
anim_save("eddington_animation.gif")
