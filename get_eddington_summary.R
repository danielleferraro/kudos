#' get_eddington_summary.R 
#' Author: Danielle Ferraro
#' Date: 2025-11-07
#' Purpose:
#' Calculate Eddington number, defined as
#' the largest integer E, where you have cycled
#' at least E miles on at least E days, and
#' how many more days at goal E mileage needed to 
#' reach goal E.
#'
#' @param distances numeric vector of ride distances
#' @param goal_E goal Eddington number, default current Eddington number + 1
#'
#' @return a 1x4 tibble
#' 
#' @examples
#' get_eddington_summary(danielle_clean$distance)
#'# A tibble: 1 × 4
#'      E next_target rides_over_next needed_for_next
#'  <int>       <dbl>           <int>           <dbl>
#'1    33          34              32               2

get_eddington_summary <- function(distances, goal_E = current_E + 1) {
  
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
