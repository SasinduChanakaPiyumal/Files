# ============================================================================
# Continuous Random Walk Simulator
# ============================================================================
# Constants
TIME_STEP <- 0.01
PLOT_Y_MIN <- 45
PLOT_Y_MAX <- 100

# ============================================================================
# Helper Functions
# ============================================================================

#' Generate a single random walk vector
#'
#' @param initial_value Starting value for the random walk
#' @param num_steps Number of steps to simulate
#' @param sd Standard deviation of the normal distribution
#'
#' @return A numeric vector representing the random walk
generate_random_walk <- function(initial_value, num_steps, sd) {
  walk <- vector(mode = "numeric", length = num_steps)
  walk[1] <- initial_value
  
  for (i in 2:num_steps) {
    walk[i] <- walk[i - 1] + rnorm(1, mean = 0, sd = sd)
  }
  
  return(walk)
}

#' Generate multiple random walk vectors
#'
#' @param num_vectors Number of random walks to generate
#' @param initial_value Starting value for each random walk
#' @param num_steps Number of steps per walk
#' @param sd Standard deviation of the normal distribution
#'
#' @return A list of numeric vectors, each representing a random walk
generate_multiple_walks <- function(num_vectors, initial_value, num_steps, sd) {
  walks <- vector("list", length = num_vectors)
  
  for (j in 1:num_vectors) {
    walks[[j]] <- generate_random_walk(initial_value, num_steps, sd)
  }
  
  return(walks)
}

#' Create time vector and data frame from random walks
#'
#' @param walks List of random walk vectors
#' @param time_step Step size for time increments
#' @param last Maximum time value
#'
#' @return A data frame with time column and walk columns
prepare_data_frame <- function(walks, time_step, last) {
  time <- seq(0, last, by = time_step)
  df <- data.frame(x = time, walks)
  return(df)
}

#' Plot multiple random walks
#'
#' @param df Data frame containing time and walk data
#' @param num_vectors Number of walks to plot
#' @param y_min Minimum y-axis value
#' @param y_max Maximum y-axis value
plot_random_walks <- function(df, num_vectors, y_min, y_max) {
  # Plot the first walk
  plot(df$x, df[, 2],
    type = "o",
    col = 1,
    xlab = "Time",
    ylab = "Value",
    ylim = c(y_min, y_max)
  )
  
  # Add subsequent walks as lines
  for (i in 2:num_vectors) {
    lines(df$x, df[, i + 1], col = i, type = "o")
  }
}

# ============================================================================
# Main Function
# ============================================================================

#' Simulate and visualize multiple continuous random walks
#'
#' @param num_vectors Number of random walk vectors to generate
#' @param initial_value Starting value for the random walks
#' @param last Duration of the simulation (maximum time value)
#' @param sdinput Standard deviation of the random increments
#'
#' @return Invisibly returns the data frame (side effect: displays plot)
cntworkermany <- function(num_vectors, initial_value, last, sdinput) {
  # Adjust num_vectors (legacy behavior)
  num_vectors <- num_vectors + 1
  
  # Calculate parameters
  num_steps <- last / TIME_STEP + 1
  
  # Generate random walks
  walks <- generate_multiple_walks(num_vectors, initial_value, num_steps, sdinput)
  
  # Prepare data
  df <- prepare_data_frame(walks, TIME_STEP, last)
  
  # Visualize
  plot_random_walks(df, num_vectors, PLOT_Y_MIN, PLOT_Y_MAX)
  
  # Return data invisibly
  invisible(df)
}
