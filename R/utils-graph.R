# Function that returns number of observations per group (location) and
# position value relative to median value
give_n <- function(x, prefix_chars = "") { # "italic(n) == "
  data.frame(
    y = ifelse(
      test = 90 > median(x, na.rm = TRUE) & 
        median(x, na.rm = TRUE) < 110 & 
        0 > diff(range(x)) & diff(range(x)) < 25,
      yes = quantile(x, na.rm = TRUE)["75%"] * 1.05,
      no = quantile(x, na.rm = TRUE)["75%"] * 1.2
    ),
    label = length(x))
  # experiment with the multiplier to find the perfect position
}

# position value relative to median value
give_n <- function(x) {
  data.frame(
    y = quantile(x, probs = 0.75), # Position
    label = paste0("", length(x))
  )
}