#' @title Sliding Window
#'
#' @param x Dataset
#' @param y Column in your data to manipulate as data$column
#' @param z Sliding window size
#' @param q Allowed threshold size

# Main iterative function

sliding_window <- function(data,column,window_size,threshold_size) {

  if (window_size > nrow(data)) {
    stop("Window size exceeds the number of rows in the data.")
  }

  else if(window_size < 2) {
    window_size <- 2
    return("Window size specified to < 2, reverting to minimum window_size == 2")
  }

  # Function to calculate the maximum and minimum differences within a sliding window for the specified column
  difference_window <- function(window) {

      max_diff <- abs(max(diff(window)))
      min_diff <- abs(min(diff(window)))
      min_val <- min(window)

      return(c(MaxDifference = max_diff, MinDifference = min_diff, MinValue = min_val))
  }

  max_diff <- numeric(nrow(data))
  min_value <- numeric(nrow(data))

  for (i in 1:nrow(data)) {

    #Specify window

    window <- data$W[i:min(i + window_size - 1, nrow(data))]

    # Use previous helping functions with window as input.

    differences <- difference_window(window)

    #Extract results from helping functions and put them into vectors.

    max_diff[i] <- differences["MaxDifference"]
    min_value[i] <- differences["MinValue"]

    # Check if MaxDifference is != NA and > threshold. If true, apply the max_diff (the first max_diff to exceed the threshold
    # up until that point [i] and add the max_diff[i] to max_diff[1:i]

    if (!is.na(max_diff[i]) && max_diff[i] > threshold_size) {
      max_diff[1:i] <- max_diff[1:i] + max_diff[i]
    }
  }

  # Add the new vectors as column in the original dataframe.
  data$MaxDifference <- max_diff
  data$MinValue <- min_value

  return(data)
}

# Example usage
# Assuming df is your data frame with columns S and W
# Example data frame creation: df <- data.frame(S = 1:10, W = c(2, 4, 1, 5, 7, 3, 9, 8, 6, 10))

window_size <- 3
threshold_size <- 3
df_processed <- iterative_func(df, window_size, threshold_size)

ggplot(df_processed, aes(T)) +
  geom_line(aes(y=W,color="W")) +
  geom_line(aes(y=MaxDifference, color="MaxDiff")) +
  geom_line(aes(y=MinValue, color="MinVal")) +
  scale_color_manual(values = c("red", "gray", "blue")) +
  theme_bw()
