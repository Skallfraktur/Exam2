#' Sliding Window function.
#'
#' @param x A data set
#' @param y A column/vector in your data to manipulate formated as data$column
#' @param z Numeric: Sliding window size
#' @param q Numeric: Allowed threshold size
#'
#' @examples
#'
#' vec1 <- rnorm(100)
#' vec2 <- runif(100)
#' vec3 <- rpois(100, lambda = 5)
#'
#' df <- data.frame(column1 = vec1, column2 = vec2, column3 = vec3)
#'
#' processed_data <- sliding_window(df, df$column1, window_size, threshold_size)


# Sliding window function

sliding_window <- function(data,col,window_size,threshold_size) {

  if (window_size > nrow(data)) {
    stop("Window size exceeds the number of rows in the data.")
  }

  else if(window_size < 2) {
    window_size <- 2
    return("Window size specified to < 2, reverting to minimum window_size == 2")
  }

  # Function to calculate the maximum and minimum differences within a sliding window for the specified column
  difference_window <- function(window) {

    if(length(window) > 1) {
      max_diff <- abs(max(diff(window, na.rm = TRUE)))
      min_diff <- abs(min(diff(window, na.rm = TRUE)))
      min_val <- min(window)

      return(c(MaxDifference = max_diff, MinDifference = min_diff, MinValue = min_val))
    }

    else {
      return(c(MaxDifference = 0, MinDifference = 0))
    }
  }



  max_diff <- numeric(nrow(data))
  min_value <- numeric(nrow(data))

  for (i in 1:nrow(data)) {

    #Specify window

    window <- col[i:min(i + window_size - 1, nrow(data))]

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

#' Sorting Speed Function
#'
#' This function compares the speed of specified sorting algorithms on the given array. It allows you to insert a
#' numeric array, specify which sorting algorithms to compare and get output in a list format.
#'
#' @param array The input array to be sorted.
#' @param y Character vector specifying which sorting algorithms to evaluate.
#' @return A list containing timing information and sorted arrays for each selected algorithm.
#'
#' @examples
#' sorting_speed(sample(1:10000), c("bubble", "insertion", "selection", "quick"))
#'


sorting_speed <- function(array, y) {

# Defining all sorting functions.

# Bubble Sort
  bubble_sort <- function(arr) {
    # Get the length of the array
    n <- length(arr)

    # Outer loop: Traverse the array from the beginning to the second-to-last element
    for (i in 1:(n - 1)) {
      # Inner loop: Traverse the unsorted part of the array
      for (j in 1:(n - i)) {
        # Compare adjacent elements and swap if they are in the wrong order
        if (arr[j] > arr[j + 1]) {
          temp <- arr[j]
          arr[j] <- arr[j + 1]
          arr[j + 1] <- temp
        }
      }
    }

    # Return the sorted array
    return(sorted_array = arr)
  }

# Insertion Sort
insertion_sort <- function(arr) {
  # Get the length of the array
  n <- length(arr)

  # Iterate through the array starting from the second element
  for (i in 2:n) {
    # Store the current element in key
    key <- arr[i]

    # Initialize a variable to traverse the sorted part of the array
    j <- i - 1

    # Move elements greater than key to one position ahead of their current position
    while (j > 0 && arr[j] > key) {
      arr[j + 1] <- arr[j]
      j <- j - 1
    }

    # Place key at its correct position in the sorted array
    arr[j + 1] <- key
  }

  # Return the sorted array
  return(sorted_array = arr)
}

# Selection Sort
selection_sort <- function(arr) {
  # Get the length of the array
  n <- length(arr)

  # Iterate through the array
  for (i in 1:(n - 1)) {
    # Set current index as the minimum
    min_index <- i

    # Iterate through the unsorted part of the array to find the minimum element
    for (j in (i + 1):n) {
      # Checking if the current element is smaller than the element at min_index
      if (arr[j] < arr[min_index]) {
        # Update min_index if a smaller element is found
        min_index <- j
      }
    }

    # Swap the found minimum value with the element at position i
    temp <- arr[min_index]
    arr[min_index] <- arr[i]
    arr[i] <- temp
  }

  # Return the sorted array
  return(sorted_array = arr)
}

# Quicksort
quicksort <- function(arr) {
  # Base case: If the length of the array is 0 or 1, it is already sorted
  if (length(arr) <= 1) {
    return(arr)
  }

  # Choose a pivot element (here, the middle element)
  pivot <- arr[length(arr) %/% 2]

  # Partition the array into three parts: elements smaller, equal, and greater than the pivot
  left <- arr[arr < pivot]         # Elements smaller than the pivot
  middle <- arr[arr == pivot]      # Elements equal to the pivot
  right <- arr[arr > pivot]        # Elements greater than the pivot

  # Recursively apply QuickSort to the left and right partitions
  return(c(quicksort(left), middle, quicksort(right)))
}

# Converting character vector to lower case in order to make it case insensitive.

sorted_array <- list()
timings <- list()

y <- tolower(y)

if ("bubble" %in% y) {
  timings[["bubble"]] <- system.time({
    bubble_sort(array)
  })[["elapsed"]]
  sorted_array [["Bubble sort"]]<- bubble_sort(array)
}

if ("insertion" %in% y) {
  timings[["insertion"]] <- system.time({
    insertion_sort(array)
  })[["elapsed"]]
  sorted_array[["Insertion sort"]] <- insertion_sort(array)
}

if ("selection" %in% y) {
  timings[["selection"]] <- system.time({
    selection_sort(array)
  })[["elapsed"]]
  sorted_array[["Selection sort"]] <- selection_sort(array)
}

if ("quick" %in% y) {
  timings[["quick"]] <- system.time({
    quicksort(array)
  })[["elapsed"]]
  sorted_array[["Quicksort"]] <- quicksort(array)
}

# Extract minimum timings value, return a message informing which sorting algorithm is the fastest.
fastest_algorithm <- names(timings)[which.min(timings)]
message <- paste(fastest_algorithm, "sort is the fastest sorting algorithm for your data")

return(list(elapsed_times = timings, message = message, sorted_array))

}
