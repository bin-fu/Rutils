################################################################################
# updated date: 11/04/2015
#
################################################################################

discretize_interval <- function(x, y, k_start = 10, integer_breaks = F, thres=1.2) {
  # bin numeric variables to categorical variables based on contrast
  # patterns
  #
  if (!is.numeric(x)) {
    stop('only deal with numeric variables')
  }

  # initialization break points
  min_value <- min(x, na.rm = T)
  max_value <- max(x, na.rm = T)
  split_values <- seq(min_value,max_value, length.out = k_start + 1)
  if (integer_breaks == T) {
    split_values <- floor(split_values)
  }
  split_values <- unique(split_values)
  split_values <- split_values[2:(length(split_values) - 1)]

  # remove bins with less population
  x_bin <- cut(x, breaks = c(-Inf, split_values, Inf), right = T)
  x_dist <- table(x_bin, y)
  row_sums <- x_dist[, 1] + x_dist[, 2]
  row_sums = round(row_sums/length(x),4)
  index <- which(row_sums < 0.05)
  first_in <- 1 %in% index
  if (length(index) > 0) {
    index <- index - 1
    index <- index[which(index %in% 1:length(split_values))]
    if(first_in) index <- c(1, index)
  }
  if (length(index) > 0)
    split_values <- split_values[-index]

  # initialization
  x_bin <- cut(x, breaks = c(-Inf, split_values, Inf), right = T)
  x_dist <- table(x_bin, y)
  col_sums <- colSums(x_dist)
  for (i in 1:ncol(x_dist)) {
    x_dist[,i] <- round(x_dist[,i]/col_sums[i],4)
  }
  contrast_ratio <- x_dist[,1]/x_dist[,2]

  significance_label <- rep(0, nrow(x_dist))
  significance_label[which(contrast_ratio >= thres)] <- 1
  significance_label[which(contrast_ratio <= 1/thres)] <- 2

  # merge the bins
  remove_index <- integer(0)
  i <- 1
  label_length <- length(significance_label)
  while (i < label_length) {
    j <- i + 1
    while ((j <= label_length) & (significance_label[j] == significance_label[i])) {
      remove_index <- c(remove_index,j - 1)
      j <- (j + 1)
    }
    i <- j
  }

  if (length(remove_index) > 0) {
    split_values <- split_values[-remove_index]
  }

  res <- cut(x, breaks = c(-Inf, split_values, Inf))
  return(res)
}


discretize_freq <- function(x, y, k_start = 10, integer_breaks = F, thres=1.2) {
  # bin numeric variables to categorical variables based on contrast
  # patterns
  #
  if (!is.numeric(x)) {
    stop('only deal with numeric variables')
  }

  # initialization break points
  x_sorted <- sort(x)
  index <- dm::bin_index(length(x), k_start)
  split_values <- unique(x_sorted[index])
  if (integer_breaks == T) {
    split_values <- floor(split_values)
  }
  split_values <- unique(split_values)
  split_values <- split_values[1:(length(split_values) - 1)]

  # remove bins with less population
  x_bin <- cut(x, breaks = c(-Inf, split_values, Inf), right = T)
  x_dist <- table(x_bin, y)
  row_sums <- x_dist[, 1] + x_dist[, 2]
  row_sums = round(row_sums/length(x),4)
  index <- which(row_sums < 0.05)
  first_in <- 1 %in% index
  if (length(index) > 0) {
    index <- index - 1
    index <- index[which(index %in% 1:length(split_values))]
    if(first_in) index <- c(1, index)
  }
  if (length(index) > 0)
    split_values <- split_values[-index]

  # initialization
  x_bin <- cut(x, breaks = c(-Inf, split_values, Inf), right = T)
  x_dist <- table(x_bin, y)
  col_sums <- colSums(x_dist)
  for (i in 1:ncol(x_dist)) {
    x_dist[,i] <- round(x_dist[,i]/col_sums[i],4)
  }
  contrast_ratio <- x_dist[,1]/x_dist[,2]

  significance_label <- rep(0, nrow(x_dist))
  significance_label[which(contrast_ratio >= thres)] <- 1
  significance_label[which(contrast_ratio <= 1/thres)] <- 2

  # merge the bins
  remove_index <- integer(0)
  i <- 1
  label_length <- length(significance_label)
  while (i < label_length) {
    j <- i + 1
    while ((j <= label_length) & (significance_label[j] == significance_label[i])) {
      remove_index <- c(remove_index,j - 1)
      j <- (j + 1)
    }
    i <- j
  }

  if (length(remove_index) > 0) {
    split_values <- split_values[-remove_index]
  }

  res <- cut(x, breaks = c(-Inf, split_values, Inf))
  return(res)
}








