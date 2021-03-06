# Title: SwapColumns
# Date: 9 Septembre 2017
# Author: Stephane Caron
# Subject: Define a function to swap cols between players


swap_cols <- function(data, seed = 666) {
  
  # Define rows and cols to swap
  set.seed(seed)
  data <- data[order(tourney_date, match_num, tourney_name)]
  row_swap <- sample(1:nrow(data), 0.5 * nrow(data))
  names <- colnames(data)
  modified_name <- gsub(paste("p1", "p2", sep = "|"), "", names[-ncol(data)])
  
  # Here we place similar stats (p1 and p2) side to side
  new_order <- list(NA)
  new_order <- sapply(1:length(modified_name), function(x) {
    if (any(modified_name[x] == modified_name) == TRUE) {
      new_order[x + 1] <- which(modified_name[x] == modified_name)
    } else {
      new_order[x] <- x
    }
  })
  data <- subset(data, select = c(unlist(unique(new_order)), ncol(data)))
  names <- colnames(data)
  
  col_swap <- which(sapply(1:length(modified_name), function(x) sum(modified_name[x] == modified_name)) > 1)
  # Always swap the cols next to the other ...
  col_swap2 <- c(sapply(1:(length(col_swap)/2), function(x) col_swap[c(2 * x, 2 * x - 1)]))
  
  # Switch the col names
  names[col_swap] <- names[col_swap2]
  
  data_non_swaped <- data.frame(data)[-row_swap,]
  data_swaped <- data.frame(data)[row_swap, names]
  colnames(data_swaped) <- colnames(data_non_swaped)
  data2 <- data.table(rbind(data_non_swaped, data_swaped))[order(tourney_date, match_num)]
  data2[row_swap,]$p1_win  <- 0
  
  # Return data swaped
  data2
}
