# Title: ImportModelingData
# Date: 27 septembre 2017
# Author: Stephane Caron
# Subject: The goal of this script is to import the splitted data and bind it into one data.table that can be used for moedeling


# Load packages -----------------------------------------------------------

library(data.table)

# Import data -------------------------------------------------------------

import_splitted_data <- function(filename, split_number) {
  data_temp <- list(NA)
  for (split in 1:split_number) {
    data_temp[[split]] <- fread(paste0(filename, "_", split, ".csv"))
  }
  data <- do.call(rbind, data_temp)
  return(data)
}

