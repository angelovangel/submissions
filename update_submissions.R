#! /usr/bin/env Rscript

source("global.R")

args <- commandArgs(trailingOnly = TRUE)

#usertoken='AD3521A5-0113-44A1-92B5-3E514166FE08#7'

# read old data, update and write back updated data

# args[1] is olddf
# args[2] is usertoken

olddf <- readRDS(args[1])

# Try updating the data
tryCatch({
  newdf <- update_data(olddf = olddf, token = args[2])
  # Save the updated data only if update_data works
  saveRDS(newdf, args[1])
}, error = function(e) {
  cat("Error in update_data:", e$message, "\n")
  quit(status = 1) # Exit with an error status
})
