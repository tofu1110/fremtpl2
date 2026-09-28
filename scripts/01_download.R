# Download freMTPL2freq and freMTPL2sev from CASdatasets and cache them
# as .rds files in data/raw/.
#  Run once in the beginning.

library(CASdatasets)
library(here)

dir.create(here("data", "raw"), recursive = TRUE, showWarnings = FALSE)

freq_path <- here("data", "raw", "freMTPL2freq.rds")
sev_path <- here("data", "raw", "freMTPL2sev.rds")

if (!file.exists(freq_path) || !file.exists(sev_path)) {
  
  data("freMTPL2freq", package = "CASdatasets")
  data("freMTPL2sev", package = "CASdatasets")
  
  saveRDS(freMTPL2freq, freq_path)
  saveRDS(freMTPL2sev, sev_path)
  
  # For reproducibility
  writeLines(
    c(paste("CASdatasets version:", as.character(packageVersion("CASdatasets"))),
      paste("R version:", R.version.string),
      paste("Downloaded on:", Sys.Date()),
      paste("freMTPL2freq dim:", paste(dim(freMTPL2freq), collapse = " x ")),
      paste("freMTPL2sev  dim:", paste(dim(freMTPL2sev),  collapse = " x "))),
    here("data", "raw", "SOURCE.txt")
  )
  
  message("Saved raw data to data/raw/")
  
} else {
  message("Raw data already cached")
}
