# Randomly split policies into training and test sets.
# The data split is done at policy level, so that all claims of a policy
# fall into the same set. This prevents leakage between the freq and sev train/test sets.

split_data <- function(freq, sev, test_prop = 0.2, seed = 123) {
  
  freq <- copy(freq)
  sev <- copy(sev)
  
  set.seed(seed)
  test_ids <- sample(freq$IDpol, size = round(test_prop * nrow(freq)))
  
  freq[, fold := fifelse(IDpol %in% test_ids, "test", "train")]
  sev[freq, fold := i.fold, on = "IDpol"]
  
  list(
    freq_train = freq[fold == "train"],
    freq_test = freq[fold == "test"],
    sev_train = sev[fold == "train"],
    sev_test = sev[fold == "test"]
  )
  
}