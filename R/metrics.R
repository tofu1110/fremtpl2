# Define the three metric functions to compare models:
# freq_metrics: Poisson deviance, Actual / Expected
# sev_metrics : Gamma deviance, Actual / Expected
# pp_metrics: lift, Gini coefficient

poisson_deviance <- function(y, mu) {
  
  2 * mean(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
  
}

gamma_deviance <- function(y, mu) {
  
  2 * mean(-log(y / mu) + (y - mu) / mu)
  
}

ae_ratio <- function(y, mu) {
  
  sum(y) / sum(mu)
  
}

# pred is the predicted loss including exposure. Policies are ranked by
# predicted pure premium and split into n_bins groups of equal exposure

lift_table <- function(actual, pred, exposure, n_bins = 10) {
  
  dt <- data.table(actual, pred, exposure)[order(pred / exposure)]
  dt[, bin := pmin(ceiling(n_bins * cumsum(exposure) / sum(exposure)), n_bins)]
  
  dt[, .(
    exposure = sum(exposure),
    pred_pp = sum(pred) / sum(exposure),
    actual_pp = sum(actual) / sum(exposure)
  ), keyby = bin]
  
}

gini <- function(actual, pred, exposure) {
  
  o <- order(pred / exposure)
  x <- c(0, cumsum(exposure[o]) / sum(exposure))
  y <- c(0, cumsum(actual[o]) / sum(actual))
  
  1 - sum(diff(x) * (head(y, -1) + tail(y, -1)))
  
}

# Actual pure premium of the highest-risk bin divided by the lowest-risk bin
lift_ratio <- function(tab) {
  
  tab[.N, actual_pp] / tab[1, actual_pp]
  
}

freq_metrics <- function(y, mu) {
  
  data.table(deviance = poisson_deviance(y, mu), ae = ae_ratio(y, mu))
  
}

sev_metrics <- function(y, mu) {
  
  data.table(deviance = gamma_deviance(y, mu), ae = ae_ratio(y, mu))
  
}

pp_metrics <- function(actual, pred, exposure, n_bins = 10) {
  
  tab <- lift_table(actual, pred, exposure, n_bins)
  data.table(lift = lift_ratio(tab), gini = gini(actual, pred, exposure))
  
}