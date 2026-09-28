# Clean the raw data and add the variables used in the models.
# sev_cap: per-claim cap applied to ClaimAmount
# Return a list with the prepared freq and sev tables

prepare_data <- function(freq, sev, sev_cap = 1e5) {
  
  freq <- copy(freq)
  sev <- copy(sev)
  
  freq[, IDpol := as.integer(as.character(IDpol))]
  sev[, IDpol := as.integer(as.character(IDpol))]
  
  sev[, ClaimAmountCap := pmin(ClaimAmount, sev_cap)]
  
  freq[, ClaimNb := as.integer(pmin(ClaimNb, 4L))]
  freq[, Exposure := pmin(Exposure, 1)]
  freq[, VehPowerGrp := as.factor(pmin(VehPower, 9))]
  freq[, VehAgeGrp := cut(VehAge,
    breaks = c(0, 1, 11, Inf), right = FALSE,
    labels = c("0", "1-10", "11+"))]
  freq[, DrivAgeGrp := cut(DrivAge,
    breaks = c(18, 21, 26, 31, 41, 51, 71, Inf), right = FALSE,
    labels = c("18-20", "21-25", "26-30", "31-40", "41-50", "51-70", "71+"))]
  freq[, BonusMalus := as.integer(pmin(BonusMalus, 150))]
  freq[, LogDensity := log(Density)]
  
  sev <- freq[sev, on = "IDpol"]
  
  list(freq = freq, sev = sev)
  
}