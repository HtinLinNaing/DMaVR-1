packages <- c("data.table", "fst")
for (i in packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i)
    library(i, character.only = TRUE)
  }
}


## PART 1: Hydrological Years and Catchment Selection ##
# Task 1: Assigning Hydrological Years
# import the data from  file into a data.table
data <- as.data.table(read_fst("data_snipet.fst")) # assuming data file is at the smae directory
head(data)
tail(data)

# create the HYR column with the facts if MNTH is October, November, or December, set HYR = YR + 1. Otherwise, keep HYR = YR
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# verify HYR 
subset(data, MNTH == c(10, 11, 12))



# Task 2: Compute Overall Runoff Coefficients per Catchment
# group by ID and calculate total PRCP and OBS_RUN
rc_data <- dta[, .(
  total_prcp = sum(PRCP, na.rm = TRUE), 
  total_runoff = sum(OBS_RUN, na.rm = TRUE)
  ), by = ID]

# calculate runoff coefficient and store as RC column
rc_data[, RC := total_runoff / total_prcp]

# TASK 3: Classify Catchments Based on Runoff Coefficients
# assign RC_class based on quantile cuts
#q <- quantile(rc_data$RC, probs = seq(0, 1, 0.2))
#print(q)
rc_data[, RC_class := cut(RC,
                          breaks = quantile(RC, probs = seq(0, 1, 0.2), na.rm = TRUE),
                          labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                          include.lowest = TRUE)]

# randomly select one catchment from each RC class
selected_catchments <- rc_data[, .SD[sample(.N, 1)], by = RC_class]

# verify the selection spans the full range of runoff coefficients
print(selected_catchments)



## PART 2: Water Balance and Snowmelt Contribution to Runoff ##
# TASK 4: Compute Monthly and Annual Water Balance for Selected Catchments
# -----------------------------------------------
# filter data for the 5 selected catchments
selected_ids <- selected_catchments$ID
data_sel <- data[ID %in% selected_ids]
summary(data_sel) 
summary(data)

# compute monthly means per catchment, year, and month
monthly_means <- data_sel[, .(
  PRCP = mean(PRCP, na.rm = TRUE),
  PET = mean(PET, na.rm = TRUE),
  OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
  ), by = .(HYR, ID, MNTH)]

# compute total annual values
annual_values <- monthly_means[, .(
  annual_PRCP = sum(PRCP),
  annual_PET = sum(PET),
  annual_OBS_RUN = sum(OBS_RUN)
  ), by = .(HYR)]

# compute water balance: WB = PRCP - PET
monthly_means[, WB := PRCP - PET]
annual_values[, WB := annual_PRCP - annual_PET]

# identify months with potential water deficit (WB < 0)
water_deficit_months <- monthly_means[WB < 0]
print(water_deficit_months)


# TASK 5: Snowmelt Contribution to Runoff
# -----------------------------------------------
# compute mean monthly SWE
monthly_SWE <- data_sel[, .(mean_SWE = mean(SWE, na.rm = TRUE)), by = .(ID, HYR, MNTH)]

# get maximum SWE in each HYR
max_SWE <- monthly_SWE[, .SD[which.max(mean_SWE)], by = .(ID, HYR)]
setnames(max_SWE, "mean_SWE", "max_SWE")

# merge to find SWE in months following max SWE
monthly_SWE <- merge(monthly_SWE, max_SWE, by = c("ID", "HYR"))

# estimate snowmelt = max SWE - SWE of that month
monthly_SWE[, snowmelt := max_SWE - mean_SWE]

# merge with runoff
merged_runoff_snowmelt <- merge(
  monthly_SWE[, .(ID, HYR, MNTH, snowmelt)],  # select only needed columns
  monthly_means[, .(ID, HYR, MNTH, OBS_RUN)],
  by = c("ID", "HYR", "MNTH"),
  all = FALSE
)

# correlation between snowmelt and runoff for spring (March-May)
spring_data <- merged_data[MNTH %in% c(3, 4, 5)]

# compute correlation by catchment
cor_results <- spring_data[, .(
  cor_snowmelt_runoff = cor(snowmelt, OBS_RUN, use = "complete.obs")
), by = ID]

print(cor_results)
