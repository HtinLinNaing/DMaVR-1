packages <- c("data.table", "fst", "ggplot2", "stats", "plotly", "ggridges", "ggExtra", "mgcv", "dplyr")
for (i in packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i)
    library(i, character.only = TRUE)
  }
}

# import data
import_data <- function(path) {
  dta <- data.table::as.data.table(fst::read_fst(path)) # assuming data file is at the smae directory
  dta[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]
  return(dta)
}
dta <- import_data("data_snipet.fst")
head(dta)
tail(dta)


## Part 1: Exploratory Data Analysis and Temporal Dynamics ##
# Task 1: Advanced Time-Series Analysis of Runoff
plot_timeseries_runoff <- function(dta) {
  p <- ggplot2::ggplot(dta, ggplot2::aes(x = as.Date(paste(YR, MNTH, "15", sep = "-")), y = OBS_RUN)) +
    labs(title = "Monthly Observed Runoff with Anomalies", x = "Date", y = "Observed Runoff") +
    ggplot2::geom_line(color = "black") +
    ggplot2::geom_point(data = dta[OBS_RUN > stats::quantile(OBS_RUN, 0.95, na.rm = TRUE)], ggplot2::aes(x = as.Date(paste(YR, MNTH, "15", sep = "-")), y = OBS_RUN), color = "red", shape = 18) +
    ggplot2::facet_wrap(~ID) +
    ggplot2::theme_minimal()
  return(plotly::ggplotly(p))
}
plot_timeseries_runoff(dta)


# Task 2: Precipitation-Runoff Relationship and Non-linear Analysis
plot_prcp_runoff <- function(dta) {
  p <- ggplot2::ggplot(dta, ggplot2::aes(x = PRCP, y = OBS_RUN)) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x) +
    ggplot2::stat_summary(fun = stats::median, geom = "line", ggplot2::aes(group = 1), color = "red") +
    ggplot2::facet_grid(ID ~ HYR) +
    ggplot2::theme_minimal()
  return(plotly::ggplotly(p))
}
plot_prcp_runoff(dta)


## Part 2: Complex Snowmelt and Runoff Dynamics ##
# Task 3: Comprehensive Visualization of SWE Dynamics
plot_swe_dynamic <- function(dta) {
  p <- ggplot2::ggplot(dta, ggplot2::aes(x = SWE, y = factor(MNTH), fill = factor(MNTH))) +
    ggridges::geom_density_ridges(scale = 1.2, alpha = 0.6) +
    ggplot2::geom_jitter(height = 0.1, alpha = 0.3) +
    ggplot2::facet_grid(ID ~ HYR) +
    ggplot2::theme_minimal()
  return(plotly::ggplotly(p))
}
plot_swe_dynamic(dta)


# Task 4: Multivariate Snowmelt and Runoff Correlation Analysis
plot_multivariate_snowmelt <- function(dta) {
  p <- ggplot2::ggplot(dta, ggplot2::aes(x = SWE, y = OBS_RUN, color = PET)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
    ggplot2::facet_wrap(~ID) +
    ggplot2::theme_minimal()
  return(ggExtra::ggMarginal(plotly::ggplotly(p), type = "density", groupColour = TRUE))
}
plot_multivariate_snowmelt(dta)


# Task 5: Water Balance and Surplus-Deficit Dynamics
plot_water_balance <- function(dta) {
  dta[, WB := PRCP - PET]
  dta[, Quarter := factor(ceiling(MNTH / 3), labels = c("Winter", "Spring", "Summer", "Fall"))]
  p <- ggplot2::ggplot(dta, ggplot2::aes(x = MNTH, y = WB, fill = WB)) +
    ggplot2::geom_col() +
    ggplot2::geom_line(ggplot2::aes(group = HYR), color = "black") +
    ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    ggplot2::facet_grid(ID ~ Quarter) +
    ggplot2::theme_minimal()
  return(plotly::ggplotly(p))
}
plot_water_balance(dta)
