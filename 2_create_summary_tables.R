
rm(list = ls())

### Required packages

library(testthat)
library(dplyr)
library(purrr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

source("support_functions.r")

analysis_data <- read.csv("analysis_data.csv", stringsAsFactors = FALSE)

# subset to patients whose EKG's have been coded by our cardiologists
checked_data <- analysis_data[which(analysis_data$checked == 1), ]

# and split out each population
mos <- checked_data[which(checked_data$population == "Moseten"), ]
tsi <- checked_data[which(checked_data$population == "Tsimane"), ]

expect_true(nrow(checked_data) == 1848) # down from 1867 b/c of missing ages
expect_true(abs(mean(checked_data$obs_time) - 1569) < 1) 
expect_true(abs(sum(checked_data$obs_time)/365 - 7945) < 1) 
expect_true(abs(mean(checked_data$obs_time_over40, na.rm = TRUE) - 1567) < 1)  
expect_true(abs(sum(checked_data$obs_time_over40, na.rm = TRUE)/365 - 7937) < 1) 

expect_true(sum(checked_data$population == "Tsimane" & checked_data$incidence == 1) == 1059)
expect_true(sum(checked_data$population == "Moseten" & checked_data$incidence == 1) == 310)

expect_true(all(checked_data$obs_time[checked_data$incidence == 1] > 45))

expect_true(mean(checked_data$obs_time[checked_data$population == "Moseten" & checked_data$incidence == 1]) - 647 < 30)
expect_true(mean(checked_data$obs_time[checked_data$population == "Tsimane" & checked_data$incidence == 1]) - 2549 < 30)
expect_true(sum(checked_data$obs_time[checked_data$population == "Tsimane" & checked_data$incidence == 1])/365 - 7395 < 1)
expect_true(sum(checked_data$obs_time[checked_data$population == "Moseten" & checked_data$incidence == 1])/365 - 550 < 1)

### Tsimane Table 1 ###

# Reduce to Tsimane and split by age category
d <- tsi
dbs <- split(d, d$agecats)

# also stash the age weights for weighting later
tsim_age_weights <- prop.table(table(d$agecats))

list(
  list(
    label = "N with ECG",
    agecat_values = lapply(dbs, function(z) as.character(nrow(z))),
    Total = as.character(nrow(d))
  ) %>% flatten(),
  list(
    label = "Percent Male",
    agecat_values = lapply(dbs, function(z) sprintf("%.2f", mean(z$male, na.rm = TRUE))),
    Total = sprintf("%.2f", mean(d$male, na.rm = TRUE))
  ) %>% flatten(),
  list(
    label = "Anthropometric Measures"
  ),
  list(
    label = "Height cm",
    N = sum(!is.na(d$height)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$height)),
    Total = mean_sd(d$height),
    Significance = calc_age_pvalue("height", d)
  ) %>% flatten(),
  list(
    label = "Weight kg",
    N = sum(!is.na(d$weight)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$weight)),
    Total = mean_sd(d$weight),
    Significance = calc_age_pvalue("weight", d)
  ) %>% flatten(),
  list(
    label = "BMI",
    N = sum(!is.na(d$bmi)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$bmi)),
    Total = mean_sd(d$bmi),
    Significance = calc_age_pvalue("bmi", d)
  ) %>% flatten(),
  list(
    label = "% BMI > 30",
    N = sum(!is.na(d$bmi_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$bmi_risk, prop = TRUE)),
    Total = mean_se(d$bmi_risk, prop = T),
    Significance = calc_age_pvalue("bmi_risk", d)
  ) %>% flatten(),
  list(
    label = "Waist Circ.",
    N = sum(!is.na(d$waist)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$waist)),
    Total = mean_sd(d$waist),
    Significance = calc_age_pvalue("waist", d)
  ) %>% flatten(),
  list(
    label = "% Waist Circ. High",
    N = sum(!is.na(d$waist_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$waist_risk, prop = TRUE)),
    Total = mean_se(d$waist_risk, prop = TRUE),
    Significance = calc_age_pvalue("waist_risk", d)
  ) %>% flatten(),
  list(
    label = "Lipid Profile"
  ),
  list(
    label = "Total Cholesterol mg/dL",
    N = sum(!is.na(d$cholesterol)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$cholesterol)),
    Total = mean_sd(d$cholesterol),
    Significance = calc_age_pvalue("cholesterol", d)
  ) %>% flatten(),
  list(
    label = "% Cholesterol > 240 mg/dL",
    N = sum(!is.na(d$cholesterol_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$cholesterol_risk, prop = TRUE)),
    Total = mean_se(d$cholesterol_risk, prop = TRUE),
    Significance = calc_age_pvalue("cholesterol_risk", d)
  ) %>% flatten(),
  list(
    label = "LDL mg/dL",
    N = sum(!is.na(d$ldl)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$ldl)),
    Total = mean_sd(d$ldl),
    Significance = calc_age_pvalue("ldl", d)
  ) %>% flatten(),
  list(
    label = "% LDL > 130 mg/dL",
    N = sum(!is.na(d$ldl_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$ldl_risk, prop = TRUE)),
    Total = mean_se(d$ldl_risk, prop = TRUE),
    Significance = calc_age_pvalue("ldl_risk", d)
  ) %>% flatten(),
  list(
    label = "HDL mg/dL",
    N = sum(!is.na(d$hdl)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$hdl)),
    Total = mean_sd(d$hdl),
    Significance = calc_age_pvalue("hdl", d)
  ) %>% flatten(),
  list(
    label = "Triglycerides mg/dL",
    N = sum(!is.na(d$triglycerides)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$triglycerides)),
    Total = mean_sd(d$triglycerides),
    Significance = calc_age_pvalue("triglycerides", d)
  ) %>% flatten(),
  list(
    label = "% Triglycerides > 200 mg/dL",
    N = sum(!is.na(d$triglycerides_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$triglycerides_risk, prop = TRUE)),
    Total = mean_se(d$triglycerides_risk, prop = TRUE),
    Significance = calc_age_pvalue("triglycerides_risk", d)
  ) %>% flatten(),
  list(
    label = "Inflammatory Markers"
  ),
  list(
    label = "HS-CRP mg/dL",
    N = sum(!is.na(d$crp)),
    agecat_values = lapply(dbs, function(z) median_mad(z$crp)),
    Total = median_mad(d$crp),
    Significance = calc_age_pvalue("crp", d)
  ) %>% flatten(),
  list(
    label = "% HS-CRP > 3 mg/dL",
    N = sum(!is.na(d$crp_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$crp_risk, prop = TRUE)),
    Total = mean_se(d$crp_risk, prop = TRUE),
    Significance = calc_age_pvalue("crp_risk", d)
  ) %>% flatten(),
  list(
    label = "ESR mm/h",
    N = sum(!is.na(d$esr)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$esr)),
    Total = mean_sd(d$esr),
    Significance = calc_age_pvalue("esr", d)
  ) %>% flatten(),
  list(
    label = "% Elevated ESR",
    N = sum(!is.na(d$esr_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$esr_risk, prop = TRUE)),
    Total = mean_se(d$esr_risk, prop = TRUE),
    Significance = calc_age_pvalue("esr_risk", d)
  ) %>% flatten(),
  list(
    label = "IL6 (pm/mL)",
    N = sum(!is.na(d$il6)),
    agecat_values = lapply(dbs, function(z) median_mad(z$il6)),
    Total = median_mad(d$il6),
    Significance = calc_age_pvalue("log(il6)", d)
  ) %>% flatten(),
  list(
    label = "Blood Pressure and Hypertension"
  ),
  list(
    label = "Heart Rate (BPM)",
    N = sum(!is.na(d$hr)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$hr)),
    Total = mean_sd(d$hr),
    Significance = calc_age_pvalue("hr", d)
  ) %>% flatten(),
  list(
    label = "Systolic Blood Pressure mmHg",
    N = sum(!is.na(d$systolic_bp)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$systolic_bp)),
    Total = mean_sd(d$systolic_bp),
    Significance = calc_age_pvalue("systolic_bp", d)
  ) %>% flatten(),
  list(
    label = "% SBP > 120 mmHg",
    N = sum(!is.na(d$systolic_bp_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$systolic_bp_risk, prop = TRUE)),
    Total = mean_se(d$systolic_bp_risk, prop = TRUE),
    Significance = calc_age_pvalue("systolic_bp_risk", d)
  ) %>% flatten(),
  list(
    label = "Diastolic Blood Pressure mmHg",
    N = sum(!is.na(d$diastolic_bp)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$diastolic_bp)),
    Total = mean_sd(d$diastolic_bp),
    Significance = calc_age_pvalue("diastolic_bp", d)
  ) %>% flatten(),
  list(
    label = "% DBP > 80 mmHg",
    N = sum(!is.na(d$diastolic_bp_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$diastolic_bp_risk, prop = TRUE)),
    Total = mean_se(d$diastolic_bp_risk, prop = TRUE),
    Significance = calc_age_pvalue("diastolic_bp_risk", d)
  ) %>% flatten(),
  list(
    label = "% with Hypertension",
    N = sum(!is.na(d$hypertension)),
    agecat_values = lapply(dbs, function(z) mean_se(z$hypertension, prop = TRUE)),
    Total = mean_se(d$hypertension, prop = TRUE),
    Significance = calc_age_pvalue("hypertension", d)
  ) %>% flatten(),
  list(
    label = "ECG Measures"
  ),
  list(
    label = "PR",
    N = sum(!is.na(d$pr)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$pr)),
    Total = mean_sd(d$pr),
    Significance = calc_age_pvalue("pr", d)
  ) %>% flatten(),
  list(
    label = "% PR > 200",
    N = sum(!is.na(d$pr_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$pr_risk, prop = TRUE)),
    Total = mean_se(d$pr_risk, prop = TRUE),
    Significance = calc_age_pvalue("pr_risk", d)
  ) %>% flatten(),
  list(
    label = "QRS",
    N = sum(!is.na(d$qrs)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$qrs)),
    Total = mean_sd(d$qrs),
    Significance = calc_age_pvalue("qrs", d)
  ) %>% flatten(),
  list(
    label = "% QRS > 120",
    N = sum(!is.na(d$qrs_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$qrs_risk, prop = TRUE)),
    Total = mean_se(d$qrs_risk, prop = TRUE),
    Significance = calc_age_pvalue("qrs_risk", d)
  ) %>% flatten(),
  list(
    label = "Coronary Calcium scores from CT scan"
  ),
  list(
    label = "% CAC > 100 AU",
    N = sum(!is.na(d$cac_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$cac_risk, prop = TRUE)),
    Total = mean_se(d$cac_risk, prop = TRUE),
    Significance = calc_age_pvalue("cac_risk", d)
  ) %>% flatten()
) %>% bind_rows() %>% as.data.frame() -> out

out[is.na(out)] <- ""
out$Significance[out$Significance == "0.000"] <- "<0.001"

out <- select(out, label, N, everything())

out <- rename(out,
  `Age Group` = label
)

write.csv(out, "full_table_tsimane.csv", row.names = FALSE)

print("Tsimane table created")

##### Moseten Table 1 #####

# Reduce to Moseten, and split by age category
d <- mos
dbs <- split(d, d$agecats)

list(
  list(
    label = "N with ECG",
    agecat_values = lapply(dbs, function(z) as.character(nrow(z))),
    Total = as.character(nrow(d))
  ) %>% flatten(),
  list(
    label = "Percent Male",
    agecat_values = lapply(dbs, function(z) sprintf("%.2f", mean(z$male, na.rm = TRUE))),
    Total = sprintf("%.2f", mean(d$male, na.rm = TRUE))
  ) %>% flatten(),
  list(
    label = "Anthropometric Measures"
  ),
  list(
    label = "Height cm",
    N = sum(!is.na(d$height)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$height)),
    Total = adj_mean_sd(d$height, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("height", d)
  ) %>% flatten(),
  list(
    label = "Weight kg",
    N = sum(!is.na(d$weight)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$weight)),
    Total = adj_mean_sd(d$weight, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("weight", d)
  ) %>% flatten(),
  list(
    label = "BMI",
    N = sum(!is.na(d$bmi)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$bmi)),
    Total = adj_mean_sd(d$bmi, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("bmi", d)
  ) %>% flatten(),
  list(
    label = "% BMI > 30",
    N = sum(!is.na(d$bmi_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$bmi_risk, prop = TRUE)),
    Total = adj_mean_se(100*d$bmi_risk, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("bmi_risk", d)
  ) %>% flatten(),
  list(
    label = "Lipid Profile"
  ),
  list(
    label = "Total Cholesterol mg/dL",
    N = sum(!is.na(d$cholesterol)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$cholesterol)),
    Total = adj_mean_sd(d$cholesterol, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("cholesterol", d)
  ) %>% flatten(),
  list(
    label = "% Cholesterol > 240 mg/dL",
    N = sum(!is.na(d$cholesterol_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$cholesterol_risk, prop = TRUE)),
    Total = adj_mean_se(d$cholesterol_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("cholesterol_risk", d)
  ) %>% flatten(),
  list(
    label = "LDL mg/dL",
    N = sum(!is.na(d$ldl)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$ldl)),
    Total = adj_mean_sd(d$ldl, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("ldl", d)
  ) %>% flatten(),
  list(
    label = "% LDL > 130 mg/dL",
    N = sum(!is.na(d$ldl_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$ldl_risk, prop = TRUE)),
    Total = adj_mean_se(d$ldl_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("ldl_risk", d)
  ) %>% flatten(),
  list(
    label = "HDL mg/dL",
    N = sum(!is.na(d$hdl)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$hdl)),
    Total = adj_mean_sd(d$hdl, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("hdl", d)
  ) %>% flatten(),
  list(
    label = "Triglycerides mg/dL",
    N = sum(!is.na(d$triglycerides)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$triglycerides)),
    Total = adj_mean_sd(d$triglycerides, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("triglycerides", d)
  ) %>% flatten(),
  list(
    label = "% Triglycerides > 200 mg/dL",
    N = sum(!is.na(d$triglycerides_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$triglycerides_risk, prop = TRUE)),
    Total = adj_mean_se(d$triglycerides_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("triglycerides_risk", d)
  ) %>% flatten(),
  list(
    label = "Inflammatory Markers"
  ),
  list(
    label = "HS-CRP mg/dL",
    N = sum(!is.na(d$crp)),
    agecat_values = lapply(dbs, function(z) median_mad(z$crp)),
    Total = median_mad(d$crp),
    Significance = calc_age_pvalue("crp", d)
  ) %>% flatten(),
  list(
    label = "% HS-CRP > 3 mg/dL",
    N = sum(!is.na(d$crp_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$crp_risk, prop = TRUE)),
    Total = adj_mean_se(d$crp_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("crp_risk", d)
  ) %>% flatten(),
  list(
    label = "ESR mm/h",
    N = sum(!is.na(d$esr)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$esr)),
    Total = adj_mean_sd(d$esr, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("esr", d)
  ) %>% flatten(),
  list(
    label = "% Elevated ESR",
    N = sum(!is.na(d$esr_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$esr_risk, prop = TRUE)),
    Total = adj_mean_se(d$esr_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("esr_risk", d)
  ) %>% flatten(),
  list(
    label = "IL6 (pm/mL)",
    N = sum(!is.na(d$il6)),
    agecat_values = lapply(dbs, function(z) median_mad(z$il6)),
    Total = median_mad(d$il6),
    Significance = calc_age_pvalue("log(il6)", d)
  ) %>% flatten(),
   list(
    label = "Blood Pressure and Hypertension"
  ),
  list(
    label = "Heart Rate (BPM)",
    N = sum(!is.na(d$hr)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$hr)),
    Total = adj_mean_sd(d$hr, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("hr", d)
  ) %>% flatten(),
  list(
    label = "Systolic Blood Pressure mmHg",
    N = sum(!is.na(d$systolic_bp)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$systolic_bp)),
    Total = adj_mean_sd(d$systolic_bp, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("systolic_bp", d)
  ) %>% flatten(),
  list(
    label = "% SBP > 120 mmHg",
    N = sum(!is.na(d$systolic_bp_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$systolic_bp_risk, prop = TRUE)),
    Total = adj_mean_se(d$systolic_bp_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("systolic_bp_risk", d)
  ) %>% flatten(),
  list(
    label = "Diastolic Blood Pressure mmHg",
    N = sum(!is.na(d$diastolic_bp)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$diastolic_bp)),
    Total = adj_mean_sd(d$diastolic_bp, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("diastolic_bp", d)
  ) %>% flatten(),
  list(
    label = "% DBP > 80 mmHg",
    N = sum(!is.na(d$diastolic_bp_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$diastolic_bp_risk, prop = TRUE)),
    Total = adj_mean_se(d$diastolic_bp_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("diastolic_bp_risk", d)
  ) %>% flatten(),
  list(
    label = "% with Hypertension",
    N = sum(!is.na(d$hypertension)),
    agecat_values = lapply(dbs, function(z) mean_se(z$hypertension, prop = TRUE)),
    Total = adj_mean_se(d$hypertension*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("hypertension", d)
  ) %>% flatten(),
  list(
    label = "ECG Measures"
  ),
  list(
    label = "PR",
    N = sum(!is.na(d$pr)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$pr)),
    Total = adj_mean_sd(d$pr, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("pr", d)
  ) %>% flatten(),
  list(
    label = "% PR > 200",
    N = sum(!is.na(d$pr_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$pr_risk, prop = TRUE)),
    Total = adj_mean_se(d$pr_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("pr_risk", d)
  ) %>% flatten(),
  list(
    label = "QRS",
    N = sum(!is.na(d$qrs)),
    agecat_values = lapply(dbs, function(z) mean_sd(z$qrs)),
    Total = adj_mean_sd(d$qrs, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("qrs", d)
  ) %>% flatten(),
  list(
    label = "% QRS > 120",
    N = sum(!is.na(d$qrs_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$qrs_risk, prop = TRUE)),
    Total = adj_mean_se(d$qrs_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("qrs_risk", d)
  ) %>% flatten(),
  list(
    label = "Coronary Calcium scores from CT scan"
  ),
  list(
    label = "% CAC > 100 AU",
    N = sum(!is.na(d$cac_risk)),
    agecat_values = lapply(dbs, function(z) mean_se(z$cac_risk, prop = TRUE)),
    Total = adj_mean_se(d$cac_risk*100, d$agecats, tsim_age_weights),
    Significance = calc_age_pvalue("cac_risk", d)
  ) %>% flatten()
) %>% bind_rows() %>% as.data.frame() -> out

out[is.na(out)] <- ""
out$Significance[out$Significance == "0.000"] <- "<0.001"

out <- select(out, label, N, everything())

out <- rename(out,
  `Age Group` = label
)

write.csv(out, "full_table_moseten.csv", row.names = FALSE)

print("Moseten table created")

#### Table 3 - Afib Incidence and Prevalence #####

tsi_dbs <- split(tsi, tsi$agecats)
mos_dbs <- split(mos, mos$agecats)

# we need to resolve this issue!
tsi_values <- rbind(
  c(0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 1)
)
colnames(tsi_values) <- c("40-49", "50-59", "60-69", "70-79", "80+")

list(
  list(
    label = "Prevalence"
  ),
  list(
    label = "Tsimane"
  ),
  list(
    label = "N at risk",
    agecat_values = lapply(tsi_dbs, function(z) as.character(nrow(z))) -> atrisk,
    Total = as.character(nrow(tsi))
  ) %>% flatten(),
  list(
    label = "N of cases",
    agecat_values = lapply(tsi_values[1, ], function(x) as.character(x)) -> cases,
    Total = as.character(1)
  ) %>% flatten(),
  list(
    label = "Prevalence (per 1,000)",
    agecat_values = mapply(function(x, y) as.character(round(as.numeric(x)*1000/as.numeric(y), 2)), cases, atrisk),
    Total = as.character(round(1000/nrow(tsi), 2))
  ) %>% flatten(),
  list(
    label = "Moseten"
  ),
  list(
    label = "N at risk",
    agecat_values = lapply(mos_dbs, function(z) as.character(nrow(z))),
    Total = as.character(nrow(mos))
  ) %>% flatten(),
  list(
    label = "N of cases",
    agecat_values = lapply(mos_dbs, function(z) as.character(sum(z$afib_first_visit))),
    Total = as.character(sum(mos$afib_first_visit))
  ) %>% flatten(),
  list(
    label = "Prevalence (per 1,000)",
    agecat_values = lapply(mos_dbs, function(z) as.character(round(sum(z$afib_first_visit)*1000/(nrow(z)), 2))),
    Total = as.character(round(sum(mos$afib_first_visit)*1000/(nrow(mos)), 2))
  ) %>% flatten(),
  list(
    label = "Incidence"
  ),
  list(
    label = "Tsimane"
  ),
  list(
    label = "N with >1 ECG",
    agecat_values = lapply(tsi_dbs, function(z) as.character(sum(z$n_ecg_visits>1))),
    Total = as.character(sum(tsi$n_ecg_visits>1))
  ) %>% flatten(),
  list(
    label = "Risk Years",
    agecat_values = lapply(tsi_dbs, function(z) as.character(round(sum(z$obs_time_over40)/365), 2)) -> riskyrs,
    Total = as.character(round(sum(tsi$obs_time_over40)/365, 2))
  ) %>% flatten(),
  list(
    label = "Average Risk Years",
    agecat_values = lapply(tsi_dbs, function(z) as.character(round(sum(z$obs_time_over40)/(365*sum(z$n_ecg_visits>1)), 2))),
    Total = as.character(round(sum(tsi$obs_time_over40)/(365*sum(tsi$n_ecg_visits>1)), 2))
  ) %>% flatten(),
  list(
    label = "N of New cases",
    agecat_values = lapply(tsi_values[2, ], function(x) as.character(x)) -> newcases,
    Total = as.character(1)
  ) %>% flatten(),
  list(
    label = "Incidence (per 1,000 person-years)",
    agecat_values = mapply(function(x, y) as.character(round(as.numeric(x)*1000/as.numeric(y), 2)), newcases, riskyrs),
    Total = as.character(round(1000/sum(tsi$obs_time_over40)/365, 2))
  ) %>% flatten(),
  list(
    label = "Moseten"
  ),
  list(
    label = "N with >1 ECG",
    agecat_values = lapply(mos_dbs, function(z) as.character(sum(z$n_ecg_visits>1))),
    Total = as.character(sum(mos$n_ecg_visits>1))
  ) %>% flatten(),
  list(
    label = "Risk Years",
    agecat_values = lapply(mos_dbs, function(z) as.character(round(sum(z$obs_time_over40)/365, 2))),
    Total = as.character(round(sum(mos$obs_time_over40)/365, 2))
  ) %>% flatten(),
  list(
    label = "Average Risk Years",
    agecat_values = lapply(mos_dbs, function(z) as.character(round(sum(z$obs_time_over40)/(365*sum(z$n_ecg_visits>1)), 2))),
    Total = as.character(round(sum(mos$obs_time_over40)/(365*sum(mos$n_ecg_visits>1)), 2))
  ) %>% flatten(),
  list(
    label = "N of New cases",
    agecat_values = lapply(mos_dbs, function(z) as.character(sum(z$incident_afib))),
    Total = as.character(sum(mos$incident_afib))
  ) %>% flatten(),
  list(
    label = "Incidence (per 1,000 person-years)",
    agecat_values = lapply(mos_dbs, function(z) as.character(sum(z$incident_afib)/(sum(z$obs_time_over40)/365))),
    Total = as.character(round(sum(mos$incident_afib)/(sum(mos$obs_time_over40)/365), 2))
  ) %>% flatten()
) %>% bind_rows() %>% as.data.frame() -> out

out[is.na(out)] <- ""

out <- select(out, label, everything())

out <- rename(out,
  `Age Group` = label
)

write.csv(out, "table_3.csv", row.names = FALSE)

print("Table 3 created")


####### Strobe Diagrams ######

### Tsimane ###

grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']

      # edge definitions with the node IDs
      tab1 -> tab2-> tab3;
      }

      [1]: paste0('Census population that met inclusion criteria (age>40): N=',sum(analysis_data$population=='Tsimane'))
      [2]: paste0('Prevalence phase ECG: N=', nrow(tsi))
      [3]: paste0('Incidence phase ECG: N=', sum(tsi$n_ecg_visits>1))

      ") %>% export_svg() %>%
  charToRaw %>% 
  rsvg_png("./Tsimane_STROBE.png", height = 700)

### Moseten ###
grViz("digraph flowchart {
        # node definitions with substituted label text
        node [fontname = Helvetica, shape = rectangle]        
        tab1 [label = '@@1']
        tab2 [label = '@@2']
        tab3 [label = '@@3']

        # edge definitions with the node IDs
        tab1 -> tab2-> tab3;
      }
      [1]: paste0('Census population that met inclusion criteria (age>40): N=', sum(analysis_data$population=='Moseten'))
      [2]: paste0('Prevalence phase ECG: N=', nrow(mos))
      [3]: paste0('Incidence phase ECG: N=', sum(mos$n_ecg_visits>1))
      ") %>% export_svg() %>%
  charToRaw %>% 
  rsvg_png("./Moseten_STROBE.png", height = 700)

print("STROBE diagrams created")


### Table 4 - Comparison of Sampled and Unsampled Groups ###

analysis_data$incidence <- as.numeric(analysis_data$n_ecg_visits > 1)

dbm <- analysis_data[which(analysis_data$population=="Moseten"),]
dbt <- analysis_data[which(analysis_data$population=="Tsimane"),]

list(
  list(
    Variable = "Tsimane"
  ),
  calc_comparisons("age", "Age", dbt),
  calc_comparisons("ldl", "LDL", dbt),
  calc_comparisons("hdl", "HDL", dbt),
  calc_comparisons("cholesterol", "Cholesterol", dbt),
  calc_comparisons("bmi", "BMI", dbt),
  calc_comparisons("systolic_bp", "Systolic BP", dbt),
  calc_comparisons("log_il6", "Log IL6", dbt),
  calc_comparisons("esr", "ESR", dbt),
  list(
    Variable = "Moseten"
  ),
  calc_comparisons("age", "Age", dbm),
  calc_comparisons("ldl", "LDL", dbm),
  calc_comparisons("hdl", "HDL", dbm),
  calc_comparisons("cholesterol", "Cholesterol", dbm),
  calc_comparisons("bmi", "BMI", dbm),
  calc_comparisons("systolic_bp", "Systolic BP", dbm),
  calc_comparisons("log_il6", "Log IL6", dbm),
  calc_comparisons("esr", "ESR", dbm)
) %>% bind_rows() %>% as.data.frame() -> out

out[is.na(out)] <- ""

write.csv(out, "supplementary_table_1.csv", row.names = FALSE)

print("SI table 1 created")

