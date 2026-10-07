# Generate the bundled ESTROBE-ACS teaching cohort.
# Run from the package root with:
# & "C:\Program Files\R\R-4.4.3\bin\Rscript.exe" data-raw/generate-epitabl.R

set.seed(20260711)

n <- 1500L
inv_logit <- function(x) 1 / (1 + exp(-x))
clip <- function(x, lower, upper) pmin(pmax(x, lower), upper)
draw_yes_no <- function(lp) {
  factor(ifelse(stats::runif(length(lp)) < inv_logit(lp), "Yes", "No"),
         levels = c("No", "Yes"))
}
as_yes_no <- function(x) {
  factor(ifelse(!is.na(x) & x, "Yes", "No"), levels = c("No", "Yes"))
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# 1. Identification --------------------------------------------------------
participant_id <- sprintf("ACS%04d", seq_len(n))

# 2. Demographics and baseline cardiovascular risk ---------------------------
age <- round(clip(stats::rnorm(n, mean = 62, sd = 13), 18, 94), 1)
sex <- factor(ifelse(stats::runif(n) < 0.46, "Female", "Male"), levels = c("Female", "Male"))

smoking <- factor(
  sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.48, 0.32, 0.20)),
  levels = c("Never", "Former", "Current")
)
bmi <- round(clip(stats::rnorm(n, 27.4 + 1.2 * (smoking == "Current"), 4.8), 16, 48), 1)
hypertension <- draw_yes_no(-0.45 + 0.040 * (age - 50) + 0.06 * (bmi - 25))
diabetes <- draw_yes_no(-1.65 + 0.025 * (age - 50) + 0.055 * (bmi - 25))
dyslipidemia <- draw_yes_no(-0.75 + 0.025 * (age - 50) + 0.45 * (diabetes == "Yes"))
cholesterol <- round(clip(stats::rnorm(n, 186 + 15 * (dyslipidemia == "Yes"), 38), 80, 390), 0)

# 3. Clinical presentation and renal function ------------------------------
presentation_hours <- round(clip(stats::rgamma(n, shape = 2.2, scale = 3.0), 0.2, 24), 1)
systolic_bp <- round(clip(
  126 + 0.28 * (age - 60) + 8 * (hypertension == "Yes") + stats::rnorm(n, 0, 19),
  70, 230
), 0)

egfr_latent <- clip(
  98 - 0.72 * (age - 40) - 11 * (diabetes == "Yes") -
    7 * (hypertension == "Yes") + stats::rnorm(n, 0, 16),
  5, 135
)
renal_impairment <- as_yes_no(egfr_latent < 60)
# Rare dialysis for Firth penalized likelihood demonstration (~0.9%)
dialysis <- as_yes_no(renal_impairment == "Yes" & stats::runif(n) < 0.04)

# 4. Primary diagnosis and diagnostic substudy -----------------------------
acs_lp <- -0.95 + 0.022 * (age - 60) +
  0.42 * (diabetes == "Yes") + 0.38 * (smoking == "Current") +
  0.30 * (sex == "Male") + 0.35 * (systolic_bp > 140)
adjudicated_acs <- draw_yes_no(acs_lp)

diagnostic_substudy <- as_yes_no(stats::runif(n) < 0.60)
poc_hstn_value <- rep(NA_real_, n)
has_poc <- diagnostic_substudy == "Yes"
poc_hstn_value[has_poc] <- round(exp(
  stats::rnorm(
    sum(has_poc),
    mean = 1.85 + 1.35 * (adjudicated_acs[has_poc] == "Yes") +
      0.45 * (renal_impairment[has_poc] == "Yes"),
    sd = 0.72
  )
), 1)
technical_failure <- has_poc & stats::runif(n) < 0.025
poc_hstn_value[technical_failure] <- NA_real_
poc_hstn_positive <- factor(
  ifelse(is.na(poc_hstn_value), NA_character_,
         ifelse(poc_hstn_value >= 18, "Positive", "Negative")),
  levels = c("Negative", "Positive")
)

# 5. Hospital care and count / multi-outcomes ------------------------------
admitted <- stats::runif(n) < inv_logit(-0.20 + 1.5 * (adjudicated_acs == "Yes"))
length_of_stay <- ifelse(
  admitted,
  pmax(1L, round(stats::rgamma(n, shape = 2.2, scale = 1.4 + 0.6 * (adjudicated_acs == "Yes")))),
  0L
)
ed_visits <- stats::rpois(
  n,
  lambda = exp(-0.40 + 0.35 * (adjudicated_acs == "Yes") +
                 0.25 * (renal_impairment == "Yes") + 0.20 * (diabetes == "Yes"))
)
rehospitalized <- as_yes_no(ed_visits > 0 & stats::runif(n) < 0.60)

# 6. Survival: 365-day follow-up for all 1,500 participants ----------------
mace_prob <- inv_logit(
  -2.40 + 0.85 * (adjudicated_acs == "Yes") +
    0.60 * (renal_impairment == "Yes") + 0.40 * (diabetes == "Yes") +
    0.015 * (age - 60)
)
annual_hazard <- -log(1 - mace_prob)
latent_mace_time <- stats::rexp(n, rate = annual_hazard / 365)
censor_prob <- 0.08
lost <- stats::runif(n) < censor_prob
censor_time <- ifelse(lost, stats::runif(n, 30, 360), 365)
mace_event_logical <- latent_mace_time <= censor_time & latent_mace_time <= 365
mace_time_days <- round(ifelse(mace_event_logical, latent_mace_time, pmin(censor_time, 365)), 1)
mace_time_days <- pmax(mace_time_days, 1.0)
mace_event <- as_yes_no(mace_event_logical)

# 7. Documented incidental missingness -------------------------------------
bmi[stats::runif(n) < 0.05] <- NA_real_
presentation_hours[stats::runif(n) < 0.05] <- NA_real_
cholesterol[stats::runif(n) < 0.05] <- NA_real_

# 8. Assemble canonical 22-column dataframe -------------------------------
epitabl <- data.frame(
  participant_id = participant_id,
  age = age,
  sex = sex,
  bmi = bmi,
  smoking = smoking,
  hypertension = hypertension,
  diabetes = diabetes,
  dyslipidemia = dyslipidemia,
  cholesterol = cholesterol,
  presentation_hours = presentation_hours,
  systolic_bp = systolic_bp,
  renal_impairment = renal_impairment,
  dialysis = dialysis,
  adjudicated_acs = adjudicated_acs,
  diagnostic_substudy = diagnostic_substudy,
  poc_hstn_value = poc_hstn_value,
  poc_hstn_positive = poc_hstn_positive,
  length_of_stay = as.integer(length_of_stay),
  ed_visits = as.integer(ed_visits),
  rehospitalized = rehospitalized,
  mace_time_days = mace_time_days,
  mace_event = mace_event,
  stringsAsFactors = FALSE
)

labels <- c(
  participant_id = "Study participant identifier",
  age = "Age at index presentation (years)",
  sex = "Sex recorded for clinical assessment",
  bmi = "Body mass index (kg/m2)",
  smoking = "Smoking status",
  hypertension = "History of hypertension",
  diabetes = "History of diabetes",
  dyslipidemia = "History of dyslipidemia",
  cholesterol = "Total cholesterol (mg/dL)",
  presentation_hours = "Hours from symptom onset to presentation",
  systolic_bp = "Systolic blood pressure (mmHg)",
  renal_impairment = "Renal impairment (eGFR below 60 mL/min/1.73 m2)",
  dialysis = "Maintenance dialysis",
  adjudicated_acs = "Adjudicated acute coronary syndrome",
  diagnostic_substudy = "Enrolled in point-of-care diagnostic substudy",
  poc_hstn_value = "Point-of-care high-sensitivity troponin (ng/L)",
  poc_hstn_positive = "Point-of-care troponin index-test result",
  length_of_stay = "Index hospital length of stay (days)",
  ed_visits = "Emergency department revisits during 1-year follow-up",
  rehospitalized = "Hospital readmission during 1-year follow-up",
  mace_time_days = "Time to MACE or censoring after presentation (days)",
  mace_event = "Observed MACE event before censoring"
)
for (variable in names(labels)) {
  attr(epitabl[[variable]], "label") <- labels[[variable]]
}

# 9. Generator invariants --------------------------------------------------
stopifnot(
  nrow(epitabl) == 1500L,
  ncol(epitabl) == 22L,
  length(unique(epitabl$participant_id)) == 1500L,
  all(is.na(epitabl$poc_hstn_value[epitabl$diagnostic_substudy == "No"])),
  !anyNA(epitabl$mace_event),
  !anyNA(epitabl$mace_time_days)
)

proportion_yes <- function(x) mean(x == "Yes", na.rm = TRUE)
complete_diag <- !is.na(epitabl$poc_hstn_positive)
test_positive <- epitabl$poc_hstn_positive[complete_diag] == "Positive"
condition_positive <- epitabl$adjudicated_acs[complete_diag] == "Yes"
diagnostic_sensitivity <- mean(test_positive[condition_positive])
diagnostic_specificity <- mean(!test_positive[!condition_positive])

stopifnot(
  proportion_yes(epitabl$adjudicated_acs) >= 0.25,
  proportion_yes(epitabl$adjudicated_acs) <= 0.45,
  proportion_yes(epitabl$renal_impairment) >= 0.15,
  proportion_yes(epitabl$renal_impairment) <= 0.35,
  proportion_yes(epitabl$mace_event) >= 0.08,
  proportion_yes(epitabl$mace_event) <= 0.25,
  proportion_yes(epitabl$dialysis) >= 0.005,
  proportion_yes(epitabl$dialysis) <= 0.03,
  diagnostic_sensitivity >= 0.65,
  diagnostic_sensitivity <= 0.95,
  diagnostic_specificity >= 0.65,
  diagnostic_specificity <= 0.95
)

# 10. Generate the machine-readable dictionary -----------------------------
missingness_note <- rep("None planned", ncol(epitabl))
names(missingness_note) <- names(epitabl)
missingness_note[c("poc_hstn_value", "poc_hstn_positive")] <-
  "Structural outside diagnostic substudy; occasional technical failure inside"
missingness_note[c("bmi", "presentation_hours", "cholesterol")] <-
  "Documented clinical-workflow missingness"

variable_role <- rep("Analysis variable", ncol(epitabl))
names(variable_role) <- names(epitabl)
variable_role["participant_id"] <- "Study structure"
variable_role["diagnostic_substudy"] <- "Substudy structure"

epitabl_codebook <- data.frame(
  variable = names(epitabl),
  label = vapply(epitabl, function(x) attr(x, "label") %||% "", character(1)),
  class = vapply(epitabl, function(x) class(x)[[1L]], character(1)),
  levels = vapply(epitabl, function(x) paste(levels(x) %||% character(), collapse = " | "), character(1)),
  role = unname(variable_role[names(epitabl)]),
  missingness = unname(missingness_note[names(epitabl)]),
  stringsAsFactors = FALSE
)

dir.create("data", showWarnings = FALSE)
dir.create("data-raw", showWarnings = FALSE)
dir.create(file.path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(epitabl_codebook, file.path("data-raw", "epitabl-variable-spec.csv"), row.names = FALSE)
utils::write.csv(epitabl_codebook, file.path("inst", "extdata", "epitabl-codebook.csv"), row.names = FALSE)
save(epitabl, file = file.path("data", "epitabl.rda"), compress = "xz")
message("Successfully generated epitabl (1500 x 22) and codebooks.")
