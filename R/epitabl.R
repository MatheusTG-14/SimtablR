#' Simulated Epidemiological Dataset
#'
#' A simulated dataset containing demographic, clinical, and outcome variables
#' for 500 individuals. Designed for demonstrating table creation and
#' diagnostic testing analysis.
#'
#' @format A data frame with 500 rows and 19 variables:
#' \describe{
#'   \item{id}{Unique patient identifier}
#'   \item{age}{Age in years (Numeric)}
#'   \item{sex}{Biological sex (Female, Male)}
#'   \item{bmi}{Body Mass Index in kg/m² (Numeric, contains NAs)}
#'   \item{smoking}{Smoking status (Never, Former, Current)}
#'   \item{exercise}{Physical activity level (Low, Moderate, High)}
#'   \item{education}{Educational attainment (High School, Some College, College+)}
#'   \item{income}{Annual household income (<30k, 30-60k, 60k+)}
#'   \item{disease}{Disease status - primary outcome (No, Yes)}
#'   \item{rapid_test}{Result of rapid diagnostic test (Negative, Positive)}
#'   \item{lab_confirmed}{Laboratory confirmation - gold standard (No, Yes)}
#'   \item{comorbidity_score}{Score 0-5 based on medical history}
#'   \item{outcome1}{Count of primary care visits in past year}
#'   \item{outcome2}{Count of specialist visits in past year}
#'   \item{outcome3}{Count of emergency department visits in past year}
#'   \item{hospitalized}{Hospitalized in past year (No, Yes)}
#'   \item{systolic_bp}{Systolic blood pressure in mmHg}
#'   \item{cholesterol}{Total cholesterol in mg/dL}
#'   \item{region}{Geographic region (North, South, East, West)}
#' }
#' @source Simulated data for the SimtablR package.
#' @examples
#' data(epitabl)
#'
#' # Basic description
#' tb(epitabl, sex, disease)
"epitabl"
