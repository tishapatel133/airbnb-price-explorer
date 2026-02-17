# ============================================================================
# utils.R
# Helper functions used across the analysis scripts
# ============================================================================

library(tidyverse)
library(caret)

# --- Clean price strings to numeric ---
# Converts "$1,234.00" format to 1234.00
parse_price <- function(price_col) {
  as.numeric(gsub("[$,]", "", price_col))
}


# --- Extract number from bathroom text ---
# Handles "2 baths", "1 shared bath", "Half-bath", etc.
parse_bathrooms <- function(bath_text) {
  result <- rep(NA_real_, length(bath_text))
  
  half <- grepl("half", bath_text, ignore.case = TRUE)
  result[half] <- 0.5
  
  nums <- as.numeric(str_extract(bath_text, "[0-9\\.]+"))
  result[!half & !is.na(nums)] <- nums[!half & !is.na(nums)]
  
  return(result)
}


# --- Print confusion matrix summary ---
# Cleaner output than the full caret confusionMatrix print
print_model_summary <- function(cm, model_name = "Model") {
  cat("\n---", model_name, "---\n")
  cat("Accuracy:   ", round(cm$overall["Accuracy"], 4), "\n")
  cat("Sensitivity:", round(cm$byClass["Sensitivity"], 4), "\n")
  cat("Specificity:", round(cm$byClass["Specificity"], 4), "\n")
  cat("Balanced Acc:", round(cm$byClass["Balanced Accuracy"], 4), "\n")
}


# --- Create a clean comparison table from multiple confusion matrices ---
compare_models <- function(cm_list) {
  # cm_list should be a named list like list("Random Forest" = rf_cm, "XGBoost" = xgb_cm)
  
  map_dfr(names(cm_list), function(name) {
    cm <- cm_list[[name]]
    tibble(
      Model = name,
      Accuracy = round(cm$overall["Accuracy"], 4),
      Sensitivity = round(cm$byClass["Sensitivity"], 4),
      Specificity = round(cm$byClass["Specificity"], 4),
      Balanced_Accuracy = round(cm$byClass["Balanced Accuracy"], 4)
    )
  })
}


# --- Save a ggplot to reports/figures/ ---
save_plot <- function(plot, filename, w = 10, h = 6) {
  path <- paste0("reports/figures/", filename)
  ggsave(path, plot, width = w, height = h, dpi = 300, bg = "white")
  cat("Saved:", path, "\n")
}


# --- Quick missing value summary ---
check_missing <- function(df) {
  missing <- colSums(is.na(df))
  missing <- missing[missing > 0]
  
  if (length(missing) == 0) {
    cat("No missing values found.\n")
  } else {
    tibble(
      column = names(missing),
      n_missing = unname(missing),
      pct = round(100 * missing / nrow(df), 1)
    ) %>% arrange(desc(n_missing))
  }
}