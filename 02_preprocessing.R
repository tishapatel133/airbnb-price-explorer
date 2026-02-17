# ============================================================================
# 02_preprocessing.R
# Cleans raw Airbnb data, engineers features, creates target variable
# Input:  data/raw/all_cities_raw.csv
# Output: data/processed/airbnb_cleaned.csv
# ============================================================================

library(tidyverse)

# --- Load raw data ---
raw <- read_csv("data/raw/all_cities_raw.csv", show_col_types = FALSE)
cat("Raw dataset:", nrow(raw), "rows x", ncol(raw), "columns\n")


# ==========================================================================
# 1. CLEAN PRICE COLUMN
# ==========================================================================
# Price is stored as "$1,234.00" string — convert to numeric

raw <- raw %>%
  mutate(
    price = as.numeric(gsub("[\\$,]", "", price))
  )

# Remove rows with missing or zero price
cat("Missing prices:", sum(is.na(raw$price)), "\n")
cat("Zero prices:", sum(raw$price == 0, na.rm = TRUE), "\n")

df <- raw %>%
  filter(!is.na(price), price > 0)

cat("After removing bad prices:", nrow(df), "rows\n")


# ==========================================================================
# 2. CLEAN BATHROOMS
# ==========================================================================
# bathrooms_text is like "2 baths", "1 shared bath", "Half-bath" — extract number

df <- df %>%
  mutate(
    bathrooms = case_when(
      str_detect(bathrooms_text, regex("half", ignore_case = TRUE)) ~ 0.5,
      TRUE ~ as.numeric(str_extract(bathrooms_text, "[0-9]+\\.?[0-9]*"))
    ),
    bathroom_shared = ifelse(str_detect(bathrooms_text, regex("shared", ignore_case = TRUE)), 1, 0)
  )


# ==========================================================================
# 3. CLEAN HOST COLUMNS
# ==========================================================================
# Convert percentage strings like "95%" to numeric, boolean t/f to 1/0

df <- df %>%
  mutate(
    host_response_rate = as.numeric(gsub("%", "", host_response_rate)) / 100,
    host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)) / 100,
    host_is_superhost = case_when(
      host_is_superhost %in% c("t", "TRUE", TRUE) ~ 1,
      host_is_superhost %in% c("f", "FALSE", FALSE) ~ 0,
      TRUE ~ NA_real_
    ),
    host_identity_verified = case_when(
      host_identity_verified %in% c("t", "TRUE", TRUE) ~ 1,
      host_identity_verified %in% c("f", "FALSE", FALSE) ~ 0,
      TRUE ~ NA_real_
    ),
    instant_bookable = case_when(
      instant_bookable %in% c("t", "TRUE", TRUE) ~ 1,
      instant_bookable %in% c("f", "FALSE", FALSE) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Host tenure: how many years since they joined
df <- df %>%
  mutate(
    host_since = as.Date(host_since),
    host_years = as.numeric(difftime(Sys.Date(), host_since, units = "days")) / 365.25
  )


# ==========================================================================
# 4. REMOVE EXTREME OUTLIERS
# ==========================================================================
# Listings priced above $2000/night or below $10 are likely errors or luxury outliers

cat("\nPrice distribution before outlier removal:\n")
cat("  Min:", min(df$price), "| Median:", median(df$price), 
    "| Mean:", round(mean(df$price), 2), "| Max:", max(df$price), "\n")

df <- df %>%
  filter(price >= 10, price <= 2000)

cat("After outlier removal:", nrow(df), "rows\n")
cat("  Min:", min(df$price), "| Median:", median(df$price), 
    "| Mean:", round(mean(df$price), 2), "| Max:", max(df$price), "\n")


# ==========================================================================
# 5. CREATE TARGET VARIABLE: is_expensive
# ==========================================================================
# A listing is "expensive" if its price is in the top 25% within its 
# city AND neighbourhood. This makes the comparison fair across locations.

df <- df %>%
  group_by(city, neighbourhood_cleansed) %>%
  mutate(
    neighbourhood_p75 = quantile(price, 0.75, na.rm = TRUE),
    is_expensive = ifelse(price >= neighbourhood_p75, 1, 0)
  ) %>%
  ungroup()

cat("\nTarget variable distribution:\n")
print(table(df$is_expensive))
cat("Expensive ratio:", round(mean(df$is_expensive) * 100, 1), "%\n")

# Also keep a per-city breakdown
cat("\nis_expensive by city:\n")
df %>%
  group_by(city) %>%
  summarise(
    n = n(),
    n_expensive = sum(is_expensive),
    pct_expensive = round(mean(is_expensive) * 100, 1)
  ) %>%
  print()


# ==========================================================================
# 6. HANDLE MISSING VALUES
# ==========================================================================

# Check what's still missing
cat("\nMissing values in key columns:\n")
missing <- colSums(is.na(df))
print(missing[missing > 0])

# Strategy:
# - bedrooms/beds/bathrooms: impute with median (by room_type)
# - review scores: impute with city median (listings with 0 reviews have NA scores)
# - host columns: impute with median

df <- df %>%
  group_by(room_type) %>%
  mutate(
    bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm = TRUE), bedrooms),
    beds = ifelse(is.na(beds), median(beds, na.rm = TRUE), beds),
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE), bathrooms)
  ) %>%
  ungroup()

# Review scores — impute with city median
review_cols <- c("review_scores_rating", "review_scores_accuracy", 
                 "review_scores_cleanliness", "review_scores_checkin",
                 "review_scores_communication", "review_scores_location", 
                 "review_scores_value", "reviews_per_month")

df <- df %>%
  group_by(city) %>%
  mutate(across(all_of(review_cols), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# Host columns — fill with median
host_cols <- c("host_response_rate", "host_acceptance_rate", "host_is_superhost",
               "host_identity_verified", "host_listings_count", "host_years")

df <- df %>%
  mutate(across(all_of(host_cols), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Drop any remaining rows with critical NAs
df <- df %>% drop_na(price, latitude, longitude, room_type)

cat("\nAfter imputation — remaining NAs:\n")
remaining <- colSums(is.na(df))
print(remaining[remaining > 0])
cat("\nFinal dataset:", nrow(df), "rows\n")


# ==========================================================================
# 7. FEATURE ENGINEERING
# ==========================================================================

df <- df %>%
  mutate(
    # Log price (useful for regression and visualizations)
    log_price = log(price),
    
    # Property size proxy
    ppl_per_bedroom = ifelse(bedrooms > 0, accommodates / bedrooms, accommodates),
    
    # Review activity indicator
    has_reviews = ifelse(number_of_reviews > 0, 1, 0),
    
    # Host professionalism proxy (hosts with many listings are likely professionals)
    is_professional_host = ifelse(calculated_host_listings_count >= 5, 1, 0),
    
    # Availability buckets
    availability_category = case_when(
      availability_365 == 0 ~ "unavailable",
      availability_365 <= 90 ~ "low",
      availability_365 <= 180 ~ "medium",
      availability_365 <= 270 ~ "high",
      TRUE ~ "very_high"
    ),
    
    # Simplify room type for cleaner analysis
    room_type_simple = case_when(
      room_type == "Entire home/apt" ~ "Entire home",
      room_type == "Private room" ~ "Private room",
      room_type == "Hotel room" ~ "Hotel room",
      room_type == "Shared room" ~ "Shared room"
    )
  )


# ==========================================================================
# 8. SELECT FINAL COLUMNS & SAVE
# ==========================================================================

final <- df %>%
  select(
    # IDs & labels
    id, city, neighbourhood_cleansed,
    
    # Target
    is_expensive, price, log_price,
    
    # Location
    latitude, longitude,
    
    # Property
    room_type, room_type_simple, property_type,
    accommodates, bedrooms, beds, bathrooms, bathroom_shared,
    
    # Host
    host_is_superhost, host_identity_verified, host_response_rate, 
    host_acceptance_rate, host_listings_count, host_years,
    is_professional_host,
    
    # Availability
    availability_30, availability_60, availability_90, availability_365,
    availability_category,
    
    # Reviews
    number_of_reviews, number_of_reviews_ltm, number_of_reviews_l30d,
    review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
    review_scores_checkin, review_scores_communication,
    review_scores_location, review_scores_value,
    reviews_per_month, has_reviews,
    
    # Booking
    minimum_nights, maximum_nights, instant_bookable,
    calculated_host_listings_count,
    
    # Engineered
    ppl_per_bedroom
  )


cat("\n========================================\n")
cat("FINAL PROCESSED DATASET\n")
cat("========================================\n")
cat("Rows:", nrow(final), "\n")
cat("Columns:", ncol(final), "\n")
cat("Cities:", paste(unique(final$city), collapse = ", "), "\n")
cat("Target balance:", round(mean(final$is_expensive) * 100, 1), "% expensive\n")
cat("========================================\n")

# Save
write_csv(final, "data/processed/airbnb_cleaned.csv")
cat("\n✓ Saved to data/processed/airbnb_cleaned.csv\n")
cat("  File size:", round(file.size("data/processed/airbnb_cleaned.csv") / 1024 / 1024, 1), "MB\n")