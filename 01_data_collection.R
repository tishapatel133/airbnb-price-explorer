# ============================================================================
# 01_data_collection.R
# Downloads Airbnb listing data from InsideAirbnb for 6 major US cities
# Source: http://insideairbnb.com
# ============================================================================

library(tidyverse)

# --- Define cities and their InsideAirbnb URLs ---
cities <- list(
  boston = list(
    name = "Boston",
    state = "ma",
    url_path = "united-states/ma/boston",
    date = "2025-09-23"
  ),
  chicago = list(
    name = "Chicago",
    state = "il",
    url_path = "united-states/il/chicago",
    date = "2025-09-22"
  ),
  washington_dc = list(
    name = "Washington D.C.",
    state = "dc",
    url_path = "united-states/dc/washington-dc",
    date = "2025-09-22"
  ),
  hawaii = list(
    name = "Hawaii",
    state = "hi",
    url_path = "united-states/hi/hawaii",
    date = "2025-09-16"
  ),
  denver = list(
    name = "Denver",
    state = "co",
    url_path = "united-states/co/denver",
    date = "2025-09-29"
  ),
  san_diego = list(
    name = "San Diego",
    state = "ca",
    url_path = "united-states/ca/san-diego",
    date = "2025-09-25"
  )
)

# --- Download function ---
download_city <- function(city_info) {
  
  base_url <- paste0(
    "http://data.insideairbnb.com/",
    city_info$url_path, "/", city_info$date, "/data/listings.csv.gz"
  )
  
  dest_file <- paste0("data/raw/", tolower(gsub("[. ]", "_", city_info$name)), "_listings.csv.gz")
  
  cat("Downloading", city_info$name, "...\n")
  cat("URL:", base_url, "\n")
  
  tryCatch({
    download.file(base_url, destfile = dest_file, mode = "wb")
    cat("  Done ->", dest_file, "\n\n")
    return(dest_file)
  }, error = function(e) {
    cat("  FAILED:", e$message, "\n")
    cat("  Check the date at https://insideairbnb.com/get-the-data/\n\n")
    return(NULL)
  })
}


# --- Download all cities ---
downloaded_files <- list()

for (city_key in names(cities)) {
  result <- download_city(cities[[city_key]])
  if (!is.null(result)) {
    downloaded_files[[city_key]] <- result
  }
}


# --- Read and combine ---
cat("\n--- Reading and combining datasets ---\n\n")

all_listings <- map_dfr(names(downloaded_files), function(city_key) {
  
  file_path <- downloaded_files[[city_key]]
  city_name <- cities[[city_key]]$name
  
  cat("Reading", city_name, "...")
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(city = city_name)
  
  cat(nrow(df), "listings\n")
  return(df)
})

cat("\n========================================\n")
cat("Total combined listings:", nrow(all_listings), "\n")
cat("Cities included:", n_distinct(all_listings$city), "\n")
cat("Columns:", ncol(all_listings), "\n")
cat("========================================\n")


# --- Select columns we'll use ---
listings_clean <- all_listings %>%
  select(
    id, city,
    neighbourhood_cleansed, latitude, longitude,
    property_type, room_type, accommodates, bedrooms, beds, bathrooms_text,
    price,
    availability_30, availability_60, availability_90, availability_365,
    host_since, host_response_time, host_response_rate, host_acceptance_rate,
    host_is_superhost, host_listings_count, host_total_listings_count,
    host_identity_verified,
    number_of_reviews, number_of_reviews_ltm, number_of_reviews_l30d,
    first_review, last_review, review_scores_rating, review_scores_accuracy,
    review_scores_cleanliness, review_scores_checkin, review_scores_communication,
    review_scores_location, review_scores_value, reviews_per_month,
    minimum_nights, maximum_nights, instant_bookable,
    calculated_host_listings_count
  )

cat("\nSelected dataset:", nrow(listings_clean), "rows x", ncol(listings_clean), "columns\n")
cat("\nListings per city:\n")
print(table(listings_clean$city))

cat("\nRoom types:\n")
print(table(listings_clean$room_type))

cat("\nMissing values:\n")
missing <- colSums(is.na(listings_clean))
print(missing[missing > 0])

# --- Save ---
write_csv(listings_clean, "data/raw/all_cities_raw.csv")
cat("\nSaved to data/raw/all_cities_raw.csv\n")
cat("File size:", round(file.size("data/raw/all_cities_raw.csv") / 1024 / 1024, 1), "MB\n")