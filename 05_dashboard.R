# ============================================================================
# 05_dashboard.R
# Interactive Airbnb Price Dashboard — 6 US Cities
# "Is this listing expensive for its neighborhood?"
# ============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(randomForest)
library(DT)

# --- Load data ---
df <- read_csv("data/processed/airbnb_cleaned.csv", show_col_types = FALSE)

df <- df %>%
  mutate(
    city = factor(city),
    room_type = factor(room_type),
    is_expensive = factor(is_expensive, levels = c(0, 1), labels = c("No", "Yes")),
    host_is_superhost = factor(host_is_superhost),
    instant_bookable = factor(instant_bookable),
    price_label = paste0("$", round(price, 0)),
    # Popup text for map markers
    popup_text = paste0(
      "<strong>", neighbourhood_cleansed, "</strong><br>",
      "<b>Price:</b> $", round(price, 0), "/night<br>",
      "<b>Room:</b> ", room_type, "<br>",
      "<b>Beds:</b> ", beds, " | <b>Bedrooms:</b> ", bedrooms, "<br>",
      "<b>Rating:</b> ", ifelse(is.na(review_scores_rating), "No reviews", review_scores_rating), "<br>",
      "<b>Superhost:</b> ", ifelse(host_is_superhost == "1", "Yes", "No"), "<br>",
      "<span style='color:", ifelse(is_expensive == "Yes", "#e74c3c", "#27ae60"), "; font-weight:bold;'>",
      ifelse(is_expensive == "Yes", "EXPENSIVE for neighborhood", "Competitively priced"), "</span>"
    )
  )

# --- City center coordinates for map zoom ---
city_centers <- tibble(
  city = c("Boston", "Chicago", "Denver", "Hawaii", "San Diego", "Washington D.C."),
  lat = c(42.36, 41.88, 39.74, 20.80, 32.72, 38.91),
  lng = c(-71.06, -87.63, -104.99, -156.32, -117.16, -77.04),
  zoom = c(12, 11, 12, 9, 11, 12)
)

# --- Train prediction model ---
model_features <- c("accommodates", "bedrooms", "beds", "bathrooms",
                    "availability_365", "minimum_nights", "number_of_reviews",
                    "review_scores_rating", "host_listings_count",
                    "host_is_superhost", "instant_bookable",
                    "reviews_per_month", "host_years")

model_df <- df %>%
  select(is_expensive, all_of(model_features)) %>%
  drop_na()

set.seed(42)
rf_model <- randomForest(is_expensive ~ ., data = model_df, ntree = 200)


# ============================================================================
# UI
# ============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "", width = "0"),
    "Airbnb Price Explorer"
  ),
  id = "nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    "navbar-bg" = "#2c3e50"
  ),
  bg = "#2c3e50",
  
  # Add custom CSS
  header = tags$head(tags$style(HTML("
    .bslib-page-dashboard { background: #f0f2f5; }
    
    .map-container { border-radius: 12px; overflow: hidden; box-shadow: 0 4px 15px rgba(0,0,0,0.1); }
    
    .summary-strip {
      background: white; border-radius: 10px; padding: 12px 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06); margin-top: 10px;
      display: flex; justify-content: space-around; text-align: center;
    }
    .summary-strip .stat { display: inline-block; }
    .summary-strip .stat-value { font-size: 20px; font-weight: 700; color: #2c3e50; }
    .summary-strip .stat-label { font-size: 11px; color: #95a5a6; text-transform: uppercase; }
    
    .result-card {
      border-radius: 12px; padding: 30px; text-align: center;
      margin-top: 20px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    }
    .result-expensive { background: linear-gradient(135deg, #e74c3c, #c0392b); color: white; }
    .result-affordable { background: linear-gradient(135deg, #27ae60, #2ecc71); color: white; }
    .result-card h2 { margin: 0 0 8px 0; font-size: 26px; }
    .result-card p { margin: 0; opacity: 0.9; }
    
    .insight-card {
      background: white; border-radius: 10px; padding: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06); height: 100%;
    }
    .insight-card h5 { color: #2c3e50; margin-bottom: 15px; font-weight: 600; }
    
    .filter-section label { font-weight: 600; color: #2c3e50; font-size: 13px; }
    
    .legend-strip {
      background: white; border-radius: 8px; padding: 8px 15px;
      margin-bottom: 10px; font-size: 13px; text-align: center;
      box-shadow: 0 1px 4px rgba(0,0,0,0.05);
    }
  "))),
  
  
  # ================================================================
  # TAB 1: EXPLORE MAP
  # ================================================================
  nav_panel(
    title = "Explore Map",
    icon = icon("map-location-dot"),
    
    layout_sidebar(
      fillable = TRUE,
      
      sidebar = sidebar(
        title = "Find Listings",
        width = 280,
        
        selectInput("map_city", "City",
                    choices = levels(df$city),
                    selected = "Boston"),
        
        selectInput("map_room", "Room Type",
                    choices = c("Any" = "all", levels(df$room_type)),
                    selected = "all"),
        
        sliderInput("map_guests", "Guests",
                    min = 1, max = 12, value = c(1, 4), step = 1),
        
        sliderInput("map_bedrooms", "Bedrooms",
                    min = 0, max = 8, value = c(0, 3), step = 1),
        
        sliderInput("map_price", "Price Range ($/night)",
                    min = 0, max = 1000, value = c(0, 500), step = 10,
                    pre = "$"),
        
        checkboxInput("map_superhost", "Superhosts only", FALSE),
        
        hr(),
        
        div(class = "legend-strip",
            tags$span(style = "color: #27ae60; font-size: 16px;", "\u25CF"),
            " Affordable  ",
            tags$span(style = "color: #e74c3c; font-size: 16px;", "\u25CF"),
            " Expensive"
        ),
        
        p("Click any dot on the map to see listing details.",
          style = "font-size: 12px; color: #95a5a6; margin-top: 5px;")
      ),
      
      # Main content - map + summary
      div(
        div(class = "map-container",
            leafletOutput("listing_map", height = "520px")
        ),
        
        # Summary strip below map
        uiOutput("map_summary")
      )
    )
  ),
  
  
  # ================================================================
  # TAB 2: PRICE CHECK
  # ================================================================
  nav_panel(
    title = "Price Check",
    icon = icon("tag"),
    
    layout_columns(
      col_widths = c(8, 4),
      fillable = FALSE,
      
      # Left: input form
      card(
        card_header(
          class = "bg-primary text-white",
          "Enter Your Listing Details"
        ),
        card_body(
          p("Fill in the details below to see if your listing would be classified as
            expensive (top 25% price) in its neighborhood.", 
            style = "color: #666; margin-bottom: 20px;"),
          
          layout_columns(
            col_widths = c(4, 4, 4),
            
            # Column 1: Property
            div(
              h6("Property", style = "color: #2c3e50; font-weight: 700; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
              numericInput("pred_accommodates", "How many guests?", value = 4, min = 1, max = 16),
              numericInput("pred_bedrooms", "Bedrooms", value = 1, min = 0, max = 10),
              numericInput("pred_beds", "Beds", value = 2, min = 0, max = 20),
              numericInput("pred_bathrooms", "Bathrooms", value = 1, min = 0, max = 8, step = 0.5)
            ),
            
            # Column 2: Availability
            div(
              h6("Availability", style = "color: #2c3e50; font-weight: 700; border-bottom: 2px solid #e74c3c; padding-bottom: 5px;"),
              sliderInput("pred_avail", "Days available/year", min = 0, max = 365, value = 200),
              numericInput("pred_min_nights", "Minimum night stay", value = 2, min = 1, max = 365),
              selectInput("pred_superhost", "Are you a Superhost?",
                          choices = c("No" = "0", "Yes" = "1")),
              selectInput("pred_instant", "Instant booking?",
                          choices = c("No" = "0", "Yes" = "1"))
            ),
            
            # Column 3: Reviews & Host
            div(
              h6("Reviews & Host", style = "color: #2c3e50; font-weight: 700; border-bottom: 2px solid #2ecc71; padding-bottom: 5px;"),
              numericInput("pred_reviews", "Total reviews", value = 20, min = 0, max = 500),
              numericInput("pred_rating", "Avg rating (1-5)", value = 4.5, min = 1, max = 5, step = 0.1),
              numericInput("pred_rpm", "Reviews per month", value = 1.5, min = 0, max = 20, step = 0.1),
              numericInput("pred_host_listings", "Your total listings", value = 1, min = 1, max = 100),
              numericInput("pred_host_years", "Years hosting", value = 3, min = 0, max = 20)
            )
          ),
          
          div(style = "text-align: center; margin-top: 15px;",
              actionButton("predict_btn", "Check My Price",
                           class = "btn-primary btn-lg",
                           style = "padding: 12px 50px; font-size: 16px; border-radius: 8px;")
          )
        )
      ),
      
      # Right: result + context
      div(
        uiOutput("prediction_result"),
        
        card(
          class = "mt-3",
          card_header("What Drives Premium Pricing?"),
          card_body(
            p("Based on our Random Forest model trained on 63,000+ listings across 6 cities,
              these factors matter most:", style = "font-size: 13px; color: #666;"),
            tags$ol(style = "font-size: 13px;",
                    tags$li(tags$b("More space"), " — accommodates, bedrooms, and beds are the strongest predictors"),
                    tags$li(tags$b("Better reviews"), " — higher ratings correlate with premium pricing"),
                    tags$li(tags$b("Superhost status"), " — provides a modest pricing advantage"),
                    tags$li(tags$b("Availability"), " — listings available year-round tend to be priced higher"),
                    tags$li(tags$b("Instant booking"), " — slight positive effect on premium classification")
            )
          )
        )
      )
    )
  ),
  
  
  # ================================================================
  # TAB 3: INSIGHTS
  # ================================================================
  nav_panel(
    title = "Insights",
    icon = icon("chart-bar"),
    
    layout_columns(
      col_widths = c(12),
      fillable = FALSE,
      
      # Row 1: Value boxes
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Total Listings",
          value = formatC(nrow(df), format = "d", big.mark = ","),
          showcase = icon("house"),
          theme = "primary",
          p("Across 6 US cities")
        ),
        value_box(
          title = "Best Accuracy",
          value = "86.8%",
          showcase = icon("bullseye"),
          theme = "success",
          p("Random Forest (Tuned)")
        ),
        value_box(
          title = "Best AUC",
          value = "0.915",
          showcase = icon("chart-line"),
          theme = "info",
          p("XGBoost")
        ),
        value_box(
          title = "Cross-City Transfer",
          value = "79-82%",
          showcase = icon("globe"),
          theme = "warning",
          p("Model generalizes across markets")
        )
      )
    ),
    
    # Row 2: Charts
    layout_columns(
      col_widths = c(6, 6),
      fillable = FALSE,
      
      card(
        card_header("Model Performance Comparison"),
        card_body(plotOutput("model_comp_plot", height = "320px"))
      ),
      card(
        card_header("Feature Importance (Random Forest)"),
        card_body(plotOutput("feat_imp_plot", height = "320px"))
      )
    ),
    
    # Row 3: Cross-city + Price by city
    layout_columns(
      col_widths = c(6, 6),
      fillable = FALSE,
      
      card(
        card_header("Cross-City Generalization"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(class = "insight-card", style = "text-align: center;",
                h5("Train 5 Cities \u2192 Test Hawaii"),
                h2("78.9%", style = "color: #e74c3c; margin: 10px 0;"),
                p("Sensitivity: 34.0%", style = "font-size: 12px; color: #999;"),
                p("Specificity: 94.0%", style = "font-size: 12px; color: #999;")
            ),
            div(class = "insight-card", style = "text-align: center;",
                h5("Train Hawaii \u2192 Test 5 Cities"),
                h2("81.9%", style = "color: #27ae60; margin: 10px 0;"),
                p("Sensitivity: 43.1%", style = "font-size: 12px; color: #999;"),
                p("Specificity: 95.3%", style = "font-size: 12px; color: #999;")
            )
          ),
          p("The model transfers well across markets. What makes a listing 'expensive'
            relative to its neighborhood follows similar patterns nationwide, though
            sensitivity drops — the model is conservative about labeling unfamiliar
            listings as expensive.",
            style = "font-size: 13px; color: #666; margin-top: 15px;")
        )
      ),
      
      card(
        card_header("Price Distribution by City"),
        card_body(plotOutput("city_price_plot", height = "320px"))
      )
    )
  )
)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Color palette
  city_colors <- c(
    "Boston" = "#e74c3c", "Chicago" = "#3498db", "Denver" = "#2ecc71",
    "Hawaii" = "#f39c12", "San Diego" = "#9b59b6", "Washington D.C." = "#1abc9c"
  )
  
  theme_clean <- theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  
  # ================================================================
  # TAB 1: MAP
  # ================================================================
  
  # Filtered data for map
  map_data <- reactive({
    d <- df %>%
      filter(
        city == input$map_city,
        price >= input$map_price[1],
        price <= input$map_price[2],
        accommodates >= input$map_guests[1],
        accommodates <= input$map_guests[2],
        bedrooms >= input$map_bedrooms[1] | is.na(bedrooms),
        bedrooms <= input$map_bedrooms[2] | is.na(bedrooms)
      )
    
    if (input$map_room != "all") d <- d %>% filter(room_type == input$map_room)
    if (input$map_superhost) d <- d %>% filter(host_is_superhost == "1")
    
    d
  })
  
  # Render the map
  output$listing_map <- renderLeaflet({
    center <- city_centers %>% filter(city == input$map_city)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = center$lng, lat = center$lat, zoom = center$zoom)
  })
  
  # Update markers when filters change
  observe({
    d <- map_data()
    
    # Sample if too many points (keeps map responsive)
    if (nrow(d) > 3000) {
      d <- d %>% sample_n(3000)
    }
    
    colors <- ifelse(d$is_expensive == "Yes", "#e74c3c", "#27ae60")
    
    leafletProxy("listing_map") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = d,
        lng = ~longitude, lat = ~latitude,
        radius = 5,
        color = colors,
        fillColor = colors,
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        opacity = 0.9,
        popup = ~popup_text,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 40,
          spiderfyOnMaxZoom = TRUE,
          showCoverageOnHover = FALSE
        )
      )
  })
  
  # Summary strip below map
  output$map_summary <- renderUI({
    d <- map_data()
    n <- nrow(d)
    avg_price <- round(mean(d$price, na.rm = TRUE), 0)
    pct_expensive <- round(mean(d$is_expensive == "Yes", na.rm = TRUE) * 100, 1)
    med_rating <- round(median(d$review_scores_rating, na.rm = TRUE), 1)
    
    div(class = "summary-strip",
        div(class = "stat",
            div(class = "stat-value", formatC(n, big.mark = ",")),
            div(class = "stat-label", "Listings Found")
        ),
        div(class = "stat",
            div(class = "stat-value", paste0("$", avg_price)),
            div(class = "stat-label", "Avg Price/Night")
        ),
        div(class = "stat",
            div(class = "stat-value", paste0(pct_expensive, "%")),
            div(class = "stat-label", "Expensive")
        ),
        div(class = "stat",
            div(class = "stat-value", ifelse(is.nan(med_rating), "—", med_rating)),
            div(class = "stat-label", "Median Rating")
        )
    )
  })
  
  
  # ================================================================
  # TAB 2: PRICE CHECK
  # ================================================================
  
  observeEvent(input$predict_btn, {
    new_data <- tibble(
      accommodates = input$pred_accommodates,
      bedrooms = input$pred_bedrooms,
      beds = input$pred_beds,
      bathrooms = input$pred_bathrooms,
      availability_365 = input$pred_avail,
      minimum_nights = input$pred_min_nights,
      number_of_reviews = input$pred_reviews,
      review_scores_rating = input$pred_rating,
      host_listings_count = input$pred_host_listings,
      host_is_superhost = factor(input$pred_superhost, levels = levels(model_df$host_is_superhost)),
      instant_bookable = factor(input$pred_instant, levels = levels(model_df$instant_bookable)),
      reviews_per_month = input$pred_rpm,
      host_years = input$pred_host_years
    )
    
    pred_class <- predict(rf_model, newdata = new_data)
    pred_prob <- predict(rf_model, newdata = new_data, type = "prob")
    prob_exp <- round(pred_prob[1, "Yes"] * 100, 1)
    
    if (pred_class == "Yes") {
      output$prediction_result <- renderUI({
        div(class = "result-card result-expensive",
            tags$h2(icon("arrow-trend-up"), " Premium Priced"),
            tags$p(style = "font-size: 18px; margin: 10px 0;",
                   paste0(prob_exp, "% probability")),
            tags$p("This listing would likely be in the top 25% price for its neighborhood."),
            tags$p(style = "font-size: 12px; opacity: 0.8; margin-top: 10px;",
                   "Based on Random Forest model trained on 63,271 listings")
        )
      })
    } else {
      output$prediction_result <- renderUI({
        div(class = "result-card result-affordable",
            tags$h2(icon("check-circle"), " Competitively Priced"),
            tags$p(style = "font-size: 18px; margin: 10px 0;",
                   paste0(round(100 - prob_exp, 1), "% confidence")),
            tags$p("This listing falls in the lower 75% price range for its neighborhood."),
            tags$p(style = "font-size: 12px; opacity: 0.8; margin-top: 10px;",
                   "Based on Random Forest model trained on 63,271 listings")
        )
      })
    }
  })
  
  
  # ================================================================
  # TAB 3: INSIGHTS
  # ================================================================
  
  output$model_comp_plot <- renderPlot({
    comp <- tibble(
      Model = factor(
        c("Logistic Reg.", "Random Forest", "RF (Tuned)", "XGBoost"),
        levels = c("Logistic Reg.", "Random Forest", "RF (Tuned)", "XGBoost")
      ),
      Accuracy = c(0.809, 0.863, 0.868, 0.866),
      AUC = c(0.823, 0.908, 0.912, 0.915)
    ) %>%
      pivot_longer(-Model, names_to = "Metric", values_to = "Value")
    
    ggplot(comp, aes(x = Model, y = Value, fill = Metric)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(aes(label = sprintf("%.1f%%", Value * 100)),
                position = position_dodge(width = 0.7), vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = c("Accuracy" = "#2c3e50", "AUC" = "#3498db")) +
      scale_y_continuous(limits = c(0, 1.05), labels = scales::percent) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_clean
  })
  
  output$feat_imp_plot <- renderPlot({
    imp <- importance(rf_model) %>%
      as.data.frame() %>%
      rownames_to_column("Feature") %>%
      arrange(desc(MeanDecreaseGini)) %>%
      head(10) %>%
      mutate(Feature = fct_reorder(Feature, MeanDecreaseGini))
    
    ggplot(imp, aes(x = Feature, y = MeanDecreaseGini)) +
      geom_col(fill = "#3498db", width = 0.7) +
      geom_text(aes(label = round(MeanDecreaseGini, 0)), hjust = -0.2, size = 3.5) +
      coord_flip() +
      labs(x = NULL, y = "Mean Decrease in Gini") +
      theme_clean +
      expand_limits(y = max(imp$MeanDecreaseGini) * 1.15)
  })
  
  output$city_price_plot <- renderPlot({
    df %>%
      filter(price > 0, price <= 500) %>%
      ggplot(aes(x = reorder(city, price, FUN = median), y = price, fill = city)) +
      geom_boxplot(outlier.alpha = 0.15, show.legend = FALSE) +
      scale_fill_manual(values = city_colors) +
      coord_flip() +
      labs(x = NULL, y = "Price ($/night)") +
      theme_clean
  })
}


# ============================================================================
# RUN
# ============================================================================
shinyApp(ui = ui, server = server)