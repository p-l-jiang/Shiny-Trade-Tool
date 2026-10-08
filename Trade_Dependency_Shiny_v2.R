library(tidyverse)
library(shiny)
library(plotly)
library(DT)

# ============================================================
# DATA LOADING AND PREPARATION
# ============================================================

# Base directory
base_dir <- "C:/Users/JiangPe/OneDrive - Government of Ontario/Documents/Steel/CIMT Data"
setwd("C:/Users/JiangPe/Documents")

# Load description files
load_descriptions <- function(base_dir) {
  descriptions <- list()
  
  # HS10 - 8 columns: code, start_date, end_date, uom, desc_en, desc_fr, source, period
  descriptions$hs10 <- read_csv(
    file.path(base_dir, "ODPF_1_HS10Desc.csv"),
    col_names = c("code", "start_date", "end_date", "uom", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr, uom)
  
  # HS8 - 8 columns
  descriptions$hs8 <- read_csv(
    file.path(base_dir, "ODPF_2_HS8Desc.csv"),
    col_names = c("code", "start_date", "end_date", "uom", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr, uom)
  
  # HS6 Import - 8 columns
  descriptions$hs6_import <- read_csv(
    file.path(base_dir, "ODPF_3_HS6MDesc.csv"),
    col_names = c("code", "start_date", "end_date", "uom", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr, uom)
  
  # HS6 Export - 8 columns
  descriptions$hs6_export <- read_csv(
    file.path(base_dir, "ODPF_4_HS6XDesc.csv"),
    col_names = c("code", "start_date", "end_date", "uom", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr, uom)
  
  # HS2 - 7 columns: code, start_date, end_date, desc_en, desc_fr, source, period (NO UOM)
  descriptions$hs2 <- read_csv(
    file.path(base_dir, "ODPF_5_HS2Desc.csv"),
    col_names = c("code", "start_date", "end_date", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr)
  
  # Country - 8 columns: code, numeric_code, start_date, end_date, desc_en, desc_fr, source, period
  descriptions$country <- read_csv(
    file.path(base_dir, "ODPF_6_CtyDesc.csv"),
    col_names = c("code", "numeric_code", "start_date", "end_date", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr) %>% 
    unique() %>% 
    filter(!desc_en %in% c("Byelorussian Soviet Socialist Republic", "Serbia and Montenegro",
                           "West Germany", "Democratic Kampuchea",
                           "Trust Territories", "Union of Soviet Socialist Republics",
                           "Former Union of Soviet Socialist Republics", "French Southern Antarctic Territories",
                           "French Southern Territories", "Ukraine Soviet Socialist Republic",
                           "United States Virgin Islands", "Northern Yemen", "Former Yugoslavia"))
  
  # State - 7 columns (adjusted)
  descriptions$state <- read_csv(
    file.path(base_dir, "ODPF_7_StateDesc.csv"),
    col_names = c("code", "date_code1", "date_code2", "desc_en", "desc_fr", "source1", "source2"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr)
  
  # Province - 8 columns: numeric_code, code, start_date, end_date, desc_en, desc_fr, source, period
  descriptions$province <- read_csv(
    file.path(base_dir, "ODPF_8_ProvDesc.csv"),
    col_names = c("numeric_code", "code", "start_date", "end_date", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr)
  
  # UOM - 7 columns: code, start_date, end_date, desc_en, desc_fr, source, period
  descriptions$uom <- read_csv(
    file.path(base_dir, "ODPF_9_UOMDesc.csv"),
    col_names = c("code", "start_date", "end_date", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ) %>%
    select(code, desc_en, desc_fr)
  
  descriptions
}

# Load all trade data files
load_trade_data <- function(base_dir, start_year = 2012, end_year = 2025) {
  files <- list.files(base_dir, pattern = "ODPFN.*\\.csv$", full.names = TRUE)
  
  all_data <- list()
  
  for (file in files) {
    filename <- basename(file)
    
    # Extract type and date
    parts <- str_match(filename, "ODPFN(\\d+)_(\\d{6})[NC]\\.csv")
    type_code <- parts[2]
    date_code <- parts[3]
    
    # Skip if date is outside range
    year <- as.numeric(substr(date_code, 1, 4))
    if (year < start_year || year > end_year) next
    
    # Read file
    data <- read_csv(file, col_types = cols(.default = "c", 
                                            `YearMonth/AnnéeMois` = "i", 
                                            `Value/Valeur` = "d", 
                                            `Quantity/Quantité` = "i"),
                     show_col_types = FALSE)
    
    # Categorize by type
    if (type_code == "014") {
      all_data$import_hs10 <- bind_rows(all_data$import_hs10, data)
    } else if (type_code == "015") {
      all_data$import_hs6 <- bind_rows(all_data$import_hs6, data)
    } else if (type_code == "022") {
      all_data$import_hs2 <- bind_rows(all_data$import_hs2, data)
    } else if (type_code == "016") {
      all_data$export_hs8 <- bind_rows(all_data$export_hs8, data)
    } else if (type_code == "018") {
      all_data$export_hs6 <- bind_rows(all_data$export_hs6, data)
    } else if (type_code == "020") {
      all_data$export_hs2 <- bind_rows(all_data$export_hs2, data)
    }
  }
  
  all_data
}

# Calculate HHI - MODIFIED to accept metric
calculate_hhi <- function(data, product_col, metric_col = "Value") {
  data %>%
    group_by(!!sym(product_col), Country) %>%
    summarize(total_metric = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
    group_by(!!sym(product_col)) %>%
    mutate(
      market_share = total_metric / sum(total_metric),
      hhi_component = (market_share * 100)^2
    ) %>%
    summarize(
      hhi = sum(hhi_component),
      n_partners = n(),
      top_partner_share = max(market_share),
      total_trade_metric = sum(total_metric),
      .groups = "drop"
    ) %>%
    mutate(
      concentration_level = case_when(
        hhi < 1500 ~ "Low (Diversified)",
        hhi < 2500 ~ "Moderate",
        TRUE ~ "High (Concentrated)"
      )
    )
}

# Calculate U.S. dependency ratios - MODIFIED to accept metric
calculate_us_dependency <- function(data, product_col, metric_col = "Value", flow_type = "export") {
  # Calculate total trade metric by product
  total_by_product <- data %>%
    group_by(product = !!sym(product_col)) %>%
    summarize(total_metric = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
  
  # Calculate U.S. trade metric by product
  us_by_product <- data %>%
    filter(Country == "US") %>%
    group_by(product = !!sym(product_col)) %>%
    summarize(us_metric = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
  
  # Join and calculate dependency ratio
  total_by_product %>%
    left_join(us_by_product, by = "product") %>%
    mutate(
      us_metric = replace_na(us_metric, 0),
      us_dependency = us_metric / total_metric,
      us_dependency = ifelse(is.nan(us_dependency) | is.infinite(us_dependency), 0, us_dependency)
    ) %>%
    select(product, us_dependency, total_metric)
}

# Calculate bilateral trade balance - MODIFIED to accept metric
calculate_bilateral_balance <- function(export_data, import_data, product_col, metric_col = "Value") {
  # Calculate Canada's exports to US by product
  canada_to_us <- export_data %>%
    filter(Country == "US") %>%
    group_by(product = !!sym(product_col)) %>%
    summarize(canada_exports_to_us = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
  
  # Calculate US exports to Canada (Canada's imports from US) by product
  us_to_canada <- import_data %>%
    filter(Country == "US") %>%
    group_by(product = !!sym(product_col)) %>%
    summarize(us_exports_to_canada = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
  
  # Calculate bilateral balance
  full_join(canada_to_us, us_to_canada, by = "product") %>%
    mutate(
      canada_exports_to_us = replace_na(canada_exports_to_us, 0),
      us_exports_to_canada = replace_na(us_exports_to_canada, 0),
      total_bilateral_trade = canada_exports_to_us + us_exports_to_canada,
      # Balance ranges from -1 (US dominates) to +1 (Canada dominates)
      bilateral_balance = (canada_exports_to_us - us_exports_to_canada) / 
        pmax(total_bilateral_trade, 1),  # Avoid division by zero
      bilateral_balance = ifelse(is.nan(bilateral_balance) | is.infinite(bilateral_balance), 
                                 0, bilateral_balance),
      # Convert to 0-1 scale where 1 = most vulnerable (US dominates)
      competitive_disadvantage = (1 - bilateral_balance) / 2
    ) %>%
    select(product, competitive_disadvantage, total_bilateral_trade)
}

# Calculate Price Competitiveness Index (PCI) for HS6 level
calculate_pci <- function(export_data, import_data, product_col = "HS6") {
  # Calculate average export prices
  export_prices <- export_data %>%
    filter(!is.na(Quantity), Quantity > 0, Value > 0) %>%
    group_by(product = !!sym(product_col)) %>%
    summarize(
      avg_export_price = sum(Value, na.rm = TRUE) / sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate average import prices
  import_prices <- import_data %>%
    filter(!is.na(Quantity), Quantity > 0, Value > 0) %>%
    group_by(product = !!sym(product_col)) %>%
    summarize(
      avg_import_price = sum(Value, na.rm = TRUE) / sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate PCI
  full_join(export_prices, import_prices, by = "product") %>%
    filter(!is.na(avg_export_price), !is.na(avg_import_price),
           avg_import_price > 0) %>%
    mutate(
      pci = avg_export_price / avg_import_price,
      # Winsorize extreme values at 5th and 95th percentiles
      pci = pmin(pmax(pci, quantile(pci, 0.05, na.rm = TRUE)),
                 quantile(pci, 0.95, na.rm = TRUE))
    ) %>%
    select(product, pci)
}

# Create geographic labels based on flow type
create_geo_label <- function(data, descriptions, is_export = TRUE, flow_type = "country_province") {
  data %>%
    left_join(
      descriptions$country %>% 
        select(Country = code, Country_Name = desc_en), 
      by = "Country"
    ) %>%
    left_join(
      descriptions$province %>% 
        select(Province = code, Province_Name = desc_en), 
      by = "Province"
    ) %>%
    left_join(
      descriptions$state %>% 
        select(State = code, State_Name = desc_en), 
      by = "State"
    ) %>%
    mutate(
      Country_Name = ifelse(is.na(Country_Name), Country, Country_Name),
      Province_Name = ifelse(is.na(Province_Name) | Province_Name == "", Province, Province_Name),
      State_Name = ifelse(is.na(State_Name) | State_Name == "", State, State_Name),
      
      # Create flow label based on flow type selection
      flow_label = case_when(
        # Country-Country flows
        flow_type == "country_country" ~ if(is_export) {
          paste("Canada to", Country_Name)
        } else {
          paste(Country_Name, "to Canada")
        },
        
        # State-Province flows (only for US, otherwise country-province)
        flow_type == "state_province" & State != "" & Country == "US" ~ if(is_export) {
          paste(Province_Name, "to", State_Name, "(U.S.)")
        } else {
          paste(State_Name, "(U.S.) to", Province_Name)
        },
        
        flow_type == "state_province" & (State == "" | Country != "US") ~ if(is_export) {
          paste(Province_Name, "to", Country_Name)
        } else {
          paste(Country_Name, "to", Province_Name)
        },
        
        # Default: Country-Province flows
        TRUE ~ if(is_export) {
          paste(Province_Name, "to", Country_Name)
        } else {
          paste(Country_Name, "to", Province_Name)
        }
      ),
      
      # Flag internal trades (Canada to/from Canada)
      is_internal = Country == "CA"
    )
}

# Map HS2 codes to HS sections
map_hs_to_section <- function(hs2_code) {
  hs2_num <- as.numeric(hs2_code)
  
  case_when(
    hs2_num >= 1 & hs2_num <= 5 ~ "I: Live Animals",
    hs2_num >= 6 & hs2_num <= 14 ~ "II: Vegetable Products",
    hs2_num == 15 ~ "III: Fats & Oils",
    hs2_num >= 16 & hs2_num <= 24 ~ "IV: Prepared Foodstuffs",
    hs2_num >= 25 & hs2_num <= 27 ~ "V: Mineral Products",
    hs2_num >= 28 & hs2_num <= 38 ~ "VI: Chemicals",
    hs2_num >= 39 & hs2_num <= 40 ~ "VII: Plastics & Rubber",
    hs2_num >= 41 & hs2_num <= 43 ~ "VIII: Hides & Skins",
    hs2_num >= 44 & hs2_num <= 46 ~ "IX: Wood",
    hs2_num >= 47 & hs2_num <= 49 ~ "X: Pulp & Paper",
    hs2_num >= 50 & hs2_num <= 63 ~ "XI: Textiles",
    hs2_num >= 64 & hs2_num <= 67 ~ "XII: Footwear & Headgear",
    hs2_num >= 68 & hs2_num <= 70 ~ "XIII: Stone & Glass",
    hs2_num == 71 ~ "XIV: Precious Stones & Metals",
    hs2_num >= 72 & hs2_num <= 83 ~ "XV: Base Metals",
    hs2_num >= 84 & hs2_num <= 85 ~ "XVI: Machinery",
    hs2_num >= 86 & hs2_num <= 89 ~ "XVII: Vehicles",
    hs2_num >= 90 & hs2_num <= 92 ~ "XVIII: Instruments",
    hs2_num == 93 ~ "XIX: Arms",
    hs2_num >= 94 & hs2_num <= 96 ~ "XX: Miscellaneous",
    hs2_num == 97 ~ "XXI: Art & Antiques",
    hs2_num >= 98 & hs2_num <= 99 ~ "XXII: Special",
    TRUE ~ "Other"
  )
}

# ============================================================
# LOAD ALL DATA AT STARTUP
# ============================================================

descriptions <- load_descriptions(base_dir)

trade_data <- readRDS("trade_data.rds")

trade_data <- load_trade_data(base_dir)
names(trade_data$import_hs10) <- c("YearMonth", "HS10", "Country", "Province", 
                                   "State", "Value", "Quantity", "Unit")
names(trade_data$import_hs6) <- c("YearMonth", "HS6", "Country", "Province", 
                                  "State", "Value", "Quantity", "Unit")
names(trade_data$import_hs2) <- c("YearMonth", "HS2", "Country", "Province", 
                                  "State", "Value")
names(trade_data$export_hs8) <- c("YearMonth", "HS8", "Country", "Province", 
                                  "State", "Value", "Quantity", "Unit")
names(trade_data$export_hs6) <- c("YearMonth", "HS6", "Country", "Province", 
                                  "State", "Value", "Quantity", "Unit")
names(trade_data$export_hs2) <- c("YearMonth", "HS2", "Country", "Province", 
                                  "State", "Value")
saveRDS(trade_data, "trade_data.rds")

# ============================================================
# SHINY APP
# ============================================================

ui <- fluidPage(
  titlePanel("Trade Vulnerability Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # HS level selection
      radioButtons(
        "hs_level",
        "HS Classification Level:",
        choices = c("HS2" = "hs2", "HS6" = "hs6"),
        selected = "hs2"
      ),
      
      # METRIC SELECTION - NEW
      radioButtons(
        "metric",
        "Analysis Metric:",
        choices = c("Value (CAD)" = "Value", "Quantity (Units)" = "Quantity"),
        selected = "Value"
      ),
      
      # Add warning for HS2 + Quantity combination
      conditionalPanel(
        condition = "input.hs_level == 'hs2' && input.metric == 'Quantity'",
        div(
          style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 10px; margin: 10px 0; border-radius: 4px;",
          strong("Warning:"), " Quantity data is not available at HS2 level. Please select HS6 or switch to Value metric."
        )
      ),
      
      # Product selection with multiple selection enabled
      selectizeInput(
        "product_code",
        "Select Product(s):",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(maxItems = 10)
      ),
      
      # Year range selection
      sliderInput(
        "year_range",
        "Select Years:",
        min = 2012,
        max = 2025,
        value = c(2022, 2025),
        step = 1,
        sep = ""
      ),
      
      # Geographic scope selection
      radioButtons(
        "geo_scope",
        "Geographic Scope:",
        choices = c("Canada (All Provinces)" = "canada",
                    "Ontario Only" = "ontario"),
        selected = "canada"
      ),
      
      # Flow type selection
      selectInput(
        "flow_type",
        "Geographic Flow Type:",
        choices = c(
          "Country-Province" = "country_province",
          "Country-Country" = "country_country",
          "State-Province" = "state_province"
        ),
        selected = "country_province"
      ),
      
      # Internal trade filter
      checkboxInput(
        "exclude_internal",
        "Exclude Internal Trade (Canada-to-Canada)",
        value = FALSE
      ),
      
      # Top N partners
      sliderInput(
        "top_n",
        "Show Top N Partners:",
        min = 1,
        max = 50,
        value = 3,
        step = 1
      ),
      
      hr(),
      
      # Product information
      h4("Product Information"),
      verbatimTextOutput("product_info")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          "Dependency Matrix",
          fluidRow(
            column(6, plotlyOutput("dependency_matrix_export", height = "500px")),
            column(6, plotlyOutput("dependency_matrix_import", height = "500px"))
          ),
          hr(),
          h4("Selected Product Position"),
          verbatimTextOutput("dependency_text")
        ),
        
        tabPanel(
          "Time Series",
          plotlyOutput("time_plot_combined", height = "600px")
        ),
        
        tabPanel(
          "Market Share",
          fluidRow(
            column(6, 
                   h4("Export Market Share"),
                   plotlyOutput("market_share_export", height = "500px")),
            column(6, 
                   h4("Import Market Share"),
                   plotlyOutput("market_share_import", height = "500px"))
          )
        ),
        
        tabPanel(
          "HHI Analysis",
          fluidRow(
            column(6,
                   h4("Export HHI Distribution"),
                   plotlyOutput("hhi_plot_export", height = "500px")),
            column(6,
                   h4("Import HHI Distribution"),
                   plotlyOutput("hhi_plot_import", height = "500px"))
          )
        ),
        
        tabPanel(
          "Data Table",
          h4("Export Data"),
          DTOutput("export_table"),
          hr(),
          h4("Import Data"),
          DTOutput("import_table")
        ),
        
        tabPanel(
          "Pillar 2 Matrix",
          conditionalPanel(
            condition = "input.hs_level == 'hs6' && input.metric == 'Value'",
            plotlyOutput("pillar2_matrix", height = "600px"),
            hr(),
            h4("Selected Product Competitive Position"),
            verbatimTextOutput("pillar2_text")
          ),
          conditionalPanel(
            condition = "input.hs_level != 'hs6'",
            div(
              style = "text-align: center; padding: 100px;",
              h3("Pillar 2 Analysis Only Available at HS6 Level"),
              p("Please select 'HS6' classification level to view competitive position analysis."),
              p("This analysis requires quantity data which is only available at HS6 level and below.")
            )
          ),
          conditionalPanel(
            condition = "input.hs_level == 'hs6' && input.metric == 'Quantity'",
            div(
              style = "text-align: center; padding: 100px;",
              h3("Pillar 2 Analysis Only Available with Value Metric"),
              p("Please select 'Value (CAD)' metric to view competitive position analysis."),
              p("This analysis uses price competitiveness calculations that require Value data.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive: Get current HS level data
  current_export_data <- reactive({
    if (input$hs_level == "hs2") trade_data$export_hs2 else trade_data$export_hs6
  })
  
  current_import_data <- reactive({
    if (input$hs_level == "hs2") trade_data$import_hs2 else trade_data$import_hs6
  })
  
  # Check if quantity is available
  quantity_available <- reactive({
    input$hs_level == "hs6"
  })
  
  # Get effective metric (force Value if Quantity not available)
  effective_metric <- reactive({
    if (input$metric == "Quantity" && !quantity_available()) {
      return("Value")
    }
    input$metric
  })
  
  # Filtered data by year range and geographic scope
  filtered_export_data <- reactive({
    req(input$year_range)
    data <- current_export_data() %>%
      mutate(year = floor(YearMonth / 100)) %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    # Apply geographic scope filter
    if (input$geo_scope == "ontario") {
      data <- data %>% filter(Province == "ON")
    }
    
    data
  })
  
  filtered_import_data <- reactive({
    req(input$year_range)
    data <- current_import_data() %>%
      mutate(year = floor(YearMonth / 100)) %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2])
    
    # Apply geographic scope filter
    if (input$geo_scope == "ontario") {
      data <- data %>% filter(Province == "ON")
    }
    
    data
  })
  
  # Apply internal trade filter
  filtered_export_data_with_internal <- reactive({
    data <- filtered_export_data()
    if (input$exclude_internal) {
      data <- data %>% filter(Country != "CA")
    }
    data
  })
  
  filtered_import_data_with_internal <- reactive({
    data <- filtered_import_data()
    if (input$exclude_internal) {
      data <- data %>% filter(Country != "CA")
    }
    data
  })
  
  # Calculate HHI based on filtered data - UPDATED with metric parameter
  current_export_hhi <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    calculate_hhi(filtered_export_data_with_internal(), col_name, effective_metric())
  })
  
  current_import_hhi <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    calculate_hhi(filtered_import_data_with_internal(), col_name, effective_metric())
  })
  
  # Calculate U.S. dependency ratios based on filtered data - UPDATED with metric parameter
  current_export_us_dependency <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    calculate_us_dependency(filtered_export_data_with_internal(), col_name, effective_metric(), "export")
  })
  
  current_import_us_dependency <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    calculate_us_dependency(filtered_import_data_with_internal(), col_name, effective_metric(), "import")
  })
  
  # Calculate Pillar 2 components (only for HS6 with Value metric)
  current_bilateral_balance <- reactive({
    req(input$hs_level == "hs6", effective_metric() == "Value")
    calculate_bilateral_balance(
      filtered_export_data_with_internal(),
      filtered_import_data_with_internal(),
      "HS6",
      "Value"
    )
  })
  
  current_pci <- reactive({
    req(input$hs_level == "hs6", effective_metric() == "Value")
    calculate_pci(
      filtered_export_data_with_internal(),
      filtered_import_data_with_internal(),
      "HS6"
    )
  })
  
  # Update product choices when HS level changes
  observe({
    hs_desc <- if (input$hs_level == "hs2") {
      descriptions$hs2
    } else {
      descriptions$hs6_export
    }
    
    hhi_col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    
    products <- current_export_hhi() %>%
      select(code = !!sym(hhi_col_name)) %>%
      distinct() %>%
      left_join(hs_desc, by = "code") %>%
      mutate(
        desc_en = ifelse(is.na(desc_en) | desc_en == "", code, desc_en),
        label = paste0(code, " - ", desc_en)
      ) %>%
      arrange(code)
    
    choices <- setNames(products$code, products$label)
    
    updateSelectizeInput(
      session,
      "product_code",
      label = if(input$hs_level == "hs2") "HS2 Product(s):" else "HS6 Product(s):",
      choices = choices,
      selected = if(length(choices) > 0) choices[1] else NULL
    )
  })
  
  # Product-specific filtered data with geographic labels
  product_export_data <- reactive({
    req(input$product_code)
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    
    filtered_export_data_with_internal() %>%
      filter(!!sym(col_name) %in% input$product_code) %>%
      create_geo_label(descriptions, is_export = TRUE, flow_type = input$flow_type)
  })
  
  product_import_data <- reactive({
    req(input$product_code)
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    
    filtered_import_data_with_internal() %>%
      filter(!!sym(col_name) %in% input$product_code) %>%
      create_geo_label(descriptions, is_export = FALSE, flow_type = input$flow_type)
  })
  
  # Output: Product information - UPDATED with metric label
  output$product_info <- renderText({
    req(input$product_code)
    
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_label <- if(effective_metric() == "Value") "Value" else "Quantity"
    
    # Summarize across all selected products
    export_hhi_summary <- current_export_hhi() %>%
      filter(!!sym(hhi_col) %in% input$product_code) %>%
      summarize(
        avg_hhi = mean(hhi, na.rm = TRUE),
        avg_partners = mean(n_partners, na.rm = TRUE),
        n_products = n()
      )
    
    import_hhi_summary <- current_import_hhi() %>%
      filter(!!sym(hhi_col) %in% input$product_code) %>%
      summarize(
        avg_hhi = mean(hhi, na.rm = TRUE),
        avg_partners = mean(n_partners, na.rm = TRUE),
        n_products = n()
      )
    
    export_us_dep <- current_export_us_dependency() %>%
      filter(product %in% input$product_code) %>%
      summarize(avg_us_dep = mean(us_dependency, na.rm = TRUE))
    
    import_us_dep <- current_import_us_dependency() %>%
      filter(product %in% input$product_code) %>%
      summarize(avg_us_dep = mean(us_dependency, na.rm = TRUE))
    
    paste0(
      "Metric: ", metric_label, "\n",
      "Selected Products: ", length(input$product_code), "\n",
      "Avg Export HHI: ", round(export_hhi_summary$avg_hhi, 1), "\n",
      "Avg Import HHI: ", round(import_hhi_summary$avg_hhi, 1), "\n",
      "Avg U.S. Export Dependency: ", round(export_us_dep$avg_us_dep, 3), "\n",
      "Avg U.S. Import Dependency: ", round(import_us_dep$avg_us_dep, 3), "\n",
      "Avg Export Partners: ", round(export_hhi_summary$avg_partners, 1), "\n",
      "Avg Import Partners: ", round(import_hhi_summary$avg_partners, 1)
    )
  })
  
  # Output: Dependency Matrix - Export - UPDATED with metric-aware labels
  output$dependency_matrix_export <- renderPlotly({
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_label <- if(effective_metric() == "Value") "CAD" else "Units"
    
    data <- current_export_hhi() %>%
      rename(product = !!sym(hhi_col)) %>%
      left_join(current_export_us_dependency(), by = "product") %>%
      mutate(
        hs2 = substr(product, 1, 2),
        section = map_hs_to_section(hs2),
        is_selected = product %in% input$product_code,
        # Use log scale for sizes to handle wide range of values
        log_value = log10(total_trade_metric + 1)
      )
    
    avg_hhi <- mean(data$hhi, na.rm = TRUE)
    avg_us_dep <- mean(data$us_dependency, na.rm = TRUE)
    
    # Define colors for HS sections
    section_colors <- c(
      "I: Live Animals" = "#8B4513",
      "II: Vegetable Products" = "#228B22",
      "III: Fats & Oils" = "#FFD700",
      "IV: Prepared Foodstuffs" = "#FF8C00",
      "V: Mineral Products" = "#696969",
      "VI: Chemicals" = "#9370DB",
      "VII: Plastics & Rubber" = "#FF1493",
      "VIII: Hides & Skins" = "#D2691E",
      "IX: Wood" = "#8B4513",
      "X: Pulp & Paper" = "#F5DEB3",
      "XI: Textiles" = "#4169E1",
      "XII: Footwear & Headgear" = "#DC143C",
      "XIII: Stone & Glass" = "#B0C4DE",
      "XIV: Precious Stones & Metals" = "#FFD700",
      "XV: Base Metals" = "#708090",
      "XVI: Machinery" = "#FF4500",
      "XVII: Vehicles" = "#1E90FF",
      "XVIII: Instruments" = "#00CED1",
      "XIX: Arms" = "#8B0000",
      "XX: Miscellaneous" = "#9932CC",
      "XXI: Art & Antiques" = "#DAA520",
      "XXII: Special" = "#A9A9A9"
    )
    
    plot_ly(data, x = ~hhi, y = ~us_dependency, 
            type = 'scatter', mode = 'markers',
            color = ~section,
            colors = section_colors,
            size = ~log_value,
            sizes = c(10, 100),
            marker = list(opacity = 0.6, line = list(width = 1, color = 'white')),
            text = ~paste0(
              if(input$hs_level == "hs2") "HS2: " else "HS6: ",
              product,
              "<br>Section: ", section,
              "<br>HHI: ", round(hhi, 1),
              "<br>U.S. Dependency: ", round(us_dependency, 3),
              "<br>", effective_metric(), ": ", scales::comma(total_trade_metric), " ", metric_label
            ),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Export Dependency Matrix (", effective_metric(), ")"),
        xaxis = list(title = "Herfindahl-Hirschman Index"),
        yaxis = list(title = "U.S. Export Dependency", range = c(0, 1)),
        showlegend = TRUE,
        shapes = list(
          list(
            type = "line",
            x0 = avg_hhi, x1 = avg_hhi,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "black", width = 2, dash = "dash")
          ),
          list(
            type = "line",
            x0 = 0, x1 = 1,
            xref = "paper",
            y0 = avg_us_dep, y1 = avg_us_dep,
            line = list(color = "black", width = 2, dash = "dash")
          )
        )
      )
  })
  
  # Output: Dependency Matrix - Import - UPDATED with metric-aware labels
  output$dependency_matrix_import <- renderPlotly({
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_label <- if(effective_metric() == "Value") "CAD" else "Units"
    
    data <- current_import_hhi() %>%
      rename(product = !!sym(hhi_col)) %>%
      left_join(current_import_us_dependency(), by = "product") %>%
      mutate(
        hs2 = substr(product, 1, 2),
        section = map_hs_to_section(hs2),
        is_selected = product %in% input$product_code,
        # Use log scale for sizes
        log_value = log10(total_trade_metric + 1)
      )
    
    avg_hhi <- mean(data$hhi, na.rm = TRUE)
    avg_us_dep <- mean(data$us_dependency, na.rm = TRUE)
    
    # Define colors for HS sections
    section_colors <- c(
      "I: Live Animals" = "#8B4513",
      "II: Vegetable Products" = "#228B22",
      "III: Fats & Oils" = "#FFD700",
      "IV: Prepared Foodstuffs" = "#FF8C00",
      "V: Mineral Products" = "#696969",
      "VI: Chemicals" = "#9370DB",
      "VII: Plastics & Rubber" = "#FF1493",
      "VIII: Hides & Skins" = "#D2691E",
      "IX: Wood" = "#8B4513",
      "X: Pulp & Paper" = "#F5DEB3",
      "XI: Textiles" = "#4169E1",
      "XII: Footwear & Headgear" = "#DC143C",
      "XIII: Stone & Glass" = "#B0C4DE",
      "XIV: Precious Stones & Metals" = "#FFD700",
      "XV: Base Metals" = "#708090",
      "XVI: Machinery" = "#FF4500",
      "XVII: Vehicles" = "#1E90FF",
      "XVIII: Instruments" = "#00CED1",
      "XIX: Arms" = "#8B0000",
      "XX: Miscellaneous" = "#9932CC",
      "XXI: Art & Antiques" = "#DAA520",
      "XXII: Special" = "#A9A9A9"
    )
    
    plot_ly(data, x = ~hhi, y = ~us_dependency, 
            type = 'scatter', mode = 'markers',
            color = ~section,
            colors = section_colors,
            size = ~log_value,
            sizes = c(10, 100),
            marker = list(opacity = 0.6, line = list(width = 1, color = 'white')),
            text = ~paste0(
              if(input$hs_level == "hs2") "HS2: " else "HS6: ",
              product,
              "<br>Section: ", section,
              "<br>HHI: ", round(hhi, 1),
              "<br>U.S. Dependency: ", round(us_dependency, 3),
              "<br>", effective_metric(), ": ", scales::comma(total_trade_metric), " ", metric_label
            ),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Import Dependency Matrix (", effective_metric(), ")"),
        xaxis = list(title = "Herfindahl-Hirschman Index"),
        yaxis = list(title = "U.S. Import Dependency", range = c(0, 1)),
        showlegend = TRUE,
        shapes = list(
          list(
            type = "line",
            x0 = avg_hhi, x1 = avg_hhi,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "black", width = 2, dash = "dash")
          ),
          list(
            type = "line",
            x0 = 0, x1 = 1,
            xref = "paper",
            y0 = avg_us_dep, y1 = avg_us_dep,
            line = list(color = "black", width = 2, dash = "dash")
          )
        )
      )
  })
  
  # Output: Dependency text interpretation
  output$dependency_text <- renderText({
    req(input$product_code)
    
    if (length(input$product_code) == 1) {
      hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
      
      export_hhi_val <- current_export_hhi() %>%
        filter(!!sym(hhi_col) == input$product_code) %>%
        pull(hhi)
      
      import_hhi_val <- current_import_hhi() %>%
        filter(!!sym(hhi_col) == input$product_code) %>%
        pull(hhi)
      
      export_us_dep_val <- current_export_us_dependency() %>%
        filter(product == input$product_code) %>%
        pull(us_dependency)
      
      import_us_dep_val <- current_import_us_dependency() %>%
        filter(product == input$product_code) %>%
        pull(us_dependency)
      
      avg_export_hhi <- mean(current_export_hhi()$hhi, na.rm = TRUE)
      avg_import_hhi <- mean(current_import_hhi()$hhi, na.rm = TRUE)
      avg_export_us_dep <- mean(current_export_us_dependency()$us_dependency, na.rm = TRUE)
      avg_import_us_dep <- mean(current_import_us_dependency()$us_dependency, na.rm = TRUE)
      
      export_quadrant <- case_when(
        export_us_dep_val > avg_export_us_dep & export_hhi_val > avg_export_hhi ~ 
          "With limited options for diversification, reliance on U.S. markets remains high",
        export_us_dep_val > avg_export_us_dep & export_hhi_val <= avg_export_hhi ~ 
          "Although reliance on U.S. markets is high, opportunities for diversification remain",
        export_us_dep_val <= avg_export_us_dep & export_hhi_val > avg_export_hhi ~ 
          "Diversification may not be possible, but dependence on U.S. markets is low",
        TRUE ~ "There are fewer concerns with diversification or reliance on U.S. markets"
      )
      
      import_quadrant <- case_when(
        import_us_dep_val > avg_import_us_dep & import_hhi_val > avg_import_hhi ~ 
          "With limited options for diversification, reliance on U.S. markets remains high",
        import_us_dep_val > avg_import_us_dep & import_hhi_val <= avg_import_hhi ~ 
          "Although reliance on U.S. markets is high, opportunities for diversification remain",
        import_us_dep_val <= avg_import_us_dep & import_hhi_val > avg_import_hhi ~ 
          "Diversification may not be possible, but dependence on U.S. markets is low",
        TRUE ~ "There are fewer concerns with diversification or reliance on U.S. markets"
      )
      
      paste0(
        "EXPORT POSITION:\n",
        "HHI: ", round(export_hhi_val, 1), " (Avg: ", round(avg_export_hhi, 1), ")\n",
        "U.S. Dependency: ", round(export_us_dep_val, 3), " (Avg: ", round(avg_export_us_dep, 3), ")\n",
        export_quadrant, "\n\n",
        "IMPORT POSITION:\n",
        "HHI: ", round(import_hhi_val, 1), " (Avg: ", round(avg_import_hhi, 1), ")\n",
        "U.S. Dependency: ", round(import_us_dep_val, 3), " (Avg: ", round(avg_import_us_dep, 3), ")\n",
        import_quadrant
      )
    } else {
      "Select a single product to see detailed position analysis."
    }
  })
  
  # Output: Combined time series with "Other" category - UPDATED with metric
  output$time_plot_combined <- renderPlotly({
    req(input$product_code)
    
    metric_col <- effective_metric()
    metric_label <- if(metric_col == "Value") "CAD" else "Units"
    
    # Get top N export flows
    top_export_flows <- product_export_data() %>%
      group_by(flow_label) %>%
      summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(input$top_n) %>%
      pull(flow_label)
    
    # Get top N import flows
    top_import_flows <- product_import_data() %>%
      group_by(flow_label) %>%
      summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(input$top_n) %>%
      pull(flow_label)
    
    # Prepare export data with "Other" category
    export_plot_data <- product_export_data() %>%
      mutate(
        date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")),
        flow_label = ifelse(flow_label %in% top_export_flows, flow_label, "Other")
      ) %>%
      group_by(date, flow_label) %>%
      summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
      mutate(flow_type = "Export")
    
    # Prepare import data with "Other" category
    import_plot_data <- product_import_data() %>%
      mutate(
        date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")),
        flow_label = ifelse(flow_label %in% top_import_flows, flow_label, "Other")
      ) %>%
      group_by(date, flow_label) %>%
      summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
      mutate(flow_type = "Import")
    
    # Combine data
    combined_data <- bind_rows(export_plot_data, import_plot_data)
    
    # Create plot
    plot_ly(combined_data, x = ~date, y = ~metric_value, 
            color = ~flow_label, 
            linetype = ~flow_type,
            type = 'scatter', mode = 'lines',
            line = list(width = 2)) %>%
      layout(
        title = paste("Trade Flows Over Time -", paste(input$product_code, collapse = ", "), "(", metric_col, ")"),
        xaxis = list(title = "Date"),
        yaxis = list(title = paste(metric_col, "(", metric_label, ")")),
        hovermode = "x unified",
        legend = list(orientation = "v", x = 1.02, y = 1)
      )
  })
  
  # Output: Market share plots - UPDATED with metric
  output$market_share_export <- renderPlotly({
    req(input$product_code)
    
    metric_col <- effective_metric()
    
    data <- product_export_data() %>%
      group_by(flow_label) %>%
      summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      mutate(
        flow_label = if_else(row_number() <= input$top_n, flow_label, "Other")
      ) %>%
      group_by(flow_label) %>%
      summarize(total = sum(total), .groups = "drop")
    
    plot_ly(data, labels = ~flow_label, values = ~total, type = 'pie',
            textinfo = 'label+percent',
            textposition = 'outside') %>%
      layout(title = paste("Export Market Share -", paste(input$product_code, collapse = ", "), "(", metric_col, ")"))
  })
  
  output$market_share_import <- renderPlotly({
    req(input$product_code)
    
    metric_col <- effective_metric()
    
    data <- product_import_data() %>%
      group_by(flow_label) %>%
      summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      mutate(
        flow_label = if_else(row_number() <= input$top_n, flow_label, "Other")
      ) %>%
      group_by(flow_label) %>%
      summarize(total = sum(total), .groups = "drop")
    
    plot_ly(data, labels = ~flow_label, values = ~total, type = 'pie',
            textinfo = 'label+percent',
            textposition = 'outside') %>%
      layout(title = paste("Import Market Share -", paste(input$product_code, collapse = ", "), "(", metric_col, ")"))
  })
  
  # Output: HHI distribution plots
  output$hhi_plot_export <- renderPlotly({
    req(input$product_code)
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    
    if (input$hs_level == "hs2") {
      data <- current_export_hhi()
    } else {
      selected_hs2 <- unique(substr(input$product_code, 1, 2))
      data <- current_export_hhi() %>%
        filter(substr(!!sym(hhi_col), 1, 2) %in% selected_hs2)
    }
    
    plot_ly(data, x = ~hhi, type = "histogram", nbinsx = 50,
            color = ~concentration_level,
            colors = c("Low (Diversified)" = "green", 
                       "Moderate" = "orange", 
                       "High (Concentrated)" = "red")) %>%
      layout(
        title = paste("Export HHI Distribution", 
                      if(input$hs_level == "hs6") paste("(HS2:", paste(selected_hs2, collapse = ", "), ")") else "",
                      "(", effective_metric(), ")"),
        xaxis = list(title = "HHI Score"),
        yaxis = list(title = "Number of Products"),
        shapes = list(
          list(type = "line", x0 = 1500, x1 = 1500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black")),
          list(type = "line", x0 = 2500, x1 = 2500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black"))
        )
      )
  })
  
  output$hhi_plot_import <- renderPlotly({
    req(input$product_code)
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    
    if (input$hs_level == "hs2") {
      data <- current_import_hhi()
    } else {
      selected_hs2 <- unique(substr(input$product_code, 1, 2))
      data <- current_import_hhi() %>%
        filter(substr(!!sym(hhi_col), 1, 2) %in% selected_hs2)
    }
    
    plot_ly(data, x = ~hhi, type = "histogram", nbinsx = 50,
            color = ~concentration_level,
            colors = c("Low (Diversified)" = "green", 
                       "Moderate" = "orange", 
                       "High (Concentrated)" = "red")) %>%
      layout(
        title = paste("Import HHI Distribution",
                      if(input$hs_level == "hs6") paste("(HS2:", paste(selected_hs2, collapse = ", "), ")") else "",
                      "(", effective_metric(), ")"),
        xaxis = list(title = "HHI Score"),
        yaxis = list(title = "Number of Products"),
        shapes = list(
          list(type = "line", x0 = 1500, x1 = 1500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black")),
          list(type = "line", x0 = 2500, x1 = 2500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black"))
        )
      )
  })
  
  # Output: Data tables - UPDATED with metric
  output$export_table <- renderDT({
    req(input$product_code)
    
    metric_col <- effective_metric()
    
    # Build table based on available columns
    table_data <- product_export_data() %>%
      mutate(
        Date = format(as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")), "%Y-%m")
      )
    
    if (metric_col == "Value") {
      table_data <- table_data %>%
        mutate(
          Value = scales::dollar(Value, prefix = "$", big.mark = ",")
        ) %>%
        select(Date, Flow = flow_label, Value, Quantity, Unit)
    } else {
      table_data <- table_data %>%
        select(Date, Flow = flow_label, Quantity, Unit, Value)
    }
    
    table_data %>%
      arrange(desc(Date))
  }, options = list(pageLength = 10))
  
  output$import_table <- renderDT({
    req(input$product_code)
    
    metric_col <- effective_metric()
    
    # Build table based on available columns
    table_data <- product_import_data() %>%
      mutate(
        Date = format(as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")), "%Y-%m")
      )
    
    if (metric_col == "Value") {
      table_data <- table_data %>%
        mutate(
          Value = scales::dollar(Value, prefix = "$", big.mark = ",")
        ) %>%
        select(Date, Flow = flow_label, Value, Quantity, Unit)
    } else {
      table_data <- table_data %>%
        select(Date, Flow = flow_label, Quantity, Unit, Value)
    }
    
    table_data %>%
      arrange(desc(Date))
  }, options = list(pageLength = 10))
  
  # Output: Pillar 2 Matrix (HS6 only, Value metric only)
  output$pillar2_matrix <- renderPlotly({
    req(input$hs_level == "hs6", effective_metric() == "Value")
    
    # Combine bilateral balance and PCI
    pillar2_data <- current_bilateral_balance() %>%
      left_join(current_pci(), by = "product") %>%
      filter(!is.na(pci), !is.na(competitive_disadvantage)) %>%
      mutate(
        hs2 = substr(product, 1, 2),
        section = map_hs_to_section(hs2),
        is_selected = product %in% input$product_code,
        # Use log scale for bubble sizes
        log_value = log10(total_bilateral_trade + 1)
      )
    
    # Calculate averages for reference lines
    avg_comp_disadv <- mean(pillar2_data$competitive_disadvantage, na.rm = TRUE)
    avg_pci <- mean(pillar2_data$pci, na.rm = TRUE)
    
    # Define colors for HS sections
    section_colors <- c(
      "I: Live Animals" = "#8B4513",
      "II: Vegetable Products" = "#228B22",
      "III: Fats & Oils" = "#FFD700",
      "IV: Prepared Foodstuffs" = "#FF8C00",
      "V: Mineral Products" = "#696969",
      "VI: Chemicals" = "#9370DB",
      "VII: Plastics & Rubber" = "#FF1493",
      "VIII: Hides & Skins" = "#D2691E",
      "IX: Wood" = "#8B4513",
      "X: Pulp & Paper" = "#F5DEB3",
      "XI: Textiles" = "#4169E1",
      "XII: Footwear & Headgear" = "#DC143C",
      "XIII: Stone & Glass" = "#B0C4DE",
      "XIV: Precious Stones & Metals" = "#FFD700",
      "XV: Base Metals" = "#708090",
      "XVI: Machinery" = "#FF4500",
      "XVII: Vehicles" = "#1E90FF",
      "XVIII: Instruments" = "#00CED1",
      "XIX: Arms" = "#8B0000",
      "XX: Miscellaneous" = "#9932CC",
      "XXI: Art & Antiques" = "#DAA520",
      "XXII: Special" = "#A9A9A9"
    )
    
    plot_ly(pillar2_data, 
            x = ~pci, 
            y = ~competitive_disadvantage,
            type = 'scatter', 
            mode = 'markers',
            color = ~section,
            colors = section_colors,
            size = ~log_value,
            sizes = c(10, 100),
            marker = list(opacity = 0.6, line = list(width = 1, color = 'white')),
            text = ~paste0(
              "HS6: ", product,
              "<br>Section: ", section,
              "<br>Competitive Disadvantage: ", round(competitive_disadvantage, 3),
              "<br>Price Ratio (Export/Import): ", round(pci, 2),
              "<br>Bilateral Trade: $", scales::comma(total_bilateral_trade)
            ),
            hoverinfo = 'text') %>%
      layout(
        title = "Canada vs US Competitive Comparison",
        xaxis = list(title = "Price Disadvantage (Export Price / Import Price)"),
        yaxis = list(title = "Revealed Comparative Disadvantage<br>(0 = Canada dominates, 1 = US dominates)"),
        showlegend = TRUE,
        shapes = list(
          # Vertical line at average PCI
          list(
            type = "line",
            x0 = avg_pci, x1 = avg_pci,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "black", width = 2, dash = "dash")
          ),
          # Horizontal line at average competitive disadvantage
          list(
            type = "line",
            x0 = 0, x1 = 1,
            xref = "paper",
            y0 = avg_comp_disadv, y1 = avg_comp_disadv,
            line = list(color = "black", width = 2, dash = "dash")
          )
        ),
        annotations = list(
          # Quadrant labels
          list(x = 0.25, y = 0.95, xref = "paper", yref = "paper",
               text = "Competitively priced<br>but less specialized",
               showarrow = FALSE, font = list(size = 10, color = "gray")),
          list(x = 0.75, y = 0.95, xref = "paper", yref = "paper",
               text = "More expensive and<br>limited advantage",
               showarrow = FALSE, font = list(size = 10, color = "gray")),
          list(x = 0.25, y = 0.05, xref = "paper", yref = "paper",
               text = "Strong specialization<br>with competitive pricing",
               showarrow = FALSE, font = list(size = 10, color = "gray")),
          list(x = 0.75, y = 0.05, xref = "paper", yref = "paper",
               text = "More expensive but<br>high advantage",
               showarrow = FALSE, font = list(size = 10, color = "gray"))
        )
      )
  })
  
  # Output: Pillar 2 interpretation text
  output$pillar2_text <- renderText({
    req(input$hs_level == "hs6", input$product_code, effective_metric() == "Value")
    
    if (length(input$product_code) == 1) {
      pillar2_data <- current_bilateral_balance() %>%
        left_join(current_pci(), by = "product") %>%
        filter(product == input$product_code)
      
      if (nrow(pillar2_data) == 0) {
        return("Insufficient data for Pillar 2 analysis for this product.")
      }
      
      comp_disadv <- pillar2_data$competitive_disadvantage
      pci_val <- pillar2_data$pci
      
      avg_comp_disadv <- mean(current_bilateral_balance()$competitive_disadvantage, na.rm = TRUE)
      avg_pci <- mean(current_pci()$pci, na.rm = TRUE)
      
      # Determine quadrant
      quadrant <- case_when(
        comp_disadv > avg_comp_disadv & pci_val > avg_pci ~ 
          "The goods are both more expensive and with limited global comparative advantage",
        comp_disadv > avg_comp_disadv & pci_val <= avg_pci ~ 
          "The goods are competitively priced but are less specialized",
        comp_disadv <= avg_comp_disadv & pci_val > avg_pci ~ 
          "The goods are more expensive, but this does not prevent a high global comparative advantage",
        TRUE ~ 
          "The country's specialization in the good, combined with competitive pricing, strengthens its position"
      )
      
      # Interpret bilateral balance
      balance_raw <- (comp_disadv * 2) - 1  # Convert back to -1 to +1 scale
      balance_interp <- if(balance_raw > 0.3) {
        "US strongly dominates this bilateral trade"
      } else if(balance_raw > 0.1) {
        "US has moderate advantage in this bilateral trade"
      } else if(balance_raw > -0.1) {
        "Balanced bilateral trade between Canada and US"
      } else if(balance_raw > -0.3) {
        "Canada has moderate advantage in this bilateral trade"
      } else {
        "Canada strongly dominates this bilateral trade"
      }
      
      paste0(
        "COMPETITIVE POSITION (vs US):\n",
        "Bilateral Balance: ", round(balance_raw, 3), " (", balance_interp, ")\n",
        "Competitive Disadvantage Index: ", round(comp_disadv, 3), 
        " (Avg: ", round(avg_comp_disadv, 3), ")\n\n",
        "PRICE COMPETITIVENESS:\n",
        "Export/Import Price Ratio: ", round(pci_val, 2),
        " (Avg: ", round(avg_pci, 2), ")\n",
        if(pci_val > 1) "Canada exports at higher prices than it imports\n" else "Canada exports at lower prices than it imports\n",
        "\nINTERPRETATION:\n",
        quadrant
      )
    } else {
      "Select a single HS6 product to see detailed Pillar 2 analysis."
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

# Load the dplyr library for data manipulation
library(dplyr)

# This code assumes the 'trade_data' object is loaded and prepared.

# Calculate the total value of imports from the U.S. in 2024
total_imports_from_us_2024 <- trade_data$import_hs6 %>%
  filter(floor(YearMonth / 100) == 2024 & Country == "US") %>%
  summarise(total_value = sum(Value, na.rm = TRUE)) %>%
  pull(total_value)

# Calculate the total value of exports to the U.S. in 2024
total_exports_to_us_2024 <- trade_data$export_hs6 %>%
  filter(floor(YearMonth / 100) == 2024 & Country == "US") %>%
  summarise(total_value = sum(Value, na.rm = TRUE)) %>%
  pull(total_value)

# Print the results in a formatted way
cat(paste("Total value of imports from the U.S. in 2024:", 
          scales::dollar(total_imports_from_us_2024), "\n"))
cat(paste("Total value of exports to the U.S. in 2024:  ", 
          scales::dollar(total_exports_to_us_2024), "\n"))