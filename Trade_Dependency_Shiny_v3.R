library(tidyverse)
library(shiny)
library(plotly)
library(DT)

# ============================================================
# DATA LOADING AND PREPARATION
# ============================================================

# Base directory
base_dir <- "C:/Users/JiangPe/Documents/10. Steel/NEW STEEL/CIMT Data"
setwd("C:/Users/JiangPe/Documents/10. Steel")

# Load unit conversion table
load_unit_conversions <- function(base_dir) {
  uom <- read_csv(
    file.path(base_dir, "ODPF_9_UOMDesc.csv"),
    col_names = c("code", "start_date", "end_date", "desc_en", "desc_fr", "source", "period"),
    col_types = cols(.default = "c"),
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
  
  # Create conversion factors to kilograms
  conversions <- tibble(
    unit = c("GRM", "MIL", "TNE", "KGM", "CTM", "LBR"),
    to_kg = c(0.001, 0.000001, 1000, 1, 100, 0.453592)
  )
  
  # Match UOM codes with conversion factors
  uom %>%
    left_join(conversions, by = c("code" = "unit")) %>%
    select(code, desc_en, to_kg)
}

# Load steel product categories
load_steel_categories <- function() {
  steel_cats <- read_csv("Steel Product Categories.csv", show_col_types = FALSE)
  
  # Transform to long format with category names
  steel_long <- steel_cats %>%
    pivot_longer(
      cols = everything(),
      names_to = "category",
      values_to = "hs6"
    ) %>%
    filter(!is.na(hs6)) %>%
    mutate(hs6 = sprintf("%06d", as.integer(hs6)))
  
  return(steel_long)
}

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

# Convert quantity to kilograms
convert_to_kg <- function(data, unit_conversions) {
  data %>%
    left_join(unit_conversions, by = c("Unit" = "code")) %>%
    mutate(
      Quantity_kg = ifelse(!is.na(to_kg) & !is.na(Quantity), 
                          Quantity * to_kg, 
                          Quantity),
      Unit_converted = ifelse(!is.na(to_kg), "KGM", Unit)
    ) %>%
    select(-to_kg, -desc_en)
}

# Calculate HHI
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

# Calculate U.S. dependency ratios
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

# Calculate bilateral trade balance
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
      bilateral_balance = (canada_exports_to_us - us_exports_to_canada) / 
        pmax(total_bilateral_trade, 1),
      bilateral_balance = ifelse(is.nan(bilateral_balance) | is.infinite(bilateral_balance), 
                                 0, bilateral_balance),
      competitive_disadvantage = (1 - bilateral_balance) / 2
    ) %>%
    select(product, competitive_disadvantage, total_bilateral_trade)
}

# Calculate Price Competitiveness Index (PCI)
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
      pci = pmin(pmax(pci, quantile(pci, 0.05, na.rm = TRUE)),
                 quantile(pci, 0.95, na.rm = TRUE))
    ) %>%
    select(product, pci)
}

# Create geographic labels
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
      
      flow_label = case_when(
        flow_type == "country_country" ~ if(is_export) {
          paste("Canada to", Country_Name)
        } else {
          paste(Country_Name, "to Canada")
        },
        
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
        
        TRUE ~ if(is_export) {
          paste(Province_Name, "to", Country_Name)
        } else {
          paste(Country_Name, "to", Province_Name)
        }
      ),
      
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
unit_conversions <- load_unit_conversions(base_dir)
steel_categories <- load_steel_categories()

trade_data <- readRDS("trade_data.rds")

# names(trade_data$import_hs10) <- c("YearMonth", "HS10", "Country", "Province", 
#                                    "State", "Value", "Quantity", "Unit")
# names(trade_data$import_hs6) <- c("YearMonth", "HS6", "Country", "Province", 
#                                   "State", "Value", "Quantity", "Unit")
# names(trade_data$import_hs2) <- c("YearMonth", "HS2", "Country", "Province", 
#                                   "State", "Value")
# names(trade_data$export_hs8) <- c("YearMonth", "HS8", "Country", "Province", 
#                                   "State", "Value", "Quantity", "Unit")
# names(trade_data$export_hs6) <- c("YearMonth", "HS6", "Country", "Province", 
#                                   "State", "Value", "Quantity", "Unit")
# names(trade_data$export_hs2) <- c("YearMonth", "HS2", "Country", "Province", 
#                                   "State", "Value")

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
      
      # METRIC SELECTION
      radioButtons(
        "metric",
        "Analysis Metric:",
        choices = c("Value (CAD)" = "Value", "Quantity (Units)" = "Quantity"),
        selected = "Value"
      ),
      
      # Warning for HS2 + Quantity
      conditionalPanel(
        condition = "input.hs_level == 'hs2' && input.metric == 'Quantity'",
        div(
          style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 10px; margin: 10px 0; border-radius: 4px;",
          strong("Warning:"), " Quantity data is not available at HS2 level. Please select HS6 or switch to Value metric."
        )
      ),
      
      # Product selection OR Steel category selection
      conditionalPanel(
        condition = "input.main_tabs != 'Steel Analysis'",
        selectizeInput(
          "product_code",
          "Select Product(s):",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(maxItems = 10)
        )
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Steel Analysis' && input.hs_level == 'hs6'",
        selectizeInput(
          "steel_category",
          "Select Steel Category(ies):",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(maxItems = 10)
        )
      ),
      
      # Year period selection (NEW)
      checkboxGroupInput(
        "year_periods",
        "Select Time Periods:",
        choices = c("2015-2019" = "2015_2019",
                    "2019-2022" = "2019_2022", 
                    "2022-2025" = "2022_2025"),
        selected = "2022_2025"
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
            column(6, 
                   plotlyOutput("dependency_matrix_export", height = "500px"),
                   downloadButton("download_dep_export_plot", "Download Plot Data"),
                   downloadButton("download_dep_export_tab", "Download Tab Data")),
            column(6, 
                   plotlyOutput("dependency_matrix_import", height = "500px"),
                   downloadButton("download_dep_import_plot", "Download Plot Data"),
                   downloadButton("download_dep_import_tab", "Download Tab Data"))
          ),
          hr(),
          h4("Selected Product Position"),
          verbatimTextOutput("dependency_text")
        ),
        
        tabPanel(
          "Time Series",
          # NEW: Flow direction filter
          checkboxGroupInput(
            "time_series_flows",
            "Show Flow Directions:",
            choices = c("Export", "Import"),
            selected = c("Export", "Import"),
            inline = TRUE
          ),
          plotlyOutput("time_plot_combined", height = "600px"),
          downloadButton("download_time_plot", "Download Plot Data"),
          downloadButton("download_time_tab", "Download Tab Data")
        ),
        
        tabPanel(
          "Market Share",
          fluidRow(
            column(6, 
                   h4("Export Market Share"),
                   plotlyOutput("market_share_export", height = "500px"),
                   downloadButton("download_market_export_plot", "Download Plot Data")),
            column(6, 
                   h4("Import Market Share"),
                   plotlyOutput("market_share_import", height = "500px"),
                   downloadButton("download_market_import_plot", "Download Plot Data"))
          ),
          downloadButton("download_market_tab", "Download Tab Data")
        ),
        
        tabPanel(
          "HHI Analysis",
          fluidRow(
            column(6,
                   h4("Export HHI Distribution"),
                   plotlyOutput("hhi_plot_export", height = "500px"),
                   downloadButton("download_hhi_export_plot", "Download Plot Data")),
            column(6,
                   h4("Import HHI Distribution"),
                   plotlyOutput("hhi_plot_import", height = "500px"),
                   downloadButton("download_hhi_import_plot", "Download Plot Data"))
          ),
          downloadButton("download_hhi_tab", "Download Tab Data")
        ),
        
        tabPanel(
          "Data Table",
          h4("Export Data"),
          DTOutput("export_table"),
          downloadButton("download_export_table", "Download Export Table"),
          hr(),
          h4("Import Data"),
          DTOutput("import_table"),
          downloadButton("download_import_table", "Download Import Table")
        ),
        
        tabPanel(
          "Pillar 2 Matrix",
          conditionalPanel(
            condition = "input.hs_level == 'hs6' && input.metric == 'Value'",
            plotlyOutput("pillar2_matrix", height = "600px"),
            downloadButton("download_pillar2_plot", "Download Plot Data"),
            downloadButton("download_pillar2_tab", "Download Tab Data"),
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
        ),
        
        # NEW: Steel Analysis Tab
        tabPanel(
          "Steel Analysis",
          conditionalPanel(
            condition = "input.hs_level == 'hs6'",
            tabsetPanel(
              id = "steel_tabs",
              
              tabPanel(
                "Steel Dependency Matrix",
                fluidRow(
                  column(6, 
                         plotlyOutput("steel_dependency_matrix_export", height = "500px"),
                         downloadButton("download_steel_dep_export_plot", "Download Plot Data")),
                  column(6, 
                         plotlyOutput("steel_dependency_matrix_import", height = "500px"),
                         downloadButton("download_steel_dep_import_plot", "Download Plot Data"))
                ),
                downloadButton("download_steel_dep_tab", "Download Tab Data"),
                hr(),
                h4("Selected Steel Category Position"),
                verbatimTextOutput("steel_dependency_text")
              ),
              
              tabPanel(
                "Steel Time Series",
                checkboxGroupInput(
                  "steel_time_series_flows",
                  "Show Flow Directions:",
                  choices = c("Export", "Import"),
                  selected = c("Export", "Import"),
                  inline = TRUE
                ),
                plotlyOutput("steel_time_plot_combined", height = "600px"),
                downloadButton("download_steel_time_plot", "Download Plot Data"),
                downloadButton("download_steel_time_tab", "Download Tab Data")
              ),
              
              tabPanel(
                "Steel Market Share",
                fluidRow(
                  column(6, 
                         h4("Export Market Share"),
                         plotlyOutput("steel_market_share_export", height = "500px"),
                         downloadButton("download_steel_market_export_plot", "Download Plot Data")),
                  column(6, 
                         h4("Import Market Share"),
                         plotlyOutput("steel_market_share_import", height = "500px"),
                         downloadButton("download_steel_market_import_plot", "Download Plot Data"))
                ),
                downloadButton("download_steel_market_tab", "Download Tab Data")
              ),
              
              tabPanel(
                "Steel HHI Analysis",
                fluidRow(
                  column(6,
                         h4("Export HHI Distribution"),
                         plotlyOutput("steel_hhi_plot_export", height = "500px"),
                         downloadButton("download_steel_hhi_export_plot", "Download Plot Data")),
                  column(6,
                         h4("Import HHI Distribution"),
                         plotlyOutput("steel_hhi_plot_import", height = "500px"),
                         downloadButton("download_steel_hhi_import_plot", "Download Plot Data"))
                ),
                downloadButton("download_steel_hhi_tab", "Download Tab Data")
              )
            )
          ),
          conditionalPanel(
            condition = "input.hs_level != 'hs6'",
            div(
              style = "text-align: center; padding: 100px;",
              h3("Steel Analysis Only Available at HS6 Level"),
              p("Please select 'HS6' classification level to view steel product analysis.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Convert year period selections to year vector (de-duplicated)
  selected_years <- reactive({
    periods <- input$year_periods
    years <- c()
    
    if ("2015_2019" %in% periods) {
      years <- c(years, 2015:2019)
    }
    if ("2019_2022" %in% periods) {
      years <- c(years, 2019:2022)
    }
    if ("2022_2025" %in% periods) {
      years <- c(years, 2022:2025)
    }
    
    unique(years)
  })
  
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
  
  # Get effective metric
  effective_metric <- reactive({
    if (input$metric == "Quantity" && !quantity_available()) {
      return("Value")
    }
    input$metric
  })
  
  # Filtered data by year periods and geographic scope
  filtered_export_data <- reactive({
    req(selected_years())
    data <- current_export_data() %>%
      mutate(year = floor(YearMonth / 100)) %>%
      filter(year %in% selected_years())
    
    if (input$geo_scope == "ontario") {
      data <- data %>% filter(Province == "ON")
    }
    
    # Apply unit conversion if using Quantity metric and HS6
    if (effective_metric() == "Quantity" && input$hs_level == "hs6") {
      data <- convert_to_kg(data, unit_conversions)
    }
    
    data
  })
  
  filtered_import_data <- reactive({
    req(selected_years())
    data <- current_import_data() %>%
      mutate(year = floor(YearMonth / 100)) %>%
      filter(year %in% selected_years())
    
    if (input$geo_scope == "ontario") {
      data <- data %>% filter(Province == "ON")
    }
    
    # Apply unit conversion if using Quantity metric and HS6
    if (effective_metric() == "Quantity" && input$hs_level == "hs6") {
      data <- convert_to_kg(data, unit_conversions)
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
  
  # Calculate HHI based on filtered data
  current_export_hhi <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    calculate_hhi(filtered_export_data_with_internal(), col_name, metric_col)
  })
  
  current_import_hhi <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    calculate_hhi(filtered_import_data_with_internal(), col_name, metric_col)
  })
  
  # Calculate U.S. dependency ratios
  current_export_us_dependency <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    calculate_us_dependency(filtered_export_data_with_internal(), col_name, metric_col, "export")
  })
  
  current_import_us_dependency <- reactive({
    col_name <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    calculate_us_dependency(filtered_import_data_with_internal(), col_name, metric_col, "import")
  })
  
  # Calculate Pillar 2 components
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
  
  # Update steel category choices
  observe({
    req(input$hs_level == "hs6")
    
    categories <- steel_categories %>%
      select(category) %>%
      distinct() %>%
      arrange(category)
    
    choices <- setNames(categories$category, categories$category)
    
    updateSelectizeInput(
      session,
      "steel_category",
      choices = choices,
      selected = if(length(choices) > 0) choices[1] else NULL
    )
  })
  
  # Get HS6 codes for selected steel categories
  selected_steel_hs6 <- reactive({
    req(input$steel_category)
    steel_categories %>%
      filter(category %in% input$steel_category) %>%
      pull(hs6) %>%
      unique()
  })
  
  # Product-specific filtered data
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
  
  # Steel-specific filtered data
  steel_export_data <- reactive({
    req(input$steel_category, input$hs_level == "hs6")
    hs6_codes <- selected_steel_hs6()
    
    filtered_export_data_with_internal() %>%
      filter(HS6 %in% hs6_codes) %>%
      create_geo_label(descriptions, is_export = TRUE, flow_type = input$flow_type)
  })
  
  steel_import_data <- reactive({
    req(input$steel_category, input$hs_level == "hs6")
    hs6_codes <- selected_steel_hs6()
    
    filtered_import_data_with_internal() %>%
      filter(HS6 %in% hs6_codes) %>%
      create_geo_label(descriptions, is_export = FALSE, flow_type = input$flow_type)
  })
  
  # Steel HHI calculations
  steel_export_hhi <- reactive({
    req(input$steel_category, input$hs_level == "hs6")
    hs6_codes <- selected_steel_hs6()
    metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
    
    filtered_export_data_with_internal() %>%
      filter(HS6 %in% hs6_codes) %>%
      mutate(steel_category = input$steel_category[1]) %>%
      calculate_hhi("steel_category", metric_col)
  })
  
  steel_import_hhi <- reactive({
    req(input$steel_category, input$hs_level == "hs6")
    hs6_codes <- selected_steel_hs6()
    metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
    
    filtered_import_data_with_internal() %>%
      filter(HS6 %in% hs6_codes) %>%
      mutate(steel_category = input$steel_category[1]) %>%
      calculate_hhi("steel_category", metric_col)
  })
  
  # Steel US dependency
  steel_export_us_dependency <- reactive({
    req(input$steel_category, input$hs_level == "hs6")
    hs6_codes <- selected_steel_hs6()
    metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
    
    filtered_export_data_with_internal() %>%
      filter(HS6 %in% hs6_codes) %>%
      mutate(steel_category = input$steel_category[1]) %>%
      calculate_us_dependency("steel_category", metric_col, "export")
  })
  
  steel_import_us_dependency <- reactive({
    req(input$steel_category, input$hs_level == "hs6")
    hs6_codes <- selected_steel_hs6()
    metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
    
    filtered_import_data_with_internal() %>%
      filter(HS6 %in% hs6_codes) %>%
      mutate(steel_category = input$steel_category[1]) %>%
      calculate_us_dependency("steel_category", metric_col, "import")
  })
  
  # Output: Product information
  output$product_info <- renderText({
    req(input$product_code)
    
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_label <- if(effective_metric() == "Value") "Value" else "Quantity (kg)"
    
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
  
  # Output: Dependency Matrix - Export
  output$dependency_matrix_export <- renderPlotly({
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_label <- if(effective_metric() == "Value") "CAD" else "kg"
    
    data <- current_export_hhi() %>%
      rename(product = !!sym(hhi_col)) %>%
      left_join(current_export_us_dependency(), by = "product") %>%
      mutate(
        hs2 = substr(product, 1, 2),
        section = map_hs_to_section(hs2),
        is_selected = product %in% input$product_code,
        log_value = log10(total_trade_metric + 1)
      )
    
    avg_hhi <- mean(data$hhi, na.rm = TRUE)
    avg_us_dep <- mean(data$us_dependency, na.rm = TRUE)
    
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
  
  # Output: Dependency Matrix - Import
  output$dependency_matrix_import <- renderPlotly({
    hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
    metric_label <- if(effective_metric() == "Value") "CAD" else "kg"
    
    data <- current_import_hhi() %>%
      rename(product = !!sym(hhi_col)) %>%
      left_join(current_import_us_dependency(), by = "product") %>%
      mutate(
        hs2 = substr(product, 1, 2),
        section = map_hs_to_section(hs2),
        is_selected = product %in% input$product_code,
        log_value = log10(total_trade_metric + 1)
      )
    
    avg_hhi <- mean(data$hhi, na.rm = TRUE)
    avg_us_dep <- mean(data$us_dependency, na.rm = TRUE)
    
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
  
  # Output: Time series with flow filter
  output$time_plot_combined <- renderPlotly({
    req(input$product_code, length(input$time_series_flows) > 0)
    
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    metric_label <- if(effective_metric() == "Value") "CAD" else "kg"
    
    combined_data <- NULL
    
    # Add export data if selected
    if ("Export" %in% input$time_series_flows) {
      top_export_flows <- product_export_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        head(input$top_n) %>%
        pull(flow_label)
      
      export_plot_data <- product_export_data() %>%
        mutate(
          date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")),
          flow_label = ifelse(flow_label %in% top_export_flows, flow_label, "Other")
        ) %>%
        group_by(date, flow_label) %>%
        summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(flow_type = "Export")
      
      combined_data <- export_plot_data
    }
    
    # Add import data if selected
    if ("Import" %in% input$time_series_flows) {
      top_import_flows <- product_import_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        head(input$top_n) %>%
        pull(flow_label)
      
      import_plot_data <- product_import_data() %>%
        mutate(
          date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")),
          flow_label = ifelse(flow_label %in% top_import_flows, flow_label, "Other")
        ) %>%
        group_by(date, flow_label) %>%
        summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(flow_type = "Import")
      
      combined_data <- bind_rows(combined_data, import_plot_data)
    }
    
    plot_ly(combined_data, x = ~date, y = ~metric_value, 
            color = ~flow_label, 
            linetype = ~flow_type,
            type = 'scatter', mode = 'lines',
            line = list(width = 2)) %>%
      layout(
        title = paste("Trade Flows Over Time -", paste(input$product_code, collapse = ", "), "(", effective_metric(), ")"),
        xaxis = list(title = "Date"),
        yaxis = list(title = paste(effective_metric(), "(", metric_label, ")")),
        hovermode = "x unified",
        legend = list(orientation = "v", x = 1.02, y = 1)
      )
  })
  
  # Output: Market share - Export
  output$market_share_export <- renderPlotly({
    req(input$product_code)
    
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    
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
      layout(title = paste("Export Market Share -", paste(input$product_code, collapse = ", "), "(", effective_metric(), ")"))
  })
  
  # Output: Market share - Import
  output$market_share_import <- renderPlotly({
    req(input$product_code)
    
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    
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
      layout(title = paste("Import Market Share -", paste(input$product_code, collapse = ", "), "(", effective_metric(), ")"))
  })
  
  # Output: HHI distribution - Export
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
  
  # Output: HHI distribution - Import
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
  
  # Output: Data tables
  output$export_table <- renderDT({
    req(input$product_code)
    
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    
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
        select(Date, Flow = flow_label, Quantity_kg, Unit_converted, Value)
    }
    
    table_data %>%
      arrange(desc(Date))
  }, options = list(pageLength = 10))
  
  output$import_table <- renderDT({
    req(input$product_code)
    
    metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
    
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
        select(Date, Flow = flow_label, Quantity_kg, Unit_converted, Value)
    }
    
    table_data %>%
      arrange(desc(Date))
  }, options = list(pageLength = 10))
  
  # Output: Pillar 2 Matrix
  output$pillar2_matrix <- renderPlotly({
    req(input$hs_level == "hs6", effective_metric() == "Value")
    
    pillar2_data <- current_bilateral_balance() %>%
      left_join(current_pci(), by = "product") %>%
      filter(!is.na(pci), !is.na(competitive_disadvantage)) %>%
      mutate(
        hs2 = substr(product, 1, 2),
        section = map_hs_to_section(hs2),
        is_selected = product %in% input$product_code,
        log_value = log10(total_bilateral_trade + 1)
      )
    
    avg_comp_disadv <- mean(pillar2_data$competitive_disadvantage, na.rm = TRUE)
    avg_pci <- mean(pillar2_data$pci, na.rm = TRUE)
    
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
          list(
            type = "line",
            x0 = avg_pci, x1 = avg_pci,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "black", width = 2, dash = "dash")
          ),
          list(
            type = "line",
            x0 = 0, x1 = 1,
            xref = "paper",
            y0 = avg_comp_disadv, y1 = avg_comp_disadv,
            line = list(color = "black", width = 2, dash = "dash")
          )
        ),
        annotations = list(
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
  
  # Output: Pillar 2 text
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
      
      balance_raw <- (comp_disadv * 2) - 1
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
  
  # STEEL ANALYSIS OUTPUTS
  
  # Steel Dependency Matrix - Export
  output$steel_dependency_matrix_export <- renderPlotly({
    req(input$steel_category, input$hs_level == "hs6")
    
    metric_label <- if(effective_metric() == "Value") "CAD" else "kg"
    
    data <- steel_export_hhi() %>%
      rename(product = steel_category) %>%
      left_join(steel_export_us_dependency(), by = "product") %>%
      mutate(
        is_selected = product %in% input$steel_category,
        log_value = log10(total_trade_metric + 1)
      )
    
    avg_hhi <- mean(data$hhi, na.rm = TRUE)
    avg_us_dep <- mean(data$us_dependency, na.rm = TRUE)
    
    plot_ly(data, x = ~hhi, y = ~us_dependency, 
            type = 'scatter', mode = 'markers',
            size = ~log_value,
            sizes = c(10, 100),
            marker = list(opacity = 0.6, line = list(width = 1, color = 'red')),
            text = ~paste0(
              "Category: ", product,
              "<br>HHI: ", round(hhi, 1),
              "<br>U.S. Dependency: ", round(us_dependency, 3),
              "<br>", effective_metric(), ": ", scales::comma(total_trade_metric), " ", metric_label
            ),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Steel Export Dependency Matrix (", effective_metric(), ")"),
        xaxis = list(title = "Herfindahl-Hirschman Index"),
        yaxis = list(title = "U.S. Export Dependency", range = c(0, 1)),
        showlegend = FALSE,
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
  
  # Steel Dependency Matrix - Import
  output$steel_dependency_matrix_import <- renderPlotly({
    req(input$steel_category, input$hs_level == "hs6")
    
    metric_label <- if(effective_metric() == "Value") "CAD" else "kg"
    
    data <- steel_import_hhi() %>%
      rename(product = steel_category) %>%
      left_join(steel_import_us_dependency(), by = "product") %>%
      mutate(
        is_selected = product %in% input$steel_category,
        log_value = log10(total_trade_metric + 1)
      )
    
    avg_hhi <- mean(data$hhi, na.rm = TRUE)
    avg_us_dep <- mean(data$us_dependency, na.rm = TRUE)
    
    plot_ly(data, x = ~hhi, y = ~us_dependency, 
            type = 'scatter', mode = 'markers',
            size = ~log_value,
            sizes = c(10, 100),
            marker = list(opacity = 0.6, line = list(width = 1, color = 'blue')),
            text = ~paste0(
              "Category: ", product,
              "<br>HHI: ", round(hhi, 1),
              "<br>U.S. Dependency: ", round(us_dependency, 3),
              "<br>", effective_metric(), ": ", scales::comma(total_trade_metric), " ", metric_label
            ),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Steel Import Dependency Matrix (", effective_metric(), ")"),
        xaxis = list(title = "Herfindahl-Hirschman Index"),
        yaxis = list(title = "U.S. Import Dependency", range = c(0, 1)),
        showlegend = FALSE,
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
  
  # Steel Dependency text
  output$steel_dependency_text <- renderText({
    req(input$steel_category, input$hs_level == "hs6")
    
    if (length(input$steel_category) == 1) {
      export_hhi_val <- steel_export_hhi() %>%
        filter(steel_category == input$steel_category[1]) %>%
        pull(hhi)
      
      import_hhi_val <- steel_import_hhi() %>%
        filter(steel_category == input$steel_category[1]) %>%
        pull(hhi)
      
      export_us_dep_val <- steel_export_us_dependency() %>%
        filter(product == input$steel_category[1]) %>%
        pull(us_dependency)
      
      import_us_dep_val <- steel_import_us_dependency() %>%
        filter(product == input$steel_category[1]) %>%
        pull(us_dependency)
      
      avg_export_hhi <- mean(steel_export_hhi()$hhi, na.rm = TRUE)
      avg_import_hhi <- mean(steel_import_hhi()$hhi, na.rm = TRUE)
      avg_export_us_dep <- mean(steel_export_us_dependency()$us_dependency, na.rm = TRUE)
      avg_import_us_dep <- mean(steel_import_us_dependency()$us_dependency, na.rm = TRUE)
      
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
        "STEEL CATEGORY: ", input$steel_category[1], "\n\n",
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
      "Select a single steel category to see detailed position analysis."
    }
  })
  
  # Steel Time Series
  output$steel_time_plot_combined <- renderPlotly({
    req(input$steel_category, input$hs_level == "hs6", length(input$steel_time_series_flows) > 0)
    
    metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
    metric_label <- if(effective_metric() == "Value") "CAD" else "kg"
    
    combined_data <- NULL
    
    if ("Export" %in% input$steel_time_series_flows) {
      top_export_flows <- steel_export_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        head(input$top_n) %>%
        pull(flow_label)
      
      export_plot_data <- steel_export_data() %>%
        mutate(
          date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")),
          flow_label = ifelse(flow_label %in% top_export_flows, flow_label, "Other")
        ) %>%
        group_by(date, flow_label) %>%
        summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(flow_type = "Export")
      
      combined_data <- export_plot_data
    }
    
    if ("Import" %in% input$steel_time_series_flows) {
      top_import_flows <- steel_import_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        head(input$top_n) %>%
        pull(flow_label)
      
      import_plot_data <- steel_import_data() %>%
        mutate(
          date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-")),
          flow_label = ifelse(flow_label %in% top_import_flows, flow_label, "Other")
        ) %>%
        group_by(date, flow_label) %>%
        summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
        mutate(flow_type = "Import")
      
      combined_data <- bind_rows(combined_data, import_plot_data)
    }
    
    plot_ly(combined_data, x = ~date, y = ~metric_value, 
            color = ~flow_label, 
            linetype = ~flow_type,
            type = 'scatter', mode = 'lines',
            line = list(width = 2)) %>%
      layout(
        title = paste("Steel Trade Flows Over Time -", paste(input$steel_category, collapse = ", "), "(", effective_metric(), ")"),
        xaxis = list(title = "Date"),
        yaxis = list(title = paste(effective_metric(), "(", metric_label, ")")),
        hovermode = "x unified",
        legend = list(orientation = "v", x = 1.02, y = 1)
      )
  })
  
  # Steel Market Share - Export
  output$steel_market_share_export <- renderPlotly({
    req(input$steel_category, input$hs_level == "hs6")
    
    metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
    
    data <- steel_export_data() %>%
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
      layout(title = paste("Steel Export Market Share -", paste(input$steel_category, collapse = ", "), "(", effective_metric(), ")"))
  })
  
  # Steel Market Share - Import
  output$steel_market_share_import <- renderPlotly({
    req(input$steel_category, input$hs_level == "hs6")
    
    metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
    
    data <- steel_import_data() %>%
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
      layout(title = paste("Steel Import Market Share -", paste(input$steel_category, collapse = ", "), "(", effective_metric(), ")"))
  })
  
  # Steel HHI - Export
  output$steel_hhi_plot_export <- renderPlotly({
    req(input$steel_category, input$hs_level == "hs6")
    
    data <- steel_export_hhi()
    
    plot_ly(data, x = ~hhi, type = "histogram", nbinsx = 50,
            color = ~concentration_level,
            colors = c("Low (Diversified)" = "green", 
                       "Moderate" = "orange", 
                       "High (Concentrated)" = "red")) %>%
      layout(
        title = paste("Steel Export HHI Distribution (", effective_metric(), ")"),
        xaxis = list(title = "HHI Score"),
        yaxis = list(title = "Number of Categories"),
        shapes = list(
          list(type = "line", x0 = 1500, x1 = 1500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black")),
          list(type = "line", x0 = 2500, x1 = 2500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black"))
        )
      )
  })
  
  # Steel HHI - Import
  output$steel_hhi_plot_import <- renderPlotly({
    req(input$steel_category, input$hs_level == "hs6")
    
    data <- steel_import_hhi()
    
    plot_ly(data, x = ~hhi, type = "histogram", nbinsx = 50,
            color = ~concentration_level,
            colors = c("Low (Diversified)" = "green", 
                       "Moderate" = "orange", 
                       "High (Concentrated)" = "red")) %>%
      layout(
        title = paste("Steel Import HHI Distribution (", effective_metric(), ")"),
        xaxis = list(title = "HHI Score"),
        yaxis = list(title = "Number of Categories"),
        shapes = list(
          list(type = "line", x0 = 1500, x1 = 1500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black")),
          list(type = "line", x0 = 2500, x1 = 2500, y0 = 0, y1 = 1,
               yref = "paper", line = list(dash = "dash", color = "black"))
        )
      )
  })
  
  # ============================================================
  # DOWNLOAD HANDLERS
  # ============================================================
  
  # Dependency Matrix Downloads
  output$download_dep_export_plot <- downloadHandler(
    filename = function() {
      paste0("dep_matrix_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
      data <- current_export_hhi() %>%
        rename(product = !!sym(hhi_col)) %>%
        left_join(current_export_us_dependency(), by = "product")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_dep_import_plot <- downloadHandler(
    filename = function() {
      paste0("dep_matrix_import_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
      data <- current_import_hhi() %>%
        rename(product = !!sym(hhi_col)) %>%
        left_join(current_import_us_dependency(), by = "product")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_dep_export_tab <- downloadHandler(
    filename = function() {
      paste0("dep_tab_export_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
      export_data <- current_export_hhi() %>%
        rename(product = !!sym(hhi_col)) %>%
        left_join(current_export_us_dependency(), by = "product")
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$download_dep_import_tab <- downloadHandler(
    filename = function() {
      paste0("dep_tab_import_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
      import_data <- current_import_hhi() %>%
        rename(product = !!sym(hhi_col)) %>%
        left_join(current_import_us_dependency(), by = "product")
      write.csv(import_data, file, row.names = FALSE)
    }
  )
  
  # Time Series Downloads
  output$download_time_plot <- downloadHandler(
    filename = function() {
      paste0("time_series_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
      
      combined_data <- NULL
      
      if ("Export" %in% input$time_series_flows) {
        export_data <- product_export_data() %>%
          mutate(date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-"))) %>%
          group_by(date, flow_label) %>%
          summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
          mutate(flow_type = "Export")
        combined_data <- export_data
      }
      
      if ("Import" %in% input$time_series_flows) {
        import_data <- product_import_data() %>%
          mutate(date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-"))) %>%
          group_by(date, flow_label) %>%
          summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
          mutate(flow_type = "Import")
        combined_data <- bind_rows(combined_data, import_data)
      }
      
      write.csv(combined_data, file, row.names = FALSE)
    }
  )
  
  output$download_time_tab <- downloadHandler(
    filename = function() {
      paste0("time_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_data <- product_export_data()
      import_data <- product_import_data()
      combined <- bind_rows(
        export_data %>% mutate(flow_type = "Export"),
        import_data %>% mutate(flow_type = "Import")
      )
      write.csv(combined, file, row.names = FALSE)
    }
  )
  
  # Market Share Downloads
  output$download_market_export_plot <- downloadHandler(
    filename = function() {
      paste0("market_share_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
      data <- product_export_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_market_import_plot <- downloadHandler(
    filename = function() {
      paste0("market_share_import_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
      data <- product_import_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_market_tab <- downloadHandler(
    filename = function() {
      paste0("market_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity" && input$hs_level == "hs6") "Quantity_kg" else effective_metric()
      export_data <- product_export_data() %>%
        group_by(flow_label) %>%
        summarize(export_total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      import_data <- product_import_data() %>%
        group_by(flow_label) %>%
        summarize(import_total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      combined <- full_join(export_data, import_data, by = "flow_label")
      write.csv(combined, file, row.names = FALSE)
    }
  )
  
  # HHI Downloads
  output$download_hhi_export_plot <- downloadHandler(
    filename = function() {
      paste0("hhi_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
      if (input$hs_level == "hs2") {
        data <- current_export_hhi()
      } else {
        selected_hs2 <- unique(substr(input$product_code, 1, 2))
        data <- current_export_hhi() %>%
          filter(substr(!!sym(hhi_col), 1, 2) %in% selected_hs2)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_hhi_import_plot <- downloadHandler(
    filename = function() {
      paste0("hhi_import_", Sys.Date(), ".csv")
    },
    content = function(file) {
      hhi_col <- if(input$hs_level == "hs2") "HS2" else "HS6"
      if (input$hs_level == "hs2") {
        data <- current_import_hhi()
      } else {
        selected_hs2 <- unique(substr(input$product_code, 1, 2))
        data <- current_import_hhi() %>%
          filter(substr(!!sym(hhi_col), 1, 2) %in% selected_hs2)
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_hhi_tab <- downloadHandler(
    filename = function() {
      paste0("hhi_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_hhi <- current_export_hhi() %>% mutate(flow = "Export")
      import_hhi <- current_import_hhi() %>% mutate(flow = "Import")
      combined <- bind_rows(export_hhi, import_hhi)
      write.csv(combined, file, row.names = FALSE)
    }
  )
  
  # Data Table Downloads
  output$download_export_table <- downloadHandler(
    filename = function() {
      paste0("export_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- product_export_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_import_table <- downloadHandler(
    filename = function() {
      paste0("import_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- product_import_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Pillar 2 Downloads
  output$download_pillar2_plot <- downloadHandler(
    filename = function() {
      paste0("pillar2_matrix_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- current_bilateral_balance() %>%
        left_join(current_pci(), by = "product")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_pillar2_tab <- downloadHandler(
    filename = function() {
      paste0("pillar2_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- current_bilateral_balance() %>%
        left_join(current_pci(), by = "product")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Steel Analysis Downloads
  output$download_steel_dep_export_plot <- downloadHandler(
    filename = function() {
      paste0("steel_dep_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- steel_export_hhi() %>%
        left_join(steel_export_us_dependency(), by = c("steel_category" = "product"))
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_steel_dep_import_plot <- downloadHandler(
    filename = function() {
      paste0("steel_dep_import_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- steel_import_hhi() %>%
        left_join(steel_import_us_dependency(), by = c("steel_category" = "product"))
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_steel_dep_tab <- downloadHandler(
    filename = function() {
      paste0("steel_dep_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_data <- steel_export_hhi() %>%
        left_join(steel_export_us_dependency(), by = c("steel_category" = "product")) %>%
        mutate(flow = "Export")
      import_data <- steel_import_hhi() %>%
        left_join(steel_import_us_dependency(), by = c("steel_category" = "product")) %>%
        mutate(flow = "Import")
      combined <- bind_rows(export_data, import_data)
      write.csv(combined, file, row.names = FALSE)
    }
  )
  
  output$download_steel_time_plot <- downloadHandler(
    filename = function() {
      paste0("steel_time_series_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
      
      combined_data <- NULL
      
      if ("Export" %in% input$steel_time_series_flows) {
        export_data <- steel_export_data() %>%
          mutate(date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-"))) %>%
          group_by(date, flow_label) %>%
          summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
          mutate(flow_type = "Export")
        combined_data <- export_data
      }
      
      if ("Import" %in% input$steel_time_series_flows) {
        import_data <- steel_import_data() %>%
          mutate(date = as.Date(paste(floor(YearMonth/100), YearMonth %% 100, "01", sep = "-"))) %>%
          group_by(date, flow_label) %>%
          summarize(metric_value = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop") %>%
          mutate(flow_type = "Import")
        combined_data <- bind_rows(combined_data, import_data)
      }
      
      write.csv(combined_data, file, row.names = FALSE)
    }
  )
  
  output$download_steel_time_tab <- downloadHandler(
    filename = function() {
      paste0("steel_time_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_data <- steel_export_data()
      import_data <- steel_import_data()
      combined <- bind_rows(
        export_data %>% mutate(flow_type = "Export"),
        import_data %>% mutate(flow_type = "Import")
      )
      write.csv(combined, file, row.names = FALSE)
    }
  )
  
  output$download_steel_market_export_plot <- downloadHandler(
    filename = function() {
      paste0("steel_market_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
      data <- steel_export_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_steel_market_import_plot <- downloadHandler(
    filename = function() {
      paste0("steel_market_import_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
      data <- steel_import_data() %>%
        group_by(flow_label) %>%
        summarize(total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_steel_market_tab <- downloadHandler(
    filename = function() {
      paste0("steel_market_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metric_col <- if(effective_metric() == "Quantity") "Quantity_kg" else effective_metric()
      export_data <- steel_export_data() %>%
        group_by(flow_label) %>%
        summarize(export_total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      import_data <- steel_import_data() %>%
        group_by(flow_label) %>%
        summarize(import_total = sum(!!sym(metric_col), na.rm = TRUE), .groups = "drop")
      combined <- full_join(export_data, import_data, by = "flow_label")
      write.csv(combined, file, row.names = FALSE)
    }
  )
  
  output$download_steel_hhi_export_plot <- downloadHandler(
    filename = function() {
      paste0("steel_hhi_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- steel_export_hhi()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_steel_hhi_import_plot <- downloadHandler(
    filename = function() {
      paste0("steel_hhi_import_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- steel_import_hhi()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$download_steel_hhi_tab <- downloadHandler(
    filename = function() {
      paste0("steel_hhi_tab_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_hhi <- steel_export_hhi() %>% mutate(flow = "Export")
      import_hhi <- steel_import_hhi() %>% mutate(flow = "Import")
      combined <- bind_rows(export_hhi, import_hhi)
      write.csv(combined, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
