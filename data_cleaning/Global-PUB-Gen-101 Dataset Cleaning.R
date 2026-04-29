library(readxl)

###ECONOMIC COMPLEXITY INDEX CLEANING
ECI_raw <- read.csv("Data-ECI-Trade.csv")

# 1. Identify columns
#'Country' is the second to last and 'Country ID' is the last
ECI_n_cols <- ncol(ECI_raw)
ECI_year_cols <- names(ECI_raw)[1:(ECI_n_cols - 2)]  # Everything except the last two
ECI_id_cols <- names(ECI_raw)[(ECI_n_cols - 1):ECI_n_cols] # The last two columns

# 2. Reshape from Wide to Long
ECI_raw_long <- reshape(ECI_raw, 
                   direction = "long",
                   varying = list(ECI_year_cols),
                   v.names = "ECI",         # Name for the data
                   timevar = "Year",          # Name for the year column
                   times = ECI_year_cols,         # Current names (with the X)
                   idvar = ECI_id_cols)           # COUNTRY COLUMNS

# 3. Strip the "X" from the Year column - This turns "X1990" into "1990"
ECI_raw_long$Year <- gsub("X", "", ECI_raw_long$Year)

# 4. Remove row names and reorder columns
rownames(ECI_raw_long) <- NULL
ECI_raw_long <- ECI_raw_long[, c(ECI_id_cols, "Year", "ECI")]

#HUMAN CAPITAL - PWT CLEANING
HC_PWT_raw <- read_excel("pwt_11_qogdata_25_04_2026.xlsx")
HC_PWT_long <- HC_PWT_raw[, c("ccodealp", "year", "pwt_hci")]

##estimating human capital from pwt for 2024
# 1. Ensure the data is sorted correctly by country and year
HC_PWT_long <- HC_PWT_long[order(HC_PWT_long$ccodealp, HC_PWT_long$year), ]

# 2. Function to project the next 2 years based on a 3-year trend
project_hc <- function(df) {
  
  # 1. Identify rows up to 2023
  known_data <- df[df$year <= 2023, ]
  
  # 2. Safety check: We need at least 2 years to find a trend
  if (nrow(known_data) < 2) {
    return(df)
  }
  
  # 3. Calculate Average Growth Factor
  # We use tail to get the most recent data available up to 2023
  recent_values <- tail(known_data$pwt_hci, 4)
  growths <- recent_values[-1] / recent_values[-length(recent_values)]
  avg_r <- mean(growths, na.rm = TRUE)
  
  # 4. Get the starting point (the very last known hc value)
  last_val <- tail(known_data$pwt_hci, 1)
  
  # 5. Safety check: If math failed (NA or empty), don't project
  if (length(last_val) == 0 || is.na(avg_r)) {
    return(df)
  }
  
  # 6. Calculate projections
  val_2024 <- last_val * avg_r
  val_2025 <- val_2024 * avg_r
  
  # 7. Create the new rows
  # We use rep() to make sure 'ccodealp' has 2 rows to match the 2 years
  new_rows <- data.frame(
    ccodealp = rep(unique(df$ccodealp), 2),
    year = c(2024, 2025),
    pwt_hci = c(val_2024, val_2025),
    stringsAsFactors = FALSE
  )
  
  return(rbind(df, new_rows))
}

# 3. Apply the function to every country in your dataset
# (Note: make sure your column names are 'ccodealp', 'year', and 'hc' exactly)
hc_list <- lapply(split(HC_PWT_long, HC_PWT_long$ccodealp), project_hc)
HC_PWT_long <- do.call(rbind, hc_list)

# 4. Final Cleanup
rownames(HC_PWT_long) <- NULL
HC_PWT_long <- HC_PWT_long[order(HC_PWT_long$ccodealp, HC_PWT_long$year), ]


#####Human Capital Plus - Education Pillar by WB - Cleaning for Quadrant Analysis later
HCI_EDUC_raw <- read.csv("HD_HCIP_EDUC.csv") 
HCI_EDUC_long <- HCI_EDUC_raw[HCI_EDUC_raw$SEX == "_T",c("REF_AREA", "TIME_PERIOD", "OBS_VALUE")]


#TRADE OPENESS CLEANING
OPENNESS_raw <- read_excel("Openness - API_NE.TRD.GNFS.ZS_DS2_en_excel_v2_731.xls", sheet = "Data", skip=3)

# Convert the object to a standard data frame
OPENNESS_raw <- as.data.frame(OPENNESS_raw)


OPENNESS_raw_long <- reshape(OPENNESS_raw,
                             direction = "long",
                             varying = list(names(OPENNESS_raw)[5:ncol(OPENNESS_raw)]),
                              v.names = "Openness",         # Name for the data
                              timevar = "Year",          # Name for the year column
                              times = names(OPENNESS_raw)[5:ncol(OPENNESS_raw)],         # Current names (with the X)
                              idvar = names(OPENNESS_raw)[2])         # COUNTRY COLUMNS


rownames(OPENNESS_raw_long) <- NULL

# Removes columns 1, 3, and 4
OPENNESS_raw_long <- OPENNESS_raw_long[, -c(1, 3, 4)]


#CAPITAL ACCESS CLEANING
CA_raw <- read_excel("TCA - API_FS.AST.PRVT.GD.ZS_DS2_en_csv_v2_19.xls", sheet = "Data", skip=3)

# Convert the object to a standard data frame
CA_raw <- as.data.frame(CA_raw)


CA_raw_long <- reshape(CA_raw,
                             direction = "long",
                             varying = list(names(CA_raw)[5:ncol(CA_raw)]),
                             v.names = "CA",         # Name for the data
                             timevar = "Year",          # Name for the year column
                             times = names(CA_raw)[5:ncol(CA_raw)],         # Current names (with the X)
                             idvar = names(CA_raw)[2])         # COUNTRY COLUMNS


rownames(CA_raw_long) <- NULL

# Removes columns 1, 3, and 4
CA_raw_long <- CA_raw_long[, -c(1, 3, 4)]



#INSTITUTIONAL FACTORS CLEANING & AVERAGING
INST_raw_va <- read_excel("INST - wgidataset_with_sourcedata-2025.xlsx", sheet = "va")
INST_raw_va <- as.data.frame(INST_raw_va)
names(INST_raw_va)[13] <- "VA" 

INST_raw_pv <- read_excel("INST - wgidataset_with_sourcedata-2025.xlsx", sheet = "pv")
INST_raw_pv <- as.data.frame(INST_raw_pv)
names(INST_raw_pv)[13] <- "PV" 

INST_raw_ge <- read_excel("INST - wgidataset_with_sourcedata-2025.xlsx", sheet = "ge")
INST_raw_ge <- as.data.frame(INST_raw_ge)
names(INST_raw_ge)[13] <- "GE" 

INST_raw_rq <- read_excel("INST - wgidataset_with_sourcedata-2025.xlsx", sheet = "rq")
INST_raw_rq <- as.data.frame(INST_raw_rq)
names(INST_raw_rq)[13] <- "RQ" 

INST_raw_rl <- read_excel("INST - wgidataset_with_sourcedata-2025.xlsx", sheet = "rl")
INST_raw_rl <- as.data.frame(INST_raw_rl)
names(INST_raw_rl)[13] <- "RL" 

INST_raw_cc <- read_excel("INST - wgidataset_with_sourcedata-2025.xlsx", sheet = "cc")
INST_raw_cc <- as.data.frame(INST_raw_cc)
names(INST_raw_cc)[13] <- "CC" 

# 1. Merge the first two, selecting only the necessary columns from the second one
# Column 1 = Country Code, Column 3 = Year, Column 5 = The Value (for example)
INST_raw_long <- merge(INST_raw_va[, c("Economy (code)", "Year", "VA")], 
                       INST_raw_pv[, c("Economy (code)", "Year", "PV")], 
                       by = c("Economy (code)", "Year"), 
                       all = TRUE)

# 2. Add the third one
INST_raw_long <- merge(INST_raw_long, 
                       INST_raw_ge[, c("Economy (code)", "Year", "GE")], 
                       by = c("Economy (code)", "Year"), 
                       all = TRUE)

# 3. Repeat for the rest...
INST_raw_long <- merge(INST_raw_long, 
                       INST_raw_rq[, c("Economy (code)", "Year", "RQ")], 
                       by = c("Economy (code)", "Year"), 
                       all = TRUE)

INST_raw_long <- merge(INST_raw_long, 
                       INST_raw_rl[, c("Economy (code)", "Year", "RL")], 
                       by = c("Economy (code)", "Year"), 
                       all = TRUE)

INST_raw_long <- merge(INST_raw_long, 
                       INST_raw_cc[, c("Economy (code)", "Year", "CC")], 
                       by = c("Economy (code)", "Year"), 
                       all = TRUE)

# 1. Calculate the mean across the 6 value columns
INST_raw_long$INST <- rowMeans(INST_raw_long[, 3:8], na.rm = TRUE)
#master_data$Average_Value[is.nan(master_data$Average_Value)] <- NA

#Rename all Country Code columns and make then uppercase in Dataset
names(ECI_raw_long)[2] <- "Country (Code)"
names(HCI_EDUC_long)[1] <- "Country (Code)"
names(INST_raw_long)[1] <- "Country (Code)"
names(OPENNESS_raw_long)[1] <- "Country (Code)"
names(CA_raw_long)[1] <- "Country (Code)"
names(HC_PWT_long)[1] <- "Country (Code)"
names(HCI_EDUC_long)[2] <- "Year"
names(HC_PWT_long)[2] <- "Year"


ECI_raw_long$`Country (Code)`<- toupper(ECI_raw_long$`Country (Code)`)
HCI_EDUC_long$`Country (Code)` <- toupper(HCI_EDUC_long$`Country (Code)`)
INST_raw_long$`Country (Code)`<- toupper(INST_raw_long$`Country (Code)`)
OPENNESS_raw_long$`Country (Code)`<- toupper(OPENNESS_raw_long$`Country (Code)`)
CA_raw_long$`Country (Code)`<- toupper(CA_raw_long$`Country (Code)`)
HC_PWT_long$`Country (Code)`<- toupper(HC_PWT_long$`Country (Code)`)

#Merge Long Data and Create the Master Dataset for Analysis
Global_PUB_Gen_101_fulldata <- merge(ECI_raw_long[, c("Country", "Country (Code)", "Year", "ECI")], 
                                      HC_PWT_long[, c("Country (Code)", "Year", "pwt_hci")], 
                                      by = c("Country (Code)", "Year"), 
                                      all = TRUE)

#change variable name for HCI
names(Global_PUB_Gen_101_fulldata)[5] <- "HC"

Global_PUB_Gen_101_fulldata <- merge(Global_PUB_Gen_101_fulldata, 
                                     INST_raw_long[, c("Country (Code)", "Year", "INST")], 
                                     by = c("Country (Code)", "Year"), 
                                     all = TRUE)

Global_PUB_Gen_101_fulldata <- merge(Global_PUB_Gen_101_fulldata, 
                                     OPENNESS_raw_long[, c("Country (Code)", "Year", "Openness")], 
                                     by = c("Country (Code)", "Year"), 
                                     all = TRUE)

Global_PUB_Gen_101_fulldata <- merge(Global_PUB_Gen_101_fulldata, 
                                     CA_raw_long[, c("Country (Code)", "Year", "CA")], 
                                     by = c("Country (Code)", "Year"), 
                                     all = TRUE)
#order columns
Global_PUB_Gen_101_fulldata <- Global_PUB_Gen_101_fulldata[, c(3,1,2,4,5,6,7,8)]

#fill in missing country names from WB CA & INST cc dataset as ECI was used as basis of country names
missing_rows <- is.na(Global_PUB_Gen_101_fulldata$Country) | Global_PUB_Gen_101_fulldata$Country == ""
matches <- match(Global_PUB_Gen_101_fulldata$`Country (Code)`[missing_rows], CA_raw$`Country Code`)
Global_PUB_Gen_101_fulldata$Country[missing_rows] <- CA_raw$`Country Name`[matches]

missing_rows <- is.na(Global_PUB_Gen_101_fulldata$Country) | Global_PUB_Gen_101_fulldata$Country == ""
matches <- match(Global_PUB_Gen_101_fulldata$`Country (Code)`[missing_rows], INST_raw_cc$`Economy (code)`)
Global_PUB_Gen_101_fulldata$Country[missing_rows] <- INST_raw_cc$`Economy (name)`[matches]

#####
rownames(Global_PUB_Gen_101_fulldata) <- NULL

####filter out regions, irrelevant years and countries with over 50% of data missing
# Install and load
library(countrycode)

# To see the full master list of all codes (including Taiwan and territories)
iso_master_list <- countrycode::codelist[, c("country.name.en", "iso3c", "iso2c", "continent")]

# 1. Standardize the codes to Uppercase (to avoid case-sensitivity misses)
Global_PUB_Gen_101_fulldata$`Country (Code)` <- toupper(Global_PUB_Gen_101_fulldata$`Country (Code)`)
iso_master_list$iso3c <- toupper(iso_master_list$iso3c)

# 2. Filter the dataset
# This keeps only rows where the 'Country (Code)' exists in the 'iso3c' column of the master list
Global_PUB_Gen_101_fulldata <- Global_PUB_Gen_101_fulldata[Global_PUB_Gen_101_fulldata$`Country (Code)` %in% iso_master_list$iso3c, ]

# Filter to keep only rows where Year is 1998 till 2024
Global_PUB_Gen_101_fulldata <- Global_PUB_Gen_101_fulldata[Global_PUB_Gen_101_fulldata$Year >= 1998 & Global_PUB_Gen_101_fulldata$Year <= 2024, ]

rownames(Global_PUB_Gen_101_fulldata) <- NULL

# Remove country when over 50% of its data is missing
# 1. Identify the data columns (ECI, HC, INST, Openness, CA)
data_cols <- c("ECI", "HC", "INST", "Openness", "CA")

# 2. Create a function to check the missingness ratio
check_density <- function(df) {
  # Select only the data columns
  subset_data <- df[, data_cols]
  
  # Total possible cells (Rows * Columns)
  total_cells <- nrow(subset_data) * ncol(subset_data)
  
  # Count how many are NOT NA
  actual_values <- sum(!is.na(subset_data))
  
  # Calculate missingness percentage
  missing_ratio <- (total_cells - actual_values) / total_cells
  
  # Return the data only if missingness is <= 50%
  if (missing_ratio <= 0.50) {
    return(df)
  } else {
    return(NULL)
  }
}

# 3. Apply the filter per country
# We split by 'Country Code', apply the check, and bind it back together
Global_PUB_Gen_101_master_clean <- do.call(rbind, lapply(split(Global_PUB_Gen_101_fulldata, Global_PUB_Gen_101_fulldata$`Country (Code)`), check_density))

# 4. Final Cleanup
rownames(Global_PUB_Gen_101_master_clean) <- NULL

#Removing countries with no HC data as it is the main independent variable
# 1. Identify countries that have AT LEAST one non-missing HC value
countries_with_hc <- aggregate(HC ~ `Country (Code)`, 
                               data = Global_PUB_Gen_101_master_clean, 
                               FUN = function(x) sum(!is.na(x)) > 0)

# 2. Get the list of codes that actually have data
valid_codes <- countries_with_hc$`Country (Code)`[countries_with_hc$HC == TRUE]

# 3. Filter your master dataset to keep only those valid codes
Global_PUB_Gen_101_master_clean <- Global_PUB_Gen_101_master_clean[Global_PUB_Gen_101_master_clean$`Country (Code)` %in% valid_codes, ]

# Save data
write.csv(Global_PUB_Gen_101_master_clean, "Global_PUB_Gen_101_master_clean.csv", row.names = FALSE)

