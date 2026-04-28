#open master data
Global_PUB_Gen_101_data <- read.csv("Global_PUB_Gen_101_master_clean.csv")

names(Global_PUB_Gen_101_data)[2] <- "Country_Code"

#DATA PREP
library(plm)
library(dplyr)
library(stargazer)
library(corrplot)

# 1.1 Data Preparation
df_final <- Global_PUB_Gen_101_data %>%
  mutate(
    l_eci  = ECI,
    l_hc   = log(HC),
    l_inst = log(INST),
    l_open = log(Openness),
    l_ca   = log(CA),
    hc_inst_inter = l_hc * l_inst
  ) %>%
  pdata.frame(index = c("Country_Code", "Year"))

#DESCRIPTIVE STATISTICS
# 2.1 Summary Statistics Table
disc_table <- stargazer(df_final[, c("ECI", "HC", "INST", "Openness", "CA")], 
          type = "text", title = "Table 1: Descriptive Statistics", digits = 2)

# 2.2 Correlation Matrix (Checking for Multicollinearity)
cor_data <- df_final %>% select(l_eci, l_hc, l_inst, l_open, l_ca)
cor_matrix <- cor(cor_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

#ESTIMATION
# 3.1 Model A: 1998-2023 (PWT 11.0 Clean)
gmm_2023 <- pgmm(l_eci ~ lag(l_eci, 1) + l_hc + l_inst + hc_inst_inter + l_open + l_ca | 
                   lag(l_eci, 2:5) + lag(l_hc, 2:5), # Lags 2 to 5 to avoid instrument proliferation
                 data = subset(df_final, as.numeric(as.character(Year)) <= 2023),
                 effect = "individual", model = "twosteps", transformation = "ld", collapse = TRUE)

# 3.2 Model B: 1998-2024 (Extrapolated)
gmm_2024 <- pgmm(l_eci ~ lag(l_eci, 1) + l_hc + l_inst + hc_inst_inter + l_open + l_ca | 
                   lag(l_eci, 2:5) + lag(l_hc, 2:5),
                 data = df_final,
                 effect = "individual", model = "twosteps", transformation = "ld", collapse = TRUE)


#POST-ESTIMATION
# Summary with Sargan and AR tests
summary(gmm_2023, robust = TRUE)
summary(gmm_2024, robust = TRUE)

#FINAL TABLE
my_table <- stargazer(gmm_2023, gmm_2024, 
                      type = "text",
                      column.labels = c("1998-2023", "1998-2024"),
                      # We skip covariate.labels here to avoid the nchar error
                      add.lines = list(c("Sargan Test (p-val)", "0.511", "0.278"), 
                                       c("AR(2) Test (p-val)", "0.757", "0.842")))



