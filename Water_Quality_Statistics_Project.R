# 📊 R Project: Water Quality Analysis in Canada

# 📦 Load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(car)           # For Levene’s Test
library(ggpubr)        # For ggboxplot
library(reshape2)
library(forecast)
library(tseries)

# 📥 Load dataset
df <- read.csv("/Users/kai/Downloads/canada_water_pollution.csv")

# 🧼 Clean and preprocess
df$Date <- as.Date(df$Date)

df <- df %>%
  filter(!is.na(pH_Level), 
         !is.na(Turbidity_NTU), 
         !is.na(Dissolved_Oxygen_mg_L)) %>%
  mutate(Month = month(Date),
         Season = case_when(
           Month %in% c(12,1,2) ~ "Winter",
           Month %in% c(3,4,5) ~ "Spring",
           Month %in% c(6,7,8) ~ "Summer",
           Month %in% c(9,10,11) ~ "Fall"
         ))

# 📊 Descriptive Statistics
summary_stats <- df %>%
  group_by(Province) %>%
  summarise(
    pH_mean = mean(pH_Level),
    turbidity_mean = mean(Turbidity_NTU),
    dissolved_oxygen_mean = mean(Dissolved_Oxygen_mg_L)
  )
print(summary_stats)

# 📈 Boxplot: Turbidity by Province
ggplot(df, aes(x = Province, y = Turbidity_NTU)) +
  geom_boxplot(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Turbidity by Province", y = "Turbidity (NTU)")

# 🧪 ANOVA: Turbidity ~ Province
anova_model <- aov(Turbidity_NTU ~ Province, data = df)
summary(anova_model)

# 🔁 T-test: Dissolved Oxygen (Summer vs Winter)
do_summer <- df %>% filter(Season == "Summer") %>% pull(Dissolved_Oxygen_mg_L)
do_winter <- df %>% filter(Season == "Winter") %>% pull(Dissolved_Oxygen_mg_L)

t_test_result <- t.test(do_summer, do_winter, var.equal = FALSE)
print(t_test_result)

# 🔗 Correlation Matrix
cor_df <- df %>%
  select(pH_Level, Turbidity_NTU, Dissolved_Oxygen_mg_L) %>%
  na.omit()
cor_matrix <- cor(cor_df)
print(cor_matrix)

# 📉 Visualize Correlation
corr_melt <- melt(cor_matrix)
ggplot(corr_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value, 2))) +
  scale_fill_gradient2() +
  labs(title = "Correlation Matrix of Indicators")

# 📅 Prepare Time Series Data for Ontario
df_ontario <- df %>%
  filter(Province == "Ontario") %>%
  group_by(Month_Year = floor_date(Date, "month")) %>%
  summarise(Turbidity = mean(Turbidity_NTU, na.rm = TRUE)) %>%
  arrange(Month_Year)

# 📊 Convert to Time Series Object
ts_ontario <- ts(df_ontario$Turbidity, 
                 start = c(year(min(df_ontario$Month_Year)), 
                         month(min(df_ontario$Month_Year))), frequency = 12)

# 🔎 Seasonal Decomposition (Additive)
decomp <- decompose(ts_ontario, type = "additive")
plot(decomp)

# 🔮 ARIMA(2,1,2) Model Forecasting
model <- Arima(ts_ontario, order = c(2, 1, 2))
summary(model)

# Forecast next 12 months
forecast_result <- forecast(model, h = 12)

# 📈 Plot Forecast
autoplot(forecast_result) +
  labs(title = "12-Month Forecast of Turbidity in Ontario",
       x = "Year",
       y = "Turbidity (NTU)")

