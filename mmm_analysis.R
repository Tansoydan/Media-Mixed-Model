
# MEDIA MIX MODELLING
# Author: Taner Soydan


# STAGE 1: DATA LOADING & MERGING 

library(tidyverse)
library(lubridate)

weekly  <- read.csv("data/data_weekly.csv")
monthly <- read.csv("data/data_monthly.csv")

head(weekly)
head(monthly)
str(weekly)
str(monthly)

# Convert date columns from text to date objects
weekly <- weekly %>%
  mutate(Date = dmy(Date))

monthly <- monthly %>%
  mutate(Date = dmy(Date))

weekly <- weekly %>%
  mutate(year = year(Date), month = month(Date))

monthly <- monthly %>%
  mutate(year = year(Date), month = month(Date))


data <- weekly %>%
  left_join(monthly, by = c("year", "month"))

data <- data %>%
  select(-Date.y) %>%
  rename(Date = Date.x)

nrow(data)
ncol(data)
head(data)
sum(is.na(data))

write.csv(data, "data/data_merged.csv", row.names = FALSE)


# STAGE 2: EXPLORATORY DATA ANALYSIS 

data <- read.csv("data/data_merged.csv")
data <- data %>% mutate(Date = as.Date(Date))

# Sales over time
ggplot(data, aes(x = Date, y = sales)) +
  geom_line(colour = "steelblue") +
  geom_smooth(method = "lm", colour = "red", linetype = "dashed") +
  labs(title = "Weekly Sales Over Time", x = "Date", y = "Sales") +
  theme_minimal()

# Marketing spend by channel over time
spend_long <- data %>%
  select(Date, investment_tv, investment_radio, investment_press,
         investment_banners, investment_online) %>%
  pivot_longer(cols = -Date, names_to = "channel", values_to = "spend")

ggplot(spend_long, aes(x = Date, y = spend, colour = channel)) +
  geom_line() +
  labs(title = "Marketing Spend by Channel Over Time",
       x = "Date", y = "Spend", colour = "Channel") +
  theme_minimal()

# Total budget by channel
spend_summary <- data %>%
  summarise(
    TV      = sum(investment_tv),
    Radio   = sum(investment_radio),
    Press   = sum(investment_press),
    Banners = sum(investment_banners),
    Online  = sum(investment_online)
  ) %>%
  pivot_longer(everything(), names_to = "channel", values_to = "total_spend")

ggplot(spend_summary, aes(x = reorder(channel, -total_spend), y = total_spend, fill = channel)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Marketing Spend by Channel", x = "Channel", y = "Total Spend") +
  theme_minimal() +
  theme(legend.position = "none")

# Distribution of sales
ggplot(data, aes(x = sales)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  labs(title = "Distribution of Weekly Sales", x = "Sales", y = "Count") +
  theme_minimal()

# Distribution of key spend channels
ggplot(data, aes(x = investment_tv)) +
  geom_histogram(bins = 30, fill = "coral", colour = "white") +
  labs(title = "Distribution of TV Spend", x = "TV Investment", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = investment_online)) +
  geom_histogram(bins = 30, fill = "seagreen", colour = "white") +
  labs(title = "Distribution of Online Spend", x = "Online Investment", y = "Count") +
  theme_minimal()

# Correlation matrix
library(corrplot)

cor_data <- data %>%
  select(sales, investment_tv, investment_radio, investment_press,
         investment_banners, investment_online, economy_index,
         brand_knowledge, weather_index, unemployment, gdp, confidence)

cor_matrix <- cor(cor_data, use = "complete.obs")

corrplot(cor_matrix,
         method = "color", type = "upper",
         tl.cex = 0.8, addCoef.col = "black",
         number.cex = 0.7, title = "Correlation Matrix",
         mar = c(0, 0, 1, 0))

data %>%
  select(sales, investment_tv, investment_radio, investment_press,
         investment_banners, investment_online) %>%
  summary()


# STAGE 3: DATA TRANSFORMATION 

df <- read.csv("data/data_merged.csv")

# Log-transforms
df$log_sales <- log(df$sales)

# log1p handles zero spend weeks
df$log_tv      <- log1p(df$investment_tv)
df$log_radio   <- log1p(df$investment_radio)
df$log_press   <- log1p(df$investment_press)
df$log_banners <- log1p(df$investment_banners)
df$log_online  <- log1p(df$investment_online)


df$log_unemployment <- log(df$unemployment)
df$log_gdp          <- log(df$gdp)
df$log_tourists     <- log(df$tourists)
df$log_confidence   <- log(df$confidence)
df$log_economy      <- log(df$economy_index)


df$log_competition   <- log1p(df$investment_competition)
df$log_competition_1 <- log1p(df$investment_competition_1)
df$log_competition_2 <- log1p(df$investment_competition_2)

# Visualise effect 
par(mfrow = c(1, 2))
hist(df$sales,     main = "Sales (Raw)",             xlab = "Sales",      col = "steelblue")
hist(df$log_sales, main = "Sales (Log-transformed)", xlab = "log(Sales)", col = "darkorange")
par(mfrow = c(1, 1))

par(mfrow = c(2, 3))
hist(df$log_tv,      main = "log(TV)",      col = "steelblue", xlab = "")
hist(df$log_radio,   main = "log(Radio)",   col = "steelblue", xlab = "")
hist(df$log_press,   main = "log(Press)",   col = "steelblue", xlab = "")
hist(df$log_banners, main = "log(Banners)", col = "steelblue", xlab = "")
hist(df$log_online,  main = "log(Online)",  col = "steelblue", xlab = "")
par(mfrow = c(1, 1))

# Adstock function 
adstock <- function(spend, decay) {
  adstocked    <- numeric(length(spend))
  adstocked[1] <- spend[1]
  for (i in 2:length(spend)) {
    adstocked[i] <- spend[i] + decay * adstocked[i - 1]
  }
  return(adstocked)
}

# Radio excluded — near-zero spend across dataset
df$adstock_tv    <- adstock(df$investment_tv,    decay = 0.5)
df$adstock_press <- adstock(df$investment_press, decay = 0.3)

df$log_adstock_tv    <- log1p(df$adstock_tv)
df$log_adstock_press <- log1p(df$adstock_press)


par(mfrow = c(2, 1))
plot(df$investment_tv, type = "l", col = "steelblue",
     main = "TV Spend (Raw)", ylab = "Spend", xlab = "Week")
plot(df$adstock_tv, type = "l", col = "darkorange",
     main = "TV Spend (Adstocked, decay = 0.5)", ylab = "Adstock Value", xlab = "Week")
par(mfrow = c(1, 1))


df_model <- df[, c(
  "Date", "YearWeek", "log_sales",
  "log_adstock_tv", "log_adstock_press",
  "log_banners", "log_online",
  "log_economy", "log_unemployment", "log_gdp",
  "log_tourists", "log_confidence",
  "stores_opened", "public_holidays", "christmas_dummy",
  "brand_knowledge", "weather_index", "precipitation_index",
  "log_competition", "log_competition_1", "log_competition_2"
)]

write.csv(df_model, "data/data_model.csv", row.names = FALSE)
cat("Dimensions:", nrow(df_model), "rows x", ncol(df_model), "columns\n")


# STAGE 4: OLS REGRESSION MODELLING 

df_model <- read.csv("data/data_model.csv")


model_1 <- lm(log_sales ~
                log_adstock_tv + log_adstock_press + log_banners + log_online +
                log_economy + log_unemployment + log_gdp + log_tourists + log_confidence +
                stores_opened + public_holidays + christmas_dummy +
                brand_knowledge + weather_index + precipitation_index +
                log_competition + log_competition_1 + log_competition_2,
              data = df_model)

summary(model_1)

# Model 2 — insignificant variables removed
model_2 <- lm(log_sales ~
                log_adstock_press + log_online +
                log_economy + log_unemployment + log_tourists + log_confidence +
                public_holidays + christmas_dummy + brand_knowledge +
                weather_index + log_competition,
              data = df_model)

summary(model_2)


# STAGE 5: MODEL DIAGNOSTICS

library(lmtest)
library(dplyr)
library(car)

par(mfrow = c(2, 2))
plot(model_2)
par(mfrow = c(1, 1))


dwtest(model_2)
# DW = 1.20 — positive autocorrelation detected

# Inspect flagged outlier rows
df_model[c(1, 92), ]

# Fix: add lagged sales variable to absorb week-to-week momentum
df_model$log_sales_lag1 <- lag(df_model$log_sales, 1)

# final model with sales lag
model_2b <- lm(log_sales ~
                 log_sales_lag1 +
                 log_adstock_press + log_online +
                 log_economy + log_unemployment + log_tourists + log_confidence +
                 public_holidays + christmas_dummy + brand_knowledge +
                 weather_index + log_competition,
               data = df_model)

summary(model_2b)

dwtest(model_2b) # DW improved from 1.20 to 1.79

par(mfrow = c(2, 2))
plot(model_2b)
par(mfrow = c(1, 1))


plot(model_2b, which = 1)

vif(model_2b)

df_model$predicted_2b <- predict(model_2b, newdata = df_model)

plot(df_model$log_sales, df_model$predicted_2b,
     xlab = "Actual log(sales)", ylab = "Predicted log(sales)",
     main = "Actual vs Predicted — Model 2b")
abline(0, 1, col = "red")


# STAGE 6: CONTRIBUTION ANALYSIS 

# Extract model coefficients
coefs <- coef(model_2b)

# Calculate each variable's contribution: coefficient × variable value
contrib <- data.frame(
  date         = df_model$Date,
  intercept    = coefs["(Intercept)"],
  sales_lag    = coefs["log_sales_lag1"]   * df_model$log_sales_lag1,
  economy      = coefs["log_economy"]       * df_model$log_economy,
  unemployment = coefs["log_unemployment"]  * df_model$log_unemployment,
  tourists     = coefs["log_tourists"]      * df_model$log_tourists,
  confidence   = coefs["log_confidence"]    * df_model$log_confidence,
  press        = coefs["log_adstock_press"] * df_model$log_adstock_press,
  online       = coefs["log_online"]        * df_model$log_online,
  public_hols  = coefs["public_holidays"]   * df_model$public_holidays,
  christmas    = coefs["christmas_dummy"]   * df_model$christmas_dummy,
  brand        = coefs["brand_knowledge"]   * df_model$brand_knowledge,
  weather      = coefs["weather_index"]     * df_model$weather_index,
  competition  = coefs["log_competition"]   * df_model$log_competition
)

# Group contributions into meaningful buckets
contrib$macro     <- contrib$economy + contrib$unemployment +
  contrib$tourists + contrib$confidence
contrib$marketing <- contrib$press + contrib$online
contrib$controls  <- contrib$public_hols + contrib$christmas +
  contrib$weather + contrib$brand + contrib$competition
contrib$base      <- contrib$intercept + contrib$sales_lag

summary_contrib <- data.frame(
  Group = c("Base", "Macro", "Marketing", "Controls"),
  Avg_Contribution = c(
    mean(contrib$base,      na.rm = TRUE),
    mean(contrib$macro,     na.rm = TRUE),
    mean(contrib$marketing, na.rm = TRUE),
    mean(contrib$controls,  na.rm = TRUE)
  )
)

summary_contrib$Pct_Share <- round(
  summary_contrib$Avg_Contribution / sum(summary_contrib$Avg_Contribution) * 100, 1
)

print(summary_contrib)

barplot(summary_contrib$Pct_Share,
        names.arg = summary_contrib$Group,
        col  = c("#2C3E50", "#E74C3C", "#3498DB", "#95A5A6"),
        main = "Sales Contribution by Group (%)",
        ylab = "% Contribution",
        ylim = c(0, 70))


marketing_detail <- data.frame(
  Channel = c("Online", "Press (Adstock)"),
  Avg_Contribution = c(
    mean(contrib$online, na.rm = TRUE),
    mean(contrib$press,  na.rm = TRUE)
  )
)

marketing_detail$Pct_of_Marketing <- round(
  marketing_detail$Avg_Contribution /
    sum(abs(marketing_detail$Avg_Contribution)) * 100, 1
)

print(marketing_detail)

barplot(marketing_detail$Avg_Contribution,
        names.arg = marketing_detail$Channel,
        col  = c("#3498DB", "#E74C3C"),
        main = "Marketing Channel Contributions",
        ylab = "Avg Log Sales Contribution")


# STAGE 7: ROI & BUDGET ALLOCATION 


avg_actual_sales  <- mean(exp(df_model$log_sales), na.rm = TRUE)
total_avg_contrib <- mean(rowSums(contrib[, -1], na.rm = TRUE))


online_pct <- mean(contrib$online, na.rm = TRUE) / total_avg_contrib
press_pct  <- mean(contrib$press,  na.rm = TRUE) / total_avg_contrib

online_sales_annual <- online_pct * avg_actual_sales * 52
press_sales_annual  <- press_pct  * avg_actual_sales * 52

# Annual spend 
avg_spend_online    <- mean(df$investment_online, na.rm = TRUE)
avg_spend_press     <- mean(df$investment_press,  na.rm = TRUE)
annual_spend_online <- avg_spend_online * 52
annual_spend_press  <- avg_spend_press  * 52

# ROI = attributed annual sales / annual spend
roi_online <- online_sales_annual / annual_spend_online
roi_press  <- press_sales_annual  / annual_spend_press

roi_table <- data.frame(
  Channel          = c("Online", "Press"),
  Annual_Spend     = round(c(annual_spend_online, annual_spend_press)),
  Attributed_Sales = round(c(online_sales_annual, press_sales_annual)),
  ROI              = round(c(roi_online, roi_press), 2)
)

print(roi_table)