
# MEDIA MIX MODELLING

# STEP 1: LOAD & EXPLORE THE DATA

weekly  <- read.csv("data/data_weekly.csv")
monthly <- read.csv("data/data_monthly.csv")

cat("Weekly data:", nrow(weekly), "rows,", ncol(weekly), "columns\n")
cat("Monthly data:", nrow(monthly), "rows,", ncol(monthly), "columns\n")

str(weekly)
str(monthly)

summary(weekly)
summary(monthly)


cat("\n--- Missing values (weekly) ---\n")
colSums(is.na(weekly))
cat("\n--- Missing values (monthly) ---\n")
colSums(is.na(monthly))


head(weekly$Date)
head(monthly$Date)

cat("\nSales range:", min(weekly$sales), "to", max(weekly$sales), "\n")
cat("Sales mean:", mean(weekly$sales), "\n")
cat("Sales sd:", sd(weekly$sales), "\n")


# STEP 2: MERGE DATASETS & DATA PROCESSING

# Convert dates to proper Date format
weekly$Date  <- as.Date(weekly$Date, format = "%d/%m/%Y")
monthly$Date <- as.Date(monthly$Date, format = "%d/%m/%Y")

# Create a year-month key in both datasets for merging
weekly$YearMonth  <- format(weekly$Date, "%Y-%m")
monthly$YearMonth <- format(monthly$Date, "%Y-%m")

# left join to keep all weekly rows
data <- merge(weekly, monthly, by = "YearMonth", all.x = TRUE)

data$Date <- data$Date.x
data$Date.x <- NULL
data$Date.y <- NULL


cat("Merged data:", nrow(data), "rows,", ncol(data), "columns\n")

# Log transformations
data$log_sales <- log(data$sales)

# add 1 before logging because of zero-spend weeks
data$log_tv      <- log(data$investment_tv + 1)
data$log_radio   <- log(data$investment_radio + 1)
data$log_press   <- log(data$investment_press + 1)
data$log_banners <- log(data$investment_banners + 1)
data$log_online  <- log(data$investment_online + 1)

data$log_economy      <- log(data$economy_index)
data$log_unemployment <- log(data$unemployment)
data$log_confidence   <- log(data$confidence)
data$log_gdp          <- log(data$gdp)
data$log_tourists     <- log(data$tourists)

data$log_competition   <- log(data$investment_competition + 1)
data$log_competition_1 <- log(data$investment_competition_1 + 1)
data$log_competition_2 <- log(data$investment_competition_2 + 1)


data$YearMonth <- NULL
data$YearWeek  <- NULL
write.csv(data, "merged_data.csv", row.names = FALSE)


# STEP 3: INITIAL ANALYSIS


library(corrplot)

log_vars <- data[, c("log_sales", "log_tv", "log_radio", "log_press", 
                     "log_banners", "log_online", "log_economy", 
                     "log_unemployment", "log_confidence", "log_gdp", 
                     "log_tourists", "log_competition", "log_competition_1",
                     "log_competition_2")]

cor_matrix <- cor(log_vars)
round(cor_matrix[, "log_sales"], 3)


corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.6,
         tl.cex = 0.7, tl.col = "black",
         title = "Correlation Matrix — Log Variables",
         mar = c(0, 0, 2, 0))

cat("\n--- Non-logged variable correlations with log_sales ---\n")
cat("brand_knowledge:", cor(data$brand_knowledge, data$log_sales), "\n")
cat("public_holidays:", cor(data$public_holidays, data$log_sales), "\n")
cat("christmas_dummy:", cor(data$christmas_dummy, data$log_sales), "\n")
cat("weather_index:", cor(data$weather_index, data$log_sales), "\n")
cat("precipitation_index:", cor(data$precipitation_index, data$log_sales), "\n")
cat("stores_opened:", cor(data$stores_opened, data$log_sales), "\n")
cat("competitor_recognition_1:", cor(data$competitor_recognition_1, data$log_sales), "\n")
cat("competitor_recognition_2:", cor(data$competitor_recognition_2, data$log_sales), "\n")


plot(data$Date, data$sales, type = "l", col = "steelblue", lwd = 2,
     main = "Weekly Sales Over Time",
     xlab = "Date", ylab = "Sales")


# STEP 4: ADSTOCK TRANSFORMATIONS

# Create the adstock function

adstock <- function(x, rate) {
  adstocked <- numeric(length(x))
  adstocked[1] <- x[1]
  for (i in 2:length(x)) {
    adstocked[i] <- x[i] + rate * adstocked[i - 1]
  }
  return(adstocked)
}

data <- data[order(data$Date), ]

data$tv_adstock    <- adstock(data$investment_tv, rate = 0.5)
data$press_adstock <- adstock(data$investment_press, rate = 0.3)

data$log_tv_adstock    <- log(data$tv_adstock + 1)
data$log_press_adstock <- log(data$press_adstock + 1)


cat("Raw log_tv:", cor(data$log_tv, data$log_sales), "\n")
cat("Adstocked log_tv:", cor(data$log_tv_adstock, data$log_sales), "\n")
cat("\nRaw log_press:", cor(data$log_press, data$log_sales), "\n")
cat("Adstocked log_press:", cor(data$log_press_adstock, data$log_sales), "\n")

# lagged sales variable for autocorrelation fix
data$log_sales_lag <- c(NA, data$log_sales[-nrow(data)])

cat("\nWeeks with radio spend:", sum(data$investment_radio > 0), "out of", nrow(data), "\n")


# STEP 5: MODEL BUILDING

library(car)
library(lmtest)

# all plausible variables
model1 <- lm(log_sales ~ log_sales_lag + log_tv_adstock + log_press_adstock + 
               log_online + log_banners + log_economy + log_unemployment + 
               log_confidence + log_gdp + log_tourists + 
               log_competition + log_competition_1 + log_competition_2 +
               competitor_recognition_1 + competitor_recognition_2 +
               brand_knowledge + weather_index + precipitation_index +
               public_holidays + christmas_dummy + stores_opened,
             data = data)

summary(model1)
vif(model1)

# Remove clearly insignificant variables
model2 <- lm(log_sales ~ log_sales_lag + log_press_adstock + log_online +
               log_economy + log_unemployment + log_confidence + 
               log_tourists + competitor_recognition_1 + 
               brand_knowledge + weather_index +
               public_holidays + christmas_dummy,
             data = data)

summary(model2)
vif(model2)

final_model <- lm(log_sales ~ log_sales_lag + log_press_adstock + log_online +
                    log_economy + log_unemployment + log_confidence + 
                    log_tourists + competitor_recognition_1 + 
                    weather_index + public_holidays + christmas_dummy,
                  data = data)

summary(final_model)
vif(final_model)



# STEP 6: MODEL DIAGNOSTICS & VALIDATION

dwtest(final_model)  


plot(final_model, which = 1)  # Residuals vs Fitted

# Actual vs Predicted
predicted <- fitted(final_model)
actual <- data$log_sales[!is.na(data$log_sales_lag)]

plot(data$Date[!is.na(data$log_sales_lag)], actual, type = "l", col = "steelblue", lwd = 2,
     main = "Actual vs Predicted Sales",
     xlab = "Date", ylab = "Log Sales")
lines(data$Date[!is.na(data$log_sales_lag)], predicted, col = "coral", lwd = 2)
legend("bottomright", legend = c("Actual", "Predicted"), 
       col = c("steelblue", "coral"), lwd = 2)


# STEP 7: MODEL INTERPRETATION

coefs <- data.frame(
  Variable = names(coef(final_model)),
  Coefficient = round(coef(final_model), 6),
  P_Value = round(summary(final_model)$coefficients[, 4], 6)
)
print(coefs)

# Online ROI calculation
online_elasticity <- coef(final_model)["log_online"]
avg_sales <- mean(data$sales)
avg_online <- mean(data$investment_online)
online_roi <- (online_elasticity * avg_sales) / avg_online

cat("\nOnline elasticity:", round(online_elasticity, 4), "\n")
cat("Average weekly sales: £", round(avg_sales, 0), "\n")
cat("Average weekly online spend: £", round(avg_online, 0), "\n")
cat("Online ROI: £", round(online_roi, 2), "per £1 spent\n")

# Total spend by channel
cat("\n--- Total spend by channel ---\n")
cat("Online:", round(sum(data$investment_online), 0), "\n")
cat("TV:", round(sum(data$investment_tv), 0), "\n")
cat("Press:", round(sum(data$investment_press), 0), "\n")
cat("Banners:", round(sum(data$investment_banners), 0), "\n")
cat("Radio:", round(sum(data$investment_radio), 0), "\n")

# Sales decomposition
model_data <- model.matrix(final_model)
avg_values <- colMeans(model_data, na.rm = TRUE)
contributions <- coef(final_model) * avg_values

base <- sum(contributions[c("(Intercept)", "log_sales_lag")])
macro <- sum(contributions[c("log_economy", "log_unemployment", "log_confidence", "log_tourists")])
marketing <- sum(contributions[c("log_online", "log_press_adstock")])
controls <- sum(contributions[c("competitor_recognition_1", "weather_index", "public_holidays", "christmas_dummy")])
total <- base + macro + marketing + controls

cat("\n--- Sales Contribution by Group ---\n")
cat("Base:", round(base/total * 100, 1), "%\n")
cat("Macro:", round(macro/total * 100, 1), "%\n")
cat("Marketing:", round(marketing/total * 100, 1), "%\n")
cat("Controls:", round(controls/total * 100, 1), "%\n")

contributions_pct <- c(
  round(base/total * 100, 1),
  round(macro/total * 100, 1),
  round(marketing/total * 100, 1),
  round(controls/total * 100, 1)
)
group_names <- c("Base", "Macro", "Marketing", "Controls")
group_colors <- c("#1B3A5C", "#E74C3C", "#3B7DD8", "#95A5A6")

barplot(contributions_pct, names.arg = group_names, col = group_colors,
        main = "Sales Contribution by Group (%)",
        ylab = "% Contribution", border = NA, ylim = c(-5, 60),
        cex.names = 1.1, cex.main = 1.3)
abline(h = 0, col = "black", lwd = 1)
