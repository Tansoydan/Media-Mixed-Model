
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

cat("\nWeeks with radio spend:", sum(data$investment_radio > 0), "out of", nrow(data), "\n")


# STEP 5: MODEL BUILDING

library(car)
library(lmtest)

# all plausible variables
model1 <- lm(log_sales ~ + log_tv_adstock + log_press_adstock + 
               log_online + log_banners + log_economy + log_unemployment + 
               log_confidence + log_gdp + log_tourists + 
               log_competition + log_competition_1 + log_competition_2 +
               competitor_recognition_1 + competitor_recognition_2 +
               brand_knowledge + weather_index + precipitation_index +
               public_holidays + christmas_dummy + stores_opened,
             data = data)

summary(model1)
vif(model1)


model2 <- lm(log_sales ~ log_press_adstock + log_online + log_tv_adstock + log_banners +
               log_economy + log_unemployment + log_confidence + 
               log_tourists + competitor_recognition_1 + 
               brand_knowledge + weather_index +
               public_holidays + christmas_dummy,
             data = data)

summary(model2)
vif(model2)

final_model <- lm(log_sales ~ + log_press_adstock + log_online + log_tv_adstock + log_banners +
                    log_economy + log_unemployment + log_confidence + 
                    log_tourists + competitor_recognition_1 + 
                    weather_index + public_holidays + christmas_dummy,
                  data = data)

summary(final_model)
vif(final_model)


# STEP 6: MODEL DIAGNOSTICS & VALIDATION

dwtest(final_model)  


plot(final_model, which = 1)  # Residuals vs Fitted

library(ggplot2)

predicted <- fitted(final_model)
n <- length(predicted)

plot_df <- data.frame(
  Date = tail(data$Date, n),
  Actual = tail(data$log_sales, n),
  Predicted = predicted
)

ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "Actual"), linewidth = 0.8) +
  geom_line(aes(y = Predicted, colour = "Predicted"), linewidth = 0.8) +
  scale_colour_manual(values = c("Actual" = "#4A90E2", "Predicted" = "#F5A623")) +
  labs(
    title = "Actual vs Predicted Log Sales",
    subtitle = paste0("R² = ", round(summary(final_model)$r.squared, 3)),
    x = NULL, y = "Log Sales", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


# STEP 7: MODEL INTERPRETATION

coefs <- data.frame(
  Variable = names(coef(final_model)),
  Coefficient = round(coef(final_model), 6),
  P_Value = round(summary(final_model)$coefficients[, 4], 6)
)
print(coefs)




library(ggplot2)

groups_df <- data.frame(
  Group = c("Macro", "Marketing", "Competition", "External"),
  Elasticity = c(
    sum(coef(final_model)[c("log_economy", "log_unemployment", 
                            "log_confidence", "log_tourists")]),
    sum(coef(final_model)[c("log_online", "log_tv_adstock", 
                            "log_banners", "log_press_adstock")]),
    coef(final_model)["competitor_recognition_1"],
    sum(coef(final_model)[c("weather_index", "public_holidays", 
                            "christmas_dummy")])
  )
)


groups_df <- groups_df[order(abs(groups_df$Elasticity), decreasing = FALSE), ]
groups_df$Group <- factor(groups_df$Group, levels = groups_df$Group)

groups_df$Direction <- ifelse(groups_df$Elasticity > 0, "Positive", "Negative")

ggplot(groups_df, aes(x = Group, y = Elasticity, fill = Direction)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%+.2f", Elasticity),
                hjust = ifelse(Elasticity > 0, -0.15, 1.15)),
            size = 4.2, colour = "grey20") +
  scale_fill_manual(values = c("Positive" = "#3B7DD8", "Negative" = "#E74C3C")) +
  coord_flip() +
  labs(
    title = "What drives sales?",
    subtitle = "Net elasticity by driver group (sum of model coefficients)",
    x = NULL, y = "Net elasticity",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(colour = "grey40", margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12)
  ) +
  expand_limits(y = c(min(groups_df$Elasticity) * 1.4, 
                      max(groups_df$Elasticity) * 1.4))

