# Marketing Mix Modelling — Fashion Retail Client

An econometric analysis of marketing effectiveness for a fashion retail client, built as part of an interview task. The goal is to understand which marketing channels drive sales and whether the current budget allocation is efficient.

---

## Files

| File | Description |
|---|---|
| `mmm_analysis.R` | Full R analysis script — Stages 1 through 7 |
| `mmm_presentation.pptx` | Client-facing presentation slides |

---

## Data

Two input files are required. Place both in a `/data` folder inside the project directory before running the script:

| File | Frequency | Description |
|---|---|---|
| `data_weekly.csv` | Weekly | Sales, marketing spend, control variables (117 rows) |
| `data_monthly.csv` | Monthly | Macroeconomic indicators — unemployment, GDP, confidence, tourists (36 rows) |

---

## Methodology

### Stage 1 — Data Merging
Weekly and monthly datasets merged via left join on year/month keys. All 117 weekly rows retained.

### Stage 2 — Exploratory Data Analysis
Time series plots of sales and spend, distribution histograms, and a correlation matrix across all key variables.

### Stage 3 — Data Transformation
- Log/log1p transformations applied to sales, spend, and macro variables to normalise skewed distributions and enable elasticity interpretation
- Adstock transformation applied to TV (decay = 0.5) and press (decay = 0.3) to capture carry-over effects of advertising
- Radio excluded due to near-zero spend across the dataset
- Final modelling dataset saved as `data_model.csv`

### Stage 4 — OLS Regression
Three models built iteratively:
- **Model 1** — full kitchen-sink model
- **Model 2** — refined model, insignificant variables removed
- **Model 3** — competitor lag variables re-tested and confirmed insignificant (AIC confirmed Model 2 superior)

### Stage 5 — Model Diagnostics
- Standard diagnostic plots (Residuals vs Fitted, Q-Q, Scale-Location, Residuals vs Leverage)
- Durbin-Watson test — initial autocorrelation (DW = 1.20) resolved by adding lagged sales variable (DW = 1.79)
- VIF test — no multicollinearity issues (max VIF = 8.23, below threshold of 10)
- Actual vs Predicted plot confirms strong model fit

### Stage 6 — Contribution Analysis
Sales decomposed into four groups: Base (56.3%), Macro (42.5%), Marketing (0.9%), Controls (0.3%).

### Stage 7 — ROI & Budget Allocation
ROI calculated for measurable channels. Online ROI = 0.40. Press ROI negative — attributed to reverse causality, not a genuine negative ad effect.

---

## Final Model

```r
model_2b <- lm(log_sales ~
                 log_sales_lag1 +
                 log_adstock_press + log_online +
                 log_economy + log_unemployment + log_tourists + log_confidence +
                 public_holidays + christmas_dummy + brand_knowledge +
                 weather_index + log_competition,
               data = df_model)
```

| Metric | Value |
|---|---|
| Adjusted R² | 0.896 |
| Observations | 116 |
| Durbin-Watson | 1.79 |
| Max VIF | 8.23 |

---

## Key Findings

- **Online is the only marketing channel with a measurable, statistically significant positive effect on sales**
- Macroeconomic conditions (economy index, consumer confidence, unemployment) dominate sales variation at 42.5%
- Press adstock is significant but negative — interpreted as reverse causality, not a genuine negative ad effect
- TV, banners and radio could not be attributed — coefficients insignificant across all model iterations
- Online ROI = 0.40 — every £1 spent on online advertising returns £0.40 in attributed sales

---

## Requirements

```r
install.packages(c("tidyverse", "lubridate", "corrplot", "lmtest", "car"))
```

---

## Author
Taner Soydan
