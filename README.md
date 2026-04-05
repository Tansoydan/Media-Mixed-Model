# Media Mix Modelling — Fashion Retail Client

## Overview

This project builds an econometric model to measure the effectiveness of marketing channels for a fashion retail client. The goal is to identify which channels drive sales and assess whether the current budget allocation is efficient.

## Key Findings

- **Sales are primarily driven by macroeconomic conditions** (economy index, unemployment, consumer confidence) and structural brand strength — together these explain over 99% of sales variation.
- **Online is the only marketing channel with a measurable positive effect** on sales, returning £0.98 per £1 spent on a direct sales basis. True ROI is likely higher when brand awareness effects are considered.
- **TV (31% of budget) shows no measurable impact** on sales — tested with both raw spend and adstock transformations.
- **Press shows a negative coefficient**, likely reflecting reverse causality (increased spend during weak periods) rather than a genuinely harmful effect.
- **Banners show no measurable effect.** Radio had insufficient data to assess (only 6 active weeks out of 117).

## Data

Two datasets were provided:

- **data_weekly.csv** — 117 rows, 20 columns. Weekly sales, marketing spend (TV, radio, press, banners, online), competition variables, weather, brand knowledge, and other controls.
- **data_monthly.csv** — 36 rows, 5 columns. Monthly macroeconomic indicators: unemployment, tourists, GDP, consumer confidence.

Datasets were merged using a year-month key. Monthly values repeat for each week within the same month. No missing values in either dataset.

## Methodology

### Data Processing

- **Log transformations** applied to sales, marketing spend, economic variables, and competition spend. This puts variables on comparable scales and allows coefficients to be interpreted as elasticities (% change in sales per % change in spend). Marketing channels used log(x+1) to handle zero-spend weeks.
- **Adstock transformations** applied to TV (decay rate 0.5) and press (decay rate 0.3) to capture the carryover effect of advertising. TV has a longer-lasting brand-building effect; press fades faster. Adstock improved TV's correlation with sales from 0.05 to 0.20.
- **Variables not transformed:** public holidays and Christmas dummy (binary), brand knowledge and competitor recognition (proportions), weather and precipitation indices.

### Model Building

An iterative approach was used, starting with a kitchen sink model (21 variables) and systematically removing insignificant and problematic variables based on p-values, VIF scores, and business logic.

Key modelling decisions:
- **Stores opened** removed due to severe multicollinearity (VIF 19.3) — acting as a time proxy, not a genuine sales driver.
- **Lagged sales variable** added to resolve autocorrelation (Durbin-Watson improved from 1.10 to 1.91).
- **Radio excluded** from modelling — only 6 out of 117 weeks had any spend, insufficient to estimate a reliable coefficient.
- **TV and banners** removed as insignificant across all model specifications.
- **Press adstock** retained despite insignificance in some models — included to provide the client with a complete picture of marketing channel performance.

### Final Model

```
log(sales) ~ log_sales_lag + log_online + log_press_adstock +
             log_economy + log_unemployment + log_confidence +
             log_tourists + competitor_recognition_1 +
             weather_index + public_holidays + christmas_dummy
```

| Variable | Group | Coefficient | P-Value |
|---|---|---|---|
| Lagged Sales | Structural | +0.158 | 0.006 |
| Online | Marketing | +0.014 | 0.008 |
| Press (adstock) | Marketing | -0.003 | 0.011 |
| Economy Index | Economic | +0.650 | <0.001 |
| Unemployment | Economic | -0.476 | <0.001 |
| Consumer Confidence | Economic | +0.650 | <0.001 |
| Tourists | Economic | -0.080 | 0.002 |
| Competitor Recognition | Competition | -0.178 | 0.005 |
| Weather Index | External | -0.001 | 0.008 |
| Public Holidays | External | -0.021 | <0.001 |
| Christmas Dummy | External | -0.113 | <0.001 |

All variables are statistically significant. All VIFs below 10 (unemployment at 8.25 is the highest — borderline but acceptable given the coefficient is stable and highly significant).

### Model Validation

- **Adjusted R² = 0.897** — the model explains approximately 90% of sales variation.
- **Durbin-Watson = 1.91 (p = 0.14)** — no autocorrelation.
- **Shapiro-Wilk = 0.99 (p = 0.51)** — residuals are normally distributed.
- **VIF** — all values below 10, no severe multicollinearity.
- **Actual vs Predicted** — model tracks real sales closely across the full two-year period.

### Sales Decomposition

| Group | Contribution | Description |
|---|---|---|
| Base | 54.1% | Structural brand strength and weekly momentum |
| Macro | 46.0% | Economy, unemployment, confidence, tourists |
| Marketing | 0.8% | Online (positive), press (caveated) |
| Controls | -0.9% | Weather, holidays, competition |

## Project Structure

```
├── data_weekly.csv              # Weekly sales and marketing data
├── data_monthly.csv             # Monthly macroeconomic indicators
├── mmm_analysis.R               # Full analysis script
├── presentation.pptx            # Interview presentation (6 slides)
└── README.md                    # This file
```

## How to Run

1. Open the project in RStudio or Posit Cloud.
2. Ensure `data_weekly.csv` and `data_monthly.csv` are in the working directory.
3. Install required packages if not already available:
   ```r
   install.packages(c("corrplot", "car", "lmtest"))
   ```
4. Run `mmm_analysis.R` from top to bottom. The script is structured sequentially — each step depends on the previous one.

## Tools & Packages

- **R** 
- **corrplot** — correlation matrix visualisation
- **car** — VIF calculation
- **lmtest** — Durbin-Watson autocorrelation test

## Author

Taner Soydan — Interview Task
