# The J-Curve Effect: Exchange Rate and Trade Balance in Developed and Developing Countries

This repository contains the final project and supporting codes developed as part of a **Panel Data Econometrics** course. The study investigates the relationship between the **real exchange rate** and the **trade balance** in developed and developing countries, with a focus on the presence of the **J-Curve effect**. Using panel data with macroeconomic and structural controls, multiple specifications are estimated and compared.

## Research Objective

The central goal is to examine whether currency depreciations improve the trade balance over time — and whether this dynamic differs systematically between developed and developing economies. The analysis tests the empirical validity of the **J-Curve hypothesis**: the pattern where the trade balance initially worsens following a depreciation before improving in the medium to long run.

## Research Questions

- 🔹 Does the J-Curve effect manifest differently across levels of economic development?
- 🔹 Are developing countries more or less responsive to exchange rate adjustments as a trade policy instrument?
- 🔹 How do structural and macroeconomic factors condition the effectiveness of currency devaluations?

## Methodology

The empirical strategy employs **panel data techniques** with country-level data from the World Bank. Three estimators are applied and compared:

| Model | Description |
|-------|-------------|
| **Pooled OLS** | Baseline estimation treating the panel as a single cross-section. |
| **Fixed Effects** | Controls for unobserved, time-invariant country-specific heterogeneity. |
| **Random Effects** | Assumes country-specific effects are uncorrelated with regressors; compared via Hausman test. |

Each specification includes macroeconomic and structural controls to isolate the exchange rate – trade balance relationship. Robustness checks and specification tests guide model selection.

## Key Findings

The results reinforce the hypothesis that **economic structure and development level** significantly affect the efficacy of currency devaluations as a trade policy instrument.

> *The findings suggest that the J-Curve pattern is not uniform across countries, highlighting the importance of structural characteristics in shaping trade balance dynamics following exchange rate shocks.*

## Data Source

- **World Bank:** World Development Indicators (WDI) — panel data covering multiple developed and developing countries.

