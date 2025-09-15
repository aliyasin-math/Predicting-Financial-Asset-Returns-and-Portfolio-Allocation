# Predicting-Financial-Asset-Returns-and-Portfolio-Allocation
Working Paper (PDF) + reproducibility code for “Predicting Financial Asset Returns &amp; Portfolio Allocation” (2025): OLS/Ridge/Lasso/EN, ARCH/GARCH, hybrid model, portfolio allocation.

## Abstract
This project evaluates the Frequentist Regression approach (OLS, ridge regression, lasso and elastic net) and Volatility Modelling approach (ARCH/GARCH) for forecasting the monthly S&P 500 returns using 14 macro-financial indicators. This work will culminate in the production of a model which combines the two approaches in a hybrid model which out-performs every other model in both in-sample metrics and in the context of forecasting for portfolio allocation.

## Repo Structure

- R/
  - 1_data_construction.R
  - 2_frequentist_regression.R
  - 3_conditional_volatility_modelling.R
  - 4_hybrid_regression_garch_model.R
  - 5_portfolio_allocation.R
- data/
  - PredictorData2023/ (raw data set)
  - usable_data_set/ (usable dataset)
- paper/
  - Predicting_Financial_Asset_Returns_and_Portfolio_Allocation.pdf (full paper)
- README.md

