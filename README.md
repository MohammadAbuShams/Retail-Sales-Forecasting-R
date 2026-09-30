# Retail Sales Time Series Forecasting in R

This project analyzes monthly retail sales data from January 2022 to December 2025 and compares several time series forecasting methods using R.

## Project Objective

The objective is to compare different forecasting methods and identify the most accurate method using forecast error measures.

## Forecasting Methods

The project includes:

- Naive Method
- Moving Average
- Weighted Moving Average
- Simple Exponential Smoothing
- Trend-Adjusted Exponential Smoothing (FIT)
- Trend Projection
- Seasonality Analysis
- Combined Trend and Seasonal Forecast

## Model Comparison

The main forecasting methods were compared using:

- Mean Absolute Deviation (MAD)
- Mean Squared Error (MSE)
- Mean Absolute Percentage Error (MAPE)

MAD was used as the main criterion for selecting the preferred forecasting method.

## Main Result

Trend-Adjusted Exponential Smoothing (FIT) with:

- Alpha = 0.10
- Beta = 0.10

produced the lowest MAD among the methods included in the final comparison.

The 12-month Moving Average produced the lowest MSE.

## Seasonality

The data showed a clear seasonal pattern.

Sales were generally lower at the beginning of the year, especially in January and February, while December had the highest seasonal index.

A Combined Trend and Seasonal Forecast was also developed to capture both the long-term trend and monthly seasonal effects.

## Tools

- R
- RStudio
- Microsoft Excel

R was used for data preparation, forecasting calculations, accuracy evaluation, seasonality analysis, and visualization.

## Data Source

U.S. Census Bureau via FRED, Federal Reserve Bank of St. Louis.

Retail Sales: Retail Trade  
https://fred.stlouisfed.org/series/MRTSSM44000USN

## Author

Mohammad Abu Shams

MBA – Operations Management  
Birzeit University
