# Analyzing Delivery Lead Time Using Regressive Techniques

## Project Overview
This project aims to predict the **delivery time of food orders** based on various factors such as **weather conditions, road traffic density, vehicle type, and geographic coordinates** of the restaurant and delivery locations. The analysis challenges the general perception that delivery time mainly depends on traffic and identifies other influencing factors using regression techniques.

## Author
- **Naman Singh**

## Course Details
- **Course**: Time Series and Regression
- Semester 2

## Project Repository
GitHub Repository: **[Insert GitHub Link]**

## Goals
- Predict **food delivery time** based on multiple regressors.
- Optimize delivery operations and enhance customer satisfaction.
- Identify **key predictors** affecting delivery time beyond traffic conditions.

## Regressors Used
- **Delivery Person Age** (15-50 years)
- **Delivery Person Ratings** (1 to 5)
- **Weather Conditions** (Sunny, Stormy, Sandstorms, Cloudy, Fog, Windy)
- **Road Traffic Density** (High, Jam, Low, Medium)
- **Vehicle Condition** (Scale 0 to 3, Poor to Good)
- **Type of Order** (Snack, Drinks, Buffet, Meal)
- **Type of Vehicle** (Motorcycle, Scooter, Electric Scooter, Bicycle)
- **Multiple Deliveries** (0, 1, 2, 3)
- **Festival** (Yes/No)

## Data Cleaning Process
- **Convert Delivery Time** into numeric format.
- **Normalize Weather Conditions** by removing extra text.
- **Convert Dates** to standard date formats.
- **Add Day of the Week** variable.
- **Filter Out Invalid Data** (removing NaN values in categorical fields).
- **Categorize Variables** for machine learning compatibility.

## Regression Models
### 1. **Base Model**
- **Factors Considered**: All regressors
- **Results**:
  - **Multiple R-squared**: 0.5907
  - **Adjusted R-squared**: 0.5903
  - **F-statistic**: 1657 on 27 and 30998 DF
  - **p-value**: < 2.2e-16
  - **Mean Squared Error (MSE)**: 35.86135

### 2. **Backward Elimination Model**
- Performed stepwise elimination to refine the model.
- **Final Regressors**:
  - Delivery Person Age, Ratings, Weather, Traffic, Distance, Employee Efficiency, Vehicle Condition, Multiple Deliveries, Festival, City, Late Delivery Indicator
- **Results**:
  - **Multiple R-squared**: 0.8076
  - **Adjusted R-squared**: 0.8074
  - **F-statistic**: 4660 on 26 and 28863 DF
  - **p-value**: < 2.2e-16
  - **MSE**: 11.6725

## Outlier Detection & Removal
- **DFFITS Method**: Identified outliers
- **Base Model**: Removed **13,748 entries**, retaining **29,004 entries**.
- **Backward Model**:
  - Initial removal of **10,456 entries**, retaining **18,434 entries**.
  - **Final Results after Removing Outliers**:
    - **Multiple R-squared**: 0.8959
    - **Adjusted R-squared**: 0.8958
    - **F-statistic**: 6095 on 26 and 18,407 DF
    - **p-value**: < 2.2e-16
    - **MSE**: 6.14284

## Feature Engineering
- **Delivery Latency**: Binary variable indicating late deliveries (>30 min).
- **Employee Efficiency**: Categorized ratings into **Excellent, Good, Average, and Poor**.
- **Distance Calculation**: Used **Haversine Formula** to compute distances between restaurant and delivery locations.

## Future Work
- Improve model performance by incorporating additional **real-time traffic data**.
- Introduce **deep learning models** for enhanced predictive accuracy.
- Develop a **real-time prediction system** integrated into delivery applications.
- Extend the dataset to include broader geographical locations.

## References
- Research papers on **delivery optimization** and **traffic impact modeling**.
- Machine learning techniques in **logistics and time series forecasting**.

## License
This project is open-source and available under the [MIT License](LICENSE).

---
**For more details, visit the GitHub repository:** [Insert Link]
