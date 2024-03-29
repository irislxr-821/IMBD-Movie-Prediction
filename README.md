Tool: R
## Description and Objective
IMDb, the world's most reputable source for movie, TV show, and video game information, also covers industry professionals like directors, actors, and producers. This project seeks to use predictive modeling to forecast IMDb scores for 12 movies set to release in November 2022.

<p align="center">
<img src="./images/BOBA.png" width="70%" > 
</p>
<p align="center">
Our hand-drawn team logo
</p>

## Data Description
The dataset we are going to analyze contains historical data of more than 19,000 movies, including their information about language, country, actors, etc., in a total of 39 different variables. Then, based on this dataset, our goal is to build a model which can generate the most accurate prediction of the IMDb score of any given movie.

## Methodology
In general, we created 5 models before finding the final model. The difficulty lies in trying to maximize the adjusted R-squared while avoiding overfitting. Here are the steps: <br>
1. We started by creating dummy variables for categorical predictors and built Model 1, excluding movieTitle and movieID. <br>
2. We checked for nonlinearity by plotting residual plots, identifying non-linear variables like duration and nbFaces. Our overall model was deemed linear based on the Tukey Test (p-value > 0.05). <br>
3. Heteroskedasticity was detected through a residual plot and confirmed with ncvTest. We adjusted Model 1 to address this. <br>
4. After identifying 4 outliers, we removed them, improving Model 2's adjusted R-squared. <br>
5. We assessed collinearity with the variance inflation factor, finding no issues in Model 2. <br>
6. We addressed non-linear variables by testing polynomial and spline models, selecting a quadratic model for duration to create Model 3. <br>
7. For nbFaces, linear regression was chosen based on ANOVA test results. <br>
8. We optimized movieMeter_IMDBpro using cubic polynomial regression and considered spline regression, resulting in Model 4 with a higher adjusted R-squared. <br>
9. Quadratic polynomial regression was selected for releaseYear, leading to Model 5. <br>
10. In Model 6, we removed predictors with p-values over 0.7, achieving an adjusted R-squared of 0.5241. <br>

## Results
Our final regression model was selected using 25 predictor variables from a total of 39 variables (original dataset) to predict IMDb scores of 12 different movies, which will be released in November. The result showed that our model is statistically significant (p<0.001, Multiple R-squared=0.5472, Adjusted R-squared=0.5241).

## Files
Refer to [IMDB_Code.R](IMDB_Code.R) for code in R. <br>
Refer to [IMDB_Report.pdf](IMDB_Report.pdf) for a detailed explanation of the procedures.
