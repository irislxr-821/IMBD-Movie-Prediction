## Description and Objective
IMDb, the internet movie database, has the world’s most authorized source of information about movies, TV shows, and video games, as well as other industry professionals,including directors, actors, and producers. This project aims to predict the IMDB scores for 12 upcoming movies in Novemeber 2022 using predictive modelling.

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
Our final regression model was selected using 25 predictor variables from a total of 39 variables (original dataset) to predict IMDb scores of 12 different movies, which will be released in November. The result showed that our model is statistically significant (p<.001, Multiple R-squared = .5472, Adjusted R-squared = .5241).
