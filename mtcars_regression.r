# https://www.youtube.com/watch?v=_ymR-FFG44c

data(mtcars)
mtcars

plot(mtcars$mpg, mtcars$wt)

# Data partition

P1 = runif(nrow(mtcars)) # creates a random number for every data point in mtcars
P2 = order(P1) # stores the index of each corresponding number from P1 depending on how big it is

training_ds = mtcars[P2[1:24],] # mtcars has 32 data points, we are selecting 24 random points from mtcars

test_ds = mtcars[P2[25:32],] # store remaining values from mtcars not in training data


## Model Building

mtcars_Regression = lm(mpg ~ wt, data = training_ds)

abline(mtcars_Regression)

summary(mtcars_Regression)

plot(mtcars_Regression)

pred_values = predict(mtcars_Regression, newdata = test_ds)

test_ds$pred_mpg = pred_values # create new column with predicted values based on test data (on the test data data frame)

# Multiple regression

mtcars_MultRegression = lm(mpg ~ wt + disp, data = training_ds)

summary(mtcars_MultRegression)
