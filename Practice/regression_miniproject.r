# https://study.sagepub.com/stinerock/student-resources/exercises/chapter-12-simple-linear-regression

#1. Import the Cars93 data into the object named E12_1. What are the variable names? 
#How many observations are included? Find (1) the minimum and maximum values,
#(2) the median and mean, (3) the first and third quartiles, and 
#(4) the standard deviation of the two variables, MPG.city and EngineSize. 
#Comment on your initial findings.

library(MASS)

E12_1 <- Cars93
names(E12_1)
dim(E12_1)
summary(E12_1)
sd(E12_1$MPG.city)
sd(E12_1$EngineSize)

#2. Do MPG.city and EngineSize appear related in any systematic way? Comment.

library(ggplot2)

ggplot(E12_1, aes(y=MPG.city, x=EngineSize)) +
  geom_point() +
  labs(y="Miles per gallon by city", x="Engine size (liters)", title="Relationship Between City MPG and Engine Size (liters)")
        
# Yes, as EngineSize decreases, MPG.city seems to increase

#3. Make and inspect a residual plot. Does the pattern of points 
#reveal anything that might cause us to question the assumptions 
#underlying the appropriate usage of regression analysis to 
#explore the relationship between MPG.city and EngineSize?

ES_MPG_lm <- lm(MPG.city~EngineSize, data=E12_1)
summary(ES_MPG_lm)

resid(ES_MPG_lm)
ES_MPG_lm$residuals # Same as above

ggplot(data=E12_1 ,aes(x=EngineSize, y=ES_MPG_lm$residuals)) +
  geom_point() +
  labs(x="Engine Size (liters)", y="Residuals", title="Residuals Against the Independent Variable") +
  geom_hline(yintercept=0, col="red")

# The plot shows that the residuals do not follow the regression assumptions for
# too high or too low values of the engine size (the residual is far from 0 and
#moves away from the pattern)

#4. There are several possible methods for managing the problem of 
#nonlinear relationships among variables, such as what we have 
#encountered in this case. One of the approaches involves transforming 
#the variables-by way of logarithms, exponents, etc.-in such a way 
#that they are forced to be more linearly related. (This class of 
#methods, sometimes referred to as GLM or general linear model, is 
#not covered in this book.) Another procedure requires including 
#additional variables into the multiple regression model (the focus 
#of Chapter 13). Instead, the approach we employ here involves 
#subsetting the data according to some specification-such as, subset-
#ting the data in a way that includes, for example, only vehicles 
#manufactured in the US or all vehicles that have smaller engines 
#(i.e., fewer liters of displacement). The expectation (or hope) is 
#that, by subsetting, the resulting data may meet the assumptions 
#behind the appropriate application of regression analysis. As a 
#first step, subset the data stored in object E12_1 in a way that 
#excludes all vehicles with EngineSize greater than the median. 
#(See Comment 4 under the first exercise above.) Name this new object 
#E12_2. Check to make sure that E12_2 conforms to this requirement. 
#How many observations remain in the new object? List the first 3 
#observations; list the last 3 observations. If necessary, review 
#the third point of the Chapter 2 Appendix, "Can We Extract A Data 
#Subset From A Larger Data Set?"

library(dplyr)

median(E12_1$EngineSize)
E12_2 <- filter(E12_1, EngineSize <= median(EngineSize))

# Check if E12_2 are not higher than the median EngineSize of E12_1
ggplot(data=E12_2, aes(x=seq_along(EngineSize),y=EngineSize)) +
  geom_point() +
  geom_hline(yintercept=median(E12_1$EngineSize), col="red") +
  ylim(0.5, 3) +
  labs(x="Index", y="Engine Size (liters)", title="Filtered Engine Sizes and E12_1 Median (in red)")

dim(E12_2) # 49 obs. remain

# First 3
E12_2[1:3,] # OR
head(E12_2,3)
# Last 3
E12_2[47:49,] # OR
tail(E12_2, 3)

# 5. For E12_2, do MPG.city and EngineSize appear related in a systematic way?

ggplot(E12_2, aes(x=MPG.city, y=EngineSize)) +
  geom_point() +
  labs(x="City miles per gallon", y="Engine size (liters)", title="Relationship Between City MPG and Engine Size (liters) (E12_2)")

#6. Make and inspect a residual plot. Does the pattern of points appear 
#more linearly arranged than they did in the earlier exercise when the 
#data included vehicles with large engines as well as those with a smaller one?

ES_MPG_lm2 <- lm(MPG.city~EngineSize, data=E12_2)
summary(ES_MPG_lm2)

resid(ES_MPG_lm2)
ES_MPG_lm2$residuals # Same as above

ggplot(data=E12_2,aes(x=EngineSize, y=ES_MPG_lm2$residuals)) +
  geom_point() +
  labs(x="Engine Size (liters)", y="Residuals", title="Residuals Against the Independent Variable (E12_2)") +
  geom_hline(yintercept=0, col="red")

# Apart from early outliers, this residual plot seem to show that the filtered data
# complies better with our regression assumptions, since values are closer to 0
# and seem to have a pattern

#7. As part of making the residual plot in the preceding exercise,
#we used the lm() function to create the model object slr2. 
#This is an important step in residual analysis because the model 
#object (slr2) includes all the important information associated 
#with the particular regression problem at hand, including the 
#estimated regression equation itself. What is the estimated regression equation?

ES_MPG_lm2 # MPG.city = 46.15 - 10.87EngineSize

# 8. Find the 95 and 99 percent confidence interval estimates of the 
#regression coefficient b1. Describe what these con dence intervals mean.

confint(ES_MPG_lm2, level=0.95) # The interval has 95% chance of containing the correct bi
confint(ES_MPG_lm2, level=0.99) # The interval has 99% chance of containing the correct bi

# 9. What does the estimated regression equation tell us?

# It tells us that for an increase of 1 liter in engine size, we should expect
# the city MPG to decrease by 10.87. The intercept does not mean anything to us
# in this particular case because a car can't drive with no engines

# 10. What is the strength of association between the two variables, 
#MPG.city and Engine Size? Find the coefficient of determination r2 
#using the following expression for r2 (do not use the summary() 
#function to unpack the regression statistics; we will use it later). 
#This exercise provides another opportunity to hone your coding skills. 

summary(ES_MPG_lm2) #r2 = 0.5051

# Manually
# Find the total sum of squares, ss_y.
ss_y <- sum((E12_2$MPG.city - mean(E12_2$MPG.city)) ^ 2)
# Find the residual sum of squares, ss_res.
ss_res <- sum((resid(ES_MPG_lm2)) ^ 2)
# Find the coefficient of determination.
(ss_y - ss_res) / ss_y

# 11. What does the coefficient of determination r2 reveal about 
#the regression model?

# The r2 of .5051 means that the independent variable (EngineSize) can explain
#50.51% of the variation in the dependent variable (MPG.city). In other words,
#the r2 represents the explanatory power of the independent variable on the
#dependent variable.

# 12. What is the t value of the coefficient b1 on the independent 
#variable EngineSize?

summary(ES_MPG_lm2) # t-value = -6.927

# 13. What is the p-value of t = ???6.926538?

summary(ES_MPG_lm2) # 1.056e-08

# 15. Use the regression equation to find the predicted values of MPG.city 
#for the following values of EngineSize (liters of displacement): 
#1.25, 1.50, 1.75, 2.00, 2.25.

# Regression equation => MPG.city = 46.15 - 10.87EngineSize

size_new <- data.frame(EngineSize <- c(1.25, 1.50, 1.75, 2.00, 2.25))

predict(ES_MPG_lm2, size_new)

## OR

reg_eq <- function(EngineSize){
  MPG <- 46.15 - 10.87*EngineSize
  print(MPG)
}

# 1.25
reg_eq(1.25) # 32.5625
# 1.50
reg_eq(1.50) # 29.845
#1.75
reg_eq(1.75) # 27.1275
#2
reg_eq(2) #24.41
#2.25
reg_eq(2.25) #21.6925

# 16. What are the predicted values of MPG.city that were used to 
#calibrate the estimated regression equation y = 46.15-10.87x? 
#Import those predicted values into an object named mileage predicted 
#and list the first and last three elements.

mileage_predicted <- fitted(ES_MPG_lm2)
head(mileage_predicted,3)
tail(mileage_predicted,3)

# 17. Add the mileage predicted object (created in the preceding exercise) 
#to E12_2, and name the resulting object E12_3. List the first and last 
#four elements. Find the correlation of the actual and predicted variables;
#that is, the correlation of MPG.city and mileage predicted. Once you have 
#calculated the correlation, square it (i.e., raise it to the second power). 
#Does the squared correlation coefficient look familiar?

# Create new data frame with added column
E12_3 <- mutate(E12_2, mileage_predicted = mileage_predicted)
head(E12_3,4)
tail(E12_3,4)

cor(E12_3$MPG.city,E12_3$mileage_predicted)
cor(E12_3$MPG.city,E12_3$mileage_predicted)^2
# The correlation squared equals the original r2 value

# 18. Create a scatterplot with MPG.city on the vertical axis, 
#Engine Size on the horizontal axis. Add labels to both axes as 
#well as a main title. Finally, using the abline() function, add 
#a regression line to the scatterplot.

ggplot(E12_3, aes(x=EngineSize,y=MPG.city)) +
  geom_point() +
  geom_line(color='red', aes(x=EngineSize,y=mileage_predicted)) +
  labs(x="Engine Size (liters)", y="City MPG", title="Fit of Regression Line")

# 19. For additional practice structuring our data before analyzing it, 
#we now subset E12_1 by Origin. For this exercise: (1) create a new 
#object from the original data set, E12_1, that includes only vehicles 
#of non-USA origin (thus excluding all vehicles of USA origin) and name 
#it E12_4; (2) find the median EngineSize of non-USA vehicles, and 
#(3) create a new object, named E12_5, that includes only (a) those 
#vehicles having EngineSize less than or equal to the median and only 
#(b) the two variables, MPG.city and EngineSize. In other words, 
#subset the original data set, E12_1, to include only the two variables, 
#MPG.city and EngineSize, and only those vehicles that are of non-USA 
#origin and that feature engines with displacement (in liters) at or 
#below the median for the relevant category. Just to make sure E12_5 
#"looks" as it should, run a few of the same functions that were used 
#in Exercise 1.

# == non-USA
E12_4 <- filter(E12_1, Origin == "non-USA")

median_ES4 <- median(E12_4$EngineSize) #2.2

E12_5 <- filter(E12_4, EngineSize <= median_ES4)
max(E12_5$EngineSize) #2.2

# 20. For the category of vehicles of non-USA origin, do the two variables,
#MPG.city and EngineSize, seem to be related in a systematic way? If so, how?

ggplot(E12_5, aes(x=EngineSize,y=MPG.city)) +
  geom_point()
# They seem to have an inverse relationship with some outliers

# 21. Make and inspect a residual plot. Does the pattern of 
#points reveal any reason why we should not use regression to 
#analyze these data? Are there any radical departures from the 
#assumptions underlying the appropriate usage of this methodology?

E12_5_lm3 <- lm(MPG.city~EngineSize,data = E12_5)

ggplot(E12_5, aes(x=EngineSize,y=E12_5_lm3$residuals)) +
  geom_point() +
  geom_hline(yintercept=0, color="red")
# There seem to be some outliers which depart from our assumptions
#The 3 outliers all have an EngineSize of less than 1.5. For
#vehicles with EngineSize higher than 1.5, the residuals seem to
#be constant. Therefore, the violations are not radical and
# we don't have a reason to drop our model

# 22. What is the estimated regression equation?

E12_5_lm3 # MPG.city = 51.27 - 13.89EngineSize

# 23. Find the 75 and 90 percent confidence interval estimates of 
#the regression coefficient b1. How should we interpret the 
#meaning of these confidence interval estimates?

confint(E12_5_lm3, level=0.75) # The confidence has 75% chance of containing the correct bi
confint(E12_5_lm3, level=0.9) # The confidence has 90% chance of containing the correct bi

# 24. What does the estimated regression equation tell us?

# That with an increase of 1 liters in EngineSize, MPG.city
#tends to decrease by 13.89.

# 25. What is the strength of association between the two 
#variables, MPG.city and Engine Size? Find the coefficient 
#of determination r2

summary(E12_5_lm3) # r2 = 0.455

# 26. What does the coefficient of determination r2 tell us 
#about the regression model?

# The r2 tells us that 45.5% of the change in MPG.city is 
#explained by the independent variable EngineSize

# 27. What is the t value of the coefficient b1 on the independent
#variable EngineSize? 

summary(E12_5_lm3) # t-value = -4.286

# 28. What is the p-value of t = -4.286051?

summary(E12_5_lm3) # p-value = 3e-04

# 30. Use the estimated regression equation to nd the predicted 
#values of MPG.city for the following values of EngineSize 
#(liters of displacement): 1.25, 1.50, 1.75, 2.00, 2.25.

# Regression equation => MPG.city = 51.27 - 13.89EngineSize

size_new <- data.frame(EngineSize <- c(1.25, 1.50, 1.75, 2.00, 2.25))

predict(E12_5_lm3, size_new)

#### OR

reg_eq2 <- function(EngineSize){
  MPG <- 51.27 - 13.89*EngineSize
  print(MPG)
}

# 1.25
reg_eq2(1.25) # 33.9075
# 1.50
reg_eq2(1.50) # 30.435
#1.75
reg_eq2(1.75) # 26.9625
#2
reg_eq2(2) #23.49
#2.25
reg_eq2(2.25) #20.0175

# 31. What are the predicted values of MPG.city that were used to 
#calibrate the estimated regression equation  y= 51.274-13.892x? 
#Import those predicted values into an object named mileage 
#predicted and list the first and last three elements.

mileage_predicted2 <- fitted(E12_5_lm3)

# 32. Add the mileage predicted object (created in the preceding 
#exercise) to E12_5, and name the resulting object E12_6. List 
#the first and last four elements. Find the correlation of the 
#actual and predicted variables; that is, the correlation of 
#MPG.city and mileage predicted. Once you have the correlation, 
#square it (i.e., raise it to the second power). Comment on the 
#square of the correlation. What is it?

E12_6 <- mutate(E12_5, mileage_predicted = mileage_predicted2)
head(E12_6,4)
tail(E12_6,4)

cor(E12_6$MPG.city, E12_6$mileage_predicted)
cor(E12_6$MPG.city, E12_6$mileage_predicted)^2
# This corresponds to the r2 of our linear model, meaning the
#explanatory power of the independent variable in the dependent
#variable

# 33. Create a scatterplot with MPG.city on the vertical axis, 
#Engine Size on the horizontal axis. Add labels to both axes as 
#well as a main title; set blue as the color of the points. 
#Finally, using the abline() function, add a regression line to 
#the scatterplot.

ggplot(E12_6, aes(x=EngineSize, y=MPG.city)) +
  geom_point(col="blue") +
  geom_line(aes(x=EngineSize, y=mileage_predicted)) +
  labs(x="Engine Size (liters)", y="City miles per gallon", title="Results VS Regression Line")
  




