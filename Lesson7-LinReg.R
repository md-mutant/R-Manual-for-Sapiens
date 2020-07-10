########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#            Lesson 7 - Linear Regression              #
#                                                      #
########################################################

library(ggplot2)
library(PSYC201)

##############################
# 7.1 - Formulas             #
##############################

# Another type of data R can store is called a 'formula'
# This is a way of telling R how you expect data to relate in your statistical models
# For instance, let's go back to the memory experiment data
head(memory.exp)

# Now recall that one of the questions was whether the experimental group had higher scores than the control
# The way that we saw how to do this was to split the difference scores up
# based on which condition the subjects were in, then give both vectors to t.test()
cont.sc = subset(memory.exp,Group=='Control')$Score
exp.sc = subset(memory.exp,Group=='Experimental')$Score
t.test(cont.sc,exp.sc)

# However, we can reword this problem to say that the score should differ by experimental condition

# Or, in more common statistical terms, we have a dependent variable (Score)
# that varies based on an independent variable (Group)

# The way we tell R this is using formulas
# In formulas, we put the dependent variable on the left side
# This is followed by a tilde '~', which should be on the key to the left of '1'
# Finally, we put our independent variable on the right of the tilde
memory.exp$Score ~ memory.exp$Group

# Note that nothing was evaluated yet... this is a placeholder for our model
# But this is the way to tell R that we believe Score will vary by Group
# And if we have a dichotomous independent variable, we can give this formula to t.test()
t.test(memory.exp$Score ~ memory.exp$Group)

# And if we're working within the same data frame, we can tell t.test this using the data argument to make it easier:
t.test(Score ~ Group, data = memory.exp)

# Note that this is exactly the same as the original test we ran where we split the data into two vectors

# So we're just doing t.tests another way...
# Why should you care?

# First, when handed data in the form of the memory.exp data, this makes life easier
# You don't have to worry about subsetting and splitting vectors

# But more importantly, formulas are MUCH more general than dual vectors
# You can add multiple independent variables into a formula using '+' and other operators
# (we will learn about some of these later in the term)
# And when we start using the lm() command for linear models in just a moment,
# it only takes formulas, not strings of vectors

# For now, your big take-away should be that we are moving towards building models of the world
# And that the t-test is a test of a very constrained model:
# One that looks at whether the means of a variable in two categories will vary based on category
# But formulas are a way of specifying more complex and less constrained models

# Oh... and as we talk about forms of linear models (and this includes ANOVAs, logistic regression, etc.),
# you'll be using them over and over

##############################
# 7.2 - Linear regression    #
##############################

# Now we'll learn how to do linear regression with R

# To start with, let's look at the 'mtcars' dataset
head(mtcars)

# We might hypothesize that heavier cars get worse gas milage
# But can we prove it?
# We have the data - in mtcars, we have milage (mpg) and weight in 1000s of lbs (wt)

# Well first let's visually inspect the relationship:
qplot(wt,mpg, data= mtcars)

# This looks like it might have a negative linear relationship...
# And we can look at the correlation using cor()
# Recall - correlation is the covariance divided by the standard deviations of both variables
# It is a unit-free measure of the relationship between two variables
with(mtcars,cor(wt,mpg))

# Pretty significantly negative - there's evidence for a relationship
# So we need to fit a linear model for simple regression

# Recall that we want to fit the function:
# y = b0 + x*b1 + err

# And we know the equations for b0 (the intercept) and b1 (the slope)
# The slope is the covariance of the X and Y variables divided by the variance of X
mpg.slope = with(mtcars,cov(wt,mpg)/var(wt))
mpg.slope

# This is ~ -5.3
# Since wt is in 1000lb units
# This implies that an increase of 1000lbs will reduce a car's mpg by 5.3 on average

# And the intercept is calculated as the mean of Y minus the slope times the mean of X
# E.g. - tracing the prediction line through the means back to 0
mpg.int = mean(mtcars$mpg) - mpg.slope*mean(mtcars$wt)

# This intercept is 37.3
# Which implies that a weightless car would get 37.3 mpg
# Seems kind of silly to talk about a weightless car though...
# This touches on why trying to interpret intercepts doesn't always make sense
# Often having zero of a dependent variable just isn't possible in reality
# And therefore the intercept is just a theoretical concept

# Okay - so we have a slope and intercept
# But the first thing we want to ask: Is there statistical evidence for a relationship between the variables here?
# This is the same thing as asking: Is the slope different from 0?
# Because if the slope is 0, then a change in wt won't change our predictions about mpg at all

# To do this, we need to know how the slope would be distributed under the null hypothesis
# If we just randomly sampled numbers independently, we will often find a (small) relationship between them
# But this would just be noise - the generating process has no relationship between the two
# For instance:
xs = rnorm(20)
ys = rnorm(20)
cov(xs,ys)/var(xs)

rm(xs,ys)

# We can run this a few times and see that the calculated slope is not 0
# But it should be 0 in theory...
# Therefore, these slopes are just due to noise
# So how do we tease apart what is a real relationship and what is just noise?

# What we need to look at is how well does the regression equation predict Y versus the leftover noise
# Recall that the equation for linear regression is:
# y = b0 + x*b1 + err

# We can then define our best prediction for each variable as y.hat:
# y.hat = b0 + x*b1

# So: y = y.hat + err

# And we can then calculate y.hat
y.hat = mpg.int + mpg.slope*mtcars$wt
y.hat

# Which gives us our best prediction for the mpg of each car
# And the error is what's left over between the actual mpg and the prediction
err = mtcars$mpg - y.hat

# Now we need to define the error variance
# We can't just use var(err) though
# Instead, this is defined as sums of the squared error divided by (n-2)

# Why n-2?
# You can think of it as dividing by degrees of freedom
# You start with n degrees of freedom - one for each data point you have
# Whenever you calculate a statistic, you use up one of those degrees of freedom
# With prior calculations of variance, we would first need to calculate the mean, using up only 1 df
# But now we calculate both the slope and intercept, using up 2 dfs

# So we can calculate the error variance (also called Mean Square Error) as:
err.df = length(err) - 2
err.df

MSE = sum(err^2)/err.df
MSE

# Note that because the mean of the error is always 0, you don't need to explicitly subtract out the mean like normal variance calculations
# (If you mean error isn't 0 or within rounding error, you're doing something wrong)

# Then the variance of the slope under the null hypothesis is the MSE divided by the sums of squares of X's deviation from the mean
slope.var = MSE / sum((mtcars$wt-mean(mtcars$wt))^2)

# And so our best estimate for the standard deviation of the slope under the null is the square root of the variance
slope.sd = sqrt(slope.var)
slope.sd

# But now we have an estimate of the slope, and estimate of the standard deviation of the sampling distribution
#  and we want to know whether this is 0 or not

# This sounds like something we've done before: a t-test
# Recall - the t-statistic was the mean parameter estimate minus the null hypothesis divided by the standard error
# Where the standard error is the standard deviation of the sampling distribution of the mean of the parameter
# Here it's exactly the same idea - but the slope is our statistic

# So we can calculate a t-statistic easily:
mpg.t = mpg.slope / slope.sd
mpg.t

# Then we can ask - how far out on a t-distribution is this?
# Specifically - on a t-distribution with n-2 degrees of freedom
# And we have to multiply by 2 because we want to know whether the slope is different than 0 (two-tailed)
pt(-abs(mpg.t),err.df)*2

# Yep - pretty clearly different from 0

##############################
# 7.3 - Introduction to lm() #
##############################

# Okay - so we found a relationship between weight and mpg and the associated coefficients
# But that was a lot of work
# We had to calculate a lot of different things manually, and that adds the possibility of making errors
# (Think about how bad it was before computers when you'd have to do every one of the equations longhand...)
# Since regression is used so often, R has to have an easier way, right?

# Well... of course
# It's called lm() and it will become one of your best friends in this class

# So how do we use it?
# We want to measure our dependent variable (mpg) in terms of weight (wt)
# Or in R terms, we want to model: mpg ~ wt

# At its most basic, lm just takes a formula to model
# However, like t.test, you can give it the 'data' argument to tell it that we're working in a frame
# Let's try that out:
lm(mpg~wt, data = mtcars)

# That gives us some good information... we can gather the intercept and slope
# So we can easily draw the line with that information

# But this is statistics! We want to know more about this relationship!
# And besides, I wouldn't be talking up lm if that's all it did

# To get more out of our linear models, we first can store the output to a variable
mpg.lm = lm(mpg~wt,data=mtcars)

# Then the easiest thing to look at is the summary() of this variable
summary(mpg.lm)

# Notice how well this matches up with the numbers we calculated before
# First, the intercept and slope are matched up with the coefficient 'Estimate' numbers
mpg.int
mpg.slope

# Then we have all the numbers from the test for the slope next to 'wt'

# The standard error is the standard deviation of the slope estimate:
slope.sd

# And the t-value
mpg.t

# And then the p-value
pt(mpg.t,err.df)*2

# The tests for the intercept differing from 0 are typically less important, but they're there as well

# Notice that the residual standard error is just the square root of the MSE
# (This is just the relationship between the variance and standard deviation)
sqrt(MSE)

# Notice that the degrees of freedom next to the residual standard error is the n-2 we calculated:
err.df

# And finally, remember at the beginning how we calculated the correlation between the two variables?
with(mtcars,cor(wt,mpg))

# Well correlation is notated as 'r'... so if we want to square the value of r, we get R-squared
with(mtcars,cor(wt,mpg))^2

# This is the 'Multiple R-squared' just below
# We will discuss why it's 'Multiple' when we talk about multiple regression
# And all of the other statistics (adjusted R-square, F-tests) can be ignored until then too

# We will also want to pull some of these values out for homework or otherwise
# And since it's bad form to just copy the numbers we should do that with functions
# (Plus if you hand-copy it, you'll be introducing rounding errors)

# The first thing we might want to pull out is the slope and intercept
# We can do this with the coef() function
# This takes an lm() object and returns a vector with all of the coefficients
coef(mpg.lm)

# We also might want to get out the y.hat values or the errors on each point
# We can use fitted.values() for y.hat
fitted.values(mpg.lm)

# And resid() for the errors
resid(mpg.lm)

# Or we can get the degrees of freedom
df.residual(mpg.lm)

# Then of course, we might want the p-value of this test as well
# The get.p() function in the PSYC201 package will do this for you, and returns a vector with a p for each coefficient
get.p(mpg.lm)

##############################
# 7.4 - Removing intercepts  #
##############################

# What would happen if we wanted the intercept to be 0 for certain?
# This is not often used in Psychology, but in many other disciplines, this is important
# For instance, in Physics when an object is at 0 degrees, it should have no energy
# Because this is a basic fact, we don't need to estimate the intercept and can just set it to zero

# The intercept is always designated by '1' in formulas
# So if you wanted to take the intercept out, you would type:
# response ~ predictor - 1

# To see an example of this, let's go to the cars dataset
# (Note this is different than mtcars)
# cars has the distance in feet to stop after going a certain speed in mph
# Well, for 1920's cars...
cars

# So let's look at the relationship between these two variables:
qplot(speed,dist, data = cars)

# That seems to suggest a linear relationship between speed and stopping distance
# So we can build a linear model with this
car.lm.int = lm(dist ~ speed,data=cars)
summary(car.lm.int)

# But note that this has an intercept of -17.6
# Which would suggest that if you were going a 0mph,
# you would stop 17.6 feet behind where you were
# Hmm...

# For this, we have decent theoretical motivation to set the intercept to 0
# If you aren't moving, you will stop in 0 feet
# So we can rewrite the formula to account for this:
car.lm.noint = lm(dist ~ speed - 1, data=cars)
summary(car.lm.noint)

# Note that if you compare the residual standard error, the model with the intercept
# will always have a lower residual error than the model without
# (You can always fit slightly better with extra parameters)

# But also note that you don't always want to take out the intercept even if you know that it should go through (0,0)
# Sometimes you can find an (approximately) linear relationship between two variables with one range of xs
# But as you go outside that range, the relationship is non-linear
# In that case, by setting the intercept to 0, you're going to find an inappropriate slope
# So taking out the intercept should always be done with care, and typically only when you have x values that get near 0

rm(car.lm.int,car.lm.noint)

##############################
# 7.5 - Prediction           #
##############################

# With linear regressions, one of the important things we can do
# is predict the response variable from values of the predictor variable
# that you haven't observed

# Lets go back to the cars data trying to predict stopping distance based on speed
car.lm = lm(dist~speed,data=cars)
summary(car.lm)

# Let's say I'm driving a 1920's car at 20mph, and want to know how far it will take to stop
# Well we can look at cars
cars

# We notice that observations 39-43 are going at 20mpg
# So we can use fitted.values() and see the y.hat for those observations
# After all - that's our best prediction
fitted.values(car.lm)

# About 61 feet

# But now if I'm driving at 21mph, how far will I take to stop?
# We never observed the stopping distance at 21mph...

# The simplest way to do this is to plug 21 into the linear equation
# We can get the coefficients of the model out with the coef() function
coef(car.lm)

b0 = coef(car.lm)[1]
b1 = coef(car.lm)[2]

# Then y.hat = b0 + b1*x
b0 + b1*21

# But of course, why do math when R will do it for you?
# (And when we get into more complex models, the equations just get longer...)
# For this we have the predict() function

# This function, at its core, just takes an lm object
# It will then return the same as fitted.values()
predict(car.lm)

# But you can add lots of other arguments to extend functionality
# The first argument is 'newdata'
# This takes a data frame, where one of the collumns is names the same as the predictor
# The reason this requires a data frame will become clear when we do multiple regression...
# Because we'll need multiple predictors from a single data observation

# In this case, though, this is easy
# Say we want to predict the best estimate stopping distance for 15.5, 21, and 27mph
pred.xs = data.frame('speed' = c(15.5,21,27))
predict(car.lm,newdata = pred.xs)

# But of course, there's more to prediction than that
# We know that our estimate of y.hat is only that - an estimate
# But what if we want a confidence interval around that prediction?

# Let's say I'm traveling at 21mph
#  and want to know with 95% confidence how far I will go before I stop this one time
# Now there are two sources of variability we have to contend with:
#  1) Our slopes aren't precisely correct
#  2) There's variability that the model doesn't capture

# So even if I were able to predict perfectly that on average, I would stop in 65ft at 21mph
# If I stopped at 21mph, each time would be a bit different
# We can get this range by looking at the 'prediction' interval
predict(car.lm,newdata=pred.xs, interval = 'prediction')

# Here, this gives us a 95% CI of what we would expect our observations to be
# Including both the error in estimation and natural variability

# If we instead wanted a 99% CI, we can tell R to predict a different level:
predict(car.lm,newdata=pred.xs, interval='prediction',level=.99)

# Note that these can be fairly wide - there are a lot of factors influencing stopping distance
# But with this prediction, we know that we need to worry about the unaccounted variability when encountering a new observation
