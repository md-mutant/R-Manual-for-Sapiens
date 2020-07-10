########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#           Lesson 8 - Multiple regression             #
#                                                      #
########################################################

library(PSYC201)

##############################
# 8.1 - Multiple regression  #
##############################

# Setting up a multiple regression in R is about as easy as a single regression
# Let's start with a toy problem:
# We may want to predict what we can expect a car to get in miles per gallon,
# based on the engine displacement, horsepower, and weight of the car
# This information is all contained in mtcars
head(mtcars)

# To add more regressors, in a formula, all you need is a '+'
# So the formula would be written as:
mpg ~ disp + hp + wt

# And so our linear model is:
cars.lm = lm(mpg ~ disp + hp + wt, data = mtcars)

# Then we can look at it with summary()
summary(cars.lm)

# Note that this looks very similar to the single regressor model
# A couple things to note:

# 1)
# You can find the omnibus F-test at the bottom of the summary
# This is the F-test that the ENTIRE model is better than just using the mean
# You can also find the Multiple R-squared and Adj R-squared above that

# 2)
# Each of the coefficients has a p-value attached to it
# This is the p-value that each slope is different from 0
# But recall, each slope is the change when everything else is already in the model
# So this is the p-value associated with the null hypothesis that a slope
# of a predictor variable is zero given that all other predictors are included

# This means that these p-values can change as you add/take away predictors
# For instance, excluding weight from the model:
cars.lm.nowt = lm(mpg~disp+hp,data=mtcars)
summary(cars.lm.nowt)

# Notice now that displacement has become significant
# But horsepower has gone from significance to near-significance

# We'll get back to this point during model comparison

# Once you have a multiple regression, you can do everything you did with single-variable
# Pull out the coefficients (though there will be more of them):
coef(cars.lm)

# Get the residuals
resid(cars.lm)

# Prediction becomes slightly more complex (necessarily, though)
# Remember how you needed a data frame for single-variable regression
# And it seemed ridiculously complex?
# Well the data frame is needed to keep predictors straight in multiple regression
# So we need to specify each x-value for the observation we want to predict

# For instance, what if we want to predict the mpg of a car with an engine 
# displacement of 200, horsepower of 180, and weight of 3000 pounds?
# Oh, and one with disp = 120, hp = 80, weight = 2000 pounds?
pred.xs = data.frame(disp=c(200,120),hp=c(180,80),wt=c(3,2))
pred.xs

# But once you have these predicted values, the predict() function is the same
# For point predictors
predict(cars.lm, newdata = pred.xs)

# Note that this is the same as adding up the predictors times the xs:
coefs = coef(cars.lm)
coefs[1] + coefs[2]*pred.xs$disp + coefs[3]*pred.xs$hp + coefs[4]*pred.xs$wt

# For and if we wanted confidence intervals on a new observation:
predict(cars.lm, newdata = pred.xs, interval = 'prediction')

# Finally, you can pull out some of the statistics for this lm, but they come out slightly different
# For instance, get.p provides a separate p-value for each coefficient (just as it did for the intercept and single slope with one regressor)
get.p(cars.lm)

# And you can get each slope's t-statistic using the get.stat function
get.stat(cars.lm)

##############################
# 8.2 - Comparing models     #
##############################

# Now we're going to introduce a new function: the anova() function
# This looks at the various predictors in the model, and does stepwise model comparisons
# So recall the summary of cars.lm:
summary(cars.lm)

# We can then call anova() on this linear model
anova(cars.lm)

# Note that you get different p-values for two of the three predictors...
# Why?

# Recall that summary() looks at the p-value that a slope isn't zero...
# given that all of the other predictors are already in the model

# anova() builds the model in a stepwise factor
# So the p-value associated with disp is the p-value of just testing
# whether a single linear regression using disp is statistically valid
# (e.g., comparing the model with one predictor to just the mean)

# Then the p-value associated with hp is testing whether the model that
# has both disp and hp is better than predicting with just disp

# And finally, the p-value associated with wt is testing whether the model
# with all three factors is better than the model with disp and hp

# So note that only wt should have the same value in summary() and anova()
# Because it's the only one that in both cases is comparing the model with
# everything to the model excluding only that value

# Also notice that for the anova() function, the order you put in variables matters
# Let's say we put them in the other way around:
cars.lm.2 = lm(mpg~wt+hp+disp,data=mtcars)

# Summary will be the same:
summary(cars.lm.2)

# But anova won't:
anova(cars.lm.2)

# Now you can use get.p and get.stat on anova objects too
# First you must store it to a variable
cars.anova = anova(cars.lm)

# Then get.p gives you the p-values for each factor - but note these are the stepwise p-values!
get.p(cars.anova)

# And get.stat provides you with the F-statistics for each factor (again stepwise)
get.stat(cars.anova)

# These functions will come more in handy when we get to real ANOVAs

# So why use anova()?
# Two reasons:
# 1) It better summarizes predictor factors, but we won't see that until we get to real ANOVAs
# 2) You can also use it for model comparison

# Remember the cars.lm.nowt model from above?
# Well, we can easily do a model comparison between the full model and this reduced model
# Just put give the two models to the anova() function:
anova(cars.lm.nowt,cars.lm)

# How do we interpret this output?
# Note that there are two output lines, one for Model 1, and one for Model 2
# Above it tells you which model is which
# There aren't going to be any significance statistics for Model 1 - you are comparing to that model
# Let's look at the first two columns though:
# Res.Df is the count of residual (error) degrees of freedom in each model
# RSS is the Residual Sums of Squares (we note as SSE) in each model
# The following two columns look at the change in these values
# Because there is one extra parameter in Model 2, there is one less Res.Df...
# Therefore the change in degrees of freedom from Model 1 to Model 2 is one
# Likewise, Sum of Sq is the difference in the SSE from one model to another
# Put differently, it is the additional amount of variance Model 2 explains over Model 1
# Finally, they work out the F-statistics and associated p-value, where:
# F = (Sum of Sq / Df) / (RSS / Res.Df)
# p = 1-pf(F,Df,Res.Df)

# Note that this test is perfectly equivalent to the slope test from summary on wt:
summary(cars.lm)

# You can also have a difference of more than one parameter
# Let's say we wanted to compare the full model to a model using just weight
cars.lm.wtonly = lm(mpg~wt, data = mtcars)
summary(cars.lm.wtonly)
anova(cars.lm.wtonly,cars.lm)

# You can see the exact same output, only now you have a 2-DF change

# A couple notes about the anova() function:
# 1) You should order the models from smallest to largest # of parameters...
# Otherwise you get negative degrees of freedom and negative SS
# (R does work this out for you, but it's awkward and not theoretically sounds)
anova(cars.lm,cars.lm.nowt)

# 2) You should only use nested models for this function
# Nested models are models in which one is a subset of the other
# So for instance, the model with disp & hp is a subset of the model with disp, hp & wt
# And the model with just wt is a subset of the full model as well
# But the model with just wt is NOT a subset of the model with disp & hp
# So why don't we use anova() for non-nested models?
# Well... because it's theoretically incorrect and gives wonky results:
anova(cars.lm.wtonly,cars.lm.nowt)

# Note that here we have a *decrease* in sum squares regression with an extra parameter
# That isn't possible with nested models, so you know it's wrong

# There are other methods for comparing non-nested models (eg, Adj-RSquared AIC, BIC)
# Adj-RSquared can be found in the summary
# And we will discuss AIC and BIC in 201b

# And finally, you can pull out the p-value and F-statistics for model comparisons as well
# Again you must store this to a variable
comp.anova = anova(cars.lm.wtonly,cars.lm)

# Then you can get the p-value and F-statistic of the model comparison using the PSYC201 functions
get.p(comp.anova)
get.stat(comp.anova)

# Don't mind the '2' name = that's just because it's comparing the second model to the first

rm(cars.lm.2,cars.lm.nowt,cars.lm.wtonly,coefs,pred.xs, cars.anova, comp.anova)

##############################
# 8.3 - Diagnostics          #
##############################

# Linear regression rests on some assumptions about the error and model formation
# If these assumptions are violated, we could be fitting our model incorrectly
# Let's go back to our single-variable regression model from last lesson
car.lm = lm(dist~speed,data=cars)
b0 = coef(car.lm)[1]
b1 = coef(car.lm)[2]

# Obviously, the first thing we do (often before the lm) is plot the predicted value against the predictors
# We can then see if there's any evidence of a linear relationship
qplot(speed,dist,data = cars) + geom_abline(intercept=b0,slope=b1)

# The three big things we want to check are:
#  1) Is error not normally distributed?
#  2) Is error correlated with any of the terms in the model?
#  3) Are there any outliers?

# If you answer yes to any of these questions, you'll want to to rethink your model fitting
# So you should always know your data well enough to answer these questions

# There is a function in PSYC201 that makes this easy: diagnostic.plot()
# This is a function that makes commonly used plots for diagnostic tests easy to do and look somewhat nice
# It takes an lm object and the type of diagnostic plot you want to do
# (Note that this uses the default R plots, so are good for checking, but not as good for publishing)

# So we'll go through the diagnostic checks one at a time:

########################################
# 1) Is error not normally distributed?

# One of the key assumptions of linear models is that e ~ N(0,sigma)
# There are statistical tests for this, but they tend to be useless
# Either you have too little data to find significance with these tests
# Or you have so much data that the tests pick up even small deviations from normality that won't skew results
# So instead the easiest way to test this is to look at it and make sure you eyeball it as normal

# There are two easy ways to visualize this
# First, we can look at the histogram of the residual errors to quickly eyeball it
# For this, we use the 'residuals' argument for diagnostic.plot
diagnostic.plot(car.lm,type = 'residuals')

# Note that this is a slightly prettier way of writing:
# hist(resid(car.lm))

# Hmm... looks like there is some skew
# So we can look at it in more detail using a qqplot
# This is the argument 'qq' in diagnostic plot
diagnostic.plot(car.lm,type = 'qq')

# This is just a wrapper for writing:
# plot(car.lm,which=2)
# Which gives you a default qq plot with some additional annotation

# So there might be some worry about normality given the three points with the highest error...
# But before we give up on this linear model, we should note that linear models are fairly robust to slight deviations
# As long as your distribution is roughly normal, you should be fine

# We'll discuss at the end why this appears to be skewed
# But for now we can continue on

############################################################
# 2) Is error correlated with any of the terms in the model?

# Errors should be randomly distributed, and should not depend on the predictors or predicted value
# Nor should they be correlated with order
# (This might be an indication of subject getting worse over the term or RAs getting lazy as the experiment goes on)

# Again, there are statistical tests for this (eg. Brown Forsythe), but like with normality, they aren't great
# If it's bad enough to affect the results of linear models, you should be able to see diagnostic plots and say 'whoa!'

# To test for a relationship with the predicted value, we give the argument 'fitted' to diagnostic.plot
diagnostic.plot(car.lm,type='fitted')

# This is the same as:
# plot(car.lm,which = 1)
# Which just plots the fitted.value() output against the resid() output (again, with annotation)

# Here what you want to make sure is that the line stays straight around 0
# You should see that there may be a bit more variance towards the middle than at the ends...
# But it looks pretty good...

# Then we want to plot this versus each of the predictor variables
# This isn't as important with single-variable regression (since by definition it will look the same as vs. fitted)
# But is key in multiple regression
# Again we can do this with diagnostic.plot giving the argument 'predictor'
diagnostic.plot(car.lm,type='predictor')

# If there are multiple predictors (as in multiple regression) this will make a separate plot for every one
# Or we can call out which predictor we want specifically
diagnostic.plot(car.lm,type='predictor',predictor='speed')

# This is the same as:
# plot(cars$speed,resid(car.lm))
# But again prettier, and you can see the line

# Often it is also helpful to plot the absolute value of the residuals against the predictors
# This makes it easy to see heteroscedastic errors - where error is smaller in some parts than others
# We can do this with the 'abs predictor' argument
diagnostic.plot(car.lm,type = 'abs predictor')

# And finally, we want to review order effects
# We can do this with the 'time' argument
diagnostic.plot(car.lm,type = 'time')

# This is the same as:
# plot(1:50,resid(car.lm))

# It looks like theres something here, but that's because this data set was sorted by dist
# Order thus isn't important in this data set

########################################
# 3) Are there any outliers?

# The first thing to do when searching for outliers is to look at your data directly
# Are any of your data points really far from the rest of them?
# Boxplots or histograms are usually good choices for this

# This graph isn't in diagnostic.plot because we're not even looking at the lm() object
# Just the raw data
qplot(0,y=cars$speed,geom='boxplot')
qplot(0,y=cars$dist,geom='boxplot')

# There's one data point in distance a bit far out - but not so far that we'd be worried
# Plus it comes with one of the highest speeds, as we can see from the original plot of dist vs. speed
with(cars,plot(speed,dist))

# So not a worry in of itself

# But we might wonder - is that point having an undue influence on the model?
# This becomes more important in multiple regression
# If the predictor variables are correlated, a point that seems normal on individual distributions could still be an outlier

# But for now, we will just look at the influence each point is having on the model
# We will use a metric called Cook's D (where the D is for distance)
# Don't worry about the math
# But effectively this is a measure of how much predictions change if that point is taken out of the model

# There's an easy way of getting this using diagnostic.plot - 'influence'
diagnostic.plot(car.lm,type='influence')

# Which is the same as:
# plot(car.lm,type=4)

# We can see that the large dist point is having a comparatively large influence on the model...
# But it's not to the point of worry
# You can still see the other bars here
# If it looks like one line, then you should worry
# We'll see an example of this later

# Now if you just want to look at all of these together, you just give diagnostic.plot the argument 'all' or leave it blank
diagnostic.plot(car.lm)

##############################
# 8.4 - Diagnostics: mutiple #
##############################

# In multiple regression, the same big three diagnostic questions remain:
#  1) Is error not normally distributed?
#  2) Is error correlated with any of the terms in the model?
#  3) Are there any outliers?

# We'll go back to our multiple regression from the beginning of the class:
mtcars.lm = lm(mpg ~ disp + hp + wt, data = mtcars)

# And we can run diagnostics as usual with diagnostic.test
diagnostic.plot(mtcars.lm)

# Note that you now get plots of the residuals versus each of the predictors individually
# And for now, let's ignore the poor diagnostics

# Because there are a couple other considerations that have to do with multiple predictors
# These come up because predictors can be correlated with one another
# And this has the potential of causing problems with the model

# The first is a subset of (3), where we're worried about outliers
# But in this case, we're worried about outliers in multiple dimensions

# What do I mean by this?
# Let's make some hypothetical data
set.seed(563728)

# Both the predictor variables will be correlated
# And the true underlying model is:
# y = 1 + .5*x1 + x2 + e

x1 = round(runif(14,-2,2),3)
x2 = x1 + round(rnorm(14,0,.5),3)
y = round(1 + .5*x1 + x2 + rnorm(14,0,.7),3)
samp.dat.base = data.frame('y'=y,'x1'=x1,'x2'=x2)
rm(x1,x2,y)

# Then I'm going to throw on another data point
samp.dat = rbind(samp.dat.base,c(2.641,1.976,-1.863))

# And then we can do multiple regression
samp.lm.out = lm(y ~ x1 + x2,data = samp.dat)
summary(samp.lm.out)

# Well, it's highly significant...
# But notice how far off the parameters are from the true model
# This suggests that:
# y = 0.80 + 0.84*x1 + 0.03*x2 + e

# And furthermore it suggests that x2 shouldn't even be in the model
# Skipping the other diagnostic checks for now, let's ask if any of the predictors look wonky
qplot(0,samp.dat$x1,geom='boxplot')
qplot(0,samp.dat$x2,geom='boxplot')

# No outliers here, right?

# Nope!
# This only becomes apparent when you look at it in two dimensions
qplot(x1,x2,data=samp.dat)

# Can anyone find the outlier?

# When you have a large number of predictor variables though, it's harder to find these points in this way

# This is where the influence plots come in
diagnostic.plot(samp.lm.out,type='influence')

# Note the big problem here is point 15 - coincidentally the odd point I added on at the end

# The takeaway here is that data points should be reviewed for how far they stand out across all dimensions
# And the influence graphs help
# But so do 2-D slices through the predictor data (e.g. plotting predictor variables against each other)

# The other worry with multiple regression stands alone, and can be categorized as:
#  4) Are the predictor variables too related?

# Let's look back at the sample data we had, without the obvious outlier tacked on at the end
# We can make a new model now
samp.lm = lm(y ~ x1 + x2, data = samp.dat.base)
summary(samp.lm)

# Aw geez... the parameters are still far away from the real model, suggesting that:
# y = 0.76 + 0.36*x1 + 0.48*x2 + e

# And now both x1 and x2 are noted as not significant!
# (Although the entire model is sill significant at p = 0.043)
# What happened?

# Well, the answer here is multicolinearity - x1 and x2 fall fairly clearly on the same line, and are highly correlated
qplot(x1,x2,data=samp.dat.base)
with(samp.dat.base,cor(x1,x2))

# So why are neither significant in the summary?
# Well recall we learned earlier that those p-values are associated with a parameter when everything else is already in the model
# And because both x1 and x2 are so highly related, once you have one, the other really doesn't add anything to the model

# We can see this easily by looking at the reduced models:
samp.lm.x1 = lm(y ~ x1, data = samp.dat.base)
samp.lm.x2 = lm(y ~ x2, data = samp.dat.base)

# Both show a significant relationship...
summary(samp.lm.x1)
summary(samp.lm.x2)

# But adding the other variable does nothing
anova(samp.lm.x1,samp.lm)
anova(samp.lm.x2,samp.lm)

# This will happen any time that you can predict one variable in terms of the rest
# Most of the information of that variable is contained in the rest
# So adding that variable to the model does very little to improve predictive power

# And in fact, it hurts the model in general
# Notice how the parameters suggested by the full model are far off from the real slopes we used
# This is because when colinear variables are added, it adds uncertainty to the parameter estimates

# For a thought experiment, imagine x1 were exactly equal to x2, and y = 1 + 0.5*x1 + 1*x2 + e
# So the parameters b1 = 0.5 and b2 = 1 are the 'real' parameters
# But when you estimate parameters, b1 = 1 and b2 = 0.5 would fit exactly as well
# As would b1 = 7.5 and b2 = -6
# Or any combination where b1 + b2 = 1.5

# Because of uncertainty in parameter estimates, this uncertainty is still a problem when x1 and x2 are highly related
# Thus by having x1 in the model we inflate the uncertainty about the estimate of b2
# And by having x2 in the model, we inflate the uncertainty about the estimate of b1

# In this case, if our job is to predict the y value as best as possible, we should just pick one of the x1 or x2 variables
# If we want to find the actual impact of both x1 and x2 on y, there are tools to do that
# However, these tools, like ridge regression, are beyond the scope of the class
# And even then, the parameters are hard to interpret
# Remember - the slope of x2 is the effect on y of changing x2 by one unit *holding everything else constant*
# But when x1 and x2 are colinear, this doesn't make much sense - after all, changing x2 should change x1 as well

# So here our best bet is to just use samp.lm.x1 or samp.lm.x2
# Or to average them and use that as a single predictor

rm(samp.dat,samp.dat.base,samp.lm,samp.lm.out,samp.lm.x1,samp.lm.x2,cars.lm)

###############################
# 8.5 - Examples of wonkiness #
###############################

# NOTE: It's unlikely that we will make it all the way through this section in lab
# However, you should finish it yourself to see how diagnostics look when assumptions fail

set.seed(501)

# The prior examples were somewhat subtle
# Now let's look at some clear examples of wonkiness to pick out the things to look for 

######################################################
# Example 1: Non-linear relationship between variables

# In some cases, effects may start to saturate...
# For instance, if you've only studied for a test for an hour, another hour will be extremely helpful
# But if you've studied for 40 hours already, the 41st hour probably won't be as impactful
# This can be modeled using a logarithmic relationship

# So for our example... let's first define our predictor variable as hours studied
# We'll pull 30 students, and use an exponential distribution to capture that more people will barely study than do lots
# (These are undergrads after all...)
hours.studied = rexp(40,.1)+1

# Then we test them... and their test score is a logarithmic function of the hours studied plus random noise
test.score = 30*log(hours.studied) + rnorm(40,0,5)

# So let's naively use a linear model to look at this relationship
test.lm = lm(test.score~hours.studied)
summary(test.lm)

# Sweet! We got a very significant relationship!
# Case closed, right?

# Well, no...
# First let's visualize this relationship:
qplot(hours.studied,test.score) + geom_abline(intercept=coef(test.lm)[1],slope=coef(test.lm)[2])

# You can see that the line doesn't quite fit there..
# What about our regression diagnostics?
diagnostic.plot(test.lm)

# Now our big three items:
# 1) The residuals are CLEARLY not normal
# 2) You can see the exaggerated relationship of less-more-less between predictors and residuals
# 3) Maybe a few points are semi-outliers (but the rest is so bad outliers are the least of our worries)

# So what's the remediation here? A data transformation!
# If you regress against log(hours.studied), you get a better relationship
# (Because of course that's how we defined the data)
# But we also might believe this is a good transformation for the same diminishing returns idea above
test.log.lm = lm(test.score ~ log(hours.studied))
summary(test.log.lm)

qplot(log(hours.studied),test.score) + geom_abline(intercept=coef(test.log.lm)[1],slope=coef(test.log.lm)[2])

# Then new diagnostic tests:
diagnostic.plot(test.log.lm)

# And now it looks good
rm(test.score,hours.studied,test.lm,test.log.lm)

######################################################
# Example 2: Error increases with predictor variable

# In some cases, the level of variability is tied to the predictor or response variable
# For instance, if you are looking at the relationship between a 2-year-old's number of words & books in the house
# you will find that it does exist
# But you will also find that error is not constant w.r.t. the predictor value
# Why? Because if there are few enough books that the child only knows a few words, there can't be much variability
# However, as you increase the number of books, the interaction with other variables grows
# Such that if the child is expected to know a large number of words, a relative increase or decrease is absolutely much larger

# Let's see what this looks like:
# We'll take 30 two-year-olds and look at the number of books their parents own (between 10 and 1000)
books = round(runif(30,10,1000),0) # Rounding because you can't have fractional books

# Their number of words is going to be a function of this value
# But note that the error variance is a function of books as well
words = round(books/10 + rnorm(30,0,books/20),0)

# Now regress it!
word.lm = lm(words ~ books)
summary(word.lm)

# Yes! Another great linear fit!
# Until you start to visualize
qplot(books,words) + geom_abline(intercept=coef(word.lm)[1],slope=coef(word.lm)[2])

# Time to start diagnostics!
diagnostic.plot(word.lm)

# 1) It's slightly non-normal, but not too bad
# 2) No worries about error changing with any variables
# 3) Well - here three points look like they have a big influence

# Which are the influential points?
books[c(7,19,24)]
words[c(7,19,24)]

# Note that these are the points for which there is a large predictor variable...
# This is why fanning error is bad: it makes observations with high predictors have more influence on the regression line

# The solution to this?
# Weighted regression - this reduces the impact of families with a lot of books on the regression
# We won't get into this yet, but the idea is that you can give the observations with lots of books less impact
# Thus they are less likely to skew the data

rm(words,books,word.lm)

######################################################
# Example 3: Impact of time as a factor

# In some case you might find that the order of the observations has an impact
# For instance, subjects might get better at a task over time
# Or you might get great subjects at the beginning of the term, and poorly performing subjects at the end

# In the first example, let's say you have a task that involves solving puzzles with a number of pieces
# So you might expect time to finish to increase with number of pieces
# But people might also get better over time

# So we give one person 30 different puzzles:
pieces = round(runif(30,10,40),0)

# And then we can define time to finish as:
finishing = 50 + pieces*5 + rnorm(30,0,20) - seq(0,58,by=2)

# So let's build our linear model:
puzzle.lm = lm(finishing ~ pieces)
summary(puzzle.lm)

# Again, good linear fit, but by now we've learned not to trust that...
# So let's visualize:
qplot(pieces,finishing) + geom_abline(intercept=coef(puzzle.lm)[1],slope=coef(puzzle.lm)[2])

# Looks pretty good
# What about diagnostics?
diagnostic.plot(puzzle.lm)

# The only one that looks far off is the relationship between finishing time and order...

# So how do we solve this?
# One way is to account for time in the model (if there's a linear relationship)
# Another way is to ensure perfectly randomized trials
# (This will ensure trial order doesn't covary with anything you care about)

# But this is something to watch for in your experiments as well
# (I've found reviewers do ask often if people get better with time)
# So always try to measure order too
rm(pieces,finishing,puzzle.lm)

######################################################
# Example 4: Incompetent RAs (coding errors)

# On top of everything that can go wrong in an experiment, you might also have coding errors
# Because we're all grad students here, I'll blame it on the RA, but you can mistranscribe too...

# Let's say you want to predict 30 men's weight based on their height
height = round(rnorm(30,69,4),0)
weight = round(5*(height-36) + rnorm(30,0,20),0)

# But of course your incompetent RA dropped a digit in there...
weight[16] = round(weight[16]/10,0)

# Okay... so for the model
hw.lm = lm(weight ~ height)
summary(hw.lm)

# So we have statistical significance... 
# And are no longer excited just by that
# Let's plot first:
qplot(height,weight) + geom_abline(intercept=coef(hw.lm)[1],slope=coef(hw.lm)[2])

# Well something ain't right here...
# Normally you would see a point like that and immediately correct for outliers
# But for our sake, let's play this out and see the impact it has:
diagnostic.plot(hw.lm)

# In every graph, you can clearly see the wonky point...

# Now what do we do about this?
# Well, delete that point if we have evidence that it's not right
# In this case, people don't ever weigh 18lbs...
# So we can probably chalk this one up to an error
# Be careful though - sometimes outliers are due to other influencing factors
# Let's delete this one though...
height2 = height[-16]
weight2 = weight[-16]
hw.lm2 = lm(weight2 ~ height2)
summary(hw.lm2)

# And how does that influence the fit?
qplot(height,weight) + geom_abline(intercept=coef(hw.lm2)[1],slope=coef(hw.lm2)[2]) + 
  geom_abline(intercept=coef(hw.lm)[1],slope=coef(hw.lm)[2],linetype='dashed')

# The solid line is the new fit, the dotted the old
# This is a noticeable difference, and the solid line is more likely to be the right one

rm(height,weight,height2,weight2,hw.lm,hw.lm2)
