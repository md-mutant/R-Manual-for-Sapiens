########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#       Lesson 14 - GLM and logistic regression        #
#                                                      #
########################################################

library(PSYC201)
library(ggplot2)

####################################
# 14.1 - Generalized Linear Models #
####################################

# Today we'll be talking about extending the linear models we've discussed before

# What if you had data where you ask subjects in various conditions to solve a problem?
# Now your interest is whether people in certain conditions are more likely to solve that problem
# And so your dependent variable is dichotomous - solved or not
# Which breaks the assumption of linear models

# What you're really interested in is the PROBABILITY of solving based on condition
# And this is a continuous variable, which makes it slightly more tractable
# However, there is an issue... probabilities must be between 0 and 1
# So based on the formula above, if we used a linear regressor we could go outside of that range
# And we know error won't be distributed normally...
# On each individual trial, we know that the probability of success is either 100% or 0%
# (After all, the trial already happened)
# Thus error will be bimodal

# Between these two issues, the linear models we've used before are inappropriate

# So in order to model this binary data, we need to transform our predictors
# First we define:
#   eta(i) = b0 + b1*x1(i) + b2*x2(i) + ...

# Then we need to define a link function to get the mean expected value (probability) for a given observation
# We'll call this function 'm', such that:
#   y.hat(i) = m(eta(i))

# Finally, we need to define an error distribution so that:
#   y(i) = y.hat(i) + e(i)

# Now note that this is just a superset of the linear models we have been using in the past
# If we assume that m is the identity function, and that error is normally distributed...
# This just becomes the linear model we are all too familiar with
# That is why this is called the generalized linear model... because it's more general than simple linear models
# But in the end is based on a linear combination of observation parameters

# So to start off, let's see how R extends the linear model into the generalized linear model
# And for this, we're going to (again) go back to when we learned about lm()
# In our first example, we used the mtcars dataset to find a relationship between car weight and MPG
mt.lm = lm(mpg~wt,data=mtcars)
summary(mt.lm)

# Interpreting this result should be second nature by now
# But we can run the exact same test using the generalized linear model
# R uses the command glm() for generalized linear models
# But in addition to the formula, glm needs additional information: the link function and error distribution
# In R, these are called 'families' because the two tend to be very related
# So to replicate linear models, we can use the "gaussian" family
# This is a family that assumes the identity link function and normally distributed error
gaussian()

# So to use glm(), we must provide the forumula but also tell R to use the gaussian family
mt.glm = glm(mpg~wt, family = gaussian(), data = mtcars)
summary(mt.glm)

# Now note the similarity between the two summary calls
# Above the signif codes line, they are identical except for the call
# So you get the same evidence the weight is a significant predictor...
# Of course this should be true, since both calls are doing effectively the same thing

# But below these lines, there are differences...
# You should be used to the lm summary statistics
# But what are the glm summary statistics?

# The first thing to ask is what deviance is
# By one definition, deviance is another word for variation
# And we already have a measure of variation in lm... the Sum Squares
# We can get at these through the anova() command:
anova(mt.lm)

# Now note that the Residuals SS in lm() is the same as the Residual deviance in glm()
# And likewise, the degrees of freedom are the same
# So for the gaussian family, Residual deviance is SSE and dfE

# Let's try summing the two SSs now:
847.73 + 278.32

# This is the same as the Null deviance
# But if you sum SSR and SSE, you get SSTO
# So the Null deviance numbers represent SSTO and dfTO

# Next, look at MSE in the lm() anova...
# Looks a lot like the dispersion parameter in the glm() summary, right?
# Recall that MSE is the best estimate of the error variance around ANOVA estimates...
# Which thus is the variance estimate of e(i)
# But in the glm, we also need to define our error distribution
# And we've defined it as gaussian, but we need to estimate the variance
# So for the gaussian family, glm gives you this estimate as the dispersion parameter

# Then there's the AIC
# The glm models tend to be fit by maximum likelihood estimation
# And thus instead of providing R-square or R-square adjusted, summary() provides AIC
# (Which you can also get out by using AIC)
AIC(mt.glm)

# Finally, ignore Fisher Scoring iterations
# This is just additional information about fitting the model that we rarely care about

# So why do you get different information from glm than lm?
# The short answer is that things like Rsq and F-tests don't make sense outside the gaussian family
# However, deviance and degrees of freedom are universal to all families
# Thus glm summaries try to stay consistent throughout families, and so leaves out specific information
# We'll see this more in logistic regression

# But first, let's do ANOVAs with glm
# We'll go back to our workhorse dataset warpbreaks
# And recall, we can do ANOVA with a linear model by calling:
wb.lm = lm(breaks~wool*tension, data=warpbreaks)
anova(wb.lm)

# Now the glm call is the same as above... we just define the family as gaussian
wb.glm = glm(breaks~wool*tension, family = gaussian(), data=warpbreaks)

# And just like lm, we can call summary(), but it doesn't help for factor grouping
summary(wb.glm)

# Instead we'd like to test differences between factors
# But if we run anova alone, we get some numbers but no test statistics:
anova(wb.glm)

# Let's compare this to the lm anova call:
anova(wb.lm)

# Now, just like before, Deviance is the same as Sum Sq for each term, and you can see matching dfs
# But you don't get the same F statistics and p-values from the glm...
# This is because, as mentioned above, F-tests only work for gaussians
# And thus we need to tell R that we have a gaussian and care about the F-test
# We can do this easily by giving anova the test='F' parameter:
anova(wb.glm,test='F')

# This gives us the same F statistics and p-values as the anova lm
# And now we have everything we've needed from the basic lm for our gaussian glm

rm(mt.lm,mt.glm,wb.lm,wb.glm)

################################
# 14.2 - Logistic regression   #
################################

# Now that we're comfortable with the glm command, let's move onto logistic regression

# For this example, we'll use the birthwt dataset from the MASS package
# This dataset has observations of a number of babies, with the following characteristics:
#   low: indicator of birth weight less than 2.5 kg
#   age: mother’s age in years
#   lwt: mother’s weight in pounds at last menstrual period
#   race: mother’s race (1 = white, 2 = black, 3 = other)
#   smoke:  smoking status during pregnancy
#  ... and other variables we won't care about

# We might want to know whether certain characteristics of the mother impact the chance of low birthweight

# Since this isn't a default dataset, we must load it in using the data command:
data(birthwt,package = "MASS")
head(birthwt)

# Now because these are all integers, we need to coerce some of them to factors
birthwt$race = factor(birthwt$race, labels = c('White','Black','Other'))
birthwt$smoke = factor(birthwt$smoke)

# First question: does the mother's weight impact chance of low birthweight?
# Thus the response variable is low, and the independent variable is lwt
# But now low is an indicator variable... it is always either 0 or 1
# So we need to define a link function and error distribution

# The error distribution is easy: it's binomial, since there are only two choices
# We have a choice of the link function, but typically for binomial data we use the logit function
# (Hence logistic regression...)

# The logit link function is:
#   m(eta) = 1 / (1 + e^(-eta))

# And since we have one continuous independent variable, we define:
#   eta(i) = b0 + b1*x(i)
# ...where x is the value of lwt for each observation

# Throwing these together, we get:
#  y.hat(i) = 1 / (1 + e^(-b0 - b1*x(i)))

# Here, y.hat(i) is the expected probability of a baby having low birthweight
# In logistic regression, therefore, we often call y.hat 'p'
# And so this becomes:
#  p(i) = 1 / (1 + e^(-b0 - b1*x(i)))

# Now, this is easy to do in R...
# We just use glm with the binomial() link function
# Since binomial errors typically use logit link functions, these two go together
# Just like the identity link function went with gaussian error
binomial()

# And so our model is:
bw.lgt = glm(low ~ lwt, family = binomial(), data = birthwt)
summary(bw.lgt)

# Now note that this is slightly different from the gaussian summary
# First, the coefficients have z-values rather than t-values
# Second, dispersion is always 1, and doesn't need to be estimated

# The z-statistic and associated p-value is based on the Wald test
# This is similar to a z-test, but with a different way of estimating SE
# We'll get back to interpreting this in a bit

# One thing that looks similar to the gaussian family but isn't is the parameter estimate
# Recall that in basic linear models, b0 and b1 are easily interpretable:
# b0 is the intercept - the value of y.hat when the x value is 0
# b1 is the slope - the increase in y.hat with every unit increase in x

# But this is only true because m(eta) = eta
# Thus y.hat = b0 + b1*x

# But with our logistic model, we have:
#   p = 1 / (1 + e^(-b0 - b1*x)

# And so a unit increase in x isn't an increase of b1 in y.hat
# So how do we interpret these parameter estimates?

# First, we can make sense of the intercept
# The value of the intercept is the value of eta when x = 0
# And so this becomes:
#   p = 1 / (1 + e^(-b0))

# So here, we get:
1 / (1 + exp(-coef(bw.lgt)[1]))

# What this means is, if we had a mother with zero weight,
# her child has a 73.1% chance of being of low birth weight
# (Now obviously a woman with zero weight is impossible...)

# To make sense of the lwt coefficient, we must invert the link function
# When we do this, we get the logit function:
#   eta = log(p / (1-p))

# Now we know that p / (1-p) are the odds of a success
# (e.g., p(success) / p(failure))

# And therefore, eta represents the log-odds of a success
# But the slope parameter is the rate of increase in eta per unit increase in x
# So we can say that the slope parameter is the rate of increase in the log odds with a unit increase in x
# And thus we can exponentiate x and call that the increase in odds ratio
# In this case, we get:
exp(coef(bw.lgt)[2])

# What this means is that for every pound increase in mother's weight,
# The odds of having a low birthweight baby get multiplied by .986

# Note that this is not a decrease of 1.4% of the probability...
# If you are at a point at which the probability of low birthweight is 50%,
# Then the odds ratio is 1 (0.5 / 0.5)
# And so increasing a pound makes the odds ratio 0.986
# But since the odds ratio is (p / (1-p)), we can solve for p
# If the odds ratio is x, then p = (x / (1+x))
# Thus we solve for p to be 49.6% - a drop of 0.4% chance

# Likewise, if you are at a point at which the probability of low birthweight is 5%,
# Then the odds ratio is 1/19, or ~0.0526
# And now adding a pound makes this odds ratio 0.0519
# This p therefore is 4.93%, which is a drop of 0.07% chance

# Once we have the logistic model, we can also get model predictions
# Let's say we have a new woman who weighs 120 pounds
# Then we want to know  the probability that they will have a low birthweight child

# The analytic way of doing this is fitting this into the equations
# First we solve for eta:
eta = coef(bw.lgt)[1] + 120*coef(bw.lgt)[2]

# Then we transform it with the logistic function:
1/(1+exp(-eta))

# So this woman has a 33% chance based on our model

# But the predict function works just as well on glm objects, with one caveat...
# You must feed it the argument type='response'... otherwise it will just give you eta rather than p
predict(bw.lgt,newdata=data.frame(lwt=120),type='response')

# Note however that you can't use predict to get confidence intervals from glm objects...

# Now we might ask whether the mother's weight is a statistically significant addition to the model
# There are two tests to check for whether a single term is good statistically
# The first is the Wald test mentioned above in the summary
# The second is the Wilk's chi-square likelihood ratio test
#  (also often known as a chi-square likelihood test)
# glm() makes getting these tests easy

# To get the Wald test z-statistic and p-value you just enter the summary of the glm object:
summary(bw.lgt)

# So in this case, we can write this test as: z = -2.28, p = 0.0227

# If you want to pull out a Wald test score, there's a function in the PSYC201 package
# All you need to do is call get.p, and give it the logistic regression
get.p(bw.lgt)

# You can then pull out the appropriate term like you've done before
get.p(bw.lgt)['lwt']

# To get the Wilk's chi-square likelihood ratio test, you can specify this using test='Chisq' in the anova command:
anova(bw.lgt,test='Chisq')

# Now note that unlike ANOVAs, where the t-test and F-test gave the same result, we get two different ps here
# This is because the tests use different underlying assumptions about statistic distributions
# When you have a huge number of observations, they both converge to the same value

# But when you don't have a huge number (like in this case), which should you use?
# Ed and I can't find any solid arguments favoring one over the other
# (Wikipedia favors the chi-square test, but it seems to be based on specious reasoning)
# Some rough simulation testing seemed to suggest that the Wald test is conservative in logistic regression, while chi-square is anti-conservative
# This means that with a small number of observations and an alpha of 0.05
# You'll false alarm less than 5% of the time with the Wald test, and more with the chi-square test
# These both converge to a 5% chance of false alarms with a decent number of observations
# But it takes longer to do so for the Wald test under some conditions
# Usually conservativeness is good, but it also implies lower power to detect real effects

# Thus my suggestion is to start with the Wald test to protect your alpha level
# But also check the chi-square likelihood ratio test...
# If it's significant when the Wald test isn't and you have > 50 observations, the Wald test might just be underpowered

# Finally we might want to plot the results of a logistic regression
# The easiest way to do this is to plot the points at 0 and 1, then plot the response line
# Luckily, there's a function in the PSYC201 package to do this: plot.logistic
# This takes a logistic glm object and all the regular plot functions, then outputs this graph:
plot.logistic(bw.lgt)

###################################
# 14.3 - More logistic regression #
###################################

# Now that we've learned how to do logistic regression with a single explanatory variable, it's easy to extend the model
# Just like we extended linear regression into multiple regression, ANOVA, and ANCOVA...
# We can do the exact same thing for logistic regression
# And it will have many of the same considerations that those extensions have

# Let's start with multiple regression
# We might want to predict the probability of low birthweight using both mother's age and weight
# And we know the fomula is low ~ age + lwt in R-speak
# So we just do:
bw.mlg = glm(low ~ age + lwt, family = binomial(), data = birthwt)

# Then we can look for the effect of each predictor with summary:
summary(bw.mlg)

# Note that the p-value for lwt is different from the model without age...
# Just like with the t-tests in multiple linear regression,
# These are the probabilities of the parameter not being zero with everything else in the model

# Likewise, the parameter estimates will be different if you add or take out other parameters
# And thus interpreting the parameter coefficients has the same restrictions as multiple regression (with a logistic twist)
# Here, the slope of age is the effect of a unit increase in age on the log odds ratio keeping everything else constant

# And of course, because the p-value for age is greater than 0.05,
# We can reject the hypothesis that age adds something to the model based on the Wald test

# We can also test for the effect of multiple parameters
# But rather than using the F-test, we use the likelihood ratio test
# So let's say we want to see if the model with age and weight is better than nothing
# We first define the null model:
bw.null = glm(low ~ 1, family=binomial(),data=birthwt)

# Then just with lms, we can compare them with the anova() command
# But because they're glms, we need to specify the Chisq test
anova(bw.null,bw.mlg,test='Chisq')

# We can also run logistic regression on categorical variables...
# Often, this is called logistic ANOVA
# And this works just the same as regular ANOVA by setting up indicator variables
# Then doing the regression on these
# So for instance, if you wanted to look at the effect of mother's race on the probability of low birthweight:
bw.alg = glm(low ~ race, family = binomial(), data = birthwt)

# But when we call summary on this glm, we run into the same problem we had with ANOVA summaries:
summary(bw.alg)

# ... we get Wald tests for individual indicator variables, but nothing about whether race in general has an effect
# But again, just like with ANOVAs, we can use the anova command to solve this problem
# (Oh, and just like glms in the past, we need to pass it Chisq as the test...)
anova(bw.alg,test='Chisq')

# So not significant...

# As an aside, when testing logistic models, it might help to make analogies to the lm models
# Here you can think of Wald tests like the t-tests on coefficients, and Wilk's chi-sq tests like F-tests
# Wald tests give confidence intervals on individual parameters so only work for one parameter at a time
# So for the same reason we couldn't use coefficient t-tests for ANOVA, we can't use Wald for logistic ANOVA
# Wilk's chi-square test, on the other hand, tests models against each other, adjusing for parameter differences
# Thus we can use them like F-tests to compare two nested models against each other, which is what we need for logistic ANOVA
# The big difference is that while the t-test and F-test were equivalent,
# The Wald test and Wilk's test are not

# Now back to the logistic regression features...
# Just like ANOVAs, we can easily add multiple factors and interactions...
# Say between race and smoking...
bw.flg = glm(low ~ race * smoke, family = binomial(), data= birthwt)
anova(bw.flg,test='Chisq')

# Oh, and of course, in non-orthoganal designs we have to worry about the order we enter terms...
bw.flg2 = glm(low ~ smoke * race, family = binomial(), data = birthwt)
anova(bw.flg2,test = 'Chisq')

# Finally, there's logistic ANCOVA...
# Which again is an easy analogue to ANCOVA
# If we want to factor out weight before testing for smoking and race, for instance...
bw.aclg = glm(low ~ lwt + race * smoke, family = binomial(), data = birthwt)
anova(bw.aclg,test='Chisq')

# So now that you know how to do linear models and logistic regression, it's easy to combine the two
# The only difference comes in interpreting the parameters, as will happen with any logistic regression

# The only thing we haven't talked about is random effects...
# This is because to do a full random effect logistic model is really hard mathematically
# If you're using a fully within-subject design, you can add subject to the model just like you can for ANOVA within-subject designs
# However, for more complex designs, you need additional machinery beyond the scope of this lecture (and basic R packages)
# We will discuss this in a later lesson
# But otherwise, you should be set with logistic regression

rm(birthwt,bw.lgt,bw.mlg,bw.null,bw.alg,bw.flg,bw.flg2,bw.aclg,bw.aclg.int)

###################################
# 14.4 - Poisson regression       #
###################################

# Let's try another common(ish) type of generalized linear model:
# Poisson regression

# For this we're going to download some sample data
# Let's say you're trying to figure out who is a better TA...
# You, or your co-TAs Carl Clueless and Slacker Jones
# So you record how many students attend office hours (a measure of student confidence)
# And you also record the week of the term (since you *know* this will have an effect)
oh.dat = read.csv('http://vulstats.ucsd.edu/RAssignData/OHAttend.csv')
head(oh.dat)
qplot(Week,Attendees,data=oh.dat,color=TA,geom='line')

# Now we could try a linear model to fit the data... but that wouldn't make sense
# After all, you can't get a fraction of a student attending office hours
# (I hope)
# So we can't assume normal error (which requires continuity)
# Instead we'll use the Poisson distribution

# The Poisson distribution takes a single parameter - lambda
# This lambda parameter is both the mean and the standard deviation of the distribution

# So what does it look like?
# Here are some PDFs of the Poisson distribution with various (small) lambdas:
qplot(0:10,dpois(0:10,1),geom='bar',stat='identity',ylim=c(0,.5),main='lambda=1')+scale_x_continuous(breaks=0:10)
qplot(0:10,dpois(0:10,2),geom='bar',stat='identity',ylim=c(0,.5),main='lambda=2')+scale_x_continuous(breaks=0:10)
qplot(0:10,dpois(0:10,3),geom='bar',stat='identity',ylim=c(0,.5),main='lambda=3')+scale_x_continuous(breaks=0:10)
qplot(0:10,dpois(0:10,5),geom='bar',stat='identity',ylim=c(0,.5),main='lambda=5')+scale_x_continuous(breaks=0:10)

# Note that Poisson captures count data, so must be greater than 0
# And importantly, it can only take on integer values...
# A draw from a Poisson distribution will NEVER be fractional

# But that doesn't mean that lambda can't be fractional
# You can still get a mean and variance of a set of integers:
qplot(0:10,dpois(0:10,2.5),geom='bar',stat='identity',ylim=c(0,.5),main='lambda=2.5')+scale_x_continuous(breaks=0:10)

# So this seems like a nice way of going from a continuous variable
# (b0 + x1*b1 + ...)
# To a discrete variable

# But there's one other condition...
# Lambda cannot be negative (who heard of average negative counts?)
# But our eta values can... what to do?

# Remember a recent lesson where we discussed types of transformations for positive-only data?
# I hope so! It was log-transformations
# But because we need to work with the eta term, we need to do the opposite 
# (Turn a possibly negative number into a positive one)
# And the opposite of log-ing is exponentiation

# So the m function for Poisson regression is simply expoentiation
# And therefore, we can use similar logic to interpret Poisson parameters that we could with log-regression

# And so, without further ado, let's run our Poisson regression
# Since 'Weeks' is a nuisance variable, we want to treat it as a covariate and add it first without interaction
# (Trust me when I tell you that all of the covariate checks work - or run them yourselves for practice!)

# We use the exact same syntax we had for logistic regression, but instead say family=poisson
oh.pois = glm(Attendees ~ Week + TA, family=poisson, data=oh.dat)

# And then we can first ask - is there a difference by TAs?
anova(oh.pois,test='Chisq')

# Yep - looks like TA explains a lot of deviance
# So what is this model telling us?
summary(oh.pois)

# Remember, the lambda parameter in Poisson distributions is the mean (and the variance)
# So we can write the estimated lambda as:
#  lambda.hat = exp(b0 + b1*Week + b2*I.Slacker_Jones + b3*I.You)

# Let's first look at the (Intercept) term
# Just like with all other linear models we've discussed, this is the value when all else is 0
# Which means at week 0 (which doesn't exist), and for Carl Clueless (who happens to be the intercept by virtue of the C)
# The lambda parameter is exp(0.95) = 2.6
# So you would expect that in week 0, Carl would on average have 2.6 students in his office hours

# Then the week parameter is growth per week. So we can interpret it as how expected attendance changes from one week to the next
# But remember that this is through exponentiation
# So for Carl, the lambda parameter can be reformulated as:
#  lambda{Week} = exp(b0 + b1*Week)
# or:
#  lambda{Week} = exp(b0)*exp(b1*Week)

# And thus the expected change week over week can be thought of as:
#  lambda{Week+1} = exp(b0)*exp(b1*(Week+1))
#  lambda{Week+1} = exp(b0)*exp(b1*Week + b1)
#  lambda{Week+1} = exp(b0)*exp(b1*Week)*exp(b1)

# Which can be restated by saying that each week, you would expect student attendance to grow by a factor of exp(b1)
# In this case: exp(.24) = 1.27
# So you would expect office hour attendance to rise by 27% each week

# Finally, there are the TA coefficients
# Like with general ANCOVA, these represent offsets when switching from one TA to another
# In this case, though, instead of being additive, they are multiplicative
# So Slacker Jones will get more people than Carl Clueless, but it won't be a constant amount
# Instead, exp(0.476) = 1.61
# So he will get 61% more students

# Likewise, you will get exp(1.15) = 3.15 times as many students as Carl

# So like logistic regression, setting up Poisson regression is easy
# Interpreting the coefficients, on the other hand, takes a bit more work
# But glm has the tools to model count data this way