########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#      Lesson 13 - ANCOVA and data transformations     #
#                                                      #
########################################################

library(PSYC201)
library(ggplot2)
library(plyr)

load(url('http://vulstats.ucsd.edu/RAssignData/L13_data.RData'))

##############################
# 13.1 - ANCOVA              # 
##############################

# The good news is that you've learned everything you need to know to do ANCOVA
# All we need to do is learn how to interpret the output

# In this example from the PSYC201 package, we're testing how undergrads value their time
# So we give them a task that is either boring or exciting for 30 minutes, then give them $15
# Finally, we tell them they will have to continue the task for the remaining 30 minutes, but can buy their way out
# Thus we ask them how much of the $15 they would be willing to pay to get out of the task
# And lastly we measured their weekly income before they started
head(quit.payment)

# The first thing we might ask is: are undergrads willing to pay more to get out of a boring task?
# This would be a simple t-test or ANOVA:
t.test(Offer~Task,data=quit.payment)

# Well... the Boring group paid more than the Exciting group... but not much and it's not significant

# We can compare the data as well:
qplot(Task,Offer,data=quit.payment,geom='boxplot')

# That data looks pretty noisy as compared to the difference in means though
# So maybe there is a signal here, it's just being drowned out by the noise...
# How can we do better?
# Well, one way is to just collect more data. But we don't have the luxury of doing that here
# Another way is to use ANCOVA

# In this case, we might believe that undergrads who make more money will value their time more, and thus make higher offers
# For our main question - is there an effect of task excitement on the offer - this is just noise
# But it's also explainable - and in doing so we can take out some noise that will make our desired signal more clear

# So - we should ask ourselves: is weekly income a good covariate?
# A good covariate will have two characteristics:
#  1) You should have a belief that it is 'linearly related' to the dependent variable
#  2) It is 'independent' of the other experimental manipulations

# For the first part, we may not be sure about linearity, but we do believe that it should be related, so that passes muster
# For the second part, it is unlikely that the excitement of the task would change how much money subjects are earning
# (A good way to ensure the second question is passed is to ask the covariate question(s) BEFORE experimental manipulation)

# To do ANCOVA in R, all we have to do is add income to the model
# Note though, WeeklyInc must be added as the first term (we'll see why later)
offer.cov = lm(Offer ~ WeeklyInc + Task, data=quit.payment)

# Then as a basis, we take the model with just the WeeklyInc variable
offer.base = lm(Offer ~ WeeklyInc, data=quit.payment)

# Now ANCOVA is just asking - is there any effect of the Task type above and beyond what is explained by weekly income?
# But we know how to do these sorts of model comparisions - with the anova() command
anova(offer.base,offer.cov)

# And now we have a significant result - it does seem as if people in the boring task will pay more to get away

# But there's an easier way to see this result... just run anova() on the full covariance model:
anova(offer.cov)

# You'll note that the Task term seems to match up perfectly with the above model comparison
# This is why I noted you should add the covariate first:
# Since R adds items to the model in a stepwise fashion, the Task term here is the effect of Task once WeeklyInc is already in the model
# If you do it the other way around, you get a different (incorrect) result:
anova(lm(Offer~Task+WeeklyInc,data=quit.payment))

# This is calculating the SS of Task before the covariate - but under ANCOVA you want to know what you can explain after the covariate
# And thus we get different (incorrect) test statistics.

# So why does the significance increase so much?
# We can see easily by plotting the data
# This requires going back to the original Plotting lesson where we learned how to scatterplot with categorical variables
ggplot(quit.payment,aes(x=WeeklyInc,y=Offer,group=Task,color=Task)) + 
  geom_point() + geom_smooth(method='lm',se=FALSE)

# What ANCOVA is doing is asking whether the lines are shifted relative to one another
# But the error noise is just the noise around the lines
# Although still a bit messy, you should be able to see that the error around the lines is less than the error without the lines

# Now what if you wanted to get an estimate of how much more someone in the Boring task is willing to pay to get out?
# Can we just take the differences of the means?
mean(subset(quit.payment,Task=='Boring')$Offer) - mean(subset(quit.payment,Task=='Exciting')$Offer)  # 0.9855

# Well - let's look at the coefficients of our model:
summary(offer.cov)

# The TaskExciting coefficient is -1.07 - which means that exciting people pay a dollar and seven cents less according to this model
# This is different from the differences in the means - why?

# With ANCOVAs, you are asking what is the difference in the Offer based on Type *assuming WeeklyInc is the same*
# But even if there is no difference between WeeklyInc between the two Types in theory...
# (and there shouldn't be if we've done random assignment correctly)
# ... in practice there will be small differences
mean(subset(quit.payment,Task=='Boring')$WeeklyInc) # 260.35
mean(subset(quit.payment,Task=='Exciting')$WeeklyInc) # 268.5

# Now in reality this is a tiny difference
# But when comparing factors in an ANCOVA, you need to account for this, because it will cause a difference

# This is easy to do when you only have two factors, as in this case
# When you have more, however, it's easiest just to run contrasts and look at the 'Estimate'
# For instance:
contrast(offer.cov,c(1,-1))

# Note here that the Estimate is 1.07 - exactly as we got from our model

# And for the last bit of this section, we ask: why use '+' in ANCOVA models and not '*'?

# When you add an interaction to an ANCOVA model, you are bringing it beyond the bounds of ANCOVA
# When you do, you tell the model that the slope of the covariate regressed against the dependent variable
#  will chance as a function of the treatment group
# But this breaks rule #2: by definition the effect of the covariate is no longer independent of the experimental manipulations

# Note: it is perfectly acceptable to have interactions between multiple categorical terms, so long as they do not interact with continuous terms
# Thus if you were running a two factor study, it would be perfectly appropriate to cross the factors, if the covariate is not crossed
# For instance this formula would be fine:
DV ~ COV + IV1 * IV2

# But this would not be an ANCOVA:
DV ~ COV * IV1 * IV2

# This is a valid model (as we will discuss later), but not an ANCOVA

# However, you will often want to run an interaction to assure yourself that it DOESN'T exist
# If you find a significant effect, then the assumptions of ANCOVA don't hold
# And you'll need to start wondering about how the supposed covariate and the treatment interact...
anova(lm(Offer ~ WeeklyInc*Task,quit.payment))

# Another thing you don't want to see is dependence between the covariate and the independent variables of note
# To test for this, you can run an ANOVA to determine whether the covariate is predictable by all of your main factors
# Again, you don't want to see significance here
# If you do, you can't tell whether it's the difference in your treatment or the covariate that causes any experimental effects
anova(lm(WeeklyInc ~ Task, quit.payment))

rm(offer.base,offer.cov)

##############################
# 13.2 - ANCOVA caveats      # 
##############################

# For those of you paying attention to the replicability crisis, you may have seen ANCOVA getting slammed in some papers
# This has lead some journals to play down ANCOVA or ask for results without ANCOVA as well as with
# So you might be asking yourself: what should I do?

# What you should do is hard to say, but here are a couple things you should NOT do:

# 1) Pick and choose your covariates
# This is one of the original sins noted in the 'False Positive Psychology' paper
# They showed that just deciding whether to use a dummy 'gender' variable more than doubled false positive rates
# If you go through and only pick the covariates that help your case, you are capitalizing on chance
# So don't do it
# Instead, select your covariates beforehand, and use those and all of those - not a subset that seems to fit best

# 2) Covariates failed? Ditch 'em!
# This is a less extreme version of #1, but more insidious
# Rather than picking and choosing from a set of covariates, you might try the full set and get a null result
# Then since you took #1 to heart, you wouldn't be tempted to root through those covariates for just the ones that helped
# But you might think: why not just try the test without covariates at all? That's just a pure ANOVA, right?
# Therein lies the rub: if you have decent covariates, you basically double your false alarm rate by doing this
# If you have a real effect, it is extremely rare for it to not show up with covariates but show up without them
# (unless you believe the covariates are mediators - but that's another story entirely)
# So pick one before you run any analyses: use covariates or don't. But you shouldn't do both

# Thus a good rule of thumb is to have your analysis designed beforehand and run only that analysis
# Asking additional questions of your subjects so that you might have more covariates to try later is bad practice
# (However, asking those questions as CYAs for reviewer comments is fine - just be sure to note that this in your papers)

###################################
# 13.3 - Categorical + Continuous #
###################################

# Now what if we care about the effect of the continuous variables?
# E.g. we don't just care that the continuous variable soaks up error - we want to know how well it predicts our dependent variable
# For this, strict ANCOVA isn't appropriate...
# But we can use something similar - combining categorical and continuous variables in a linear model

# For this, let's look at the 'jitters' data in the PSYC201 package
head(jitters)
basejit = ggplot(jitters,aes(x=Weight,y=Taps,group=Drug,color=Drug)) + geom_point()
basejit

# In this sample data, we gave subjects 200mg of either caffeine, sugar, or Ritalin
# We then measured their jitteriness by counting how many times they tapped on a table within 5 minutes
# But we might also assume that drug effects are moderated by body weight - drugs have smaller effects on larger people
# Thus we want to model both the effects of drug and the effects of weight

# The additive model looks very similar to the way we did ANCOVA - we just include both terms in the model
# Note, however, that in this case we don't have to add the continuous variable first
jit.noint = lm(Taps ~ Drug + Weight, data=jitters)

# And then we can run anova() on this:
anova(jit.noint)

# Looks like there is an effect of drug, but not of weight...

# Why did we put Drug first instead of Weight?
# Well, there was no principled reason
# In fact, with these models, you should run it both ways
jit.noint2 = lm(Taps ~ Weight + Drug, data=jitters)
anova(jit.noint2)

# Notice that the qualtiative results are similar, but there are some quantitative differences
# This is to be expected...
# Unless the continuous variables are perfectly balanced throughout conditions (which rarely if ever happens)
# Otherwise, the continuous and categorical variables are correlated
# And we discussed the issues with this in multiple regression - this is an extension of the exact same idea
# So you should always check both ways

# But how do we interpret this model? Let's look at the summary:
summary(jit.noint)

# This combines what we know about interpreting ANOVA coefficients with what we know about multiple regression
# Since there are coefficients for sugar and Ritalin, the (Intercept) is the number of taps we would expect from someone on caffeine
# But it's what we would expect from someone on caffeine who weighed 0 pounds
# And the Weight slope is the number of extra taps you would expect to see for each extra pound

# It changes slightly for the sugar and Ritalin...
# Here, the value of DrugRIT is how much the intercept for Ritalin deviates from (Intercept)
# So we would expect someone who weighed 0 pounds on Ritalin to tap 43.9 + 46.1 = 90.0 times

# And likewise, someone who weighted 0 pounds on sugar would tap 43.9 - 26.0 = 17.9 times

# So basically, if the subject is on caffeine, you can measure the number of taps based on weight with that (Intercept) as the intercept
# And likewise, if the subject is given sugar, the linear model uses the DrugSUG + (Intercept) as the intercept with the same slope coefficient
# Then with Ritalin, the intercept is DrugRIT + (Intercept) with again the same slope coefficient
# So this model is assuming that the effect of weight should be constant for all drugs, but that the drugs affect the intercept (vertical shifts)

# We can visualize this:
basejit + geom_abline(intercept=coef(jit.noint)[1],slope=coef(jit.noint)[4],color='red') + 
  geom_abline(intercept=coef(jit.noint)[1] + coef(jit.noint)[2],slope=coef(jit.noint)[4],color='dark green') + 
  geom_abline(intercept=coef(jit.noint)[1] + coef(jit.noint)[3],slope=coef(jit.noint)[4],color='blue')

# But when we look at the plot, we see that each of the conditions doesn't seem to have the same slope
# There seems to be a positive slope for Ritalin, a negative slope for caffeine, and an unclear slope for sugar
# Our model assumed that all the slopes were the same, though, and so cancelled everything out
# Can we do better?

# Yes! Using interactions!
# Interactions between a continuous and categorical variable allow for different slopes between conditions
# Let's see how this works:
jit.full = lm(Taps ~ Drug * Weight, data=jitters)
anova(jit.full)

# And now we have a clearly significant interaction!

# But how do we interpret this model?
# It's a bit more tricky...
# But let's start with the summary...
summary(jit.full)

# You might note that it's very different from the no interaction model, and has two extra interaction terms
# Now recall that this is effectively fitting three separate linear regressions,
#  with a slope and intercept for each drug group

# Let's start with the caffeine group, because it's the easiest
# The intercept is just the (Intercept) coefficient, and the slope is the Weight coefficient
# So the equation is: y.hat = 120.2 - x * 0.3291

# Then let's move on to the Ritalin group
# Here, like the no interaction model, the intercept is DrugRIT + (Intercept): 120.20 - 120.18 = 0.02
# But the slope is not just Weight...
# Instead, the DrugRIT:Weight tells you how much the slope deviates from just the Weight coefficient
# So the slope is Weight + DrugRIT:Weight: -.33 + .93 = .60
# Thus the equation is: y.hat = 0.02 + x * 0.60

# And we can follow the same logic for sugar: y.hat = 11.85 + x * 0.1129

# And then we can visualize how these individual drug regressions differ:
basejit + geom_smooth(method='lm')

# This seems to fit much better!
# And so we can start to interpret this data:
# It looks as if weight does have a modulating effect on the drugs...
# Ritalin gets less effective at stopping jitters with more weight, caffeine gets more effective, and sugar seems to have little effect overall

rm(jit.noint,jit.noint2,jit.full,basejit)

##############################
# 13.4 - Log Transforms      #
##############################

# As you noticed the other day in the class session, interpreting log-transforms can be tricky
# However, implementing them is simple - there are two ways:
#  1) Make a new column in your data frame with the transformed data, and build your model with that
#  2) Log-transform the data in your models

# Let's look at a simple example of how to do this: asking whether income changes with education level
# For this, we'll be using the 'incdat' table from the loaded data
incdat

# Note there are two columns: highest level of educational achievement and annual income
# We want to ask whether people with more education make more money
# Naively, we could just run an ANOVA on this and see if there are any differences:
naive.aov = aov(Income ~ Education,incdat)
summary(naive.aov)

# Well, no difference here... but don't despair!
# There are problems with this analysis, since the errors are very non-normal:
diagnostic.plot(naive.aov,'qq')

# This graph is driven by one huge outlier...
# One person in the sample is making $16.7MM per year, while the next highest income is $2.7MM
# But even without them, the incomes are skewed towards the high end
diagnostic.plot(aov(Income ~ Education,incdat[-30,]),'qq')

# And now, because we know that income compounds, we might consider log-transforming it
# Is this okay? Let's ask our questions:
#  1) Can income be negative? No!
#  2) Is education additive to income? Probably not!
#  3) Are there other (upper) bounds? Nope!

# So we can reanalyze this by log-transforming income
# We will do so by adding the log-income data:
incdat$LogInc = log10(incdat$Income)
head(incdat)

# Now we run an ANOVA as usual
inc.aov = aov(LogInc ~ Education,incdat)
summary(inc.aov)

# And we find an effect!
# Plus now, the errors are for the most part normal, suggesting a more appropriate model:
diagnostic.plot(inc.aov,'qq')

# Note though that we didn't even need to make an extra column...
# We could have done the exact same thing by taking the logarithm in the model:
summary(aov(log10(Income) ~ Education, incdat))

# Be careful though - this works for logarithms, but you can't always call functions in a formula
# If you're ever worried, make another data frame column to be safe

# So how do you interpret this finding, other than that there are differences in income levels by education?
# Well, let's look at the coefficients:
coef(inc.aov)

# In regular ANOVAs, we could find our best guess for the mean income was just the intercept plus the coefficient for that condition
# (or in the case of the first factor - college - just the intercept)

# Here the response variable is log-transformed
# But this doesn't mean we can just take 10^x and get the mean of our condition
# Because we have just gotten the average of the LOG of income
# And since income is skewed, the mean of income will actually be higher
# For instance, if we tried to estimate income based solely on this model, we would get:
10^predict(inc.aov,newdata=data.frame(Education=c('College','Graduate','HS','SomeCollege')))

# But if we look at average income in our data, we get something different:
with(incdat,tapply(Income,Education,mean))

# We have sorely underestimated the averages
# So we CANNOT get mean incomes by educational level that way
# We can only get means of the log-income
# (Actually, this is useful in some sense as the geometric mean, but that's not something you need to know)

# However, we aren't holding nothing here...
# We can look at how education multiplies earnings potential
# We use the rule 10^(a+b) = 10^a * 10^b
# Thus if you go from a HS degree to a full college degree,
# Your income would go from 10^x to 10^(x + .554)
# (The .554 is the inverse of the EducationHS coeficient, since this is HS->college, not college->HS)
# Thus your new income would be 10^.554 * 10^x = 3.58 * 10^x
# Which means that a college degree would multiply income by 3.58 compared to a HS degree

# So when you do log-transformed ANOVAs, you can't get the mean out of them (easily)
# But you can look at how changes in conditions make multiplicative changes in the response variable

# Now let's try another example: predicting crimes based on population
# In crimedat we have the population of various cities, and the number of violent crimes in those cities
# We might want to ask two (related) questions:
#  1) Can we predict the number of violent crimes based on population?
#  2) Does the crime rate change with city size?

# Well, let's look at the data:
qplot(Population,Crimes,data=crimedat)

# Hmm... there's a large scale of both populations and crimes here...
# Maybe we should log transform it...
# There's an easy way to do this in plotting:
#  Just add on a scale_x_log10() and scale_y_log10():
ggplot(crimedat,aes(x=Population,y=Crimes)) + geom_point() + scale_x_log10() + scale_y_log10()

# You'll note this is useful since it log-scales the plot, but keeps the original units

# Of course, this looks pretty nice and linear
# So let's try log-log regression
crime.lm = lm(log10(Crimes) ~ log10(Population),crimedat)
summary(crime.lm)

# Well this is a significant relationship...
# But how do we interpret these coefficients?

# We have the equation:
#  log(Crime) = b0 + b1*log(Pop) + e
# So we exponentiate:
#  Crime = 10^(b0 + b1*log(Pop) + e)
# Then we use the rules of exponentiation to find that:
#  Crime = 10^b0 * Pop^b1 * 10^e
# Plugging in coefficients we find that:
#  Crime = 0.00168 * Pop^1.0823 * 10^e

# So, we are able to model crime as a function of population

# But what about the question of whether crime rate changes as a function of population?
# We can reformulate that by asking what would happen if crime rate were constant?
# In this case, you could model Crime ~ rate * Pop
# Which can be rewritten as Crime ~ rate * Pop ^ 1
# So this is in effect asking whether the log10(Population) slope is 1 or not
# There aren't any specific tests for this, but you do know how to make a 95% CI on slopes
# Just take the estimate +- qt(.975, df)
# Or, even easier, use the confint() function:
confint(crime.lm)

# Seems like 1 is not in the 95% CI, so we can say that crime rate grows exponentially with population
# (with alpha of .05)

# Also, nicely you can add log-log regression to a ggplot with the same geom_smooth()
# If you add log-axes, then ggplot knows you want to regress against the log of the variable
# So all you have to do is add an lm method geom_smooth
ggplot(crimedat,aes(x=Population,y=Crimes)) + geom_point() + 
  scale_x_log10() + scale_y_log10() + geom_smooth(method='lm')

