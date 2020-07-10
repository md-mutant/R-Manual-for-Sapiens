########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#            Lesson 6 - Hypothesis Tests               #
#                                                      #
########################################################

library(PSYC201)
library(ggplot2)
library(gridExtra)
set.seed(12733) # To make sure I know what you're getting if you run this yourself...

#################################
# 6.1 - One sample t-tests      #
#################################

# Last lab, we showed how to test whether a sample differed from the null hypothesis
# But to do so, we assumed we knew the variance of the sampling distribution of the mean
# Usually, we don't

# To start, let's sample 40 people from our population (~N(1,3)) again
pop.sample = rnorm(40,mean = 1, sd = 3)
mean(pop.sample)

# We know that if we did a lot of sampling like this, our distribution would have:
# Mean = 1
# SD.Mean = 3/sqrt(40) = 0.474

# But this time let's say that we don't know the standard deviation of the population
# Instead, we have to ESTIMATE the standard deviation based on the sample
# And therefore, the standard error of the mean is an estimate as well

# To start, our best guess of the population standard deviation is the sample SD
# So we can calculate the standard error based on that assumption
# Thus the standard error is the sample SD divided by the square root of the sample size
se.mean = sd(pop.sample)/sqrt(40)
se.mean

# Note that this is close to the standard deviation of the mean (0.474) - but not quite

# Now we can do something very similar to what we did last week - give a 95% confidence interval
# But the standard error is not distributed normally
# Instead, it's distributed using the t-statistic
# You can get the t-distribution using the standard R modifiers: r/d/p/qt
# But the t-distribution takes a parameter - 'degrees of freedom' (df)
# Let's see what it looks like for various df values
xs = seq(-3,3,by=.01)
plots = lapply(c(2,5,10,30,120), function(df) {qplot(xs,dt(xs,df),geom='line',ylim=c(0,.4),main=paste('df =',df))})
plots[[6]] = qplot(xs,dnorm(xs),geom='line',ylim=c(0,.4),main='normal')
do.call(grid.arrange,plots)
rm(xs,plots)

# Notice that as the df gets large, the t distribution approaches the z distribution

# So rather than calculating the z-limits of a confidence interval, we calculate the t-limits
# But we need to know the degrees of freedom for the sample
# For interval estimation, this is one less than the sample size - 39
# Remember - with a 95% CI, alpha is 0.05
# So now we use the qt() command, just like qnorm()
# Except with qt() we need to give it the degrees of freedom (df)
t.crit = qt(1-(0.05/2),df = 39)

# And so we can be 95% sure that the mean is within t.05 standard errors of the sample mean
top.range = mean(pop.sample) + t.crit * se.mean
bottom.range = mean(pop.sample) - t.crit * se.mean

# And again... 95% chance we're in the confidence interval
top.range
bottom.range

# Likewise, you can calculate the p-value of a t-test the same way as a z-test
# First get a 't-score', then test where it falls on the t-distribution
# The t-score is the difference in the mean from the null, divided by the standard error
# Or: t* = (mean.obs - mean.null)/se
# So in this case
t.score = (mean(pop.sample) - 0) / se.mean
t.score

# Then we can test where this falls in the t-distribution
pt(t.score, df = 39)

# But just like a z-score, you need to take 2*(1-pt) to find the p value (if you're in the upper tail)
p.ttest = 2*(pt(-abs(t.score),df=39))
p.ttest

# You can also shortcut this whole process with t.test(), just like we did with z.test()
# (Note: t.test and the other hypothesis tests are built in - not part of the PSYC201 package - only z.test is)
t.test(pop.sample)

# This should tell you all you need to know about the t-test of the sample:
# The t-value, p value, 95% confidence interval, and best estimate of the mean

# Note though, that this defaults to at 95% confidence interval
# You can change this (say to a 99% interval) using the conf.level argument:
t.test(pop.sample, conf.level = 0.99)

# And you can change your null hypothesis - by default you ask whether the mean of your sample is different than 0
# For instance, if you wanted to know if the mean of the population were different than 1
# You just set the 'mu' argument to 1
t.test(pop.sample, mu = 1)

# Let's see this in practice...
# For instance, you measure the height of a group of men to be:
heights = c(68,72,69,68,69,70,70,77,71,72,75,71,71,68,67,70,73,70,76,68)

# You also know that the average height of men in the population is 69 inches
# What's the mean of your sample?
mean(heights)

# This is slightly greater than the population average...
# But do you have any evidence that your sample is statistically different (alpha = .05)?
# You have your sample, and you know your null hypothesis, so...
t.test(heights, mu = 69)

# Yes - the p value is .0108

# And finally, you can pull out these values by storing the test to a variable and using the same functions we used for z.test
# (Note that these functions still are in the PSYC201 package)
tt = t.test(pop.sample)

get.stat(tt) # The t-value
get.ci(tt) # Confidence interval
get.df(tt) # The degrees of freedom
get.p(tt) # The p-value

# Now we clean up after ourselves
rm(bottom.range,p.ttest,pop.sample,se.mean,t.crit,t.score,top.range,tt,heights)

#################################
# 6.2 - Two-sample t-tests      #
#################################

# One sample t-tests work when you know what the mean of the null hypothesis is
# In the last example, we knew that the average population height was 69 inches
# But in many cases, you won't know the mean of the null hypothesis

# Here we'll be using the 'memory.exp' data from the PSYC201 package
# Let's say you want to know if happy music gives people better memory
# You segment 20 subjects each into two groups...
# The control group listens to neutral music then takes a memory test
# While the experimental group listens to happy music then takes a memory test
# You then get the following scores on the tests:
memory.exp

# We can break this into two vectors
cont.sc = subset(memory.exp,Group=='Control')$Score
exp.sc = subset(memory.exp,Group=='Experimental')$Score

# In this case, you don't know what the population would score on the memory test
# All you have to measure this is the scores of your control group
# But the mean of the control group isn't known exactly - only estimated through your data

# So does the experimental group have statistically higher scores?
# By the means, they are slightly higher:
mean(cont.sc)
mean(exp.sc)

# But we're doing statistics, not thumb-in-the-air analysis!

# We want to test whether the difference in the means is 0 or not
mean.diff = mean(exp.sc) - mean(cont.sc)
mean.diff

# So let's start by defining the t-statistic
# Recall: t = (exp.mean-null.mean)/std.err
# And in the one sample t-test, std.err = std.dev/sqrt(n) = std.dev*sqrt(1/n)
# But that assumes perfect knowledge about the null mean
# Because we don't have this knowledge, we need to 'pool' our std.dev

# Typically, you would calculate this by weighting the variances by the degrees of freedom:
exp.n = length(exp.sc)
cont.n = length(cont.sc)
s.pooled = sqrt(((exp.n-1)*sd(exp.sc)^2 + (cont.n-1)*sd(cont.sc)^2)/(exp.n + cont.n - 2))
s.pooled

# But when we have equal n, this becomes much more simple:
s.pooled = sqrt(0.5 * (sd(cont.sc)^2 + sd(exp.sc)^2))
s.pooled

# This is just averaging the variances of the individual distributions
# Note that this assumes that the variances are actually equal in the two groups
# The only difference is that you're taking separate (imperfect) samples
# So you're just using both samples combined to better estimate the variance

# We also need to adjust for the number of subjects in each sample
# In this case, we multiply by sqrt(1/n.cont + 1/n.exp)
# So the standard error is:
se.pooled = s.pooled*sqrt(1/20+1/20)

# So our t statistic is simply calculated as:
t.two = mean.diff/se.pooled

# Next, we need to define the degrees of freedom for the t distribution
# We define this as the total number of subjects minus 2
# (We need one degree of freedom each for each distribution)
df.t = 20 + 20 - 2

# So now that we have a t-statistic and degrees of freedom, we calculate p
p = 2*(pt(-abs(t.two),df = df.t))
p

# We can tell that there is a statistically significant difference at alpha = 0.05

# But we can also take a shortcut with t.test
# Here we give t.test both vectors, rather than just one:
t.test(exp.sc,cont.sc)

# Notice that the df and p are very slightly different from what we calculated
# This is because by default, t.test does not assume the variances are equal
# If you want to assume equal variance, you must set var.equal to TRUE
t.test(exp.sc,cont.sc, var.equal = TRUE)

# As an aside, you can gain a tiny bit of power by assuming that the variances are equal when they theoretically are
# But in general, it's better not to make that assumption unless you have very good evidence for it
# In this case, the variances are roughly equal:
var(exp.sc)
var(cont.sc)

# (And since I made the data, I can tell you that they came from distributions with the same variance)
# So the unequal variance t-test is the one we should use
# But note that it really didn't change much, which is good

# And we can change the CI level the same way we did for z-tests:
t.test(exp.sc,cont.sc,conf.level = .99)

# And then we can do all the normal things to pull numbers out of the t.test
#  ... get.p, get.ci, get.stat, etc.

# Finally, if you have your data in a data frame with one column for the value and one for the group,
#  there is an easier way of writing the t-test using tilde notation
# It was kind of a pain to separate the scores out into control and experimental above
# Instead, we can tell R we want to run a t-test, where we test if 'Score' varies as a function of 'Group'
# In R parlance, this is "Score ~ Group"
# So instead of needing to split up the Score column, we can run the test directly from the data frame:
with(memory.exp, t.test(Score ~ Group))

# You'll be hearing a lot more about the tilde in the near future
# Just remember this is the way of telling R you want to model X as a function of Y

# Now let's just clean up
rm(df.t,mean.diff,p,s.pooled,se.pooled,t.two,cont.n,exp.n,exp.sc,cont.sc)

#################################
# 6.3 - Paired t-tests          #
#################################

# In some cases, you want to measure the improvement in a single subject over time
# For instance, a drug company tests a weight loss pill will weigh subjects before
# Then weight them afterwards
# The pill is considered successful if there is a significant decrease in weight
# Let's say they did this for 20 males and got the following data:

before = c(190,174,196,153,187,182,153,165,149,116,190,171,169,151,191,145,182,149,181,160)
after = c(188,171,188,165,174,175,130,159,153,111,180,166,165,154,185,129,187,142,176,162)

# Now what happens if you run a t-test?
t.test(after,before)

# In this case, you wouldn't see any evidence of an effect...
# But you would be wrong

# A core assumption of two-sample t-tests is that the observations are independent
# But in this case, they're not - each subject contributes a before and after
# Let's look at this in a data frame to see how they match up:
wloss = data.frame('Before' = before, 'After' = after)
wloss

# Or just plot them quickly:
qplot(before,after)

# You can see that some subjects are heavier in general, and some are lighter
# But you don't care about baseline weights - you want to know if there's a change
# And from the plot, it looks pretty clear that some relationship exists...

# In the case of paired or repeated measures t-tests, you just take a difference score
# Then run a t-test on that difference
wloss$Diff = after - before
wloss

# This has the effect of changing it back into a one-sample t-test
# In this t-test, the null hypothesis is there is no change - aka a mean of 0
t.test(wloss$Diff)

# However, if you don't want to calculate the difference directly, you can take a shortcut
# You can just tell R that you want to do a paired t-test
# If you do though, make sure your numbers match up!
t.test(before,after,paired = TRUE)

# Note that if you don't make this a paired test, you don't find an effect
# The between subject differences are much bigger than the within subject differences
# If you are looking at differences within subjects,
# Always use a paired t-test by doing a t-test of the difference scores
# (This goes along with the general theme that measuring within-subject effects tend to give you much more power)

rm(after,before,wloss)

##############################
# 6.4 - Goodness of fit      #
##############################

# Binomial tests are great for dichotomous variables - if something happens or it doesn't
# But in many cases, you'll want to know about a range of occurrences
# For instance die rolls... can we tell if a die is fair?
# Let's say we rolled a die 100 times, and came up with the following data:
# 14 ones, 10 twos, 13 threes, 16 fours, 13 fives, and 34 sixes
# (Often, you can get these counts from a table() of your enumerated data)
die.rolls = c(14,10,13,16,13,34)

# So is this a fair die?
# You learned about chi-square tests recently...
# Chisq = sum((obs-exp)^2/exp)
# Well we have our observed data in die.rolls
# And our expected value is 100/6 assuming equivalence
# So we can calculate chi squared statistic as:
die.chisq = sum((die.rolls - 100/6)^2/(100/6))

# Then we want to know whether this is significant...
# So we go to the chi-square distribution
# You can get at this with p/d/q/rchisq
# It also takes it's own df (degrees of freedom) parameter

# Since there are 6 options (one per die side), there are 5 (6-1) df
# Now we can find the probability of getting that chisq statistic or LESS using pchisq
pchisq(die.chisq,df=5)

# But to get the p-value, we want to know the probability of getting something MORE extreme
# This is just 1-P(less), or:
1-pchisq(die.chisq,df=5)

# Remember, you don't need to multiply by two here...
# Your null hypothesis is that the observed distribution is far away from the expected distribution
# The lower tail of the chi-square statistic tells you that the observed and expected distributions are very similar
# Thus we only need to consider the upper tail for the test

# But of course, this being R, there's a much easier way to find this all out:
# Just use chisq.test() on your data
chisq.test(die.rolls)

# And just like binom.test and t.test, you can store this and pull data out
cst = chisq.test(die.rolls)
get.p(cst)
get.stat(cst)
get.df(cst)
# Note: these tests don't have confidence intervals, so get.ci doesn't work

# Next, what if I told you that this wasn't a fair die at all
# Instead, I rigged it so that 25% of the time it would come up 6, and other sides with equal p
# Do you have evidence that I'm lying now?
# First let's define the probabilities:
die.probs = c(.15,.15,.15,.15,.15,.25)

# Now we could go through that ugly math again...
# Or just plug it into chisq.test() using the 'p' argument
chisq.test(die.rolls,p=die.probs)

# Good - no longer significant
# You don't have evidence I'm lying, so you can (start to) trust me

# Finally, let's say you don't have the counts, but just the raw data...
# For instance, you just have the results of the die rolls rather than the count:
new.rolls = sample(1:6,100,replace=T)
new.rolls

# Now you can't just give this to chisq.test raw (bonus points: why not?)
# Instead you need to get the counts out
# But this is easy with the table() function, which just counts the number of instances in a vector:
table(new.rolls)

# So you can easily test goodness of fit by running chisq.test() on the tabulated data:
chisq.test(table(new.rolls))

rm(die.chisq,cst,die.rolls,new.rolls,die.probs)
  
##############################
# 6.5 - Test of independence #
##############################

# You may also be measuring two categorical values, and want to know if they are independent
# Let's say that we're studying how creative people are in different situations
# We give a number of subjects a sample object fixedness problem and see if they can solve it in 10 minutes
# The control group is just given the problem
# The reward group is told that they will be paid a bonus if they solve it
# And the punishment group is told that they will be chased out of the lab by a tiger if they fail
# (The IRB approvals were a pain for this...)
# Also, we had particularly lazy RAs who just assigned people as they see fit
# So there aren't equal numbers of people in each group

# Then we want to ask: does the task condition change the rate at which people solve creative problems?
# This is in effect asking, is the rate at which people solve the problem not independent of task?

# So here's the data:
creativity = matrix(c(58,63,73,72,116,186),nrow=2,byrow=TRUE,
                    dimnames = list(c('Solved','Not Solved'),c('Control','Reward','Punishment')))

creativity

# Well, first we'll notice that our RAs were sadistic and put most subjects in the Punishment condition...
# So we can just look at the number of people solving the problem in each condition

# But we can investigate solution rates within each group
creativity[,'Control']/sum(creativity[,'Control'])       # 45% success
creativity[,'Reward']/sum(creativity[,'Reward'])         # 35% success
creativity[,'Punishment']/sum(creativity[,'Punishment']) # 28% sucess

# So it looks like people are most creative under no pressure, and have problems when their life is on the line
# But we need to ask - is this statistically significant?

# To test this, we use a chi-square test of independence
# This asks whether solution rate is independent of condition
# Or in mathematical terms, does P(solution|condition) = P(solution)?

# To test this, we first need to create what the distribution of responses would be assuming independence
# For this we can use the margin.table() function
# margin.table() takes two arguments: the matrix to marginalize, and which way to do so
# If you type margin.table(mat,1), it marginalizes mat over the rows
# And if you type, margin.table(mat,2), it marginalizes mat over the collumns
margin.table(creativity,1)

# So we can get the probability of solving the problem overall as:
p.solve = margin.table(creativity,1)/sum(creativity)
p.solve

# And likewise, we can get the probability of being assigned to each condition as:
p.cond = margin.table(creativity,2)/sum(creativity)
p.cond

# We can then determine the probability of the conjunction by using the outer() function
# This function takes two vectors, and returns a matrix multiplication of their outer products
p.independent = outer(p.solve,p.cond)
p.independent

# But these are probabilities - we want expected counts
# So we multiply by the total number
cre.expected = p.independent * sum(creativity)
cre.expected

# Note these expected counts aren't whole numbers - and that's okay

# Now the chi-square test of independence is the sum over all cells of:
# (actual - expected)^2 / expected

# Or in R terms:
chsq.ind = sum((creativity - cre.expected)^2/cre.expected)
chsq.ind

# This will follow a chi-square distribution with df = 2
# Why 2? Because you count the number of rows of data (r) and number of columns (c)
# And df = (r-1) * (c-1)
# So df = (3-1) * (2-1) = 2

# And again we can use 1 - pchisq:
1-pchisq(chsq.ind,df = 2)

# This is ~0.005
# Which means that there's about a .5% chance of observing this distribution
#  if solution rate really were independent of condition

# But of course, R gives us a much easier way of doing this...
# And it's also called chisq.test()
# We can just run this on the matrix and it will give us the same answer
chisq.test(creativity)

# And just like with the goodness of fit chi-square, you can store this to a variable
# and manipulate the p-values, test statistic, etc.

# One additional parameter you might want to pull out is the expected distribution
#  under the assumption of independence
# This is done through the 'expected' data field
chisq.test(creativity)$expected

# Note that this is identical to our cre.expected values calculated above

# But chi-square tests can be brittle if you have few observations in some buckets
# Let's say you had nice RAs instead of sadistic ones
# And they didn't want to put many people in the Punishment condition
creativity.nice = matrix(c(58,63,1,72,116,4),nrow=2,byrow=TRUE,
                    dimnames = list(c('Solved','Not Solved'),c('Control','Reward','Punishment')))
creativity.nice

# Only 5 people in the Punishment condition...
# So what happens if we run a chi-square test?
chisq.test(creativity.nice)

# Notice that it now gives you a warning message
# This is because when there are less than 5 observations in a cell,
# the chi-square test statistic no longer perfectly follows the chi-square distribution
# R notices this and will tell you as much

# However, there is a (partial) solution for this: the simulate.p.value argument
# If you set this to true, R tries to simulate what the actual distribution should be under independence
# And you can get a more valid test
chisq.test(creativity.nice, simulate.p.value=TRUE)

# Note that the p-value it gives you is somewhat different and higher
# We'll learn how this works later in 201B
# For now just trust that it kind of does
# (but for a caveat, see http://andrewgelman.com/2011/11/chi-square-fail-when-many-cells-have-small-expected-values/)

# And finally, sometimes you will also need to tabulate 2-D data...
# For instance, the conjunction of a coin flip and die roll
conj = data.frame(flip = rbinom(100,1,.5),roll = sample(1:6,100,replace=T))
head(conj)

# You can tabulate this with the table() function too - just give it both vectors
# And you'll get a 2-D table:
table(conj$flip,conj$roll)

# Now you can run the chisq.test() on this data:
chisq.test(table(conj$flip,conj$roll))

# Hey! If you've been following along, you'll get statistical significance!
# So I must have fixed something so that the flips and rolls were really dependent, right?
# (or maybe 1 in 20 of my null results will come up statistically significant...)

# However, you can run the chisq.test() by just giving it both vectors independently
chisq.test(conj$flip,conj$roll)

# But it's better to tabulate this first, because you don't have any idea *how* the two conditions might be dependent
# With tabulation you can see how the numbers fall out

rm(creativity,creativity.nice,p.solve,p.cond,p.independent,cre.expected,chsq.ind, conj)