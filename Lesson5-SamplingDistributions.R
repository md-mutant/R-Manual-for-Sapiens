########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#     Lesson 5 - Sampling Distributions & z-tests      #
#                                                      #
########################################################

library(PSYC201)
library(ggplot2)
library(gridExtra)

# Setting graphical parameters so that code works on 800x600 resolutions
par(mar = c(2,2,1,1))

##################################
# 5.1 - Normal distribution      #
##################################

# In Lesson 4, we learned about four functions to work with the binomial distribution:
#  dbinom: d(ensity of the) binom(ial)
#  pbinom: p(robability) of the binom(ial)
#  rbinom: r(andom) binom(ial)
#  qbinom: q(uantile of the) binom(ial)

# But now we'll be talking a lot about the normal distribution
# So naturally we'll want to know about the density, probability and quantile of the normal, right?

# Well, the good news is that R has a common naming convention for distributions
# So we can pretty easily infer the appropriate functions for the normal distribution from what we know about the binomial

# First, let's remind ourselves of rnorm
# A normal distribution is defined by two numbers: its mean and standard deviation
# rnorm() takes three arguments: the number of samples, the mean, and the standard deviation
# So a draw from a normal with mean of 20 and standard deviation of 3 would be written as:
rnorm(1,mean=20,sd=3)

# And we can cut this down if looking at a standard normal, defined as mean = 0, sd = 1
# So 10 draws from this distribution would be 
rnorm(10)

# First, dnorm, for d(ensity of the) norm(al)
# Lets say we want to know the probability density (pdf) at the point of 17 on a normal with mean = 20 and sd = 3
# dnorm takes three arguments: the point (x), then the mean and sd
dnorm(17, mean=20, sd=3)

# But again, if we're just using the standard normal, we can forget the last two and they will default to 0 and 1 respectively
# So the pdf at 0.5?
dnorm(0.5)

# And what about the cumulative density function?
# This is the probability of getting x or less from random draw from the normal distribution
# Similarly, this is pnorm: p(robability) of the norm(al)
# It takes three arguments but can be cut down to one as well
# So the cdf of 0.5 on the standard normal is
pnorm(0.5)

# Recall that for continuous distributions, this is the integral up through a given point
# So if you sketched the pdf of the normal, drew a vertical line at point x,
#  and colored in everything to the left of that line under the curve of the pdf,
#  the cdf would be the area under that curve

# Remember how pbinom(x,...) was the same as summing the dbinoms up to x?
# This is the same thing
# Except you can count the outcomes of the binomial distribution (they're discrete)
# But you can't do the same counting with continuous variables
# So instead you integrate, but it's the same idea

# Now what if you want to know the probability of getting x or greater (e.g. the area right of x)?
# That's easy: is 1-pnorm(x)
# So the probability of getting 0.5 or greater from a standard normal is:
1-pnorm(0.5)

# But this is odd... remember how with pbinom we had to subtract 1 when looking at x or greater?
# This is because of the difference between discrete and continuous distributions
# With a discrete distributions, '5 or more heads' is not the same as 'not 5 or fewer heads'
# Because '5 or more' and '5 or fewer' both include the event of 5 heads

# But you don't have this problem with continuous distributions
# Because it's continuous, the probability of getting exactly any number is 0
# So saying '0.5 or more' or 'not 0.5 or less' on the standard normal IS the same
# Because the event of getting exactly 0.5 doesn't really exist

# Thus - remember the heuristic:
# With discrete distributions, when asking for 'greater than' take one off for the 'less than' pbinom call
# But with continuous distributions, 'greater than' is just equal to one minus the 'less than' probability

# Finally, there's qnorm for q(uantile of the) norm(al)
# What if we want to know what value we would need such that 97.5% of the volume of the standard normal is less than that?
# This is easy:
qnorm(0.975)

# Should be about 1.96 - this is an important number in statistics, as we will soon see

# And how about the value such that 97.5% of the volume of the standard normal is greater than it?
# Because the normal is symmetric, this should just be -qnorm(0.975)
# But we can't always rely on our distributions to be symmetric
# So we tell qnorm that we do not want the lower tail, and instead want the upper tail:
qnorm(0.975,lower.tail = FALSE)

# Or equivalently, having 97.5% of the density greater is the same as having 2.5% of the density lower
# Which is easy to write:
qnorm(0.025)

# Now note that the d/p/r/q prefix works with lots of distributions
# We won't be using them as much, but you can use functions like punif for the cdf of the uniform
# Or dexp to get the pdf of the exponential distribution
# For almost any commonly used statistical distribution, the d/q/r/q prefixes will exist

#################################
# 5.2 - Combining distributions #
#################################

# Now let's have some more fun with simulation!
# To begin, recall that when two distributions are combined, their sums and variances are added together
# So if I had two normals: one with mean=5, sd=3; and one with mean=10, sd = 4
# Then the combined mean would be 15 (5 + 10)
# And the combined standard deviation would be 5 (sqrt(3^2 + 4^2))
# Let's see that in action:
comb.norm = rnorm(10000,mean=5,sd=3) + rnorm(10000,mean=10,sd=4)
mean(comb.norm)
sd(comb.norm)

# The best part is - we know exactly what this distribution is going to look like
# Any time you add two normal distributions together, the resulting distribution will be normal
qplot(comb.norm)

# Another test for normality is the 'qq plot'
# This plots the quantiles of two distributions against each other
# If it's a straight line, then you know that the two distributions are the same
# So an easy way to do a quick check for normality is to qq plot a distribution against the normal
# This is easy with R - qqnorm() will do the normal comparison automatically for you (though using base graphics)
qqnorm(comb.norm)

# And if you want to add a straight line, you can with qqline():
qqline(comb.norm)

# Note that you can make a QQ plot with ggplot, but not the line (without some hacks)
# Note that the syntax is a bit different...
# Which is why I tend to use the base plots, since often this is checking your data rather than publishing a plot
qplot(sample=comb.norm,stat='qq')

# Okay - so if you add two normals, you can figure out the combined mean and sd easily...
# But does this hold for other distributions?
# What if we combine two exponential distributions?

# Let's first see what a single distribution looks like
# Exponential distributions are defined by one variable: the decay rate
# Note that in the exponential distribution, mean and sd = 1/rate (4, in this case)
sing.exp = rexp(10000,rate=.25)
mean(sing.exp)
sd(sing.exp)

# The exponential distribution is decidedly non-normal:
qplot(seq(0,10,by=.01),dexp(seq(0,10,by=.01),rate=.25),geom = 'line')

# Now let's add two exponentials with mean/sd = 4
# Will the combined means and standard dev
comb.2exp = rexp(10000,rate = .25) + rexp(10000,rate = .25)

# The mean should be 8 (4 + 4)
mean(comb.2exp)

# And the sd should be about 5.66 (sqrt(4^2 + 4^2) = 4*sqrt(2))
sd(comb.2exp)

# But how does it look?
# Here we're going to do a density plot - this is basically just a smoothed histogramc
qplot(comb.2exp,geom='density',xlim = c(0,20))

# Well that's not a normal...
# But it's not an exponential distribution either...
# What if we add three exponentials?
comb.3exp = rexp(10000,rate = .25) + rexp(10000,rate = .25) + rexp(10000,rate = .25)

# Again, the mean should be 12, and the sd should be ~6.92 (4*sqrt(3))
mean(comb.3exp)
sd(comb.3exp)

# And what does this look like?
qplot(comb.3exp,geom='density',xlim = c(0,30))

# That's starting to look like a skewed normal...

# So let's now sum 10 of these distributions:
comb.10exp = replicate(10000,sum(rexp(10,rate = .25)))

# The mean should be 40, and the sd should be ~12.65 (4*sqrt(10))
mean(comb.10exp)
sd(comb.10exp)

# And visualize:
qplot(comb.10exp,geom='density',xlim = c(0,100))

# Well this is starting to look more normal...
# But if we qq plot it, we can see some skew:
qqnorm(comb.10exp)
qqline(comb.10exp)

# And 100?
comb.100exp = replicate(10000,sum(rexp(100,rate = .25)))

# The mean should be 400, and the sd should be 40 (4*sqrt(100))
mean(comb.100exp)
sd(comb.100exp)

# And visualize:
qplot(comb.100exp,geom='density',xlim = c(0,1000))

# This looks pretty normal
# And the qq plot will confirm that for us (it's a little off in the tails, but barely):
qqnorm(comb.100exp)
qqline(comb.100exp)

# But this is the key thing: if you keep adding up distributions,
# in the limit, the resulting distribution will be normal, regardless of the underlying distributions

# And here's the greatest part about it... what if you didn't know the mean of the exponential distribution?
# Well, if you took 100 samples, what would your best esimate of the mean of the distribution be?
# How about the mean of the 100 samples?
# Which of course, is just the sum of all of the samples, divided by 100...

# So if we know that the sum of n samples from a distribution is distributed as a normal
# with a mean of pop.mean*n and a standard deviation of pop.sd*sqrt(n)...
# then if you divide both of those by n, your estimate of the mean will be normal
# with a mean of pop.mean and a standard deviation of pop.sd/sqrt(n)

mean.exp = comb.100exp/100
mean(mean.exp) # Should be ~4
sd(mean.exp) # Should be ~ 0.4 (4 / sqrt(100))
hist(mean.exp) # Looks pretty normal...

# First we clean up...
rm(comb.norm,sing.exp,comb.2exp,comb.3exp,comb.10exp,comb.100exp,mean.exp)

# Then we get to the...

#################################
# 5.3 - Central Limit Theorem   #
#################################

# Here we're going to demonstrate the Central Limit Theorem in action

# Let's say we have a population has an attribute with a mean of 1 and sd of 3
# Often in our experiments, we'll only be able to take a small sample from the population - say 20 people
# Then our best estimate of the population mean will be the mean of that sample

# Let's try it:
mean(rnorm(20,mean = 1, sd = 3))

# But we don't get the exact population mean...
# Let's try it again:
mean(rnorm(20,mean = 1, sd = 3))

# That's not quite it either... again?
mean(rnorm(20,mean = 1, sd = 3))
mean(rnorm(20,mean = 1, sd = 3))
mean(rnorm(20,mean = 1, sd = 3))
mean(rnorm(20,mean = 1, sd = 3))
mean(rnorm(20,mean = 1, sd = 3))

# So let's do this a large number of times
lots.o.samples = replicate(10000,mean(rnorm(20,mean=1,sd=3)))

# The mean should be pretty close to 1 (or we would have a biased statistic)...
mean(lots.o.samples)

# That's good... if we do this a lot of times, on average the mean of the sample will be the population mean

# And we can look at the distribution
qplot(lots.o.samples,xlim = c(-4,6),binwidth = .1)

# That looks pretty normally distributed..
# So what's the standard deviation of the sampled means?
sd(lots.o.samples)

# Well, recall in the last section, we showed that summing lots of distributions made a normal distribution
# And that if you summed the same distribution over and over and divided by n,
# That's the same as taking a mean of a sample
# And this mean will be normally distributed around the population mean with a standard deviation of pop.sd / sqrt(n)

# And that's what we did with lots.o.samples
# We took a 20-person sample, and found the mean over and over again
# So the standard deviation of the mean should be pop.sd / sqrt(n)
# And we know that the population standard deviation is 3 (we defined it as such)
# So the standard deviation of the means should be:
3/sqrt(20)

# So if we take lots and lots of these 20-person samples,
# the mean of those samples is distributed as we would expect

# But what if we were able to take larger samples?
# What would happen to our estimate of the mean?

# According to what we've learned, the average mean would still be 1
# But as we increase n, the deviation of these means will decrease

# Let's try this with 100 people per sample
lots.o.samples = replicate(10000,mean(rnorm(100,mean=1,sd=3)))
qplot(lots.o.samples,xlim=c(-4,6),binwidth=.1)
mean(lots.o.samples) # ~1
sd(lots.o.samples) # ~ 3/sqrt(100) = 0.3

# But if we can't take 20 samples, our estimation of the mean will get worse
# On average, it will still be 1, but each estimate will deviate more from the mean
lots.o.samples = replicate(10000,mean(rnorm(5,mean=1,sd=3)))
qplot(lots.o.samples,xlim=c(-4,6),binwidth=.2)
mean(lots.o.samples) # ~1
sd(lots.o.samples) # ~ 3/sqrt(5) = 1.342

# And for a side by side comparison (with the basic distribution as well):

n1 = qplot(rnorm(10000,1,3),xlim=c(-12,14),binwidth=.2,ylim=c(0,3000),main='n = 1')
n5 = qplot(replicate(10000,mean(rnorm(5,1,3))),xlim=c(-12,14),binwidth=.2,ylim=c(0,3000),main='n = 5')
n20 = qplot(replicate(10000,mean(rnorm(20,1,3))),xlim=c(-12,14),binwidth=.2,ylim=c(0,3000),main='n = 20')
n100 = qplot(replicate(10000,mean(rnorm(100,1,3))),xlim=c(-12,14),binwidth=.2,ylim=c(0,3000),main='n = 100')
grid.arrange(n1,n5,n20,n100)

# So far we're assuming that the population attribute is distributed normally
# But this is often not the case
# The great part about the central limit theorem is it doesn't matter if the sample is large enough

# Let's try this by generating uniform random numbers between 0 and 1
# We can get a sample of 20 of these
mean(runif(20))
mean(runif(20))
mean(runif(20))

# The mean should be 0.5 - and we're getting close to it...
# But we have R; so let's test it by simulation!
lots.o.samples = replicate(10000,mean(runif(20)))

# How is this shaped?
qplot(lots.o.samples,xlim=c(0,1),binwidth=.02)

# Looks pretty normal, right?
# So what are the mean and standard devation of this distribution?
mean(lots.o.samples)
sd(lots.o.samples)

# Does this match with intuition?
# Well, the mean should be 0.5
# And take it from me on faith that the sd of a uniform from 0 to 1 is ~0.288
# So the sd should be:
0.288/sqrt(20)

# If we increase the number of people in each sample, this again gets more refined:
lots.o.samples = replicate(10000,mean(runif(100)))
qplot(lots.o.samples,xlim=c(0,1),binwidth=.02)
sd(lots.o.samples) # .288/sqrt(100) = .0288

# Or shrink the people per sample
lots.o.samples = replicate(10000,mean(runif(5)))
qplot(lots.o.samples,xlim=c(0,1),binwidth=.02)
sd(lots.o.samples) # .288/sqrt(5) = .129

# Note that even with a uniform distribution and 5 people per sample,
# the distribution of the sample means is still pretty normal
# (it will have fat tails, but it isn't too bad)
qqnorm(lots.o.samples)
qqline(lots.o.samples)

# But what if the data itself is EXTREMELY non-normal
# Here we're going to use a beta distribution
#  parameterized so that it's very bimodal

# This is what the pdf looks like:
qplot(seq(0,1,by=.01),dbeta(seq(0,1,by=0.01),.02,.02),geom='line')

# Now this pdf is symmetric around 0.5, so on average we should get 0.5

# And I'll just tell you the standard deviation is ~0.490

# With that in mind, what does the sampling distribution look like?
# Let's take lots of samples of 20 people
lots.o.samples = replicate(10000,mean(rbeta(20,.02,.02)))
qplot(lots.o.samples,xlim=c(0,1),binwidth=.05)

# Well, it's sort of normal...
# So let's check the mean and sd:
mean(lots.o.samples) # ~ 0.5
sd(lots.o.samples) # ~ .490/sqrt(20) = .110

# And we can increase the sample size to refine our observations
lots.o.samples = replicate(10000,mean(rbeta(100,.02,.02)))
qplot(lots.o.samples,xlim=c(0,1),binwidth=.05)
mean(lots.o.samples) # ~ 1
sd(lots.o.samples) # ~ .490/sqrt(100) = .049

# And that looks pretty normal

# But now let's take a small sample:
lots.o.samples = replicate(10000,mean(rbeta(5,.02,.02)))
qplot(lots.o.samples,xlim=c(0,1),binwidth=.05)

# Uh oh... this has some serious spikiness
# The mean and standard deviation look okay:
mean(lots.o.samples) # ~ 1
sd(lots.o.samples) # ~ .490 / sqrt(5) = .219

# But many of our statistical tests for the mean require that it come from a normal distribution
# So we can't build confidence intervals or do standard statistical tests on the mean...

# We would need a larger sample to ensure that the sampling mean is normally distributed
# Usually about 25-30 samples guarantees normality

# That's a good rule of thumb - 25-30 samples won't steer you wrong

# Let's see how that works with this data
# Here we'll plot the sampling distribution for samples sizes 1, 5, 10, 15, 20 & 30
n1 = qplot(replicate(10000,mean(rbeta(1,.02,.02))), xlim=c(0,1),binwidth=.05,main='n = 1')
n5 = qplot(replicate(10000,mean(rbeta(5,.02,.02))), xlim=c(0,1),binwidth=.05,main='n = 5')
n10 = qplot(replicate(10000,mean(rbeta(10,.02,.02))), xlim=c(0,1),binwidth=.05,main='n = 10')
n15 = qplot(replicate(10000,mean(rbeta(15,.02,.02))), xlim=c(0,1),binwidth=.05,main='n = 15')
n20 = qplot(replicate(10000,mean(rbeta(20,.02,.02))), xlim=c(0,1),binwidth=.05,main='n = 20')
n30 = qplot(replicate(10000,mean(rbeta(30,.02,.02))), xlim=c(0,1),binwidth=.05,main='n = 30')
grid.arrange(n1,n5,n10,n15,n20,n30)

# Note for n1, there should be equal numbers of observations at 0 and 1, but it looks odd due to wierdness of ggplot...

# And let's see how that looks in qq plot form:
par(mfrow = c(2,3))
for(n in c(1,5,10,15,20,30)) {
  samps = replicate(10000,mean(rbeta(n,.02,.02)))
  title = paste('n=',n)
  qqnorm(samps, main=title)
  qqline(samps)
}
par(mfrow = c(1,1))

# Notice how the distribution gets less jagged as the sample size increases

# And that's it for the central limit theorem simulations...
# Let's clean up
rm(lots.o.samples,samps,title,n,n1,n5,n10,n15,n20,n30)

#################################
# 5.4 - z-tests                 #
#################################

# Preamble: This makes sure I know what random numbers come out when you run the script
# That way I guarantee that I'm right when you run this script (at least on a mac)
set.seed(5391)

# So now what if we're only able to take one sample?
# We want to now say something about the population mean based on the sample
# Well, our best guess is going to be the mean of the sample
pop.sample = rnorm(20,mean = 1, sd = 3)
mean(pop.sample)

# It's not exact though...
# But we just saw that the mean of a sample follows a rational distribution
# It will be drawn from a normal distribution centered around the population mean
# And will have a standard deviation pop.sd / sqrt(n)

# So in this case, the mean of the sampling distribution will be 1
# And the standard deviation of the mean will be:
sd.mean = 3 / sqrt(20) # ~ .671
sd.mean

# So knowing the mean of the sample and its standard deviation, can we figure out the mean of the population?
# Well - to an extent
# Our best guess is the mean of the sample
# But we can define a confidence interval within the population falls with some probability
# So let's say we want to be 95% sure that the mean falls within a given interval

# First, let's recall that a known standard error is distributed normally
# So we want to find the 95% middle of the normal distribution
# This means from 2.5% of the CDF to 97.5% of the CDF
top.z = qnorm(.975)
bottom.z = qnorm(.025)
top.z
bottom.z

# Note that bottom.z is just negative top.z (since the normal is symmetrical)
# So we can just subtract top.z rather than adding bottom.z

# Thus the true population mean should be within -1.96 to 1.96 standard deviations of the population mean
top.range = mean(pop.sample) + top.z * sd.mean
bottom.range = mean(pop.sample) - top.z * sd.mean

# Note that 1 - the real mean - is within the range of the confidence interval
# (or at least there's a 95% chance it is...)
top.range
bottom.range

# You can do this with any range...
# Let's say you want to have a 99% confidence interval
# We'll call this having an alpha of 0.01 (one minus the confidence level)
# Alpha is the probability that the mean is NOT in the confidence interval
# In this case you want to split alpha equally to the top and the bottom
# So you can find the z-score by finding the part of the normal distribution below 1-alpha/2
z.99 = qnorm(1-(0.01/2))

# Now the top and bottom of the confidence interval can be calculated as:
top.range.99 = mean(pop.sample) + z.99 * sd.mean
bottom.range.99 = mean(pop.sample) - z.99 * sd.mean

# Now we can be pretty (99%) sure that the true population mean is in this range
top.range.99
bottom.range.99

# Or let's say we only need to be 80% sure
# That's an alpha of 0.2 (1 - 80%)
z.80 = qnorm(1-(0.2/2))
top.range.80 = mean(pop.sample) + z.80 * sd.mean
bottom.range.80 = mean(pop.sample) - z.80 * sd.mean

# Now we're pretty sure that the range contains 1, but less sure than before
top.range.80
bottom.range.80

# Note that the range get's smaller with higher alpha, and larger with lower alpha
# As we shrink our confidence interval, we become less sure that it contains the real mean
# And we can have more confidence, but the range that could contain the mean gets larger

# Next, let's ask whether we can safely say that the mean of the population is not zero
# We know a priori that it's not - it's 1 - but given our sample of 20 can we prove it?
# YES! With a z-test
# ... or at least we can be sure to a given alpha tolerance (e.g., .05)
# In a z-test, you want to know how many standard deviations from the null hypothesis your mean is
# This is calculated as (obs.mean - null.mean)/sd.mean
# Since null mean in this case is 0, we can determine the z-score as:
z = (mean(pop.sample) - 0)/sd.mean
z

# We then test where on the standard normal cdf the z-score lies
pnorm(z)

# This gives the probability that you would have gotten that mean OR LESS if the null hypothesis were true
# But we want to know what the probability of getting that mean OR GREATER is...
1 - pnorm(z)

# And since we're looking for a two-tailed test, and the normal is symmetric...
# We would require an alpha value of at least twice that value
# Thus the p value is equal to 2*(1-pnorm)
p.ztest = 2*(1-pnorm(z))
p.ztest

# Since this is greater than our chosen alpha (.05), we can't reject the null
# Remeber the lower bound of the 95% confidence interval?
bottom.range

# Notice it's lower than 0...
# If the CI ever includes the null hypothesis, you can't reject the null at that alpha value

# But what if we got more observations for greater power?
pop.sample.2 = rnorm(50,mean = 1, sd = 3)
mean(pop.sample.2)

# Then the sd of the mean would be tighter (because of the higher n)
sd.mean.2 = 3 / sqrt(50)
sd.mean.2

# And so the z score will be larger
z.2 = mean(pop.sample.2)/sd.mean.2
z.2

# Now we can test the p value again
p.ztest.2 = 2*(1-pnorm(z.2))
p.ztest.2

# This p is less than our designated alpha, so we CAN reject the null in this case

# And finally, there's a function in PSYC201 that makes this easy: z.test
# z.test() takes two required arguments:
# A vector of values to test, and the known standard deviation of the population (sigma)
# Remember that z-tests require knowing the population standard deviation, so you can't skip this step
# But it's easy to run (here we'll use the original pop.sample):
z.test(pop.sample,sigma = 3)

# Note that this gives the same statistics we had discussed earlier:
# The z-score and p-value are given at the top (ignore the df)
# The best estimate is given as the 'mean of x' at the bottom
# The bottom.range and top.range are contained in the confidence interval

# Now we can store this to a variable and pull numbers out of it (for homework)
zt.1 = z.test(pop.sample,sigma = 3)

# We can get out the p-value like with binomial tests:
get.p(zt.1)

# We can also get out the z-score (the test statistic)
get.stat(zt.1)

# And we can get out the confidence interval as a vector or [lower, upper]
get.ci(zt.1)

# Note that the confidence interval has an 'attr(,"conf.level")'
# This does nothing other than tell you what range of confidence you have
# You can still pull out the numbers with no issues, for instance grabbing the lower range is:
get.ci(zt.1)[1]

# All of the above get. functions are in the PSYC201 package, so don't try to use them without loading it
# ... or R will hate you

# You can also change the confidence level for the CI range
# For instance, getting a 99% confidence interval would be:
z.test(pop.sample,sigma = 3, conf.level = .99)

# Note that nothing changes except the confidence interval
# The p-value is exactly the same - the only difference is the level at which you accept or reject

# Finally, we have been testing whether our sample is different from 0
# But what if we wanted a difference from another number?
# For instance, we know that IQs are distributed with a mean of 100 and standard deviation of 15
# What if we want to test whether a sample of peoples' IQs are different from average?
# If we just use a regular z.test, we would find statistical significance in even the most average sample
# Because unless we're testing rocks, we will get IQs of greater than 0
# So if we had the following sample of 9 IQs:
samp.IQ = c(130,122,99,130,114,109,79,121,67)

# We can test using the basic z-test
z.test(samp.IQ,sigma=15)

# Well very clearly that's different from 0...
# But if we want to test whether it's different from the population mean (mu), we have a different z-statistic
# Remember, the z-statistic is (sample mean - null mean)/(standard error of the mean)
# Or:
z.IQ = (mean(samp.IQ) - 100) / (15 / sqrt(9))
z.IQ

# Then we calculate the p-value the same way as above
2*(1-pnorm(z.IQ))

# And now we see we don't have enough evidence that it is

# Or we can make this easy by giving z.test a 'mu' argument
z.test(samp.IQ, mu=100, sigma=15)

# You can see that the alternative hypothesis is a bit different now
# But otherwise, you can do the exact same get. functions to this test

# Let's clean our variables
rm(bottom.range,bottom.range.99,bottom.range.80,bottom.z,top.range,top.range.99)
rm(top.range.80,top.z,z.99,z.80, pop.sample,pop.sample.2,z,sd.mean,sd.mean.2,z.2)
rm(p.ztest,p.ztest.2,zt.1,samp.IQ,z.IQ)
par(mar = c(5,4,4,2) + .1)

