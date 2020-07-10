########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#          Lesson 16 - Robust Statistics               #
#                                                      #
########################################################

library(PSYC201)
library(ggplot2)
load(url('http://vulstats.ucsd.edu/RAssignData/L16_data.RData'))

##############################
# 16.1 - Rank correlation    #
##############################

# Let's start looking at robust statistics
# Robust statistics are used when you want to say that there's some sort of relationship,
# But you don't want to claim too much about that relationship

# The first thing we'll look at is rank correlations
# Let's look at the 'iqdat' data
# In this dataset, we're measuring how people of various IQs score on a cognitive test
iqdat

# Now we could ask, do smarter (higher IQ) people score higher?
# And we have a simple test for that:
summary(lm(Score ~ IQ, iqdat))

# And it has a high Pearson's correlation!
with(iqdat, cor(IQ, Score))

# So it seems that there's a relationship... but that is a test for a linear relationship
# Does it look linear?
qplot(IQ,Score,data=iqdat)

# Not really... low IQs score low, and high IQs score high... but there seems to be a ceiling and floor...
# So if we try to fit a linear trend, we do a poor job...
qplot(IQ,Score,data=iqdat) + geom_smooth(method=lm)
# If we want to know the correlation, therefore it would be inappropriate to use the regular correlation
# Instead, we should use Spearman's rho
# This is simply a correlation between the ranks of the variables
# So we could write:
with(iqdat,cor(rank(IQ),rank(Score)))

# Or just tell R we want Spearman's rho:
with(iqdat, cor(IQ,Score,method = 'spearman'))
# plot them out to compare 
qplot(rank(IQ),rank(Score),data=iqdat) + geom_smooth(method=lm)

# It's that simple

# But note that here the Pearson's correlation and Spearman's rho are similar...
# So why in general should we care about ranks rather than just use normal correlations?

# I have one example that comes from reviewing a paper
# (It's not good, but I don't remember the authors at this point, so don't ask)
# This paper measured how people made choices in a variety of situations
#  and assigned a choice metric to each of those situations to measure how often people choose a prefered vs. dispreferred simulus
# They then built a model that took into account intrinsic features of the situations
#  and used the model to calculate what the choice parameter "should" be
# The claim was that the model fit the data surprisingly well:
#  there was a correlation of 0.91 between model and observations!
# I have sample data similar to this in 'moddat'
with(moddat, cor(pred,obs))

# However, a quick look at the graph of this data showed a serious flaw...
# Take a look yourself:
qplot(pred, obs,data=moddat)

# It seems that not only does the model not follow a 1:1 line with observations (as it purported to do)
#  but the whole correlation was based on fitting just the lowest and highest points
# All of the middle observations are actually ANTI-correlated
# Testing Spearman's rho suggests a much worse fit:
with(moddat, cor(pred,obs,method='spearman'))

# You can see this is pretty likely to just be chance with permutation testing:
with(moddat, cor.test(pred,obs,method='spearman'))

# So if you're ever uncertain about the functional relationship of the data, calculate the rank correlation
# And if you're ever uncertain about the presence of outliers, calculate the rank correlation
# If you have a low Pearson correlation but high Spearman correlation, you may have missed structure in your data
# If you have a high Pearson correlation but low Spearman correlation, you may be relying too much on the ends of your scale

##############################
# 16.2 - Rank tests          #
##############################

# Let's next look at some of the statistical tests you might want to run on rank data
# We'll start by looking at the fooddat data
# Here we asked 50 people to rate how much they like brussel sprouts on a 0-100 scale
#  and another 50 to rate how much they like broccoli
# Next we can ask whether the general population prefers one of the two veggies

# We know t-tests... so this is easy:
t.test(fooddat$Brussel, fooddat$Broccoli)
# People like brussel sprouts more, right?

# Well, let's look at the data:
qplot(fooddat$Brussel,binwidth = 2)
qplot(fooddat$Broccoli,binwidth = 2)

# These don't look normal...
# It seems most people don't like brussel sprouts, but some people love 'em
# And most people are kinda okay with broccoli, but some people hate it
# Instead, we might want to ask whether in general people will rate one higher than the other
#  ignoring the various quirks of the distribution
# We might not want to ask whether the *average* rating is different (as a t-test does)
# But rather, in general would you get higher ratings from one of the two distributions (stochastic dominance)?

# For this, we can use a rank t-test: the wilcoxon test
# This is done with the wilcox.test() command that works very similarly to t.test
wilcox.test(fooddat$Brussel, fooddat$Broccoli)

# So we probably don't have evidence that there are differences in the population

# The nice thing about these tests is that you can store them to variables and get info from the common PSYC201 functions:
### CC NOTE ### This is the same input as t-test (t-test equvalent versoin of non-parametric)
wct = wilcox.test(fooddat$Brussel, fooddat$Broccoli)
wct
get.p(wct)
get.stat(wct) # rarely used
# However, get.ci and get.df don't work here - after all, you aren't calculating either

# If you actually do want a 95% CI on the difference, the wilcoxon test won't get you it
# The only way I know of doing this is through bootstrapping - which we will learn in the next lesson

# What about ANOVA though?
# Well, let's go back to the income/education data in incdat we talked about for log-transforms
# Remember, just running an ANOVA got us nothing:
summary(aov(Income~Education, incdat))

# This is because there are a number of high-income outliers
# By making the assumption that income was exponentially distributed, we could get a good fit
# But what if you didn't want to make that assumption?
# Well, you could always do rank ANOVAs
# This is accomplished by the Kruskal Wallis test
# Similar to the wilcoxon test, this tests whether one group stochastically dominates one of the other groups

# In R, this is written as kruskal.test()
# The input works very much like aov():
## CC NOTE ## This is 'one-way' ANOVA equivalent of non-parametric
kruskal.test(Income ~ Education, data=incdat)

# Now, while the input is like aov(), the output is much more sparse
# It doesn't help you with coefficients or any other sort of models
# So you can't run contrasts or Tukey HSDs within a rank ANOVA
# (you would have to do individual Wilcoxon tests with Bonferroni correction)

# But you can use the same basic pull-out functions on it:
kwt = kruskal.test(Income ~ Education, data=incdat)
get.p(kwt)
get.stat(kwt) # Gets the chi-square stat
get.df(kwt)

# However, note that the Kruskal Wallis test *requires* only a one-way ANOVA
# Unfortunately, the R command won't choke on multi-way ANOVAs
# Instead, it gives you the output of only the first factor
# So, for instance, R is okay with you typing:
kruskal.test(breaks ~ wool*tension, data=warpbreaks)

# But look at the degrees of freedom... just one?
# That's because this is identical to typing
kruskal.test(breaks ~ wool, data=warpbreaks)

# R looks at the formula as wool + tension + wool:tension
# Then sees wool is the first factor and just uses that

# Finally, you can run a within-subject ANOVA in a non-parametric way
# This is done using the Friedman rank test
# Let's try that test of veggie rankings again
# Except this time, we'll ask people about carrots too
# And we'll ask people to rate all three, rather than getting one score per person
# The data can be found in fooddat2
head(fooddat2)

# Now, when we look at the food data again, we still see violations of normality
qplot(Score,data=subset(fooddat2, Food=='Brussel'),binwidth=2)
qplot(Score,data=subset(fooddat2, Food=='Broccoli'),binwidth=2)
qplot(Score,data=subset(fooddat2, Food=='Carrot'),binwidth=2)

# So basic ANOVA won't work
# We can run the Kruskal Wallis test here though:
kruskal.test(Score ~ Food, fooddat2)

# And we don't find any differences
# But we're ignoring some structure in the data:
# Some subjects love veggies (S5) while some hate them (S2, S9)
with(fooddat2,tapply(Score,Subj,mean))

# We can account for this using the Friedman rank test
# The formula here is a bit odd, though slightly reminiscient of lmers...
# It is dependent ~ independent | subj
# Or here: Score ~ Food | Subj
# You can't use Error() in this test, since it doesn't need the structure Error allows...
# You *must* use a one-way, within subject design here
# More complex designs don't work

# So how does it do?
## CC NOTE ## This non-parametric lmer ## 
friedman.test(Score ~ Food | Subj, fooddat2)

# We found significance by respecting the structure of the data
# Note, however, it doesn't let us do contrasts or Tukey HSD either
# Nor can we get out coefficients or do the typical aov stuff

# We can pull out simple data with the PSYC201 package though:
fmt = friedman.test(Score ~ Food | Subj, fooddat2)
get.p(fmt)
get.stat(fmt)
get.df(fmt)

# And that's all you need to know about one-way rank tests
# You may ask though, what about two-way tests or more?
# Well, there aren't good analytic statistics for those...
# For those, we'll need to do randomization tests, which we will learn next time