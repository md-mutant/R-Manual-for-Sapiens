########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#                 Lesson 9 - ANOVA                     #
#                                                      #
########################################################

library(PSYC201)
library(plyr)
library(ggplot2)

##############################
# 9.1 - t-tests redux       #
##############################

# Recall our example two-sample t-test from earlier lessons:
# Let's say you want to know if happy music gives people better memory
# You segment 20 subjects each into two groups...
# The control group listens to neutral music then takes a memory test
# While the experimental group listens to happy music then takes a memory test
# You then get the following scores on the tests:
memory.exp

# We then tested to see if there was a difference between the two groups by category:
t.test(Score~Group, data=memory.exp, var.equal=T)

# But we can also do this test with lm() regression and indicator variables
# Let's first make a full data frame with all of this information
# We'll need to mark all control scores with 0 and experimental scores with 1
memory.exp$Ind.Cond = rep(c(0,1),each=20)
memory.exp

# Now we can run it like a normal regression:
tt.lm = lm(Score~Ind.Cond,data=memory.exp)
summary(tt.lm)

# Note that the t-statistics and p-values for the t.test and slope are identical
# Also note that the intercept is the same as 'mean of x'
# This is the mean when Ind.Cond==0, ie when in the control condition
# Then the intercept plus the slope is the same as 'mean of y'
# This is the mean when Ind.Cond==1, ie when in the experimental condition
# Basically, you're doing the exact same thing in both cases...
# This is why linear models are so powerful

# But often you won't have 0s and 1s to denote your conditions
# Instead you'll have categories - what do we do then?
head(memory.exp)

# Well thankfully, R makes this exceedingly easy
# So long as you have a factor, R will assign these indicator variables for you
tt.lm2 = lm(Score~Group,data=memory.exp)
summary(tt.lm2)

# And we have the exact same result!

# Note that we had to set var.equal to TRUE in our t.test to get the same answer though
# This is because of one of the assumptions of regression we've noted before:
# The error variance is the same at all points - this includes at 0 and 1 here
# There is not an unequal variance regression test

rm(tt.lm,tt.lm2)

##############################
# 9.2 - One-way ANOVA       #
##############################

# So far, we've just tested whether two groups are different from each other
# But what if we have more than one experimental group...
# and we want to know if there's ANY effect?
# For this we can still use indicator variables
# Let's see this in action:

# First let's get some data from the PSYC201 package
# Let's say we have two new drugs that are supposed to lower blood pressure
# So we give drug1 to 15 people, drug2 to 15, and leave 15 out as the control
# And let's also say that the mean blood pressure of normal people is 120
# Drug1 lowers blood pressure to 100, and drug2 does nothing
# (Those last two lines were how I made the data)

# We can find it in the bp.data dataset
head(bp.data)

# And we can split it out:
ctrl = subset(bp.data,Group=='Ctrl')$BP
drug1 = subset(bp.data,Group=='Drug1')$BP
drug2 = subset(bp.data,Group=='Drug2')$BP

# Now we want to ask... do the drugs have any effect?
# We can start with t-tests of the drug groups against the control group
# (Note we're still using equal variance because that is a necessary assumption of lm)
t.test(ctrl,drug1,var.equal=TRUE) # Close, but not a statistically significant effect
t.test(ctrl,drug2,var.equal=TRUE) # And not significant

# But there's some problems:

# First, there's no alpha control... you're running two tests with a 5% false positive rate
# This gives ~10% false positives...

# Second, both of these tests are independent, but the data isn't...
# We want to know whether there is any difference in the data, not just pairwise tests
# Also, you should note that all groups were drawn from populations with the same variance - an assumption of ANOVAs
# So if you believe this assumption, the measuring the Drug2 group still gives us some extra knowledge about the error
# But the t-tests just estimate variance using two groups

# So first let's set up indicator variables
# Here we'll need a separate variable for each treatment
# We can't use a single indicator variable - because the difference isn't necessarily related
# In each case, we set the value to 1 only if the group got the specific drug
bp.data$Ind.T1 = rep(c(0,1,0),each=15)
bp.data$Ind.T2 = rep(c(0,0,1),each=15)
bp.data

# And now we can throw it through our lm() function:
bp.lm = lm(BP ~ Ind.T1 + Ind.T2, data=bp.data)
summary(bp.lm)

# So what should we be looking at here?

# Let's go back to the comment that we want to look for ANY differences in the groups
# Oh, and we only want one test for alpha control

# There is part of the summary that solves both of these problems: the omnibus F-test
# This test asks whether we can use both Drug1 and Drug2 to predict BP better than the null mode
# Or in other words, does splitting into three groups do better at predicting BP than the mean alone?
# And even simpler: is there a statistically significant difference between the three groups?
# Now for the bonus: it's only one test, so we don't need to worry about alpha control!
summary(bp.lm) # F(2,42) = 3.7, p = 0.033

# So there is a difference in there!

# And here's another surprise for you: we just did a one-way ANOVA
# But there's an easier way to do this in R...
# Just like we used a two-level factor for the t-test above, we can use a three-level factor here
# Or in this case, the Group variable
bp.lm2 = lm(BP ~ Group, data=bp.data)
summary(bp.lm2)

# Same thing!

# Also, note that summary() gives individual coefficients...
# And those can be useful
# They are by default coded in the same way that we set up the indicator variables before
# So you see parameters for GroupDrug1 and GroupDrug2
# That's because the intercept is the mean of the Control group
# Then those slopes are the same as we calculated for the indicator variables above...
# GroupDrug1 is asking how the mean of the Drug1 Group differs from the intercept (here the control)
# And same for GroupDrug2

# But we also want to know specifically does the Group factor have an effect?
# And here's where the anova() command comes in handy:
anova(bp.lm2)

# This gives you the same F-test we've seen before - aggregated by the factor
# And while you could get the same information from the bottom of summary()...
# anova() will become much more important when we get into two-way ANOVAs
rm(ctrl,drug1,drug2,bp.lm)

##############################
# 9.3 - Two-way ANOVA        #
##############################

# Now we're going to talk a bit about weaving
# ... mostly because R has a built-in dataset on weaving to work with

# More importantly, we're going to talk about two-way ANVOAs

# Let's look at the dataset 'warpbreaks'
# This dataset describes the number of breaks per loom (a length of yarn)
# But there are two types of wool (A & B)
# And three different levels of tension (L, M, & H)
head(warpbreaks)

# We already know how to look at whether there is an effect of wool or tension alone on breaks:
anova(lm(breaks~wool,data=warpbreaks))
anova(lm(breaks~tension,data=warpbreaks))

# But we might want to know the effect of both of them at the same time
# You can probably guess how we add terms at a very basic level...
# Use the + operator to include more predictors
# Note, however, that this only gives main effects
wb.lm.main = lm(breaks ~ wool + tension, data=warpbreaks)

# Now let's look at what's there...
# First let's go to our workhorse so far... summary():
summary(wb.lm.main)

# Now what might we want to pull out of here?
# The answer is: not too much

# We in the past have looked at the p-values of the omnibus F and coefficients
# But here, they don't tell us much...
# The omnibus F tells us whether there is ANY effect at all...
# But it doesn't tell us where the effects are coming from
# For that, we used to look at the p-values of the coefficients...
# But for the two tension coefficients, the p-values don't tell us about the full effect of tension

# This is where the anova() function comes to shine
anova(wb.lm.main)

# Note that now we have it broken down by the factors, not the individual coefficients
# How do we read this?

# We go to the first line: 'wool'
# This is the F-test of including wool to explain breaks as compared to the null model
# (and we've seen these tests over and over)

# Next there is the line: 'tension'
# This is the F-test of the model with wool and tension versus just the model with wool

# But we've also talked about including interactions in two-way ANOVAs...
# To do this, we need a new operator within formulas: the colon (:)

# To denote the interaction between wool and tension, we would add wool:tension to the model
wb.lm = lm(breaks ~ wool + tension + wool:tension, data=warpbreaks)

# Now we can look at the effects of the terms:
anova(wb.lm)

# So now we have a significant effect of the interaction
# This is the effect of adding the interaction *after* accounting for both main effects

# But there's an even lazier way to do interactions:
# The asterisk (*) operator
# When you put an asterisk between two terms, it tells R:
# 'Include both main effects and the interaction'
# So we can rewrite the interaction lm() as:
wb.lm2 = lm(breaks ~ wool*tension, data=warpbreaks)
anova(wb.lm2)

# So how do we interpret what's going on here?
# The coefficients are not the easiest to interpret
summary(wb.lm)

# We could calculate model expectations from these coefficients, but it's inefficient
# For instance, to get the expected number of breaks for wool B at M tension, you would have to add:
#  1) The intercept
#  2) The main effect of wool B
#  3) The main effect of tension M
#  4) The interaction of woolB:tensionM

# But there's an even easier way...
# If you use a full factorial ANOVA, you'll be fitting all of the means in each condition exactly
# So you can visualize the model predictions by taking the means of your data in each condition:
wb.means = ddply(warpbreaks, c('wool','tension'),summarize,avgbreaks = mean(breaks))
qplot(tension,avgbreaks,group=wool,color=wool,geom='line',data=wb.means)

# From this we can see that *on average* wool A breaks more than B (the main effect of wool)
# And it also seems like *on average* there are fewer breaks with higher tension (main effect of tension)
# But there's also a modulation... there's not much difference between low and mid tensions for wool B, but a drop at high tensions
# Whereas mid and high tensions are similar for wool A, but many more breaks at low tension

rm(wb.lm.main, wb.lm2)

##############################
# 9.4 - The aov function     #
##############################

# There is another command that works in very much the same way as lm()
# It's the aov() function, and it has the exact same syntax
# Also, it builds the exact same model

# So we can try it with the warpbreaks data - this is the exact same model we used above:
wb.aov = aov(breaks ~ wool*tension, data = warpbreaks)

# So what's different?
# First, aov() is built to display things that are more appropriate for ANOVAs
# So just looking at an aov object will show you the sum-squares table instead of coefficients:
wb.lm
wb.aov

# This carries through to the summary
# Calling summary() on an aov object is like calling anova() on an lm object
summary(bp.aov)
anova(bp.lm)

# But other than that, the two are incredibly similar
# You can do everything to an aov object that you would to an lm object:
coef(wb.aov) # Get coefficients out
predict(wb.aov) # Use the predict function
resid(wb.aov) # Get residuals

# However, the PSYC201 functions give you slightly different output, just like summary works slightly differently
# While get.p() gets you the p values for the individual coefficients with lm objects, it gets you the factor p values with aov
get.p(wb.lm)
get.p(wb.aov)

# Likewise, get.stat() gives you the t-values for each coefficient with lm objects, but F-values for the factors with aov
get.stat(wb.lm)
get.stat(wb.aov)

# And get.df() gets you the degrees of freedom for the omnibus ANOVA with lm objects, but a matrix of individual DFR and DFE for aovs
get.df(wb.lm)
get.df(wb.aov)

# And so you might ask yourself - why would you ever want to use aov instead of lm (or vice versa)?
# Two reasons: presentation, and some differences in the underlying mathematics

# For presentation, note that everything in the aov objects is about the factor fits, not the coefficients
# This is because lm was designed for linear models in general, and with linear regression, coefficients are important
# aov, on the other hand, is designed as a type of lm that focuses on ANOVA
# In ANOVAs, the coefficients aren't as important as just finding effects of the factors
# And so aov downplays them

# In terms of the underlying mathematics, aov() lets you do a couple things that lm() does not (random effects and TukeyHSD)
# However, beyond that, aov and lm should be roughly identical

##############################
# 9.5 - lots-o-way ANOVAs    #
##############################

# So we just did two-way ANOVA...
# Let's go for three!

# For this we're going to use a new dataset from the PSYC201 package: puzzles
# In this example dataset, we want to test influences on creativity
# So we give people puzzles in one of 8 conditions, crossed by:
#  PuzzleType: Word or Visual puzzles
#  Emotion: Whether the participant was happy or sad
#  Difficulty: Whether the puzzle is easy or hard
# Finally, we measure how long it takes them to solve the puzzle in seconds (Time)

# So what causes people to get these puzzles faster or slower?

# First we need to define our model with full interactions
# And since we just learned AOV, we can use that
# This is pretty easy... we just use another asterisk to extend the two-way
puzz.aov = aov(Time ~ PuzzleType * Emotion * Difficulty,data=puzzles)
summary(puzz.aov)

# Okay - now we don't have a three-way interaction
# ... which is good
# This would imply that the interaction between PuzzleType:Emotion changes depending on the difficulty of the puzzle
# (or equivalently, that PuzzleType:Difficulty changes depending on Emotion, or Emotion:Difficulty by PuzzleType)
# Three-way interactions are hard to get the power to find, and are often very messy to interpret

# So what if we wanted to run the model without that term?
# Well, we could built a long, drawn out formula...
Time ~ PuzzleType + Emotion + Difficulty + PuzzleType:Emotion + PuzzleType:Difficulty + Emotion:Difficulty

# But instead let's be lazy... we can use the minus sign operator
# In formulas, this tells R to remove a term, so we can use it to get rid of the threeway term
puzz.aov.red = aov(Time ~ PuzzleType * Emotion * Difficulty - PuzzleType:Emotion:Difficulty,data=puzzles)
summary(puzz.aov.red)

# Note that when you take out the three-way interaction, the SS and MS of the terms stays the same
# But the F and p values change
# This is because with the three-way interaction can soak up variance
# Thus the SSE increases, but so do the error df... and thus MSE will be different (slightly lower in this case)
# Often with a non-significant interaction, this will not change values all that much though - because you get lower SS but also one less df

# So how do we visualize this to see what's going on here?
# Recall we find main effects, and an interaction between puzzle type and difficulty
# So the easiest way to look at this is by plotting the 2-way ANOVA, split by the third factor
# Since emotion is only a main effect, we'll split on that
puzz.means = ddply(puzzles,c('PuzzleType','Emotion','Difficulty'),summarize,avgtime=mean(Time))
qplot(PuzzleType,avgtime,group=Difficulty,color=Difficulty,geom='line',data=puzz.means,facets = ~ Emotion)

# And now we can see what's going on...
# Hard problems take more time, sad people take more time, and hard word problems have an additional cost

# But remember... real data will rarely be this nice and interpretable...

# And if you want more than a 3-way ANOVA, you can keep adding terms in the lm / aov
# ... but head the warning that this can get very complex and hard to interpret
# So keep your ANOVAs as simple as possible

##############################
# 9.6 - A note of warning    #
##############################

# A note of warning when you make or import your datasets
# Let's pretend that we measure shoe sizes by seating row, and we want to see people with different sized feet segregate by row

# The sample sizes for each row are:
ssize.1 = c(9,8,8,7,9) # Length 5
ssize.2 = c(10.5,12,6,10.5,10,10,9) # Length 7
ssize.3 = c(7,7.5,7,10,8.5) # Length 5
ssize.4 = c(10,8.5,11,10) # Length 4

# So we can make the total shoe size vector as:
ssize = c(ssize.1,ssize.2,ssize.3,ssize.4)

# And the row number as:
rown = rep(c(1,2,3,4),times=c(5,7,5,4))

# Thus our dataframe is:
ss.df = data.frame(size=ssize,row=rown)
ss.df

# Now let's build our ANOVA:
ss.lm = lm(size~row,data=ss.df)

# What do you think will happen when you run anova() on this lm?
anova(ss.lm)

# Do you notice anything funny about the numbers in the table?

# ... I'm putting some space so you don't just read ahead















# Why is there only one degree of freedom for row?
# There are four rows, which implies a need for three indicator variables...
# So it should be 3 degrees of freedom...

# Note something about the way we defined this data though:
str(ss.df)

# The row variable is a number
# So lm treats this like a single-variable linear regression
# Where the x-variable is 1, 2, 3, or 4 - a continuous variable rather than categorical

# If we want to run a real ANOVA, we need to ensure that the independent variable is categorical
# We can do this with the factor() command
ss.df$row.fact = factor(ss.df$row)

ss.df # Can't see much of a difference here...
str(ss.df) # But note that row.fact is represented differently by R

# Now we can run our ANOVA:
ss.lm2 = lm(size~row.fact,data=ss.df)

anova(ss.lm2) # Now this looks a decent ANOVA degrees of freedom!

# I've said this before, but be careful... if you import a dataset that uses numbers to represent categories...
# You will need to change that variable into a factor if you want to run ANOVAs

rm(ssize.1,ssize.2,ssize.3,ssize.4,rown,ss.df,ss.lm,ss.lm2,ssize)


