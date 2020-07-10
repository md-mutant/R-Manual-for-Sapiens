########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#       Lesson 15 - Linear Mixed Effect Models         #
#                                                      #
########################################################

library(PSYC201)
library(lme4)
library(arm) # There's a good chance you'll have to install.packages() this one
library(languageR) # And install this one as well
library(ggplot2)
library(gridExtra)

####################################
# 15.1 - Introduciton to lmer()    #
####################################

# In the past, we discussed random effects models using aov() with an Error term
# But these had a number of limitations
# For one, aov() doesn't play well with unbalanced designs
# For another, you can only use one Error term
# ... and whenever you have both Subject and Item effects, this means you're missing some structure

# To extend our analysis to these cases, we'll use the lmer() function from the lme4 package
# This function uses more generalized fitting techniques to fit these more complex models
# But it does come with a slightly different syntax you'll have to deal with

# To start, let's look at the 'wine' dataset from the PSYC201 package
# Recall, we had 6 judges taste 4 different wines
# And we wanted to know whether there were differences in wines, 
#  controlling for how much the judges like wine in general
head(wine)

# We modeled this using aov() as:
wine.aov = aov(Score ~ Wine + Error(Judge/Wine),wine)

# And found that we could partition variance to within- and between-subject
# Then we tested the effect of wine within subject
summary(wine.aov)

# Recall, when we do this, we are effectively taking out the mean of each judge's ratings out of their score
# And testing how the deviation of scores for each wine differed by mean

# We can write an equivalent analysis using lmer()
# Here, however, we have to use a different error term
# With aov, we have it Error(SUBJFACTOR / WITHINFACTORS)
# With lmer, we have to explicitly specify what we want to vary by subject
# And then we write (VARYING | SUBJFACTOR)

# Since we are just assuming the intercept changes by judge,
#  we write the error term as (1 | Judge)
# (Recall that 1 typically means with intercept, and 0 means without intercept)
# So we would write:
wine.lmer = lmer(Score ~ Wine + (1|Judge),wine)

# In this case, we are solving for the equation:
# y = b0 + b1*I_Wine2 + b2*I_Wine3 + b3*I_Wine4 + c_judge + e
# Here c is an error term that varies as a function of judge
# c should be normally distributed around 0 with some variance
# And the other terms are interpretable as before

# Now how do we interpret this?
summary(wine.lmer)

# Well, note this looks more like the output of lm than aov...
# You should recognize the residuals part
# And you should also recognize generally how to read the fixed effects, with the estimate, standard error, and t value

# But wait! Where's the p value?
# That's one important thing to note about lmers...
# There's no way of accurately calculating the degrees of freedom on the error
# (and on top of that, the author of the package is morally opposed to p values)
# So you generally have to ballpark significance

# One way to do this is to take things with t > 2 as roughly significant
# Another is to get the 95% confidence interval on the parameters with confint():
confint(wine.lmer)

# Things that don't include 0 are significant at an alpha=0.05 level

# The final way to do this is with the lmerTest package
# If you load this package after loading lmer, your summary will include p values
# Note, however, that this can add a ton of computation time, since it needs to do bootstrapping any time you run summary

# However, lmers are set up to provide estimates on coefficients, not just test for significance
# If all you're doing is finding a p-value, you're not using it to full effect

# Now before we get too far astray, let's look at the rest of the summary
summary(wine.lmer)

# Note that there are rondom effects between the residuals and fixed effects
# These can be thought of as similar to the aov strata
# So you have some variability due to judges, and some due to residuals
# We will get back to what this variability due to judges is
# But the Residual variability can be interpreted as MSE

# Finally, lmer calculates correlations between the fixed effects as well
# Usually you don't need to worry about these unless you have massive colinearity of your independent variables

# Now, let's talk about what the coefficients mean, and what predictions lmer makes
# The fixed effects are relatively straightforward...
# We can go back to the original specification of this function:

# y = b0 + b1*I_Wine2 + b2*I_Wine3 + b3*I_Wine4 + c_judge + e

# There is an intercept associated with Wine1, which is the average score for that wine
# And the other three slopes are offsets for the other wines from the intercept
# You can get fixed effects out of the lmer with the fixef() function:
fixef(wine.lmer)

# And these are going to get you back to the means of the actual wines:
with(wine,tapply(Score,Wine,mean))
with(wine,tapply(Score,Wine,mean)) - with(wine,tapply(Score,Wine,mean))[1]

# So that gets us through everything but our c_judge and e terms
# Now, summary gives us the fixed effects, but only variability in the random effects
# Let's pull those out with ranef():
ranef(wine.lmer)

# These should tell us how each individual judge varies from the average person
# So on average the deviation should be zero
mean(ranef(wine.lmer)$Judge[['(Intercept)']])

# (Note the complex sytnax comes because ranef returns a list of data frames)
# But there's something else you should note...
# This doesn't just give you the average offset of each judge... it gives you something slightly closer to zero
with(wine,tapply(Score,Judge,mean)) - mean(wine$Score)

# This is because lmer gives you a partial pooling model
# aov immediately segmented error (sum squares) into within and between and assumed equal error variance in the within grouping
# lmer pools across subjects and other error to come to its estimates
# Thus it will reduce the extremeness of some of the subjects further out, and attribute that to simple error variance

# I like to think of what lmer is doing here like estimating skill at darts
# Imagine you're watching a bunch of skilled players go for the bullseye
# Most of them will have errors on each of their shots, but will be pretty close to the center on average
# But one player gets both shots far off center... what do you make of this?
# On the one hand, you won't think that player is the best...
# On the other hand, it's possible that those were just two bad shots
# So you need to figure out how to attribute those shots to the player skill versus random error

# lmer() does this for you based on the amount of variability between and around subjects and fixed effects
# (I won't go into the fitting details too much... suffice to say that it generally works)
# So if Judge1 is really positive, some is probably because that Judge likes wine...
#  but some is because that Judge might have by chance given higher ratings on that day
# Rather than assuming that the Judge will always give 11 points higher on average
# lmer tries to tone that down and say that Judge is only 9.9 points higher on average

# Now this lets us fit a full model that depends on both Judge and Wine
# We can try to combine the fixed effects and random effects by hand
# But coef() makes this much easier:
coef(wine.lmer)

# Note this now has the wines varying in the same way for each judge
# But the intercept for each judge varies... just like we asked it to

# "Okay!" You might be saying, "How do we figure out if this model is worth anything?"
# With aov() objects, we just used summary to tell if there is an effect of wine
summary(wine.aov)
# Which was similar to using anova() on an lm object
# For the reasons we couldn't get p-values above, this also doesn't work with lmers:
anova(wine.lmer)

# So we get the same SS, MS, and F...
# But because df is hard to calculate, we can't get our p-value

# Instead, we need to do an explicit model calculation
# First, we ask ourselves what the base model is
# Well, in this case, we assume that the random effect of Judge still exists
# So we can form our base model:
wine.base = lmer(Score ~ 1 + (1|Judge),wine)

# Now the summary here doesn't tell us much, so I'll skip it
# Instead, we want to do a model comparison between the two
# And how have we always done model comparisons?
# With anova(mod.reduced, mod.full):
anova(wine.base,wine.lmer)

# This gives us a test to ask the question:
# Once I've accounted for Judge random intercepts, does Wine add anything to the model?
# Because of the Chisquare likelihood ratio test, we can answer this question

####################################
# 15.2 - Unbalanced designs        #
####################################

# Now what happens when you have an unbalanced design?
# Say that Judge 6 didn't try wine 3, and Judge 5 didn't try wine 4...
wine2 = wine[-c(18,23),]

# If we try to use an aov, well, bad things happen:
summary(aov(Score ~ Wine + Error(Judge/Wine),wine2))

# Even without Rs warning, you'll note that Wine shows up in two places - within and between subjects
# This is because two of the wines are sort of between subject (the wines not tried by one judge)
# But all the wines are also sort of within subject (there is always more than one judge trying every wine)
# So never use aov for unbalanced designs

# What about lmer though?
wine.lmer.2 = lmer(Score ~ Wine + (1|Judge), wine2)
summary(wine.lmer.2)

# Hey! Nothing looks broken!
# This is because of the way lmer fits parameters
# No observation of Wine 3 for Judge 6?
# No problem! Just fit Wine 3 from the rest of the judges and estimate the Judge 6 intercept from the rest of the wines!
# We can even predict what we would expect that Judge to rate the wine:
predict(wine.lmer.2, newdata=data.frame(Judge='Judge6',Wine='Wine3'))

# Which is not too far off from the actual rating of 3:
subset(wine,Judge=='Judge6'&Wine=='Wine3')

# So if you ever have data where you don't have every observation from a subject
# You don't need to throw that subject away... instead lmer can still fit the model ignoring that data

####################################
# 15.3 - Varying slopes            #
####################################

# So far, we have only looked at data where the intercept was allowed to vary
# And with good cause... we only had one observation for each wine for each judge
# But what if we are estimating parameters where we have enough observations to get a slope for each subject?

# In this next dataset from the lme4 package, we're studying the reaction times of truckers who don't sleep
# 18 truckers only got 3 hours of sleep a night for 10 days, and their reaction times (in ms) were measured
# Let's look at the sleepstudy dataset:
head(sleepstudy) # (Is there anything else we should check for here?)
qplot(Days,Reaction,color=Subject,group=Subject,data=sleepstudy,geom='line')

# So does sleeping make your reaction times worse?
# It kind of looks like it...
# But does it actually?

# Let's build a model to test it out!
# The first thing we're going to do is try it the same way we had it above... with varying intercepts
ss.varint = lmer(Reaction ~ Days + (1|Subject),sleepstudy)

# Now we can summarize this and everything should look relatively neat:
summary(ss.varint)

# But is this describing the data as well as it could?
# Let's plot it

# For this, we'll need to use the fortify() function
# fortify() takes a linear model (lm, glm, or lmer) and returns the data with fits and residuals
head(fortify(ss.varint))

# We'll use this so we can plot fitted lines:
ggplot(fortify(ss.varint), aes(x=Days,y=Reaction)) + geom_point() + geom_line(aes(y=.fitted)) +
  theme(aspect.ratio=1) + facet_wrap(~Subject)

# This looks *okay*...
# But you'll notice that the lines don't always follow the points...
# For instance, see subject 335, who has the biggest deviation from the slope
# But it's not great elsewhere

# This is because we're assuming there is only a single slope that all subjects share
# But it looks like that's not the case...
# Some people are really hurt by lack of sleep
# While some people (like subject 335) are barely affected

# So we might want to take into account that the impact of sleep on reaction might vary by person
# To do this, we will allow for "varying slopes"
# Now we won't use 1 in our random effects... we'll use Days
# (Note this is equivalent to 1+Days, which means varying intercept and slope... you may see this syntax elsewhere)
ss.lmer = lmer(Reaction ~ Days+(Days|Subject),sleepstudy)

# How does the summary look?
summary(ss.lmer)

# Still reasonable...
# What about the plot?
ggplot(fortify(ss.lmer), aes(x=Days,y=Reaction)) + geom_point() + geom_line(aes(y=.fitted)) +
  theme(aspect.ratio=1) + facet_wrap(~Subject)

# Those lines seem to fit much better!
# But also note another thing...
# If you squint, you can see that the slopes are a little shallower than might be expected otherwise...
# We'll take a look specifically at subject 308
# The black line will be the lmer fit, the blue line a simple linear fit only to that data
ggplot(subset(fortify(ss.lmer),Subject=='308'), aes(x=Days,y=Reaction)) + geom_point() + 
  geom_line(aes(y=.fitted)) + geom_smooth(method='lm',se=F) + theme(aspect.ratio=1)

# If we do the same thing for subject 335, we find the lmer slope is closer to 0 than the fitted slope:
ggplot(subset(fortify(ss.lmer),Subject=='335'), aes(x=Days,y=Reaction)) + geom_point() + 
  geom_line(aes(y=.fitted)) + geom_smooth(method='lm',se=F) + theme(aspect.ratio=1)

# So what's going on?
# Just like in the wine example, lmer is doing partial pooling
# It's trying to figure out how much of the increase or decrease is due to that individual subject
#  and how much is due to the fact that sometimes you'll just by chance have a steep or shallow slope
# So the slopes themselves will be drawn towards the fixed effect slope

# And just like before, we can pull out the coefficients associated with each subject:
coef(ss.lmer)

# Note that here, the Days slope does change by subject, as opposed to just the intercept in the wine example

# But do we do any better with varying slopes?
# We can test this, again using anova()
# Here, the model with only varying intercepts is smaller than the full model, so we can say:
anova(ss.varint,ss.lmer)

# Again, our deviance and chisquare likelihood test tells us that we do better job explaining the data with varying slopes
# (Note the first 'refitting' warning... 
#  you will sometimes get that, but it has to do with the way parameters are estimated, and lmer usually does the right thing)

# Note something funny though...
# If you run the summary of both of those models, the t-value for Days goes *down* with the varying slopes:
summary(ss.varint)
summary(ss.lmer)

# Why is that the case?
# Because you're asking different questions with each of those two tests, which lead to different conclusions

# With the varying intercept model, we should be used to the interpretation
# This is asking whether adding the Days slope would add explanatory power to a model that
#  otherwise only allowed people to vary in their mean reaction time
# Clearly, there is a relationship between Days and Reaction, so this holds

# With the full model, the interpretation gets slightly more complex
# This is comparing adding Days as a fixed effect to a model that already allows Days to vary as a random effect
# In essence, this is saying "Yes, people vary in their slopes over Days, but is the average slope different than 0?"
# Thus there's more noise in estimating this fixed effect, so a higher standard error, and so a lower t

# In effect, if you're asking whether the Days slope in the full model is different from 0, you're running this test:
ss.nofix = lmer(Reaction ~ 1 + (Days|Subject),sleepstudy)
anova(ss.nofix,ss.lmer)

# So, this gets us to the overall test question - does Days have any effect in the full model?
# Now you have to decide what your null hypothesis is:
# 1) There is no effect of Days on Reaction, and any differences are just sampling error
#    (However, people can vary in how fast/slow they are overall)
# 2) Some people will be faster or slower as they go on with little sleep (Days increases)
#    but on average throughout the population, there will be no effect of Days

# If you chose (2), we ran the test just above
# If you chose (1), you'll need to specify a base model without any Days:
ss.base = lmer(Reaction ~ 1 + (1|Subject),sleepstudy)
anova(ss.base,ss.lmer)

# This is the tricky part about hypothesis tests with lmers...
# You have to specify what your null hypothesis is - it doesn't come baked in like in many other models
# Thus you have more control over your tests, but also must think more about what exactly you're testing

# Finally, we may want to look at how people vary
# We can do this by plotting the confidence intervals on each persons' coefficients
# The easiest way to get these CIs out is through the se.ranef function in the arm package
se.ranef(ss.lmer)

# And we can bind this with the coefficients as such:
coef.pltdat = cbind(coef(ss.lmer)$Subject,se.ranef(ss.lmer)$Subject)
names(coef.pltdat) = c('b0','b1','b0.se','b1.se')
coef.pltdat$Subject = rownames(coef.pltdat)

# Now we have a data frame with the individual subject coefficients and standard errors
# So plotting them should be easy:
(int.plt = ggplot(coef.pltdat,aes(x=Subject,y=b0,ymin=b0-2*b0.se,ymax=b0+2*b0.se)) + geom_pointrange() +
   geom_hline(y=fixef(ss.lmer)[1],linetype='dashed'))
(sl.plt = ggplot(coef.pltdat,aes(x=Subject,y=b1,ymin=b1-2*b1.se,ymax=b1+2*b1.se)) + geom_pointrange() +
   geom_hline(y=fixef(ss.lmer)[2],linetype='dashed'))

# Or together:
grid.arrange(int.plt,sl.plt)

# From this we can tell that there's a decent spread of variability in both the intercept and slope (justifying our model)

####################################
# 15.4 - Multiple random effects   #
####################################

# We're next going to talk about multiple random effects in the domain where this first gained traction: psycholinguistics
# For this, we'll use the lexdec data from the languageR package
# This is about lexical decisions (word vs nonword) made for concrete English words by native and non-native speakers
head(lexdec)

# We might want to ask whether people slow down after an error
# And also whether this might cause more dissonance (longer slowing) in non-native speakers
# So we care about the following parts of the data set:
#  Subject: the subject doing the reading
#  RT: log-transformed reaction times for the lexical decision task
#  NativeLanguage: whether the native language is English or Other
#  PrevCorrect: whether the subject got the previous lexical decision correct or not
#  Word: a factor indicating which word the decision was made on

# So first we might want to visualize the data... can we see an obvious difference?
qplot(NativeLanguage:PrevCorrect,RT,geom='boxplot',data=lexdec)

# Hmm... looks like there isn't a huge difference
# So we'll need to turn to modeling

# Now let's put this into an lmer
# The fixed effects are easy...
# We want to look at the main effects and interactions for NativeLanguage and PrevCorrect
# But what random effects do we want?
# It should be apparent that we need to account for both Subject and Word...
# Since some subjects will just be faster or slower
# And some words might be easier or harder to make decisions about

# But we don't want to just limit ourselves to just a single intercept offset for subject and word...
# What if some subjects are more or less affected after a wrong decision?
# And what if some words are more difficult for non-native speakers?
# So we will want to try the following random effects:
#  (1|Subject)
#  (1|Subject:PrevCorrect)
#  (1|Word)
#  (1|Word:PrevCorrect)
#  (1|Word:NativeLanguage)
#  (1|Word:PrevCorrect:NativeLanguage)

# Question: why do we not have random effect of (1|Subject:NativeLanguage)?

# So now we can make our full model:
ld.all = lmer(RT ~ NativeLanguage*PrevCorrect + (1|Subject) + (1|Subject:PrevCorrect) + (1|Word) + 
                (1|Word:NativeLanguage) + (1|Word:PrevCorrect) + (1|Word:PrevCorrect:NativeLanguage),data=lexdec)

# What does this tell us?
summary(ld.all)

# The first thing to notice is that there is almost no variance attributed to the Word:PrevCorrect or Subject:PrevCorrect random effects
# This might be because there aren't all that many incorrect previous answers for each word...
with(lexdec,table(Word, PrevCorrect))

# So we can probably take these out, since these terms don't add anything:
ld.lmer = lmer(RT ~ NativeLanguage*PrevCorrect + (1|Subject) + (1|Word) + (1|Word:NativeLanguage),data=lexdec)
summary(ld.lmer)

# Note that not much changed when we dropped the extra terms
# And you get no additional explanatory power from those terms:
anova(ld.lmer,ld.all)

# How about the rest of the mixed effects terms?
# Should you allow words to vary? Test against only subject random effects:
ld.noword = lmer(RT ~ NativeLanguage*PrevCorrect + (1|Subject),data=lexdec)
anova(ld.noword,ld.lmer)

# Yep - allowing words to vary is good
# How about subjects? Now with only words:
ld.nosubj = lmer(RT ~ NativeLanguage*PrevCorrect + (1|Word)+(1|Word:NativeLanguage),data=lexdec)
anova(ld.nosubj,ld.lmer)

# Oh yeah... need to keep subjects too

# Okay, now we can start testing our questions...
# Is there a difference in how getting the prior decision wrong affect English and Other native speakers?
# This is a question about the intercept, so we need to make a null hypothesis model without the interaction:
ld.noint = lmer(RT ~ NativeLanguage+PrevCorrect + (1|Subject) + (1|Word) + 
                  (1|Word:NativeLanguage),data=lexdec)

# And test whether there is a statistical difference with the interaction:
anova(ld.noint,ld.lmer)

# Hmmm... not much here. If there is a difference, it's going to be very small
# So our best explanation of the data at this point is the no-interaction model

# What about whether getting the past question wrong slows you down?
# Qualitatively, the 'PrevCorrectincorrect' slope is positive, so in the 'incorrect' condition people get slower:
fixef(ld.noint)

# But this coefficient is small... does it add explanatory power?
# We need to make a base model without that term to test:
ld.nlonly = lmer(RT ~ NativeLanguage + (1|Subject) + (1|Word) + (1|Word:NativeLanguage),data=lexdec)
anova(ld.nlonly,ld.noint)

# So yes - we have some evidence that we do better by assuming a slowing after wrong answers
# (Though it's a small effect)

# How about whether non-native speakers are slower?
# Again, we have our null model without NativeLanguage
# Note that we also want to lose the (1|Word:NativeLanguage) random effect
# Because we shouldn't expect that non-native speakers would have a different response to words
# If it did, then there would be some effect of Native Language
ld.pconly = lmer(RT ~ PrevCorrect + (1|Subject) + (1|Word),data=lexdec)
anova(ld.pconly,ld.noint)

# Okay, so native language has explanatory power as well
# Which means that we have our best model of log reaction times as the no interaction model
# But just saying that we have main effects and no interaction is just the base - what does this model tell us?

# What might we want to ask?
# There are four relatively straightforward questions we might want to ask:
# 1) What is the relationship between native speaking, prior correctness, and RT for any given person or word?
# 2) What is that relationship for one of the subjects (regardless of word)
# 3) What is that relationship for one of the words in the study (regardless of person)
# 4) What is our prediction about that relationship for a given Subject-Word combination

#############
# 1) What is the relationship between native speaking, prior correctness, and RT for any given person or word?
# This is by far the easiest question to ask
# The reason we added random effects is because we assumed there would be some variability in the population and by word
# But that variability should be centered around the fixed effects
# So if we have no clue what the person or word is, all we can do is use the fixed effects:
fixef(ld.noint)

# Thus our equation is:
#  RT.hat = 6.317 + 0.153*NL.Other + 0.037*PC.incorrect

############
# 2) What is that relationship for one of the subjects (regardless of word)
# For this we can pull out the random effects as we did before
# (I'm only pulling out a few for visibility)
lapply(ranef(ld.noint),head)

# Hmmm.... unlike before, we now have three different frames of coefficients...
# How do we interpret these?

# Let's start with the Subject random effects
# Remember, random effects are used when you are assuming you're sampling at random from possible people/words
# When we look at Subject, we can ask how subjects differ, without regards to Word
# So the Subject intercepts tell us how much slower or faster each subject would be from the associated fixed effect
# But this is also saying on average, across all possible words...
# So we can ignore the Word and Word:NativeLanguage random effects

# Let's plug this back into the equation for Subject A1
# The intercept is the fixed effect plus the random effect offset:
fixef(ld.noint)[1] + ranef(ld.noint)$Subject['A1',]

# Then the slope for NativeLanguage shouldn't matter since A1 is a native speaker
# But logRT will increase if the last word was incorrect
fixef(ld.noint)[3]

# So the full equation for A1 would be:
#  RT.hat = 6.279 + 0.037*PrevCorrect.incorrect

# How about A3?
# Well... A3 doesn't speak English natively...
head(subset(lexdec,Subject=='A3'))

# So we still have the intercept plus the random effect offset:
fixef(ld.noint)[1] + ranef(ld.noint)$Subject['A3',]

# But then we have to add in the NativeLanguage.Other fixed effect
# Since A3 will *always* have that designation:
fixef(ld.noint)[1] + ranef(ld.noint)$Subject['A3',] + fixef(ld.noint)[2]

# And then we have the same slope for PrevCorrect.incorrect:
#  RT.hat = 6.400 + 0.037*PrevCorrect.incorrect

# So this lets us go back and ask how we would expect our subject to make a decision about any new (unknown) word
# But what if we care about how fast some random person will make a decision about one of the words in the study?

#############
# 3) What is that relationship for one of the words in the study (regardless of person)

# Here we need to gloss over subject random effects, and use word random effects
# Let's go back to our ranef structure:
lapply(ranef(ld.noint),head)

# Now we can ignore the Subject, but we have to worry about the Word
# Can we ask how long it should take someone to make a lexical decision on 'almond'?

# Note that there are three random effects we need to take into account for this one word:
# The almond effect from Word
# And the almond:English and almond:Other for Word:NativeLanguage

# Let's start by asking what we would expect if almond was said by a native English speaker
# We have to start with the intercept and 'almond' Word effect:
fixef(ld.noint)[1]+ranef(ld.noint)$Word['almond',]

# But on top of this, because it's a native English speaker, we need to account for the almond:English random effect:
fixef(ld.noint)[1]+ranef(ld.noint)$Word['almond',]+ranef(ld.noint)$`Word:NativeLanguage`['almond:English',]

# And then since we have a native English speaker, we only need to worry about the prior correct response:
#  RT.hat = 6.340 + 0.037*PrevCorrect.incorrect

# What if we have a non-native English speaker?
# Then we have to account for the almond:Other random effect instead...
# But we also need to add in the fixed effect of Other speakers:
fixef(ld.noint)[1]+ranef(ld.noint)$Word['almond',]+
  ranef(ld.noint)$`Word:NativeLanguage`['almond:Other',]+
  fixef(ld.noint)[2]

# So this would be:
#  RT.hat = 6.528 + 0.037*PrevCorrect.incorrect

# So if we wanted to make it more general, we could take the difference between intercepts as the effect of Other language:
#  RT.hat = 6.340 + 0.188*NL.Other + 0.037*PC.incorrect

# To make this more general, the intercept is always accounting for the random effect of the base condition (English):
fixef(ld.noint)[1]+ranef(ld.noint)$Word['almond',]+ranef(ld.noint)$`Word:NativeLanguage`['almond:English',]

# And the slope for NL.other is the fixed effect plus the difference between the random effects for Word:NativeLanguage:
fixef(ld.noint)[2]+ranef(ld.noint)$`Word:NativeLanguage`['almond:Other',]-
  ranef(ld.noint)$`Word:NativeLanguage`['almond:English',]

# Is this slightly confusing? Well, yes... but there's not an easier way to get the equation out
# We'll come back to this if we have specific predictions though

################
# 4) What is our prediction about that relationship for a given Subject-Word combination

# Now we can ask, how fast would we expect A1 to make a lexical decision on almond?
# Here now we need *all* of the random effects
# We know that A1 is a native English speaker though, so that makes it slightly easier...
# We can add up all the appropriate random effects and ignore the NL.other term
# So the intercept is:
fixef(ld.noint)[1]+
  ranef(ld.noint)$Subject['A1',]+
  ranef(ld.noint)$Word['almond',]+
  ranef(ld.noint)$`Word:NativeLanguage`['almond:English',]

# And thus our equation is:
#  RT.hat = 6.302 + 0.037*PC.incorrect

# But let's step back a second... what if we just want to predict log-RT for something we know?
# For instance, let's assume that A1 just made a decision about almond after getting the last question correct
# Well, for this we have the predict function... and it works just like we have used it before
# Give it the lmer object and a new data frame with your new guy, and it gives you back a prediction:
# The only concern is you need to make sure that the factors retain the original levels:
ndat = data.frame(Subject=factor('A1',levels=levels(lexdec$Subject)),
                  Word=factor('almond',levels=levels(lexdec$Word)),
                  NativeLanguage=factor('English',levels=levels(lexdec$NativeLanguage)),
                  PrevCorrect=factor('correct',levels=levels(lexdec$PrevCorrect)))
ndat
predict(ld.noint,newdata=ndat)

# Hey - that's what we got before!

# But we can make this even more general
# What if we want to know how a non-native English speaker would do with 'almond'
# Here this isn't one of our subjects... so we want to ignore the random effects
# To do this we have to make a newdata frame...
# And we need to give it junk input for the Subject:
ndat = data.frame(Subject='Argle',
                  Word=factor('almond',levels=levels(lexdec$Word)),
                  NativeLanguage=factor('English',levels=levels(lexdec$NativeLanguage)),
                  PrevCorrect=factor('correct',levels=levels(lexdec$PrevCorrect)))

# And then we give it to predict, but also supply the argument allow.new.levels:
predict(ld.noint,newdata=ndat,allow.new.levels=T)

# And we can do the same for A1 making a decision about an unknown word:
ndat = data.frame(Subject=factor('A1',levels=levels(lexdec$Subject)),
                  Word='Bargle',
                  NativeLanguage=factor('English',levels=levels(lexdec$NativeLanguage)),
                  PrevCorrect=factor('correct',levels=levels(lexdec$PrevCorrect)))
predict(ld.noint,newdata=ndat,allow.new.levels=T)

# Or both:
ndat = data.frame(Subject='Argle',
                  Word='Bargle',
                  NativeLanguage=factor('English',levels=levels(lexdec$NativeLanguage)),
                  PrevCorrect=factor('correct',levels=levels(lexdec$PrevCorrect)))
predict(ld.noint,newdata=ndat,allow.new.levels=T)

# And that's how to interpret fixed and random effects

####################################
# 15.5 - Convergence errors        #
####################################

# If you use lmer a lot, you'll likely run into convergence issues
# You'll notice, because R will give you a warning along the lines of:
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
## control$checkConv, : Model failed to converge with max|grad| = 1.52673
## (tol = 0.001, component 17)

# These problems usually occur when you have a complex model with lots of effects
# The issue is that it's hard to find the maximum likelihood when there are a lot of parameters to estimate
# (Ed mentioned the black box of optimization with mle... this is one of the tricky parts about that black box)
# When R gives you this warning, it knows that it's not stopping at the maximum likelihood...
# But it also doesn't know how to get to the maximum likelihood
# So you will get some parameters, but they won't necessarily be the best

# Then what can you do?
# The first thing to do is center your estimates
# (e.g., x = x-mean(x) )
# And rescale them to be not huge or tiny numbers
# Sometimes having off center estimates can cause some wonkiness with the function

# If that doesn't work, look at the variance estimates of the random effects from summary()
# If you have some random effects that give you 0 variance, take them out
# There can be issues where the function really wants to use negative variance, but that's just not allowed
# So it gives you a warning instead
# (Note this doesn't always happen - we saw an instance where lmer converged with 0 variance - but can sometimes be the cause)

# You can try to simplify your model in other ways...
# Have lots of crossed effects and random slopes? Cut one or two!
# Again, errors can come from trying to fit too many parameters
# Since lmer tries to fit correlations as well, cutting a couple terms can greatly simplify your model
# And that might make it possible to be fit

# If none of that works, there are some other things you can try...
# e.g., manually checking for convergence, using other optimizers
# These are more advanced tricks though
# The solutions are easily found using Google
# But they're open ended enough that you should be able to understand those websites before you use those techniques
# Since there's a lot of linear algebra and optimization that needs to be used to understand it
# So be careful if you go this route