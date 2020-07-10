########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#     Lesson 11 - Contrasts & Multiple Correction      #
#                                                      #
########################################################

library(PSYC201)

##############################
# 11.1 - Contrasts           #
##############################

# Let's go back to our blood pressure example from last week
# We had two drugs to lower drug pressure, and compared their effects against a control group
head(bp.data)

# And pull out the individual vectors (we'll use these later)
ctrl = subset(bp.data,Group == 'Ctrl')$BP
drug1 = subset(bp.data,Group == 'Drug1')$BP
drug2 = subset(bp.data,Group == 'Drug2')$BP

# Then we ran a one-way ANOVA on it
bp.lm = lm(BP ~ Group, data=bp.data)
summary(bp.lm)

# Now let's say we want to test specific questions, such as:
# a) Is there a difference between the control group and Drug1 group?
# b) Is there a difference between the two drug groups?
# c) Is there a difference between the control group and both drug groups combined?

# Note that questions a & b could be answered by t-tests, but c cannot

# So let's start with question a:
# Recall that if we t-test, we don't get a significant result:
t.test(ctrl,drug1,var.equal=TRUE)

# However, let's return to the summary:
summary(bp.lm)

# Now recall what each of the coefficients means
# The intercept is the mean value of the control group
# While the GroupDrug1 value is how different the mean of Drug1 group is from the control group
# Thus if it is statistically significant, there should be a significant effect of Drug1
# And it is... but this is different from the t-test... why?

# The intuitive explanation is that with this test, you are still using some information from the Drug2 group
# Specifically, in ANOVAs you are assuming that all groups have equal variance
# And therefore, you get a better estimate of the variance using all three groups rather than only two from the t-test

# Mathematically, we can show this by defining this slope test as a contrast
# Let's figure out which contrasts we need to set...
# For this we use the levels() function
levels(bp.data$Group)

# This means that the control group is the first item, drug1 is the second, and drug2 the third
# So our question is about the contrast [ 1, -1, 0 ] - are the first and second factors different?
# Therefore, we can define the L value of this contrast as:
L.contrA = mean(ctrl) - mean(drug1)
L.contrA # Note this is the (negative) slope value

# Then the sd of this value is sqrt(MSE*sum(c.i^2 / n.i))
# Now the easiest way to pull out sqrt(MSE) is:
bp.lm.resse = summary(bp.lm)$sigma
bp.lm.resse # Should be 20.65 - same as the residual standard error from summary()

# And so:
s.contrA = bp.lm.resse*sqrt(1/length(ctrl) + 1/length(drug1))
s.contrA

# Now we can get our t-statistic for this contrast:
t.contrA = L.contrA/s.contrA
t.contrA

# And therefore the p-value (using N.total - p degrees of freedom):
(1-pt(t.contrA,45-3))*2

# Note first that the L value is the negative estimate of the GroupDrug1 slope,
# the s value is the standard error, the t-value is the negative t, and the p-value matches up

# Note next that this is pretty similar to doing a t-test, with two important differences:
# 1) Instead of s.pooled, we used the residual standard error (accounting for variance of all observations)
# 2) We had more degrees of freedom, since we estimated error off of all groups

# So if we want to look at the contrast of the control and drug1, we can go to the summary
# But importantly, this is ONLY because of the way R builds up its coefficients by default
# And we got lucky that the control group was set as the intercept
# (Because C comes before D alphabetically, it gets the first position when R builds its factors)
# If the default ever gets set differently, you can't do this

# So lets move on to contrast (b): do the drug groups differ?
# Well, we want to know is the GroupDrug1 slope different from the GroupDrug2 slope...
# But there's no easy way of seeing this in summary()...

# Instead, let's make another contrast: [ 0, 1, -1]
L.contrB = mean(drug1) - mean(drug2)
L.contrB

s.contrB = bp.lm.resse*sqrt(1/length(drug1) + 1/length(drug2))
s.contrB

t.contrB = L.contrB/s.contrB
t.contrB

pt(t.contrB,42)*2

# Okay... that wasn't too painful... but can we make it easier?
# Yes we can!

# In the PSYC201 package, there is a function called 'contrast'
# (This is different from the 'contrasts' function that is used in building lm objects - we won't get into that one, just be careful)

# It works by taking two arguments: contrast(lm.obj, contr)
#  lm.obj: The stored lm object you want to run the contrasts on
#  contr: The specific contrast you want to run as a vector like we defined above

# In the case of the latest contrast, we would use:
contrast(bp.lm,c(0,1,-1))

# Note how that matches with everything we've calculated before

# Now let's go back to question 3: comparing the control to the average of the two drugs
# For this we want to give the control a c value of 1, and each drug a c value of -0.5

# But how do you know what the order of these three numbers should be for the vector?
# There are two ways:

# 1) Use the levels() command on the independent variable to figure out the order of the vector
levels(bp.data$Group)

# This means that the contrast vector should include the c values for control, drug1, and drug2, in that order
# So to compare the control to the average of the two drugs, for instance, it would be c(1,-.5,-.5)
contrast(bp.lm,c(1,-.5,-.5))

# 2) Use the getContrastMatrix() command on the lm object (note - this is also from the PSYC201 package)
getContrastMatrix(bp.lm)

# Now you can either recreate the vector as above (noting the ctrl, drug1, drug2 order)
# Or store that to a variable and change the values, then pass it back
# For instance:
samp.contr = getContrastMatrix(bp.lm)
samp.contr[1] = 1
samp.contr[2:3] = -.5
contrast(bp.lm,samp.contr)

# Note that the output of the contrast is just a vector
# You can store it to a variable
contr.out = contrast(bp.lm,samp.contr)

# Then pull out the L value
contr.out[1]

# ... the se of that value
contr.out[2]

# ... the t-value for the test
contr.out[3]

# ... and finally the p value
contr.out[4]

# You might notice that there's one contrast I didn't mention: Drug1 vs. Ctrl & Drug2
# But if you look at the coefficients, it seems like the obvious one...
# This gets back to the fundamental question of alpha control

# If you run an ANOVA with a lot of factors, just by chance, some factors will differ from others
# Let's say I have 10 levels of a factor, and t-tested each of them against each other... that would be 45 tests
# Just by chance, you'd expect about 2 of those tests to have a p-value of less than 0.05
# By choosing only the best looking tests, you effectively are running every test first, then getting p-values
# So we'll need better ways of alpha-control...

rm(ctrl,drug1,drug2,bp.lm.resse,L.contrA,L.contrB,s.contrA,s.contrB,
   t.contrA,t.contrB,contr.out,samp.contr)

##############################
# 11.2 - Two-way contrasts   #
##############################

# Now what if we want to run contrasts on two-way ANOVAs?

# For instance, let's look back at the warpbreaks data
# Here we are looking at number of breaks as a function of wool and tension
wb.lm = lm(breaks~wool*tension,data=warpbreaks)

# And say we a priori believe that wool A will break more than B at low tension
# But breaks less than B at medium or high tension
# We can define the contrast as:
ctr = rbind(c(1/2,-1/4,-1/4),c(-1/2,1/4,1/4))
ctr

# Now we use the contrasts function in the same way
contrast(wb.lm,ctr)

# Note that the contrast becomes normalized so positive and negative cs sum to 1
# So the contrasts would be the same if we entered:
ctr = rbind(c(2,-1,-1),c(-2,1,1))
contrast(wb.lm,ctr)

# But how did I know to shape the contrast matrix this way?
# Here again the getContrastMatrix function is helpful - and labels each of the cells too!
getContrastMatrix(wb.lm)

# So we can store that to a variable and fill it in
ctr2 = getContrastMatrix(wb.lm)
ctr2[1,1] = 2
ctr2[1,2] = -1
ctr2[1,3] = -1
ctr2[2,1] = -2
ctr2[2,2] = 1
ctr2[2,3] = 1
contrast(wb.lm,ctr2)

# These functions also work for three and more way ANOVAs
# However, note that they do not work with repeated measures models or ANCOVA models
# (The math & programming for this get complex)

rm(wb.lm,ctr,ctr2)

##############################
# 11.3 - Multiple correction #
##############################

# Now we're going to talk about multiple comparisions
# In many cases, when you are running contrasts you will be doing multiple tests between factors
# But remember that in NHST, there's always a 5% false alarm rate
# So if you run two contrasts, you're about 10% likely to get a false alarm
# And if you have a few groups, and run every possible contrast, this can get hairy...

# Let's first play around with the "InsectSprays" dataset native to R
head(InsectSprays)

# This tests the efficacy of 6 insect sprays by how many bugs are on a plant treated with them

# We can run an ANOVA on this data and see if there are any difference:
is.aov = aov(count ~ spray, data=InsectSprays)
summary(is.aov)

# Well, they're certainly different, but how?
# We can look at the average insects in each group
# (This command just takes the average of count by spray in InsectSprays)
with(InsectSprays,tapply(count,spray,mean))

# Now we might want to know how each spray differs from the others
# To do this, we might just compare all of them against one another
# Don't worry too much about this code - all it does is runs every contrast, provides the estimated difference, and associated p
for(i in 1:5) {
  for(j in (i+1):6) {
    cmt = rep(0,6)
    cmt[i] = 1
    cmt[j] = -1
    ctrst = contrast(is.aov,cmt)
    writeLines(paste(levels(InsectSprays$spray)[i],' vs ',levels(InsectSprays$spray)[j],'; Estimate: ',round(ctrst[1],2),'; p = ',signif(ctrst[4],3),sep=''))
  }
}
rm(cmt,ctrst,i,j)

# Well there are a number of significant differences here!
# Are we done?

# Well... not quite...
# We just ran 15 different tests to find all these differences
# Assuming the null is true - there is no difference - each test has a 95% chance of not rejecting the null
# So in 15 tests, the probability of never rejecting the null is:
.95 ^ 15

# Thus the false alarm rate is:
1 - .95 ^ 15

# This means that with 6 groups you would have over a 50% chance of getting a false alarm somewhere in there...
# So we need to contain this somehow...


##############################
# 11.4 - Bonferroni          #
##############################

# The first way is through Bonferroni corrections
# There's no R function for doing Bonferroni corrections - mostly because they're so easy

# Remember that if you do n tests, then to keep an effective alpha you use the cutoff for Type I error as alpha/n
# Thus if you want to do a Bonferroni correction, you just run all the tests like you normally would
# Then only take the results with p less than alpha/n

# In some cases you might need to report an adjusted p-value
# That's easy: you just multiply the p-value you're given by n
# (If p is less than alpha/n, then p*n is less than alpha)

# So what about the test of C vs D above?
contrast(is.aov,c(0,0,1,-1,0,0))

# The p-value is:
contrast(is.aov,c(0,0,1,-1,0,0))[4]

# And so Bonferroni corrected (for the 15 implicit tests) it would be:
contrast(is.aov,c(0,0,1,-1,0,0))[4] * 15

# Any p-value over 1 is clearly not significant (it's over 1 due to the simplicity of Bonferroni)
# So this comparison went from marginaly significant to clearly chance

# And that's it to Bonferroni corrections in R

##############################
# 11.5 - Tukey's HSD         #
##############################

# Of course, Bonferonni corrections can penalize you needlessly
# A correction of 15 would imply that you would need p < .0033 to find significance
# Instead, if we want to do all comparisons at once, we can use Tukey's Honestly Significant Differences
# This looks at the expected range of means under the null and tests whether your data is any different

# Doing this in R is fairly simple:
# First we must make an aov() objects, then we run a simple command: TukeyHSD()
# We already have the aov object from earlier, so...
TukeyHSD(is.aov)

# And now we get a large table of, well, stuff...
# So how do we interpret this?
# Note that each row has a name that looks like one factor minus another
# Thus the first row is the difference of Spray B and Spray A
# So 'diff' is just the mean of B minus the mean of A
# Next, 'lwr' and 'upr' refer to the 95% confidence level of the range between them
# (Remember, this test looks at the expected range of means if all groups were the same)
# Finally comes the corrected p-value that these two groups are different
# Note that this is a post-hoc p-value - if you set alpha at 0.05, anything less than 0.05 is significant after correction

# So if we go down the line, we can see that B is not different from A
# Same with F vs A, but A clearly allows more bugs than C, D, and E
# Then C is different from B, and so on

# We can also visualize this using the plot() command:
plot(TukeyHSD(is.aov))

# Here, you can see the various group differences as ticks on the y-axis
# All lines are the confidence intervals of the differences
# So any line that does not cross 0 represents a significantly different pair

# But we might also adjust our alpha level (e.g., to 0.01)
# We can easily do this by giving TukeyHSD the argument conf.level as 1-alpha
TukeyHSD(is.aov, conf.level = 0.99)

# Note that the difference and p-values didn't change, but the range now becomes wider to adjust for increased certainty

# But that table can be tricky to interpret...
# We might just want to put the sunscreens into groups of sunscreen we can't tell apart from each other, but differ between them
# For this we have to use a different HSD command...
# This command isn't built in and has some pretty large limitations, but at least it does HSD grouping well
# For this you'll need the 'agricolae' package (which you've hopefully installed earlier...)
library(agricolae)

# Now we use the HSD.test() function, which takes two arguments:
# First, the aov object, second, the name of the factor you're looking at:
print(HSD.test(is.aov,'spray'))

# The most important part of this output is at the bottom
# Notice that it doesn't give pairwise comparisons or p-values...
# But it has these things called 'Groups' that are labeled either a or b
# What this means is that everything in group a is indistiguishable (by an HSD test)
# Likewise, everything in group b is indistinguishable

# So sunscreens A, B, and F cannot be told apart
# And sunscreens C, D, and E cannot either
# But the two groups are different

# Sometimes you will get a treatment with two letters (like 'ab')
# In this case that treatment cannot be told apart from anything in group a or group b
# But the treatments only in group a are different from the treatments in group b

# You can also change the level of confidence here using the alpha parameter
# But note that in opposition to TukeyHSD, this is just alpha, not 1-alpha
# Let's take a really stringent alpha of .001:
print(HSD.test(is.aov,'spray',alpha=0.001))

# If you want to get the p-values from a Tukey Test, you can use the get.p function:
get.p(TukeyHSD(is.aov))

# Note that this gives you a named vector, so if you want to pull out individual items you can call them by the difference:
get.p(TukeyHSD(is.aov))['C-B'] # Gets the Tukey-corrected p-value for the difference between sprays B and C

# One last note: we've been using aov objects here rather than lm objects
# This is because TukeyHSD can't handle lm objects
# For instance, this is equivalent to the aov call:
is.lm = lm(count ~ spray, InsectSprays)

# But try typing in the following...
#  TukeyHSD(is.lm)

# Note that you can give lm objects to HSD.test
# But since you can't get p-values, it's better to just use aov when you want post-hoc comparisons

# You might also ask what about TukeyHSD on two-way designs
# This is asking whether there are pairwise differences in any condition
# Which is effectively like flattening out the two factors into a single factor, then running a TukeyHSD test
# For example, with the warpbreaks data we looked at earlier:
wb.aov = aov(breaks ~ wool * tension,data=warpbreaks)

# We can just run TukeyHSD:
TukeyHSD(wb.aov)

# This first gives you range tests on the individual factors
# But more importantly, it gives you pairwise tests of all factors at the end
# You can see that this is the same as the one-way TukeyHSD
# Except each condition for difference is the combination of both factors (wool, then tension in this case)

# One downside of HSD.test though is that you can't use it on two-way designs without flattening the factors yourself

rm(is.aov,is.lm,wb.aov)

##############################
# 11.6 - Extensions          #
##############################

# The techniques from this class are just the beginning of linear contrasts and multiple corrections
# For instance, the commands used here can't apply to random or mixed effects models
# And there are many more multiple correction techniques that account for correlations between tests

# The good news is that there's a single package in R that extends these ideas: multcomp
# However, I haven't taught it for two reasons:
#  (1) The syntax is much more inscrutable (it tests combinations of coefficients rather than factors)
#  (2) You shouldn't use tools you don't understand, and we haven't reviewed advanced multiple correction

# If you want to learn how to use it, I would suggest reading "Multiple Comparisons Using R" (the book by the package authors)
# And there are some good examples online of how to do more advanced linear comparisons, e.g.:
#  http://cran.r-project.org/web/packages/multcomp/vignettes/multcomp-examples.pdf
#  http://www.ats.ucla.edu/stat/r/faq/testing_contrasts.htm

################################
# 11.7 - Conditional statments #
################################

# This moves away from statistics-specific work, and back into programming knowledge
# However, conditionals can be useful for data transformation

# For instance, let's look at the drug information again
# Perhaps we want to see how many people improved versus not improving on the second test
# We might want to label anyone who improved with 'Improvement' and other with 'No Improvement'

# To do this, we can use conditional (if-else) statments
# We can put this in a function that does the following:
# Test whether a value is greater than 0
# If this test returns TRUE, then return 'Improvement'
# Otherwise return 'No Improvement'

# This follows the following syntax (note the curly brackets):
# if(conditional) {
#   do what is here...
#  }
#  else {
#   do what is here...
#  }
test.imp = function(diff) {
  
  # If this test is true, then do what is in these brackets
  if(diff > 0) { 
    return('Improvement')
  }
  # Otherwise, do what is in these brackets
  else {
    return('No Improvement')
  }
}

# Let's test it out:
test.imp(4)  # Improvement
test.imp(-4) # No Improvement
test.imp(0)  # No Improvement

# But we also want to get this for each observation in the dataset
# Let's say we have a vector of improvement scores
improve = round(rnorm(20,5,10),0)
improve

# We want to run the function on these improvement scores
# and get a vector back that is the result of all of those functions
# We can't run this function on the vector though...
# It just gives us back one answer corresponding to the first evaluation:
test.imp(improve)

# Note the warning... it just used the first element
# Instead we must use the sapply() function
# sapply takes two arguments: a list or vector of input items, and a function
# It then applies that function to each element of the input items,
# and returns a vector of each of the outputs

# Let's try this on a limited set... we can try:
sapply(c(1,-2,3,-4),test.imp)

# And if we want to run test.imp on each element of the vector, we would enter:
sapply(improve,test.imp)

# Then we can count these using table()
table(sapply(improve,test.imp))

# But you might be asking yourself why we bothered to use the conditional here...
# After all, we could have entered "improvement > 0" and gotten TRUE/FALSE splits the same way

# With conditional statements, you can split it more than two ways
# So let's say that if the difference were only 2 or less, we would consider that 'Practically Equivalent'
# Then we can rewrite the function with an 'else if' statement
test.imp.2 = function(diff) {
  
  # If this test is true, then do what is in these brackets
  if(diff > 2) { 
    return('Improvement')
  }
  # If it's not true, check if this test is true, then do what is in these brackets
  else if(diff < -2) {
    return('No Improvement')
  }
  # If none of the above are true, do this
  else {
    return('Practically Equivalent')
  }
}

# Now we can use sapply to apply this function:
sapply(improve,test.imp.2)
table(sapply(improve,test.imp.2))


##############################
# 11.8 - Loops               #
##############################

# Oftentimes you might want to repeat a bit of code a number of times
# For instance, let's say you want to print out the first 20 Fibonnaci numbers
# These are numbers that are the sum of the prior two: 1, 1, 2, 3, 5, 8...

# Doing this by hand would be a pain... you would have to repeat the same thing a number of times...
fibs = rep(0,20)
fibs[1] = 1
fibs[2] = 1
fibs[3] = fibs[1] + fibs[2]
fibs[4] = fibs[2] + fibs[3]
fibs[5] = fibs[3] + fibs[4]
# ... I'm bored already...

# Instead, let's use a for loop
# The syntax is:
# for(X in Y) { Do this stuff over and over for each X }
# Note that Y should be a vector or list, and X will be each element in that list

# For instance:
fibs = rep(0,20) # Reset fibs

for(i in 1:20) {
  if(i < 3) {
    fibs[i] = 1
  } else {
    fibs[i] = fibs[i-2] + fibs[i-1]
  }
}

fibs

# So what happened in here?
# First, i was set to 1... then within the loop, i was less than 3 so fibs[1] = 1
# Next, same thing with i set to 2
# Then i gets set to 3... so fibs[3] = fibs[1] + fibs[2]
# Then i gets set to 4... so fibs[4] = fibs[2] + fibs[3]
# Then i gets set to 5... so fibs[5] = fibs[3] + fibs[4]
# ... and hopefully you see the pattern

# Note that you can put any vector or list in the Y position
# For instance, let's say you wanted to count the number of positive numbers in a vector
# We'll define our vector:
test.pos = rnorm(10,-1,2)
test.pos

# Now we know how to do this easily through some logical manipulation:
sum(test.pos > 0)

# But for pedagogical reasons, let's do this with a for loop
# Here we want to take every element of test.pos, and increment a counter if it's positive
# Also, we'll use the print command to output what it's checking at each point
pos = 0
for(x in test.pos) {
  print(x)
  if(x > 0) {
    pos = pos + 1
  }
}

pos

# But this just goes through a vector or list, one element at a time...
# What if we want to repeat some code until we hit a trigger?
# For that, we have while loops

# while loops have similar grammar to for loops:
# while(conditional is TRUE) { Do this stuff over and over }

# An easy example would be printing the numbers 1 through 10:
i = 1
while(i <= 10) {
  print(i) # Print where i is at
  i = i + 1 # Increment i (if you don't, the loop will never end)
}

# Note that the power of while shines when we want to test something that changes within the loop
# For instance, if we want to get the first positive number out of test.pos, we might say:
i = 1 # Define our counter

# Continue the loop as long as i is not positive
while(test.pos[i] < 0) {
  i = i+1
}
test.pos[i]

# Now, you might note that there are much easier ways to do these basic things with sapply or vector operations
# And so far our examples are very basic
# But loops are more flexible (though slower) than other things we've learned
# And can be important for more advanced techniques (e.g., bootstrapping/resampling)

rm(fibs,test.pos,pos,i,x)


# install.packages('multcomp')
# library('multcomp')