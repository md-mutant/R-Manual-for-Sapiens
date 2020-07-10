########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#     Lesson 4 - Probability and Simulation            #
#                                                      #
########################################################

library(PSYC201)
library(ggplot2)

###############################
# 4.1 - Binomial distribution #
###############################

# We have discussed the binomial distribution in class as the probability of observing a number of coin flips
# And we have the equations to, for instance, find the probability of observing 5 heads of 10 fair coin flips
# This can be broken down into:
#  1) The probability of heads raised to the number of heads: (0.5)^5
#  2) The probability of not heads raised to the number of tails: (1-0.5)^(10-5)
#  3) A factor representing the number of ways this arrangement could arise: choose(10,5)
choose(10,5)*(.5^5)*((1-.5)^(10-5))

# But that's messy to write... and you'll be using binomial equations a lot for the near future
# Lucky for us (read: you), R makes this really easy to do
# We can just use the function dbinom - for d(ensity of the) binom(ial)
# dbinom takes three arguments:
#  1) The number of heads observed
#  2) The total number of flips
#  3) The probability of getting heads
dbinom(5,10,0.5)

# Note that the two results are the same

# Or what about the probability of getting 7 of 10 with a bent coin that lands on heads 90% of the time?
dbinom(7,10,0.9)

# But that just gets us the probability density function for the binomial distribution...
# What if we want the CDF?
# For instance, what's the probability of getting 5 or fewer heads of 10 on a fair coin?
# The naive way is to add the prior PDFs...
dbinom(0,10,.5) + dbinom(1,10,.5) + dbinom(2,10,.5) + dbinom(3,10,.5) + dbinom(4,10,.5) + dbinom(5,10,.5)

# But a much easier way is to use pbinom - p(robability) of the binom(ial)
# This takes the same arguments as dbinom
# But it instead returns the probability of getting X heads or fewer
pbinom(5,10,0.5)

# And to get the probability of 6 or more, we just partition the event space...
# It's just one minus the probability of 5 or fewer
1 - pbinom(5,10,0.5)

# Or how about the proabability of getting between 3 and 7 heads?
# This is just the probability of getting 7 or fewer minus the probability of 2 or fewer
pbinom(7,10,0.5) - pbinom(2,10,0.5)

# Note here the difference when asking about the probability of getting GREATER THAN OR EQUAL TO n heads
# When we calculate pbinom(5,10,0.5) we are asking 'What is the probability of getting 5 or fewer heads?'

# But if we ask 'What is the probability of getting 5 or more heads?' we can't use 1-pbinom(5,10,0.5)
# This is because that equation is asking 'What is the probability of *not getting* 5 or fewer heads?'
# But if you write it out, the number of heads that fall into the 5 or fewer category are:
#  0, 1, 2, 3, 4, 5
# So 1-pbinom(5,10,.5) gives us the probability of observing the following number of heads:
#  6, 7, 8, 9 ,10

# Note that 5 is not included in the second set - that gives you the probability of six or greater
# So any time you want to ask for "n or greater heads", you need to put n-1 into the 1-pbinom equation
# In this case, the probability of 5 or greater heads is 1-pbinom(4,10,0.5)
pbinom(5,10,0.5)
1-pbinom(4,10,0.5)

# We can also have R do our coin flipping for us
# This function is rbinom - for r(andom) binom(ial)
# The syntax is rbinom(n,size,prob.heads), where n is the number of simulations you want, and size is the number of flips/simulation
# The output is a vector with an entry for each simluation, with the total number of heads in each simulation

# So just flipping a single fair coin 10 times would be written as:
rbinom(10,1,.5)

# Or if you want to flip 10 coins and count the number of heads, you would write:
rbinom(1,10,.5)

# But if you want to repeat flipping 10 coins and counting the number of heads 5 times, you would write:
rbinom(5,10,.5)

# And if you later learn the coin was bent and lands on heads 70% of the time:
rbinom(5,10,.7)

# We will use these sorts of functions to simulate data often, starting next week

# Finally, we can ask the opposite of 'What is the probability of getting 5 or fewer heads?'
# This is: 'What is the most number of heads I could get such that the probability of getting at most that many is X%'
# For this you use qbinom: q(uantile of the) binom(ial)
# This takes the form qbinom(p, total.flips, prob.heads)
# So if you wanted to know the number of heads you would no more of than with 60% probability:
qbinom(0.6,10,0.5)

# Here you get 5
# But note, pbinom(5,10,.5) is 0.623
# So what about 65%
qbinom(0.65, 10, 0.5)

# Now we get 6
# This may seem like an odd question to ask...
# but it will become very apparent why it's needed when we do sampling distributions

##############################
# 4.2 - Discrete probability #
##############################

# Here we're going to talk about more general probabilistic simulation
# And how that turns out to be the logic behind all of statistics

# The first function we'll learn for this is sample()
# sample() lets you draw random elements from a vector
# This takes at least two arguments: a vector to sample from and the numbers of samples to take
# For instance, below we're drawing 5 random integer from between 1 and 10:
sample(1:10,5)

# Let's try that again:
sample(1:10,5)

# Note that you got something different both times - this is part of the randomness

# By default, you sample without replacement - but you can change this
sample(1:10,10,replace = TRUE)

# Try this a few times, and you'll see repeats within the sampled vector

# You can also pull samples with different probabilities
# Let's say you wanted to simulate a die roll 20 times... that's not hard, it's just choosing a number from 1-6 randomly:
sample(1:6,20,replace = TRUE)

# But now you find out the die is weighted... it rolls a 1 half the time
# We can define the probabilies of landing on each of the sides as:
weighted.prob = c(.5,.1,.1,.1,.1,.1)

# Note that the first item (1) is weighted at 50% and all other sides are at 10%

# Now we can use the 'prob' argument in sample to change the probability of getting each side
sample(1:6,20,replace = TRUE, prob = weighted.prob)

# You should get a lot more ones with that sample

# So you've rolled a die a number of times, but it's hard to see how many of each side you got
# But remember conditionals? They make it easy!
# Let's roll a (fair) die 100 times
die.rolls = sample(1:6,100,replace = TRUE)

# To see how many of the rolls are 1, all we have to say is:
sum(die.rolls == 1)

# Now the fun part - let's put this all together
# Let's write a function that rolls a number (n) of dice and tells you the total on each of the faces
roll.n.dice = function(n) {
  # First, roll n dice
  rolls = sample(1:6,n,replace = TRUE)
  
  # Then add the results of all of the rolls
  tot = sum(rolls)
  
  # And finally return that value
  return(tot)
}

# So let's roll 2 dice:
roll.n.dice(2)

# Or roll 5 dice:
roll.n.dice(5)

# But what if we want to find out the result of rolling two dice 10 times?
# Let's try the rep function:
rep(roll.n.dice(2),10)

# Seems like you got a pretty rare result, huh?
# To get real results we need the replicate() function
# This is like rep() but repeats the function when it has a random call inside of it
# It takes two arguments - the number of times to repeat, and the function to evaluate
############## gold jerry 
replicate(10,roll.n.dice(2))

# Note that replicate() is slow... you'll want to avoid it when using big simulations

# Next, we'll try a large simulation and see how this approximates the probability you've learned
# What's the probability of rolling a 7 on two dice? One in six
# So if we roll a lot of pairs of dice (say, ten-thousand), about one in six should be a seven
lots.o.rolls = replicate(10^5,roll.n.dice(2))

# Now we divide the total number of sevens by the number of rolls
sum(lots.o.rolls == 7) / 10^5

# This can be calculated much easier using the mean function though:
mean(lots.o.rolls == 7)

# What about the number of 12s?
mean(lots.o.rolls == 12)

# Or the number of 3s?
mean(lots.o.rolls == 3)

# Congrats! We've just built our first simulator that's shown the 'Law of Large Numbers'
# As the sample size gets larger, the proportion of a certain event in that sample approaches the theoretical probability

# And now clean it up...
rm(lots.o.rolls,weighted.prob,die.rolls)

#################################
# 4.3 - Conditional probability #
#################################

# Now lets talk about conjunctive and conditional probability

# First, let's play with cards
# To do this, we'll need to build a deck - and we'll use a data frame for that
# Note that for card values, we're assuming J: 11, Q: 12, K: 13, A:14
# We'll use the rep() function to make this easy
cards = data.frame(suit = rep(c('Club','Spade','Heart','Diamond'),each = 13),
                   value = rep(2:14,times = 4))

cards

# Now let's ask some basic questions... first, what is the probability of getting a heart?
# To do this, we count what proportion of the cards are hearts
mean(cards$suit == 'Heart')

# And what is the probability of drawing a 7? We do the same thing as above
mean(cards$value == 7)

# What about the probability of drawing a face card (remember, this is J/Q/K/A)?
mean(cards$value >= 11)

# How about the probability of drawing a diamond face card (e.g., a face card AND a diamond)?
mean(cards$suit == 'Diamond' & cards$value >= 11)

# You can see this is P(Diamond) * P(Face Card), since P(A&B) = P(A)*P(B) if A & B are independent
mean(cards$suit == 'Diamond') * mean(cards$value >= 11)

# What about a diamond or a face card?
mean(cards$suit == 'Diamond' | cards$value >= 11)

# This isn't P(Diamond) + P(Face Card)... because you would be double counting the diamond face cards
mean(cards$suit == 'Diamond') + mean(cards$value >= 11)

# But remember - P(A or B) = P(A) + P(B) - P(A & B)
mean(cards$suit == 'Diamond') + mean(cards$value >= 11) - mean(cards$suit == 'Diamond' & cards$value >= 11)

# Now let's get a bit trickier...
# What if we know that we got either a diamond or face card... what is the probability we got an ace?
# Remember Bayes' rule: P(B | A) = P(A & B)/P(A)
# In this case P(A & B) is the just the probability of drawing an ace
# (If you've drawn a face card or diamond, that doesn't constrain which ace you could draw)
# And P(A) is the probability of drawing a face card or diamond
# So P(A & B) is equal to:
mean(cards$value == 14)

# And we know P(A) is equal to:
mean(cards$suit == 'Diamond' | cards$value >= 11)

# And so the probability of getting an ace given that constraint is:
mean(cards$value == 14) / mean(cards$suit == 'Diamond' | cards$value >= 11)

# Which should be equal to 4/25

###################################
# 4.4 - Simulation to probability #
###################################

# Now let's pretend we came down with specific amnesia for probability...
# Can we still solve these sorts of problems with R?

# Well yes we can, to an approximation
# To do so, we'll use use our new friend simulation

# Think about what the frequentist theory of probability is:
# If P(E) = X, then if you repeat your task over and over again an infinite number of times
# The proportion that will be E is X

# We can't repeat a task an infinite number of times
# But using computers we can do it a lot

# So let's go back to our deck of cards and ask, what is the probability of getting a heart?
# First we need to simulate a draw from a deck of cards
# This is easy... remember there are 52 cards, and thus we just need to select a random row from the cards table
cards[sample(1:52,1),]

# And we can do this over and over...
cards[sample(1:52,1),]

# But doing this lots of times and counting manually is hard...
# ... and I'm lazy, so let's just draw a bunch of cards over and over using R
# For this, we don't just draw one sample, we draw lots
# And we have to remember to use replace=TRUE...
# After all, we want to draw one card, put it back, shuffle the deck, and repeat

# So let's draw 10 cards
cards[sample(1:52,10,replace=TRUE),]

# Note that repeat draws are possible
# (If it didn't happen, repeat the above line a few times and you'll see it)

# But 10 draws isn't enough to get probabilities out of...
# Let's try 10,000 instead
lots.o.cards = cards[sample(1:52,10000,replace=TRUE),]

draw.n.cards = function(n){
  draw.n = cards[sample(1:52,n,replace=0),]
  sum(draw.n$suit =='Spade')
  return draw.n 
}
replicate(10000,draw.n.cards(3))


# Okay... so what about the probability of getting a heart?
with(lots.o.cards,mean(suit == 'Heart'))

# Pretty close to 0.25, right?

# Now what about the probability of drawing a face card and a diamond?
# We can do the same thing as above, but with a conjunction
with(lots.o.cards,mean(value >= 11 & suit == 'Diamond'))

# Again, should be close to 0.077

# And the probability of a diamond or a face card?
with(lots.o.cards,mean(value >= 11 | suit == 'Diamond'))

# Should be about 0.481

# But what about conditional probability?
# Say the same problem we asked above:
# What is the probability that you got an ace given you got a face card or a diamond?

# Well, this is almost as easy as above:
# All we have to do is restrict our sample to the subset of cards that are face cards or diamonds
# And we know how to do this with the subset function
with(subset(lots.o.cards,value >= 11 | suit == 'Diamond'),mean(value == 14))

# This should be close to 0.16
# But notice that we have slightly less precision here, since we aren't counting from all of the cards...
# Just the face cards and diamonds

#################################
# 4.5 - Practice problems       #
#################################

# Before we go on, let's make sure we understand these concepts with a couple practice problems
# Answers will be at the bottom of the lesson... so don't read far ahead if you don't want spoilers!
# You also won't get any extra points for going ahead and reading off the answer in class
# I made these problems... I'll know when you're doing it
# So don't be tempted... give it your best shot, and read the answers later to make sure you understand them

# Problem 1:
#  If you roll two dice and got four or more on each of them,
#  what is the probability the sum of both is 9?

# Problem 2:
#  If the sum of two dice is nine or greater,
#  what is the probability that once of the dice was a 6?

# Problem 3:
#  I draw three cards from a deck (without replacement)
#  What is the probability that any two of the cards share a suit?


###################################
# 4.6 - Sampling to statistics    #
###################################

# We've just shown how sampling and probability are interrelated
# But now we're going to take another step and go from sampling to statistics

# Let's pretend we run an experiment where we're testing whether certain majors have an unequal distribution of genders
# We don't have access to administrative records to actually count
# So instead, we pick a random class required for the major, go in, and count the men and women in the room

# In Major #1, we find that 26 of the 40 students are male (65%)
# In Major #2, we find that 115 of 200 students are male (57.5%)

# Do we have evidence that either of those two majors do not have an equal distribution of genders?

# First, let's set up the null hypothesis: that the majors do have an equal number of men and women
# In this case, randomly chosen classes should have, on average, and equal gender split
# But we were only able to choose one class, so it might be a little off

# The question we want to ask is:
# Given that we've observed a single class, how odd would it be if it came from a major with an equal gender split?

# To see how this works, we can run some simulations that look at how these classes might be formed
# We assume that we have an infinitely large major with equal genders, and we pick classes of the same size that we observed
# Then we look at the probability that we get a gender split as or more extreme than we actually observed

# To run simulations, recall the rbinom() function from Lesson 2
# Picking a hypothetical student from an equal gendered major is like flipping a coin
# So we can use rbinom to easily pick 40 of these students (for the first class)
# We use rbinom(1,40,.5) to tell it that:
#  - we want 1 sample
#  - of 40 students
#  - with a 50% probability of being male
rbinom(1,40,0.5)

# Now, we could run this code lots and lots of times...
# Or we could just tell rbinom that we want lots and lots of samples
# This way, we should approximate the distribution of hypothetical classes under the null hypothesis
maj1.null = rbinom(50000,40,0.5)

# What we have now is the number of men in 50000 hypothetical classes
# We can visualize this easily with a histogram
qplot(maj1.null,binwidth=1)

# Note that it seems to be centered around 20 - the number you would expect given a 50/50 distribution
# (There is a slight skew because this is discrete data, whereas hist() buckets according to <= 20 and >20)
# But there is some spread around the mean, where you get some classes skewed heavily to both sides of gender imbalance

# From this we can also determine what percentage would have just as many or more than the 26 men we actually observed
mean(maj1.null >= 26)

# This should show that only about 4% of the classes have 26 or more guys in them
# And just taking p < 0.05 to be our arbitrary cutoff for statistical significance,
# If only 4% of of the classes fall into this range, then we can reject the null, right?

# Well hold on a minute...
# Our null hypothesis was that there were equal genders
# And therefore, our definition of 'more extreme' should include observations of classes with more *women* than expected
# (Come on... I'm not trying to be sexist here)
# If we only saw 14 guys (26 women) in the class, we should be just as surprised
# So 'more extreme' should include both 26 or more AND 14 or fewer men
# And we can count this as well:
mean(maj1.null >= 26 | maj1.null <= 14)

# Now this should be around 8%
# Note that this is approximately twice the proportion looking at only one end
# With a 50/50 binomial distribution, the two directions of extremeness should be symmetric
# But note also that this brings the p-value above 0.05
# So while it trends towards it, we cannot call this test a statisitically significant rejection of the null hypothesis
# (We also don't have good evidence FOR the null either though... we would need more data)

# And we can do the same thing with the second major
# First we form the null distribution (picking classes of size 200 this time)
maj2.null = rbinom(50000,200,0.5)

# We can look at the distribution
qplot(maj2.null,binwidth=1)

# Note that it's again centered around the 50/50 point (100)

# And then look at the extremeness of getting 115 males
# (Remember we need to look at 85 or fewer as well)
mean(maj2.null >= 115 | maj2.null <= 85)

# Hmm... here only ~4% of the hypothetical classes are as or more extreme
# So we do have statistical evidence (at the 0.05 level) that this major does not have equal gender splits

# But this seems counterintuitive...
# For the first class, 65% were guys and we did not find evidence to reject the null
# Whereas for the second, only 57.5% were, and we did find a significant deviation from the null
# So the larger effect was not signficant, but the smaller was...

# This gets at the interaction between effect size and sample size
# We had a larger class to look at for the second major
# And that's what gave us significance
# With smaller effects, you need a larger sample size to find statistical signficance
# Conversely, with a large effect you don't need as big a sample
# We will be discussing this and related facts of hypothesis testing a lot more in the coming weeks

# But let's bring this back around to analytics rather than sampling
# Remember the funciton pbinom()?
# This tells us the probability of getting X successes out of N trials in a binomial sample
# Which means it does an analytic calculation of the same thing we've just done by sampling
# So rather than having to build a null distribution, we can just plug in numbers
# Going back to the first class, we can ask what is the probability that we have 26 or more or 14 or fewer men?
# For the 14 or fewer, we can use the basic pbinom, giving it 14 out of 40 with a probability of 50%
pbinom(14,40,0.5)

# Then for 26 or more, we say what is the probability of NOT getting 25 or fewer
1-pbinom(25,40,0.5)

# Note that they're the same
# So the theoretical probability of getting data as or more extreme as what we've observed
# is just the probability of getting 14 or fewer OR 26 or more, and since these are mutually exclusive we can add:
pbinom(14,40,0.5) + (1-pbinom(25,40,0.5))

# Note how close this is to our sampled number:
mean(maj1.null >= 26 | maj1.null <= 14)

# But there's an easier way to do this analytically: the binom.test() function
# Rather than having to get the probability of both tails, R will do this for you
# For this function, we give it three arguments:
#  x: the number of 'success' or 'heads' observations we actually got (in this case, men)
#  n: the total number of observations
#  p: the probability of 'success' or 'heads'
# So for this test, it would be:
binom.test(26,40,0.5)

# This gives us a large amount of information:
# Beyond the data line, the next line reiterates the data and gives us the p-value
# (Note that the p should be exactly equivalent to the addition of the tail probabilities from above)
# The next line tells us what we are testing - in this case, that it's not an equal split
# Then comes the 95% confidence interval on what p is given the observations - we'll discuss this more in the coming weeks
# And finally, the probability of a 'success' in the data (equivalent to x/n)

# As a quick note for the homeworks, you can store this test to a variable
bt.1 = binom.test(26,40,0.5)

# Then display it later
bt.1

# Then we can use the get.p() function to pull out the p-value
# Note that this is a function from the PSYC201 package
# If you don't load it, R will be confused because it won't understand the get.p call
get.p(bt.1)

# This should make it easy to store this value to a variable

# And we can do the same for the second major:
# Getting 85 or fewer:
pbinom(85,200,0.5)

# Getting 115 or more:
1 - pbinom(114,200,0.5)

# 115 or more extreme:
pbinom(85,200,0.5) + 1 - pbinom(114,200,0.5)

# And our sampled distribution:
mean(maj2.null >= 115 | maj2.null <= 85)

# Again - should be very close

# And we can test again with binom.test
binom.test(115,200,0.5)

# Finally, we can ask another, related question:
# Is there evidence that there are more men than women in the first major?
# Note that this is subtly different than asking if the genders are equal
# Because with this question, we wouldn't care if the data came back all women
# In that case we would simply say that it doesn't provide evidence that there are more men

# And in the process of testing our first question, we've already answered this question, as you'll soon see
# Here we have the same null hypothesis distribution - that the gender split is equal
# We do this because this is the closest we come to men being more prevalent without that being true
# But our null hypothesis is different: it's that the proportion of men is 50% or less
# And conversely, our alternative hypothesis is only that the proportion of men is greater than 50%
# This is what is called a one-tailed test
# We are only looking at one side of the distribution and ignoring the other

# This changes our definition of 'more extreme'
# We no longer consider lots of women a 'more extreme' event, because it isn't extreme as compared to our null hypothesis
# In fact, it's comfortably within the range of the null hypothesis that there aren't more men
# Thus we only have to consider the probability that we would have gotten 26 or more men in our class?
# Which from our sample, would be:
mean(maj1.null >= 26)

# This is why I mentioned we had already solved it
# And note that it's now 
# Likewise, the analytic solution is just the probability of not getting 25 or fewer:
1-pbinom(25,40,0.5)

# And finally, we can run this using binom.test()
# But here, we need to specifiy what our alternative hypothesis is
# In this case, it's that the number of men is 'greater' than 50%
# So we use the alternative argument:
binom.test(26,40,0.5,alternative = 'greater')

# If we thought that there should be fewer than 50%, we would tell it 'less'
binom.test(26,40,0.5,alternative = 'less')

# But note that because there actually are more than 50% in this sample class, the p-value will be very high

# Be careful with one-tailed tests though...
# They can be used incorrectly to cheat with statistics
# We'll discuss this further in later lessons
# But for now, don't use one-tailed tests unless you have explicit evidence to do so

# And we'll clean up...
rm(bt.1,maj1.null,maj2.null)

###################################
# 4.7 - Problem answers           #
###################################

# To avoid spoilers, I'm going to put extra space here































###################################
# Problem 1:
#  If you roll two dice and got four or more on each of them,
#  what is the probability the sum of both is 9?

# First, we need to make our set of die rolls
# To do this, let's simulate 20,000 sets of dice rolls and put it into a data frame
rolls = data.frame('Die1'=sample(1:6,20000,replace=TRUE),'Die2'=sample(1:6,20000,replace=TRUE))

# The we get the sum
rolls$Total = rolls$Die1 + rolls$Die2

# Now we can look at all of these rolls and take only the ones in which both dice were 4 or more
over.4 = subset(rolls,Die1 >= 4 & Die2 >= 4)

# And then figure out how many of these are 9s
mean(over.4$Total == 9)

# Note that there are many other ways to do this (some more efficiently, some less)
# Can you think of any others? How about doing this analyticaly?

###################################
# Problem 2:
#  If the sum of two dice is nine or greater,
#  what is the probability that at least one of the dice was a 6?

# Again we'll use our rolls data
# But now we subset that to only the rolls where the total is over nine
total.over.9 = subset(rolls, Total >= 9)

# And then we see how many of these have one 6:
with(total.over.9, mean(Die1 == 6 | Die2 == 6))

###################################
# Problem 3:
#  I draw three cards from a deck (without replacement)
#  What is the probability that any two of the cards share a suit?

# Here we need to pick back up our cards
# The easiest thing to do is write a function that gives us TRUE when the condition is met

matched.suits = function() {
  # Draw three cards
  draw = cards[sample(1:52,3,replace = FALSE),]
  
  # Then check for matches
  match1 = draw$suit[1] == draw$suit[2]
  match2 = draw$suit[1] == draw$suit[3]
  match3 = draw$suit[2] == draw$suit[3]
  
  # Then return if any of them are matches
  return(match1 | match2 | match3)
}

# Now we just repeat this over and over
draw.matches = replicate(10000,matched.suits())

# And count how many are true
mean(draw.matches)

# Advanced thought problem: can you solve this analytically?
# (Hint: what's the probability of not getting any matches?)

