########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#     Lesson 10 - Advanced plotting and data munging   #
#                                                      #
########################################################

library(PSYC201)
library(plyr)
library(ggplot2)
library(reshape2) # NOTE: you may need to use install.packages("reshape2") for this one

##############################
# 10.1 - ggplot()            #
##############################

# In the earlier plotting lesson, you learned how to use qplot()
# However, this stands for q(uick) plot, and in using it, you lose some flexibility
# If you want to have the full flexibility and power of ggplot2,
#  you need to learn how to use the ggplot() command and add layers

# Let's go back to the puzzles data we used in the plotting lesson...
head(puzzles)

# We'll first plot it as a line graph with error bars like we did in that lesson
# First we need to aggregate it
puzzagg = ddply(puzzles, c('PuzzleType','Difficulty'), summarise, AvgTime = mean(Time), SETime = se(Time))
puzzagg

# Back then we used a single function qplot:
qplot(Difficulty,AvgTime,data=puzzagg,group=PuzzleType,color=PuzzleType,
      ymin=AvgTime-SETime,ymax=AvgTime+SETime,geom=c('line','pointrange'))

# When using ggplot, on the other hand, you have to start with the data at the core using ggplot
# Then add how you want that data to be displayed on top of it

# ggplot() at its core takes two arguments - a data frame, and an aes() object
# aes() store how you want everything aranged (the aes(thetics) of the graph)
# These arangements include things like x, y, group, color, ymin, and ymax (among others)
# So if you wanted to make an aes object like our last qplot, you would type:
aes(x = Difficulty, y = AvgTime, group = PuzzleType, color = PuzzleType, fill = PuzzleType, ymin = AvgTime-SETime, ymax = AvgTime+SETime)

# So our ggplot object would be:
plotbase = ggplot(puzzagg,aes(x=Difficulty,y=AvgTime,group=PuzzleType,color=PuzzleType,ymin=AvgTime-SETime,ymax=AvgTime+SETime,fill=PuzzleType))
plotbase

# Note that we got an error... it tells us there are no "layers"
# This is because we havent told ggplot *how* to do the plotting
# To do this, we have to add geoms - just like we gave qplot the geom argument
# But here we will literally add them
# For instance, if you want to make a line graph, you could just say:
plotbase + geom_line()

# (Note that you can save anything to a variable and layer on additional display options after the fact)

# Or for point ranges:
plotbase + geom_pointrange()

# Or both:
plotbase + geom_line() + geom_pointrange()

# Well, we just recreated our original graph in a different way
# So why should we care?
# Because adding geoms gives us more flexibility

# Remember how we used qplot for bar graphs, but the error bars looked odd?
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, fill=PuzzleType,
      geom = c('bar','errorbar'), stat = 'identity', position = 'dodge',
      ymin = AvgTime - SETime, ymax = AvgTime + SETime) 

# We couldn't make the error bars skinnier because the width command affected both parts of the plot:
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, fill=PuzzleType,
      geom = c('bar','errorbar'), stat = 'identity', position = 'dodge',
      ymin = AvgTime - SETime, ymax = AvgTime + SETime, width = .5) 

# However, we can recreate the original graph using ggplot notation
# Note that we have to tell ggplot the same stat and position notation...
# But instead of needing to recreate the aes() object, we can add them to the geoms themselves:
plotbase + geom_bar(stat = 'identity',position='dodge') + geom_errorbar(position='dodge')

# And that adding to geoms gives a hint of the flexibility...
# We can change attributes of the parts individually
# So we can give a width argument to the errorbar that won't affect the bars:
plotbase + geom_bar(stat = 'identity',position='dodge') + 
  geom_errorbar(position='dodge', width = 0.3)

# But now because they are treated separately, they also set positions separately
# So we need to make one final adjustment...
# Saying position='dodge' is a shortcut for making a default position_dodge object
# Instead we will make one of our own and feed it through
# It takes one argument generally: the width to dodge at
# I tend to like 0.9 (which a lot of the online examples typically use too)
pdodge = position_dodge(0.9)
plotbase + geom_bar(stat='identity',position=pdodge) + 
  geom_errorbar(position=pdodge,width=0.3)

# And maybe we want to make the error bars black, rather than have them blend...
# We can do that too by setting the color only in the errorbar
plotbase + geom_bar(stat='identity',position=pdodge) + 
  geom_errorbar(position=pdodge,width=0.3,color='black')

# Now we can add on the themes and axes and have a good looking bar graph!
# (This just being a quick change)
plotbase + geom_bar(stat='identity',position=pdodge) + 
  geom_errorbar(position=pdodge,width=0.3,color='black') + 
  ylab('Average Time to Solve')

# But note the relative ease of this...
# We made the plotbase once, and just threw on top how we wanted it plotted
# That's the logic behind ggplot...
#  you define the data structure up front and you can change the display easily

rm(pdodge,plotbase)

##################################
# 10.2 - Duplicate and elaborate #
##################################

# Here we're going to briefly cover how to recreate the other graphs from the earlier plotting lesson in ggplot
# And then we'll add on some other helpful things to do with each of these plots
# But for examples and help, you can find more information at:
#  docs.ggplot2.org

#################
# Scatterplots

# Before we plotted displacement vs mpg from mtcars as:
qplot(disp,mpg,data=mtcars)

# Remember that the x and y variables have to be explicitly defined in aes() objects
# But otherwise it's easy to do with geom_point()
ggplot(mtcars,aes(x=disp,y=mpg)) + geom_point()

# And just as we could split colors by factor using qplot:
qplot(disp,mpg,data=mtcars,color=factor(cyl))

# We can do the same within the aes object:
ggplot(mtcars,aes(x=disp,y=mpg, color=factor(cyl))) + geom_point()

# And we add labs() labels in the same way we did with qplot
ggplot(mtcars,aes(x=disp,y=mpg, color=factor(cyl))) + geom_point() + labs(color='Cylinders',x='Engine Displacement',y='Miles Per Gallon')

# Now for the extension...

# What if we're trying to make a greyscale plot for publication and want to group by cyl?
# We can't just color here... but we can change the shape
# Here we just change color to shape in the aes() object:
ggplot(mtcars,aes(x=disp,y=mpg, shape=factor(cyl))) + geom_point()

# But if we're changing shapes, sometimes it works better to make them larger with the size argument:
ggplot(mtcars,aes(x=disp,y=mpg, shape=factor(cyl))) + geom_point(size = 3)

# Or what if we wanted to add fit lines in here?
# We discussed in the regular class how to add a linear fit line with stat_smooth()
ggplot(mtcars,aes(x=disp,y=mpg)) + geom_point() + geom_smooth(method = 'lm')

# Note that this displays a 95% confidence interval on the line (briefly discussed in class)
# If you want it without the 95% confidence interval, you just set se to FALSE
ggplot(mtcars,aes(x=disp,y=mpg)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

# But that line doesn't quite capture the trend quite right...
# What if we want to fit a smoothed line through scatter points?
# We can use loess lines, which use local information to fit the slope and thus don't have any linearity assumptions
# We do this by method = 'loess'
ggplot(mtcars,aes(x=disp,y=mpg)) + geom_point() + geom_smooth(method = 'loess')

# Or we could try piecewise linear regression
# If you group the points in the original aes object, if will group the linear fits too
ggplot(mtcars,aes(x=disp,y=mpg,group=factor(cyl))) + geom_point() + geom_smooth(method = 'lm')

# We might want to color the points...
# But if we color them in the ggplot aes object, the lines get colored too
ggplot(mtcars,aes(x=disp,y=mpg,group=factor(cyl),color=factor(cyl))) + geom_point() + geom_smooth(method = 'lm')

# So we should just add the color to the points to avoid this
ggplot(mtcars,aes(x=disp,y=mpg,group=factor(cyl))) + geom_point(aes(color=factor(cyl))) + geom_smooth(method = 'lm')

# But note the syntax inside the geom_point() call...
# color is wrapped in an aes object - why?
# Because if you leave an expression outside of an aes object, ggplot looks in the global environment, and cyl doesn't exist there
# But if you wrap it in an aes object, it looks first in the data frame from ggplot(), and cyl does exist there

# Finally, we can try to extend the lines to fill the space with fullrange
# (This matters more when the points aren't so segmented and won't look nearly as bad)
ggplot(mtcars,aes(x=disp,y=mpg,group=factor(cyl))) + geom_point(aes(color=factor(cyl))) + geom_smooth(method = 'lm',fullrange=TRUE)

# In this case, we might want to make sure the lines and fills are colored differently...
# So we'll move color to the original aes object, and add a fill argument too
ggplot(mtcars,aes(x=disp,y=mpg,group=factor(cyl),color=factor(cyl),fill=factor(cyl))) + geom_point() + geom_smooth(method = 'lm',fullrange=TRUE)

# Now what if you have points all within a factor (or integer) but still want to plot each of the points?
# Say, cyl versus mpg:
ggplot(mtcars,aes(x=factor(cyl),y=mpg)) + geom_point()

# Now you can see some dfferentiation here, but it might help if they weren't right on top of each other
# For this, we can set the point position to jitter
# The default way of doing this is:
ggplot(mtcars,aes(x=factor(cyl),y=mpg)) + geom_point(position='jitter')

# By default, though, this adds a ton of jittering
# So we need to tell ggplot exactly how much jittering should be done with a position_jitter object
# You can set jitter in the width (w) or height (h)
# Here we're just going to jitter width-wise though
ggplot(mtcars,aes(x=factor(cyl),y=mpg)) + geom_point(position=position_jitter(w=0.1))

# And a last note for scatterplots - what if you're in the (good) situation where you have too much data?
# Let's quickly look at the diamonds dataset from ggplot, and plot jewel carat versus the price of that diamond
ggplot(diamonds,aes(x=carat,y=price)) + geom_point()

# Well that just looks like a mass - because we have over 50k data points
dim(diamonds)

# In this case we can't just jitter... there's too much there
# Here we instead might want to make each point lighter so you barely notice the individual points, but can see where they mass
# To do this, we rely on changing the alpha level
# (Generally in graphics, alpha refers to how transparent an item is)
# Note that the setting for alpha is going to depend on a number of factors...
# How many data points? How grouped are they? Etc...
# So you'll need to play around with it to find something that looks good for your graph
# Here we'll use 0.05 just because I said so
# But you can see the structure better here
ggplot(diamonds,aes(x=carat,y=price)) + geom_point(alpha = 0.05)

#################
# Line plots

# We got an introduction to using ggplot() for line plots with error bars
# You just use geom_line for the line and geom_linerange with ymin and ymax for the error bars

# But what if you have time series data with confidence intervals?
# For instance, ERP data typically is displayed using shaded confidence intervals, since the times are close together
# And if you're aggregating across ages, you run into the same problem

# Let's pull out the race data we've used before in class:
load(url('http://vulstats.ucsd.edu/data/cal1020.Rdata'))

# For instance, what if we wanted to plot average speed by age?
# Well, first we would need to aggregate it (and we'll throw SE in there for good measure, and throw out any missing data)
raceagg = na.omit(ddply(cal1020, 'Age',summarise, Speed = mean(speed.mph),SESpeed = se(speed.mph)))

# Now we could plot it as a line graph:
ggplot(raceagg,aes(x=Age,y=Speed)) + geom_line()

# We could even throw in error bars for good measure
ggplot(raceagg,aes(x=Age,y=Speed,ymin=Speed-2*SESpeed,ymax=Speed+2*SESpeed)) + geom_line() + geom_pointrange()

# But that sort of looks cramped...
# Instead, it's very easy to connect the maximums and minimums into an area
# We just use geom_smooth again, but this time tell it stat='identity' to avoid any smoothing transformations
ggplot(raceagg,aes(x=Age,y=Speed,ymin=Speed-2*SESpeed,ymax=Speed+2*SESpeed)) + geom_smooth(stat='identity')

# And this looks much nicer

#################
# 1-D information

# We learned how to make histograms through basic qplots
# For instance, ggplot has data on movie ratings in the movies variable:
head(movies)

# We can plot a histogram of movie ratings easilty with qplot:
qplot(rating,data=movies,binwidth=0.2)

# This is easy to translate to ggplot:
ggplot(movies,aes(x=rating)) + geom_histogram(binwidth=0.2)

# You can also make this output the probability density instead of the count on the y-axis
# The notation is odd though... you have to say y=..density..
ggplot(movies,aes(x=rating,y=..density..)) + geom_histogram(binwidth=0.2)

# With this, you can also make a density plot - which is a smoothed histogram
ggplot(movies,aes(x=rating,y=..density..)) + geom_density()

# But for best results, you can overlay the two:
ggplot(movies,aes(x=rating,y=..density..)) + geom_histogram(binwidth=0.2,fill='grey') + geom_density()

# And maybe you want to display cumulative information...
# For instance, what percent of movies are rated 6 or lower, or 8 or lower?
# Here you can use the stat_ecdf function:
ggplot(movies,aes(x=rating)) + stat_ecdf()

# And you can even split this out by factors - for instance, does rating ecdf differ for documentaries vs other movies?
ggplot(movies,aes(x=rating,group=factor(Documentary),color=factor(Documentary))) + stat_ecdf()

# Looks like people might rate documentaries worse than other movies in general (1 means it is a documentary)

#################
# Boxplots

# Again, these were easy to do with qplot
# For instance, a boxplot of mpg split by cyl from mtcars would be:
qplot(factor(cyl),mpg,data=mtcars,geom='boxplot')

# And with ggplot its a simple transformation:
ggplot(mtcars,aes(x=factor(cyl),y=mpg)) + geom_boxplot()

#################
# Paneling plots

# In the earlier plotting lesson, we discussed paneling plots using the facets argument
# We discussed how we could give it 'rows ~ columns' to split out data
# For instance, if we wanted to split out histograms of mpg by cyl split horizontally (by column), we would type:
qplot(mpg, data = mtcars, geom = 'histogram', binwidth = 3, facets = . ~ cyl)

# Well, it's easy to translate this to ggplot - we just add in a facet_grid with the same argument:
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth=3) + facet_grid( . ~ cyl )

# And we can make it two dimensional (further splitting by gear) with:
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth=3) + facet_grid( gear ~ cyl )

rm(cal1020,puzzagg,raceagg)

###############################
# 10.3 - Data munging & QC    #
###############################

# Your 201b project will involve downloading a large dataset and running analysis on it
# I virtually guarantee that there will be data errors or missing information there
# In an earlier lesson, we discussed some data cleanliness
# Here we'll have a refresher, and get into other sorts of data transformations you might need to use

# This will be in the order of actually cleaning data, but in this section, we'll go over:
# 1) Reading in data not separated by commas
# 2) Using str() and summary() among other things to sanity check
# 3) Transforming unruly things into numbers (including times)
# 4) Using text manipulation to get information out of factors
# 5) Using ifelse() to do simple dichotomizing and replacing
# 6) Using %in% to quickly test whether items in one vector belong to a given set
# 7) How to bucket numbers into groups using cut()
# 8) Combining two factors into one with interaction()

# For this, we'll use the running data again
# But this time, we're going to use the dirty version
# First note that we can't just take it in with read.csv...
cal.raw = read.csv('http://vulstats.ucsd.edu/data/cal10202.txt')
head(cal.raw)

# You'll note there's just one column...
dim(cal.raw)

# But also note that there are a lot of '\t' characters between items...
# This is because this file is tab delimited... meaning there are tabs (\t in ASCII) between items
# Most common formats are either comma delimited (the default), tab delimited, or space delimited
# You can tell R what type of delimiter to use with the sep argument:
cal.raw = read.csv('http://vulstats.ucsd.edu/data/cal10202.txt',sep='\t')
head(cal.raw)

# Now the first thing you'll almost always want to do upon opening a new data set is run str()

# What can str() tell us?
str(cal.raw)

# Well, the first question is, are the data in the right format?
# Off the bat, time isn't - it's a factor, when it should be some sort of number
# Bib probably should be a factor, but we likely won't be using that, so it can be ignored
# Names seem fine
# City / state is the right format... but what's this '?' (We'll get back to this)
# Division seems fine
# Position and place seem fine - we would want them to be numeric
# Age is off... that should be numeric
# Zip codes should be factors, but there's also some oddness in the first few factors
# The next couple seem to be counts of runners in various division splits... so numbers seem fine
# And then the ranks, times, and paces seem off - but frankly we're just going to ignore them anyhow

# So let's get to fixing the obvious problems!
# First, let's cut out the stuff we won't be using
# We only care about the columns up to Zip, excluding position and place
# And that's the first 12 columns
# So we can select columns 1:12:
cal = cal.raw[,1:12]
cal$Class.Position = NULL
cal$Overall.Place = NULL

# Also, we'll be using chip time, so we can ignore gun time
cal$time.gun = NULL

# Side note: you should always keep raw data pure somewhere
# And copy that data to a new variable when doing your cleaning
# Just in case you need to refer back to the raw data later

# Then lets make Age a number using as.integer combined with as.character (remember factor weirdness)
cal$Age = as.integer(as.character(cal$Age))

# Oops! Warnings! Why is that?
# Well, let's look back at the raw data (glad we hung onto it, eh?)
# In specific, let's look at the possible values it could take on
# We'll do that with the levels() function
# This tells you all of the possible levels a factor can take on
levels(cal.raw$Age)

# Aha! There's our culprit! There are some "?" ages
# How many are there?
# Well, if you use as.integer on something that's not an integer, it will return NA
# So we can count them by summing NAs
sum(is.na(cal$Age))

# Okay - 5 unknown ages... we can deal with that (and maybe drop them later)

# Next lets get the times into seconds
# This isn't trivial... but is made easier by one fact
# It looks like everything is written as H:MM:SS
# Which means that we can write a function that splits the string, does multiplication, and returns the value
# Then store it back to the data frame

time.to.num = function(timestr) {
  h = as.integer(substr(timestr,1,1))
  m = as.integer(substr(timestr,3,4))
  s = as.integer(substr(timestr,6,7))
  return(3600*h + 60*m + s)
}
cal$time.sec = sapply(cal$time.chip, time.to.num)

# But wait!
# What if we're wrong in our assumptions that everything is properly formatted?
# Well, the good news is that (a) R would give us a warning, and (b) output an NA
# We didn't see any warnings, but let's check for NAs
# We can combine sum with is.na to count the cases of NAs within a vector:
sum(is.na(cal$time.sec))

# Whew! Everything checks out
# Let's get rid of chip time
cal$time.chip = NULL

# And make this a pace per mile (remember - 10 mile race)
cal$pace.sec = cal$time.sec / 10

# And a speed in mph (remember - transform seconds to hours!)
cal$speed.mph = 1 / (cal$pace.sec / 3600)

# Next, let's do some sanity checks - is our data within reason?
# A quick summary of this can be seen with, well, summary():
summary(cal)

# So what do we take from this?
# Factors are sorted by most common - so are the most common appropriate?
# Numbers provide information like minimums, maximums, and means - so are they reasonable?

# Well let's see - it's a San Diego race, so most people should be from around San Diego...
# But why is CALIFORNIA (not CA) the second most common state? Seems to be a coding error
# We'll be back to this later

# Finally, there seems to be a bit of oddness with age... the minimum age is 4?
# Let's look at that in slightly more detail... how many young 'uns are there?
table(cal$Age)

# Well, having a handful of younger teenagers seems reasonable, but the 4 year old is probably off
# Who is it? Maybe we can find an age range from the Division
subset(cal, Age == 4)

# Huh... in a wheelchair, so doesn't have an age bracket
# Well, let's just keep that in mind for any analyses that involve age for people not in wheelchairs

# Okay - next let's split out whether people are in a wheelchair or not
# How do we figure this out?
# Well, let's look at the types of division:
levels(cal$Division)

# The wheelchair racers are marked with the beginning Division of "Wheelchair"
# So if Division starts with "Wheel" they're in a wheelchair
cal$Wheelchair = (substr(cal$Division,1,5) == 'Wheel')
summary(cal$Wheelchair)

# Now we might also want Sex information
# But how do we get to this?
# This is somewhat tricky, but let's look at Division again
levels(cal$Division)

# It seems to be in there, but in a mildly inaccessible form
# The ability to get this information out is an incredibly important skill to have
# But this will take a different approach each time

# So how do we think about this?
# You might notice Division can be decomposed into three types:
# 1) The divisions starting with '10 Mile' then the sex
# 2) The wheelchair divisions
# 3) The Masters, Overall, and Top Fin divisions

# For type (1), the M/F is always in the 9th character position
substr("10 Mile F 40-44",9,9)

# For type (2), it's in the 12th position
substr("Wheelchair F 1-99",12,12)

# And for type (3), we have no information, so we have to call it NA

# So how do we get this out quickly and efficiently?
# With an ifelse() call
# ifelse() takes three arguments:
#  1) A vector of logical expressions
#  2) A vector or item for each point if they're true
#  3) A vector or item for each point if false

# So for instance, the following call should give us 1,0,3,0:
ifelse(c(T,F,T,F),c(1,2,3,4),0)

# Because it is TRUE in the first and third positions, so the first and third items are from the second argument
# And FALSE in the second and fourth, so we pull in the 0 from the third argument

# Now let's apply this to the race data
# We'll first deal with the known cases, and ignore the ones where Sex isn't known
# This just depends on whether the racer is in a wheelchair or not
cal$Sex = with(cal, ifelse(Wheelchair, substr(Division,12,12), substr(Division,9,9)))

# Now we'll set the "Unknowns" to NA
# Here we will use the %in% operator
# This returns a vector of T/F that is true if the items in the first vector are anywhere in the second
# So, for instance, the following should return F,T,F,T,F,T,F,T,F
#  since those are the positions of the numbers included in the second vector
c(1,2,3,4,5,4,3,2,1) %in% c(2,4,6,8)

# To use this for defining Sex, we'll have a list of Divisions with unknown Sex
unkns = c("10 Mile Overall","10 Mile Masters","Wheelchair Top Fin")

# Then just set Sex to NA if Division is in those, and leave as is otherwise
cal$Sex = with(cal, ifelse(Division %in% unkns, NA, Sex))

# Finally, let's set Sex to be a factor instead of character
cal$Sex = factor(cal$Sex)

# Now let's deal with the State variable
# Recall, there was some oddness in that CA and CALIFORNIA were both in the data
# Maybe we can shorten this by taking the first two state letters?
# So let's check all of the possible states:
levels(cal$State)

# This is bad (but mostly because I'm lazy)...
# We can't just take the first two characters as the state name, since:
#  1) There are three states that start with NE, and only one has a state code NE
#  2) MEXICO would be shortened to ME, but certainly isn't Maine
#  3) There are Canadian states in there as well

# So we need to do explicit recoding
# The good news is that R does have an easy (though non-obvious) way of recoding
# We can use accessing by named character vectors
# Recall, if we have a named vector, such as:
cvect = c('A' = 'Z',
          'B' = 'Y',
          'C' = 'X')

# Then we can access things from it by the names:
cvect[c('A','C','A')]
rm(cvect)

# To use this, we first have to turn State into a character vector:
cal$State = as.character(cal$State)

# Then we have to define the transformations we want, with the names being the original states and the values being the abbreviation
statetrans = c('ARIZONA' = 'AZ',
               'B.C.N' = 'MEXICO',
               'BC' = 'CANADA',
               'CALIFORNIA' = 'CA',
               'EUR' = 'EUROPE',
               'FLORIDA' = 'FL',
               'NEBRASKA' = 'NE',
               'NEVADA' = 'NV',
               'NEW JERSEY' = 'NJ',
               'UTAH' = 'UT',
               'VIRGINIA' = 'VA',
               'WASHINGTON' = 'WA')

statetrans

# Now, if any of the states are in the names of statetrans, replace them, otherwise, keep the same
cal$State = with(cal, ifelse(State %in% names(statetrans), statetrans[State], State))

# We can check our work by using the unique() function instead of levels()
# Since we made this a character vector, it no longer has levels
# But unique() returns only one of every unique thing in the vector and drops duplicates
# So with character vectors, it will do effectively the same thing
unique(cal$State)

# Seems alright so far
# But let's get rid of the "?" and make it NA so it gets dropped when we're analyzing state
cal$State = with(cal, ifelse(State == '?', NA, State))

# And finally, we make it back into a factor
cal$State = factor(cal$State)

# Now, for let's bucket ages into ranges of 5 years
# The good news is that there's an easy function in R to do this: cut()
# At its base, cut takes two arguments:
#  1) A vector of numbers you want to bucket
#  2) A vector of the breakpoints

# So if we want to split by 5s, we would need breakpoints at 5,10,15,etc
# We can start at 0 and end at 85 (since the largest age is 83)
seq(0,85,by=5)

# So to split age into groups, we could say:
cal$Age.Group = cut(cal$Age, seq(0,85,by=5))
summary(cal$Age.Group)

# Note the way things are written...
# For instance, one of the buckets is (25,30]
# In mathematical notation, the curved bracket means 'not included' while square means 'included'
# So this bucket will NOT include 25, but will include 30
# Instead 25 year olds will go into the (20,25] bucket
head(subset(cal,Age == 25))

# But what if we wanted it the other way, so the brackets were [25,30), for instance?
# We could use cut, but set the right argument to FALSE
cal$Age.Group = cut(cal$Age, seq(0,85,by=5),right=FALSE)
summary(cal$Age.Group)

# Now 25 year olds are in the [25,30) bracket
head(subset(cal,Age == 25))

# Finally, let's say we want to do analysis by City in some way
# We have a City column, but there's a potential problem:
# Different states can have the same city name
# For instance, there's a Vancouver in Canada, and a Vancouver in Washington
subset(cal, City == 'Vancouver')

# So if we want to look for differences by city, we need to include both the city and the state
# Luckily, this is easy - there are two ways to do this
# The quick and dirty way is to use the interaction() function
# This takes two vectors of characters or factors and returns a single vector that combines them with a '.'
cvects = data.frame(v1 = rep(c('a','b','c'),each=3), v2 = rep(c('x','y','z'),times=3))
cvects
cvects$interact = with(cvects,interaction(v1,v2))
cvects

# So we can combine city and state like so:
cal$CityState = with(cal, interaction(City,State))
head(cal)

# If you want to replace it with something other than the '.', you can using the sep argument
# So if you want to make it City, State, then the separator is ', '
cal$CityState = with(cal, interaction(City,State, sep = ', '))
head(cal)

# Now lets check this against Eds clean data:
load(url('http://vulstats.ucsd.edu/data/cal1020.Rdata'))
cal1020$Sex = factor(cal1020$Sex)

summary(cal)
summary(cal1020)

# All of our transformations look nearly identical!
# We have the same pace and time information
# The age breaks are the same (though labeled slightly differently)
# We have the same wheelchair counts
# The only differences are:
#  (1) Ed filled in some additional Sex info (likely based on names)
#  (2) We kept and cleaned the State data, while Ed dropped it
#  (3) Ed has corral information, but this is based on knowledge of bib numbers and we didn't include it

# So in sum - there's lots of work that goes into data cleaning, but it's necessary to do appropriate analyses
# The trick it to make sure that data is in the format you want first
# And to figure out where the specific information you want is hiding in your data and getting it out

rm(cvects, statetrans, unkns)

###############################
# 10.4 - Slicing & ordering   #
###############################

# Once you have your data, you might want to slice or review it in different ways
# There are a couple things you should remind yourself about from one of the earliest lessons:
#  1) How to use subset()
#  2) How to remove NAs where appropriate

# Subset is very easy and we've used it before
# Any time you want to select just part of your data, subset is your best friend
# For instance, let's say you want to analyze the relationship between age and speed for runners
# First we'll need to get rid of the wheelchair folk using subset:
cal.analyze = subset(cal, Wheelchair == FALSE)

# Then, because we want to find a relationship, we will want to get rid of any Age NAs
# Note that we should NOT use na.omit here - this function removes ALL NAs from the data frame
# And there are some NAs we don't particularly care about... who cares if we don't know the home state?
# Instead, we'll want to use subset and is.na together to find where Age is not NA
# (Note the '!' which means 'not' - so we want to exclude the NAs)
cal.analyze = subset(cal.analyze, !is.na(Age))
summary(cal.analyze)

# Note now that we (a) don't have wheelchairs, and (b) have no NAs in Age
# So it's good for analysis now

# You may also want to review bits of data too
# For instance, you might want to look at the youngest 50 runners
# One way to do this is figure out what quantile this would be then subset the data that way
# But another way (that we'll use here) is to sort by age and slice off the top 50

# Sorting data frames isn't as obvious as using the sort() function
# Because you have to specify what you want to sort on
# For this, we use the order() function
# order() takes in a vector of numbers, and returns a vector of indices
# These indices are placed in such a way that the first number is the index of the lowest number in the original vector
# The second number is the index of the second lowest number in the original, etc.
# So this should give us a vector of 5 (where 1 is), 4 (where 2 is), 1 (where 3 is), 3 (where 4 is), 2 (where 5 is)
order(c(3,5,4,2,1))

# And we can use this to sort data frames, because this means we can access by row, using the following notation:
cal.sorted = cal.analyze[order(cal.analyze$Age),]

# This will sort age in an ascending way - so the top of this data frame will be the youngest runners
head(cal.sorted)

# Note that if you want to sort in a descending way, all you have to do is put a minus sign before the vector to sort on
# (Since if X > Y, then -X < -Y)
head( cal.analyze[order(-cal.analyze$Age),] )

# But if we want to take the 50 youngest runners, we have to take something else into consideration - ties
# Let's look at the 45-55 items of the sorted data frame:
cal.sorted[45:55,]

# Note that they're all 19 years old
# How does order() break ties? By default it gives you the same order in the original data
# (Here the data was already sorted by time, which is why it seems to be in order already)
# Usually it's not a good idea to just default it - instead you should specify what you want for tie breakers
# The way to do that is to give TWO vectors to order()
# This way, order() will sort first on the first vector, then break any ties with the second vector
# If there are still ties, you can give it a third vector, and so on

# So let's sort first using age, then bib number
cal.sorted = cal.analyze[order(cal.analyze$Age, cal.analyze$bib),]

# Then we can look at the same 45-55 items
cal.sorted[45:55,]

# Note that now within this subset, the runners are sorted by bib (since we're looking at all age ties)

# And now if we want to pull out the youngest 50 runners, we just select again by row:
younguns = cal.sorted[1:50,]
head(younguns)

# Remember, subset and order are powerful functions for looking at specific parts of your data
# With them, you can answer complex questions, like:

# Who has a last name that begins with Z and is over 40?
subset(cal.analyze, substr(name.last,1,1)=='Z' & Age >= 40)

# Who are the 10 youngest male runners not from California?
cal.mnfca = subset(cal.analyze, Sex == 'M' & State != 'CA')
cal.mnfca = cal.mnfca[order(cal.mnfca$Age),]
cal.mnfca[1:10,]

# Does the first letter of your first name affect running speed if you're female and under 40?
cal.fu40 = subset(cal.analyze, Sex == 'F' & Age <= 40)
cal.fu40$FLetter = substr(cal.fu40$name.first,1,1)
summary(aov(speed.mph ~ FLetter, cal.fu40))

rm(cal.analyze,cal.sorted,cal.mnfca,cal.fu40,younguns)

###############################
# 10.5 - Reshaping & merging  #
###############################

# The final big thing to worry about with data munging is when data comes in with the wrong shape or in pieces

# For instance, let's take the following dataset from PSYC201, where 10 subjects are given the same test under three different conditions
# (e.g., a pretty typical repeated measures or longitudinal design)
reshape.test

# Note that there are a few things attached to each score: the subject ID, their verbal IQ, and the date they came in
# But there's a problem... if we want to analyze how Score differs by test and verbal IQ, we can't in this format
# This data is in what is refered to as 'wide' format, where each subject gets its own row
# Wide format is used for some statistical programs (e.g., SPSS) and is output by some tools (e.g., Qualtrix)
# So it won't be too odd to get wide data from your tools or colaborators
# But R needs a separate row for each observation ('long' format)
# Thankfully there's a way to transform data - and it's in the 'reshape2' library called melt()

# melt() at its core takes two arguments
# 1) The data frame you're transforming
# 2) The id.vars argument that is a character list of the column names you want to keep constant

# Once those are given, then all other column names will be put into a 'variable' factor, and the scores will go into 'value'
melt(reshape.test, id.vars = c('Subj','Verb.IQ','Date'))

# You can note that Score1 for Subj1 is 29 in both data frames, Score2 for Subj2 is 53
# And verbal IQ and date are the same for all subjects (since they're also ID variables)

# If you want to clean up the output a bit, you can give it two other arguments:
# 3) variable.name renames the 'variable' column
# 4) value.name renames the 'value' column
melt.test = melt(reshape.test, id.vars = c('Subj','Verb.IQ','Date'),variable.name='Test',value.name='Score')
str(melt.test)

# And now you have a data frame you can run your tests on, just like we've done in the past
summary(aov(Score~Test,melt.test))

# You can also use the cast() function to get your long form data back to wide format
# But since that is rarely used, we won't be covering it here
# Needless to say, though, there are lots of examples online if you need them

# The final thing we might want to do to our datasets is to combine them
# Let's say, for instance, that we get the dataset in two pieces
# Or we need to add extra data
# For this we have merge and row bind (rbind)

# For this example, pretend we have an extra test we want to add onto the data
test4 = data.frame(Subj = paste('Subj',1:10,sep=''),Score4=round(rnorm(10,60,20)))
test4

# Now merge() takes three arguments:
# The first two are the data frames you want to merge
# The third is the names of the columns you're combining by (e.g., Subj)
merge(reshape.test,test4,'Subj')

# Note that it returns a data frame with the Subj sorted
# This is because it matches up the Subj rows in both data frames...
# They don't have to be in the same order
# For instance, let's shuffle the new test data
test4 = test4[sample(1:10),]
test4

# And merge() still works:
merge(reshape.test,test4,'Subj')

# So let's store this data... because if you didn't, nothing would change about reshape.test
reshape.test = merge(reshape.test,test4,'Subj')

# Now let's say we get an 11th subjects data
sub.11 = data.frame(Subj='Subj11',Date='1/21/11',Score1=40,Score2=50,Score3=60,Score4=50,Verb.IQ=115)
sub.11

# Oh, and look, it's put in the wrong order... Verb.IQ comes last here
# But rbind takes all of that into account
# It attaches a second data frame to a first, matching by column names
reshape.test = rbind(reshape.test,sub.11)
reshape.test

# Note that to use rbind, both data frames have to have exactly the same columns

# And that's how you stitch data together

rm(reshape.test,sub.11,test4,melt.test)
