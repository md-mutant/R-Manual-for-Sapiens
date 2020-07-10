########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#                Lesson 3 - Plotting                   #
#                                                      #
########################################################

# Note that for this lesson, we'll be using two new packages
# The plyr package is for aggregating data
# While ggplot2 makes nice looking graphs
# And gridExtra allows some flexibility in plotting multiple graphs
library(PSYC201)
library(plyr)
library(ggplot2)
library(gridExtra)

##############################
# 3.1 - Aggregating data     #
##############################

# Before we get into plotting, we're going to take a quick trip through aggregation
# Often you'll want to plot things like the mean of conditions or standard errors

# For that, we'll be using the ddply() function from plyr
# The plyr package is incredibly flexible, but here we're going to learn basic summarization
# We'll start with the 'puzzles' data built into PSYC201
head(puzzles)
summary(puzzles)

# We have a few conditions (PuzzleType, Emotion, and Difficulty) and a Time value for each observation
# Let's say we wanted to figure out the average Time for each PuzzleType
# To do this, we use ddply()
# The takes a number of arguments:
#  1) The data frame
#  2) The names of the columns to aggregate over (as characters)
#  3) 'summarize' (there are different things to put here, but don't worry about them for now)
#  4) Arguments with the new, summarized columns

# For instance, averaging time over PuzzleType would be:
ddply(puzzles, c('PuzzleType'), summarize, AvgTime = mean(Time))

# Note this leaves two columns
# We're aggregating over PuzzleType, so that stays there (though there's now only one of each factor)
# And the last argument given to ddply was AvgTime, which is the new column
# Because we said AvgTime = mean(Time), this gives us the mean of the Time values within each grouping
# So the 'Visual' value is:
with(subset(puzzles, PuzzleType == 'Visual'), mean(Time))

# And the 'Word' value is:
with(subset(puzzles, PuzzleType == 'Word'), mean(Time))

# Now, if we want to group over more variables (say PuzzleType and Difficulty), we just add another grouping factor:
ddply(puzzles, c('PuzzleType','Difficulty'), summarize, AvgTime = mean(Time))

# Note that each row in the new data frame is a further subset, for instance:
with(subset(puzzles, PuzzleType == 'Visual' & Difficulty == 'Difficult'), mean(Time))

# But we also don't just have to take the mean...
# For instance, if we wanted the standard error, we have the se() function from PSYC201
# We'll discuss what standard error is in much more detail later
# But for now we can add that in:
ddply(puzzles, c('PuzzleType','Difficulty'), summarize, AvgTime = mean(Time), SETime = se(Time))

# Note that this doesn't change puzzles at all - we need to store it away
# And we'll keep this around for later
puzzagg = ddply(puzzles, c('PuzzleType','Difficulty'), summarize, AvgTime = mean(Time), SETime = se(Time))

##############################
# 3.2 - Scatterplots         #
##############################

# The most basic thing you might want to do is make a scatterplot
# This involves plotting two continuous variables against each other
# For instance, let's look at the mtcars dataset that we were introduced to last lesson
# (Recall this is built into R)
head(mtcars)

# Now let's say we want to plot engine displacement against mpg
# For this, we use the basic qplot command
# First we're going to make the variables outside of mtcars 
disp = mtcars$disp
mpg = mtcars$mpg

# Next we use qplot, putting the x-variable first and y-variable second
qplot(disp, mpg)

# But it would be wasteful have to redefine variables all the time
# So let's get rid of disp and mpg
rm(disp, mpg)

# But we can still use qplot by telling it where to find the data
# We just give mtcars to the data argument:
qplot(disp, mpg, data = mtcars)

# But this is pretty basic plotting... we might want to spice things up a bit
# First, the labels are just the names of the variables
# We might want to make them better
# For this we can give the labels to the xlab and ylab arguments:
qplot(disp, mpg, data = mtcars, xlab = 'Engine Displacement', ylab = 'Miles Per Gallon')

# But the axes are also set automatically
# What if we want to set this ourselves, to make it so that the axes start at 0?
# For this, we use the xlim and ylim arguments
# These take in a length-2 vector specifying the lowest and highest points on the graph
qplot(disp, mpg, data = mtcars, xlim = c(0,500), ylim = c(0,40))

# Now what if we want to split this by the number of cylinders?
# For this we can color things differently using the color command
qplot(disp, mpg, color = factor(cyl),data = mtcars)

# Why did we use factor() around cyl? (Hint: remember the end of last lesson?)

# But what if we don't want to add color, but change the shape instead?
# Easy - we just replace color with shape
qplot(disp, mpg, shape = factor(cyl),data = mtcars)

# So let's bring together what we know to make a good looking graph:
qplot(disp, mpg, color = factor(cyl), data = mtcars, xlim = c(0,500), ylim = c(0,40),
      xlab = "Engine Displacement", ylab = "Miles Per Gallon")

# The only thing we are missing is that the legend title is pretty ugly
# This will be your first taste of the additive nature of ggplot
# For this, we want to "add on" the legend text
# We will use the labs() part, and give the appropriate label to the color argument
# It's just a stub if you do this on its own:
labs(color = 'Cylinders')

# But when you add it to the qplot command, it does its job:
qplot(disp, mpg, color = factor(cyl), data = mtcars, xlim = c(0,500), ylim = c(0,40),
      xlab = "Engine Displacement", ylab = "Miles Per Gallon") + labs(color = 'Cylinders')

# Note that labs can have lots of arguments as well
# So we can name the x and y labels within labs rather than qplot:
qplot(disp, mpg, color = factor(cyl), data = mtcars, xlim = c(0,500), ylim = c(0,40)) +
      labs(color = 'Cylinders', x = 'Engine Displacement', y = 'Miles Per Gallon')

##############################
# 3.3 - Histograms           #
##############################

# Making histograms is also simple... 
#  it's the default of qplot when you only give one argument

# So let's say we want a histogram of mpg from mtcars
# Then all we have to do is:
qplot(mpg, data = mtcars)

# Note that it gives you a warning telling you the default bucket size
# If you don't tell it otherwise, it will give you 30 buckets
# But we can also follow directions and change that if we want skinnier buckets:
qplot(mpg, data = mtcars, binwidth = .5)

# Or fatter
qplot(mpg, data = mtcars, binwidth = 3)

# And it's that easy

##############################
# 3.4 - Bar and line plots   #
##############################

# Continuing with mtcars, say we now want to plot average mpg by cyl
# The first thing we need to do is aggregate the data to get the average mpg
# We'll use ddply for this (and we'll add in standard error for good measure)
avgmtcars = ddply(mtcars, 'cyl',summarize, avgmpg = mean(mpg), sempg = se(mpg))

# Now we can try to plot it with qplot:
qplot(cyl, avgmpg, data = avgmtcars)

# But this isn't very clear...
# We can make this better in one of two ways: as a bar plot or line plot
# The good news is this is easy to do in ggplot - all we have to do is specify the 'geom'
# 'geom' often refers to the type of plot you want
# So we can make it a line plot as:
qplot(cyl, avgmpg, data = avgmtcars, geom = 'line')

# Bar plots are only slightly more complex
# For reasons we won't get into here, you need to also tell ggplot not to take the counts
# We do that by giving the geom as 'bar' and stat as 'identity'
qplot(cyl, avgmpg, data = avgmtcars, geom = 'bar', stat = 'identity')

# Now let's say we want to add errorbars on our graphs
# We will make these standard error bars, that range from one se below to one above the line
# Thus the lower point is avgmpg - sempg, and the upper point is avgmpg + sempg
# To do this, we add a geom type, and tell qplot about the ymin and ymax
# If you're doing a line graph, usually the best geom is pointrange:
qplot(cyl, avgmpg, data = avgmtcars, geom = c('line','pointrange'),
      ymin = avgmpg - sempg, ymax = avgmpg + sempg)


# And if you're using a bar plot, errorbars can be better (at least with a lot of bars):
qplot(cyl, avgmpg, data = avgmtcars, geom = c('bar','errorbar'), stat = 'identity',
      ymin = avgmpg - sempg, ymax = avgmpg + sempg)

# We can do the same thing with two conditions
# Let's go back to the puzzagg data we put together earlier
puzzagg

# Now we might want to put each of the difficulty on the x axis and separate the lines by puzzle type
# But we can't just do what we did before, or it will look funky
qplot(Difficulty, AvgTime, data= puzzagg, geom = 'line')

# Instead we need to make it clear that we group by puzzle type with the group argument
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, geom = 'line')

# But now we don't know which line is which... so we might want to change the colors too
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, color=PuzzleType, geom = 'line')

# And of course we can add error bars in the same way
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, color=PuzzleType, 
      geom = c('line','pointrange'), ymin = AvgTime-SETime, ymax = AvgTime+SETime)

# But note that the error bars overlap...
# Maybe we want to make sure that they don't
# In this case, we give a position_dodge() object to the position argument
# We'll use position_dodge(.1) which is just a slight offset:
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, color=PuzzleType, 
      geom = c('line','pointrange'), ymin = AvgTime-SETime, ymax = AvgTime+SETime,
      position = position_dodge(.1))

# We can also do the same thing with bar plots
# With a few exceptions...
# First, instead of color, we want to change 'fill'
# Second, we always need to tell ggplot that we want a dodge (but can just say 'dodge')
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, fill=PuzzleType,
      geom = 'bar', stat = 'identity', position = 'dodge')

# Then we can add error bars in the same way
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, fill=PuzzleType,
      geom = c('bar','errorbar'), stat = 'identity', position = 'dodge',
      ymin = AvgTime - SETime, ymax = AvgTime + SETime) 

##############################
# 3.5 - Boxplots             #
##############################

# Sometimes you want to show the distribution of two or more groups against each other
# While bar and line graphs are good at showing means and very limited distribution information,
# Often it is better to show the full distribution

# For instance, we showed the means and standard deviations of mpg per cylinder:
qplot(cyl, avgmpg, data = avgmtcars, geom = c('line','pointrange'),
      ymin = avgmpg - sempg, ymax = avgmpg + sempg)

# But if we want to display all of the data points, we can just make a basic scatterplot with categorical information
qplot(factor(cyl), mpg, data = mtcars)

# However, this only works with a small number of data points...
# Too many and you will see overlaps, which can make outliers seem overweighted in the graph
# So instead, we might want to make a boxplot by just telling ggplot the right geom:
qplot(factor(cyl), mpg, data = mtcars, geom='boxplot')

# So what is this showing us?
# The center line in each box is the median of the data
# The box extends from the 25th percentile of data to the 75th percentile of data
# The dots (here only in 8-cyl) are the outliers that are more than 1.5 IQRs beyond 25th and 75th percentiles
#  IQR stands for Inter-Quartile Range, or the distance between the 25th and 75th percentile (the height of the box)
# Finally, the lines go to 1.5 IQRs beyond the box, or until the highest or lowest non-outlier value

# Note that 'outlier' is an often-used term for boxplots, but shouldn't be taken as psychologists tend to use it
# Instead, as mentioned this is any point more than 1.5 IQRs beyond the 25-75th percentiles
# With enough data, you are almost certain to see some of these points
# So don't be surprised by these points, but do use them to gauge how spread out your distributions are

# This make clear some of the differences between the three distributions beyond differences in means
# The 4-cyl mpgs fall within a wide range
# The 6-cyl mpgs fall within a very narrow range
# And the 8-cyl mpgs mostly fall in a narrow range, with a few extremes

# And looking back at our dot-plot, we find that this description captures these distributions well

##############################
# 3.6 - Paneled plots        #
##############################

# We can also look at distributions side by side in another way - by paneling
# Recall we could make a histogram of mpg easily:
qplot(mpg, data = mtcars, binwidth = 3)

# But how do we know how this differs by cyl?
# Well, we could subset mtcars and plot three histograms...
# But it's signficantly easier just to make plots in a panel using the 'facets' argument
# The facets argument takes in a 'formula' of the form:
#  X ~ Y
# Where X is the category that will split by rows, and Y is the category that will split by columns
# Note that you can replace either of those with a period if you don't want to split rows or columns

# So for instance, if you want a histogram of mpg for each cyl in three columns, you would use the formula:
. ~ cyl

# And this would slot into qplot as:
qplot(mpg, data = mtcars, geom = 'histogram', binwidth = 3, facets = . ~ cyl)

# Likewise, if you wanted the histograms to be vertically stacked, you would flip the period and cyl:
qplot(mpg, data = mtcars, geom = 'histogram', binwidth = 3, facets = cyl ~ .)

# And if you want to split by cyl and gear, for instance, you would write:
qplot(mpg, data = mtcars, geom = 'histogram', binwidth = 3, facets = cyl ~ gear)

# Now note that each of the rows represent 4, 6 or 8 cylinders, while the columns are 3, 4, or 5 gears respectively

# But the facet argument doesn't just work for histograms - it works for anything
# Before, we put bars of different colors next to each other with when plotting bar plots
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, fill=PuzzleType,
      geom = 'bar', stat = 'identity', position = 'dodge')

# But rather than use different colors, we can also use different graphs:
qplot(Difficulty, AvgTime, data= puzzagg, facets = . ~ PuzzleType,
      geom = 'bar', stat = 'identity', position = 'dodge')

# So any time you want to split the data by condition(s), you can use the facets argument

# However, let's say we want to plot two entirely separate plots on the same graph
# We can't make this out of panels...
# So instead we'll reach into the 'gridExtra' package for the 'grid.arrange' function

# Now we want to plot two graphs - one of a histogram of mpg from mtcars and one of the puzzle bars
# First, we need to store these plots into variables
mpgplot = qplot(mpg, data = mtcars, binwidth = 3)
puzzbars = qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, fill=PuzzleType,
                 geom = 'bar', stat = 'identity', position = 'dodge')

# Note that this isn't a problem... you can always store plots this way
# Then just call them again when you want to view them
mpgplot

# But for our purpose, we need these to feed into grid.arrange
# We just give grid.arrange all of the plots we want to put into one
grid.arrange(mpgplot,puzzbars)

# But let's say we want them side by side...
# All we need to do is give grid.arrange the ncol argument - here we want 2 columns
grid.arrange(mpgplot,puzzbars,ncol=2)

# And if we like we can add other annotations like titles easily
grid.arrange(mpgplot,puzzbars,ncol=2,main = 'My plots', sub = 'X-label', left = 'Y-label')

##############################
# 3.7 - Themes               #
##############################

# Beyond just layering on and faceting, you can customize how your graphs look in many other ways using themes
# This is another part of the compositional nature of ggplot - you just add themes at the end

# Here we are going to start with one of our earliest graphs and build themes on top of that:
qplot(disp, mpg, color = factor(cyl), data = mtcars) + 
  labs(color = 'Cylinders', x = 'Engine Displacement', y = 'Miles Per Gallon')

# Lets say we want to make the x and y labels bigger
# This is something we can do with themes

# Most of our theme changes will have to do with text, so we need to learn about element_text()
# element_text() is a way that we can define various aspects about how text should be rendered
# We can find the various options using our handy help function:
?element_text

# Note that there are a lot of things we can do - change the size, color, boldness, direction, and offset
# You never have to use any of those arguments either, only the ones you want to change
# Anything left unstated just uses the default of the graph element
#  (for instance, titles tend to be larger than axis ticks, so will default to different sizes)

# So if we wanted to change an elements size to 16 but leave everything else at default, we would use:
element_text(size = 16)

# Next we need to insert it into the appropriate place in the theme
# To figure out where, let's use the help function again:
?theme

# Note that there's a long list of theme elements you can change up
# If we want to change the titles along the axes, well, then one of the axis.title ones looks good
# The big difference between the three is that axis.title changes both axes, axis.title.x changes only x,
#  and axis.title.y changes only y

# Since we want to change both lables, our theme element is:
theme(axis.title = element_text(size = 16))

# Now, this doesn't do anything in of itself, but add it to a ggplot graph and shazam!
qplot(disp, mpg, color = factor(cyl), data = mtcars) + 
  labs(color = 'Cylinders', x = 'Engine Displacement', y = 'Miles Per Gallon') +
  theme(axis.title = element_text(size = 16))

# We can add to this by making the text element more complete...
qplot(disp, mpg, color = factor(cyl), data = mtcars) + 
  labs(color = 'Cylinders', x = 'Engine Displacement', y = 'Miles Per Gallon') +
  theme(axis.title = element_text(size = 16, face = 'bold.italic'))

# Or add other types of themes (like say the legend title)
qplot(disp, mpg, color = factor(cyl), data = mtcars) + 
  labs(color = 'Cylinders', x = 'Engine Displacement', y = 'Miles Per Gallon') +
  theme(axis.title = element_text(size = 16, face = 'bold.italic'), legend.title = element_text(size=14, face = 'italic'))

# Finally, there is one default theme you can throw on if you want the background to be white: theme_bw()
# Just make sure you add this before any other theme elements
qplot(disp, mpg, color = factor(cyl), data = mtcars) + 
  labs(color = 'Cylinders', x = 'Engine Displacement', y = 'Miles Per Gallon') + theme_bw() +
  theme(axis.title = element_text(size = 16, face = 'bold.italic'), legend.title = element_text(size=14, face = 'italic'))

# We don't have time to go through all the rest of the theme elements, but remember '?theme' is your friend here

##############################
# 3.8 - Saving plots         #
##############################

# Once you've made your plots, you might want to save them as a file to look at outside of R or send around
# The easiest way to do this is to plot the graph, then use RStudio to save the picture

# So we make our graph:
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, color=PuzzleType, 
      geom = c('line','pointrange'), ymin = AvgTime-SETime, ymax = AvgTime+SETime,
      position = position_dodge(.1))

# Then click the 'Export' -> 'Save plot as image'
# A screen will come up showing a preview of the plot, along with some options
# The big ones to note are:
#  Directory: where the file gets saved (defaults to the current working directory)
#  File name: what you're calling it
#  Width / Height: The dimensions of the image in pixels

# Note that if you change the width or height, you have to click update preview before you save it or it won't update
# You can also use similar options to save the image to the clipboard to paste in another document

# But what if you have the crazy idea of putting this image in a paper that you're submitting?
# Most journals are strict about how they like their pictures, and usually require 300dpi (dots per inch) images
# This is because the printing process is finer than a computer screen 
# And RStudio doesn't give you that fine detail - nor does it give you an option to do this

# Instead you need to use command line options to set the size and resolution of your files
# The way you do this is tell R that you want to plot to a file rather than the console
# Then plot
# Then tell it to stop

# So let's say I want to save the file as an 8" x 5" picture with 300dpi, and save it as a png file
# (There are other options including TIFF, JPEG, and BMP for people who care)

# Before we can make an image this way, we set the working directory to where we want it
# (I'm just putting this in my home directory)
setwd('~')

# We first want to tell it to plot to a png file using the png() command
# This takes a few important arguments:
#  - The first argument is the name of the image - for instance, 'test.png'
#  - We can give it a 'width' and a 'height'
#  - ... but we also need to tell it what 'units' it will be in, and have the option of
#        inches ('in'), millimeters ('mm') and pixels ('px')
#  - Finally, we can give it 'res'which is the dpi

# So we would open this up as:
png('test.png',height = 5, width = 8, units = 'in', res = 300)

# Then we do our regular plotting
qplot(Difficulty, AvgTime, data= puzzagg, group = PuzzleType, color=PuzzleType, 
      geom = c('line','pointrange'), ymin = AvgTime-SETime, ymax = AvgTime+SETime,
      position = position_dodge(.1))

# Note that nothing shows up in the plot area - this is because it's being sent to the file

# And finally, we close it using the dev.off() command
dev.off()

# Now if you look at the size of the picture, it will be 2400x1500 pixels

# Also, it works well here, but sometimes you may need to change the text size to make everything fit appropriately
# ... this is kind of a trial and error process

##############################
# 3.9 - Extending ggplot     #
##############################

# So far, we have only discussed how to use various incarnations of the qplot() function
# This stands for quick plot, and is a great way of doing basic things quickly
# However, it has some limitations...
# For instance, you can't overlay plots over each other, and you can't vary different parts independently
# If you want to do these and other things that give you more control over your graphs,
#  you need to learn the ggplot() function
# This is beyond the scope of this lesson, but I heavily recommend it
# Although it takes some time to learn,
#  once you have it down, you can make professional looking graphs with little effort
