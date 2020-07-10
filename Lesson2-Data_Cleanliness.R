########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#         Lesson 2 - Data Storage & Cleanliness        #
#                                                      #
########################################################

# From now on, we will often be using the PSYC201 package
# This allows us to use the custom functions written for this class
# Thus you will often see the following header to these lesson scripts:
library(PSYC201)

##############################
# 2.1 - Lists & matrices     #
##############################

# We've learned how to store multiple pieces of data in vectors
# But what if we want to structure the data into two dimensions...
# Or store multiple types of data in one variable?
# For this we have matrices and lists

# Matrices are just 2-dimensional vectors
# The way to make one is the function: matrix(data,nrow,ncol)
# Data is a vector of values that gets read off into the columns, one by one
# nrow and ncol tell R how to shape the matrix
# For instance, you can create a matrix with the numbers 1 to 6 and 2 rows, 3 columns with:
matrix(1:6,nrow=2,ncol=3)

# Note how the numbers count down the columns
# This is also different from the matrix with the same data, but 3 rows and 2 columns:
matrix(1:6,nrow=3,ncol=2)

# If you want the data to read across the rows instead of down the columns, set 'byrow' to TRUE
matrix(1:6,nrow=3,ncol=2,byrow=TRUE)

# And finally, you can leave out either the number of row or columns and get the same thing
# R knows how to calculate the other one given the length of the input vector
matrix(1:6,ncol=2,byrow=TRUE)
matrix(1:6,nrow=3,byrow=TRUE)

# Note that the length of the vector must have a length equal to nrow * ncol if you give both arguments
# If not, can you guess what happens?
# (Hint: it starts with 'r', ends with 'ecycling', and is bad)

# Now lets store a matrix from 1-9
junk.mat = matrix(1:9,nrow=3,ncol=3,byrow=TRUE)
junk.mat

# We can access individual matrix items by index, similar to the way we do for vectors
# However, rather than one index, you should use two: [row,col]
junk.mat[2,1] # Should be 4; second row, first column
junk.mat[3,2] # Should be 8; third row, second column

# Accessing by individual indices also work (it flattens the matrix back into a vector), but can get confusing

# You can also take matrix slices by ranges, just like for vectors
junk.mat[1:2,2:3] # Gets the upper right square

# And you can also access individual rows or columns, using the index [row#,] or [,col#]
junk.mat[1,] # Gives you the first row
junk.mat[,3] # Gives you the third column
junk.mat[1:2,] # Gives you rows 1 and 2

# You can assign back to a matrix by index
junk.mat[1,3] = 10 # Change one item
junk.mat

junk.mat[,2] = c(11,12,13) # Change a whole column
junk.mat

# Finally, let's say you also want to find the shape of the matrix
# We can't just use the length command... it works, but only gives us the total number of items in the matrix
length(junk.mat)

# Instead, we need to use the dim() command
# This gives us a vector of two numbers... first the number of rows, then the number of columns
dim(junk.mat)

rm(junk.mat)

# Lists are a way of storing multiple vectors or data types into one data structure
# Remember, if you try to store numbers and characters together in a vector, it changes the numbers to characters

c(1,"A",2)

# Likewise, you can't store vectors within a vector:
c(1,c(2,3),4)

# But with lists you can do both of these things
list(1,"A",2)

list(1,c(2,3),4)

# These look a little funny... that's because a list stores a number of different vectors
# The vector at [[1]] above is just 1, the vector at [[2]] is 2,3, etc.
# Let's store that into a variable
junk.list = list(1,c(2,3),4)

# Accessing items out of a list is a little different - you must use double brackets if you want the data
junk.list[[1]]

junk.list[[2]]

# If you use single brackets, it just gives you a list of length 1 back - which you can't use as a number
junk.list[1]
junk.list[1] + 2 # ERROR!
junk.list[[1]] + 2 # Okay!

class(junk.list[[1]]) # A numeric - we pulled out the vector
class(junk.list[1])   # A list - we just got that element of the list

# We can also create names lists within the list function:
junk.list = list('A' = 1, 'B' = c(2,3), 'C' = 4)
junk.list
names(junk.list)

# We can now access items from that list using the name
junk.list[['A']]

# But there's also a shortcut to the names using $ rather than the double brackets:
junk.list$A

# We can also change members of a list, or even elements of list members:
junk.list$A = c(3,5) # Replaced element A with a vector
junk.list

junk.list[[3]] = 10 # Replaced the third element (C) with 10
junk.list

junk.list[3] = 7 # You also don't have to worry about the double-brackets here
junk.list

junk.list$B[1] = 12 # Replaced the first number in B with 12
junk.list

junk.list[['B']][2] = 16 # Replaced the second number in B with 16
junk.list

# You can also add new items to a list by assigning to a new name within the list
junk.list$newitem = c('Hi','there')
junk.list

# Or remove items from a list by assigning NULL to an existing name
# NULL is a special variable that tells R that nothing is there
junk.list$B = NULL
junk.list

rm(junk.list)

##############################
# 2.2 - Data frames          #
##############################

# Now we move on to data frames - this is the way R stores most datasets
# Effectively, it is a list of vectors, but all of the vectors must be of equal length
# You can think of these as storing one data observation per row
# Each different type of information you have on those observations make up the columns
# Let's look at a builtin data frame - mtcars - as an example
mtcars

# Each of the rows is named with the car type
# Then each of the columns has different information about those cars (avg mpg, # of cylinders, etc.)
# This data is rectangular - the number of rows is equal for every column

# You can build a data frame just like a list:
data.frame('Name' = c('A','B','C'), 'Num' = c(1,2,3), 'Logic' = c(TRUE,FALSE,TRUE))

# Note how much easier this is to look at than a list with the same data
# It's easy to tell in a data frame which records go together - not so much in a list
list('Name' = c('A','B','C'), 'Num' = c(1,2,3), 'Logic' = c(TRUE,FALSE,TRUE))

# But note that if the vectors aren't equal length, R doesn' like it:
data.frame('Name' = c('A','B'), 'Num' = c(1,2,3), 'Logic' = c(TRUE,FALSE,TRUE)) # Note Name has 2 items only

# And if you have a rectangular list, you can transform it into a data frame easily
l = list('Name' = c('A','B','C'), 'Num' = c(1,2,3), 'Logic' = c(TRUE,FALSE,TRUE))
data.frame(l)

# So let's store that data frame:
junk.df = data.frame(l)
rm(l)

# In addition to typing the name of a data frame in the console, RStudio let's us look at them in a window
# Remeber, you can click on the data frame in the Workspace tab, or use the View() command
View(junk.df)

# We can also access data just like we did for lists:
junk.df$Num
junk.df[[3]]

# Or individual items from the vectors:
junk.df$Num[1] # 1: the first element of Num

# Because all the vectors are the same length, we can also use matrix indices to grab data
# However, it's advised not to use this - it's hard to tell exactly what you're getting back
junk.df[1,2] # 1: the first row and second column (in this case Num)

# Finally, we can access individual rows using matrix indices
# We do this by giving the row number, then a comma, just like with matrices
junk.df[1,] # The first data observation
junk.df[2:3,] # The second and third data observation

# We can also create data frames with row names by giving it the row.names argument
junk.df2 = data.frame('Name' = c('A','B','C'), 'Num' = c(1,2,3), 'Logic' = c(TRUE,FALSE,TRUE),
                      row.names = c('Elem.1','Elem.2','Elem.3'))
junk.df2

# Now we can access data by names:
junk.df2['Elem.1','Num']

# Or observations by name
junk.df2['Elem.1',]

# We can get these names back by using the dimnames() command
# This provides a list of names for each dimension
dimnames(junk.df2)

# If we use names(), we just get the column names
names(junk.df2)

# We can also set the dimnames by a list of names with an entry for each dimension:
dimnames(junk.df2) = list(c('E1','E2','E3'),c('Name','Number','Logic'))
junk.df2

# Or just the rownames, by changing the second item in the dimnames list:
dimnames(junk.df2)[[1]] = c('El1','El2','El3')

# You can also add new data to data frames, just by assigning it to an empty column
junk.df2$NewCol = c(10,12,14)
junk.df2

# And can add transformed data back to the table:
junk.df2$Times2 = junk.df2$Number * 2
junk.df2

# If you want to access only subsets of the observations based on conditionals, there are two ways of doing this
# First, you can have the conditional in the first part of the index
junk.df2[junk.df2$Logic == TRUE,]

# Or you can just use the subset() command
# This takes two arguments - the data frame and the conditional
# Note that you don't need to tell subset() which data frame you are drawing the columns for
# It assumes that you are using the same data frame you are subsetting
subset(junk.df2,Logic == TRUE)

# And just like conditionals from simulation, you can use multiple conditionals
subset(junk.df2,Logic == TRUE & Number == 3)

# This is very useful when you want to:
# 1) Review/analyze only parts of your data (e.g., only subjects in Condition 1)
# 2) Eliminate outliers or inappropriate data points

# In either case, you can store the reduced table into another variable for easy access
junk.df2.reduced = subset(junk.df2, Logic == TRUE)
junk.df2.reduced

# The final bit of table access we'll go through involve the with() and attach() commands

# Sometimes you'll be accessing a number of variables in the same data frame at once
# It can get long and tedious to tell R that you are accessing the same data frame over and over
# So the with() command tells R that it should just assume the variables you call or from that data frame
# This takes two commands, the data frame, and the expression you want to run
# For instance, this adds the Number data to the NewCol data
with(junk.df2, Number + NewCol)

# This is identical to typing:
junk.df2$Number + junk.df2$NewCol

# This gets very useful when you want to access only a subset of the table:
with(subset(junk.df2,Logic == TRUE), Number + NewCol)

# And then there are other times that you'll be working with one data frame exclusively
# In this case, you might be using those variables on every line, but don't want to write with() over and over
# For this, there's the attach command, which 'attaches' all of the data from the data frame to the console
attach(junk.df2)

# Once you've attached a data frame, you can access the data without having to tell R the frame
Number
NewCol
Number + NewCol

# However, be careful - if you change any of the data, it will create a copy and won't affect the data frame
Number[2] = 10
Number
junk.df2$Number

# Note that 'Number' now shows up in your workspace

# Finally, when you're done with the data frame, you can detach() it
# This will prevent default access to the data, and is good clean-up practice
detach(junk.df2)
NewCol

# But any copied/changed data will remain
Number

# A final note on data frames... sometimes you want to know what type of data is stored in each column
# For this, you can use the str() command
str(junk.df2)

# Note that this tells you what each collumn is (factor, numeric, logical, etc.) and the first few observations
# This can be very useful if you're given a new dataset and want to know what it contains

# You might also want to get just the first few records
# For this you want to use the head() command - for the first six
# This won't have an effect with junk.df2 (there are only 3 records)
# But look at mtcars
mtcars
head(mtcars)

# It's much easier to get a quick readout with 'head'

# Now let's clean up our data frames
rm(junk.df,junk.df2,Number,junk.df2.reduced)

##############################
# 2.3 - Factors              #
##############################

# In the first lesson, we talked about different types of singluar data that could be stored in variables
# These could be numbers, character strings, or logical values
# Now it's time to learn about another type of data R uses a lot: factors
# Factors come up often when importing data tables
# For instance, what happens if we look at the class of 'Cond' in sample.data?
class(sample.data$Cond)

# Notice it says "factor"
# If you look in the variable, it looks slightly different than a character string...
# Note the 'Levels'
sample.data$Cond

# So why use factors instead of characters?
# Factors are a way for R to store different conditions in a way that's easy to use and save space
# For instance, let's say you're running an experiment with three drugs, a control group, and 20 subjects
# You could store these conditions a character strings
cond.char = rep(c('DrugA','DrugB','DrugC','Control'),each = 5)

# Okay... that works and differentiates the conditions, but it takes up a lot of space
# Computers need more memory to store character strings than numbers
# This isn't really a problem with 20 subjects, but some datasets can have millions of records
# That space would add up fast

# So how about we store them as a number: 1,2,3,4?
cond.num = rep(c(1,2,3,4),each=5)

# Well, that saves space, but it's not obvious which condition is which
# Did you put control first or last? That could make a huge difference if you forget months down the line!

# Factors are a way of splitting the difference here
# They store information as numbers, but associate those numbers with 'levels'
# We can make factors by using the as.factor() command, but usually they come about from reading data in
# (We'll see data input later in this lesson)
cond.fact = as.factor(cond.char)

# When you print out the factor conditions, it looks like the character list
# But at the bottom, you see something that says 'Levels: Control DrugA DrugB DrugC'
# If you want to get the levels of a factor, you can use the levels() command:
levels(cond.fact)

# You shouldn't worry too much about factors beyond this
# This is the default way categorical data is read into R
# Most of the time, they will act just like character strings when you are doing analysis
# However, sometimes this means we will need to use special commands to deal with factors
# We'll see an example of when this is used later in the lesson
rm(cond.char,cond.num,cond.fact)

##############################
# 2.4 - Input/Output         #
##############################

# In many cases you'll want to read and write data from files
# R makes this easy for you with the read.table() and write.table() commands

# To open up a file, first you need to make sure you're in the correct 'working directory'
# This tells the computer where to look for the files
# To learn where your working directory is set, you can use the getwd() command
getwd()

# You can then use the setwd() command with the path argument to set it to where you want to go
# But there are much easier ways of doing this in RStudio

# First, you can use the Files tab
# Just go to the directory you want to access, then click 'More' and select 'Set As Working Directory'
# If you can see the file you want to open in the Files tab, then you're in the right place

# Next, you can select it manually
# Go to the 'Tools' menu, select 'Set Working Directory' and 'Choose Directory'
# You will be able to search through your files to find the right directory

# Finally, if you are working on RScript and your data is in the same directory, this is easy
# Just go to the 'Tools' menu, select 'Set Working Directory' and 'To Source File Location'

# Let's start with the Lesson2_Data1.csv file
# Download it then click on it in the Files pane
# You'll see it's just some text that looks like:

# A,20,1.5
# A,30,0.5
# B,40,2
# B,50,5.5

# This is a typical csv file - different columns of data separated by commas, with each observation on a new line
# You can read this in with the read.table() command
# Note that you must set the 'sep' argument to ',' to tell it that commas count as data breaks
read.table('Lesson2_Data1.csv',sep = ',')

# The columns aren't named - but you can do this with the col.names argument
read.table('Lesson2_Data1.csv',sep = ',',col.names = c('Cond','N1','N2'))

# And can name the rows using the row.names argument
read.table('Lesson2_Data1.csv',sep = ',',col.names = c('Cond','N1','N2'),row.names = c('E1','E2','E3','E4'))

# We can also select only some of the rows
# If you want a certain number of rows, you can use the nrows argument to tell R how many observations you want
# It will only read in the first n rows of the file
read.table('Lesson2_Data1.csv',sep = ',',col.names = c('Cond','N1','N2'),nrows = 2)

# Now let's take a look at the 'Lesson3_Data2.csv' file
# This is exactly like the Data1 file, but column names are already there
# If you just try to read in the table, it will think that the headers are part of the data
read.table('Lesson2_Data2.csv',sep = ',')

# Instead, we want to use the 'header' argument to tell R that the first line is actually a header
# This sets the column names to whatever is in the first line
read.table('Lesson2_Data2.csv',sep=',',header = TRUE)

# A shortcut to all of this is also the read.csv() command
# This is exactly like the read.table command, except it assumes that sep=',' and header=TRUE
read.csv('Lesson2_Data2.csv')

# So now let's store the data:
sample.data = read.csv('Lesson2_Data2.csv',row.names = c('E1','E2','E3','E4'))
sample.data

# It's then a data frame - we can do anything we want to it within R now
sample.data$N3 = sample.data$N1 * 4
sample.data

# But when we're done, we might want to write our data frame back out to a file
# For this, we have the write.table() command
# This needs two arguments, the data frame and the file name
write.table(sample.data,'Lesson2_Output.csv')

# But open it up - this looks a bit messy
# First, we didn't give is a sep argument, so it assumed spaces should separate columns
write.table(sample.data,'Lesson2_Output.csv',sep=',')

# If you open it up again, you'll note that the original data has been overwritten
# We can change this using by setting the append argument to TRUE
# This tells R not to overwrite the file, but instead to write to the end of the file
write.table(sample.data,'Lesson2_Output.csv',sep=',',append=TRUE)

# However, you'll see a warning when you do that - it tells you collumn names are appended
# Usually if you're just adding data, you don't want to rewrite the collumn names
# You can turn this off by setting col.names to FALSE
write.table(sample.data,'Lesson2_Output.csv',sep=',',append=TRUE, col.names = FALSE)

# Let's clear all of this and just get the basic data into this file:
write.table(sample.data,'Lesson2_Output.csv',sep=',')

# Also, like read.csv was shorthand for read.table, there is a write.csv command
# This is almost exactly like setting sep=',' in write.table
write.csv(sample.data,'Lesson2_Output2.csv')

# But open these two files side-by-side and you'll notice one important difference
# There is an extra "", at the start of the write.csv file
# If you tried opening the first output in Excel, the collumn names would be mismatched with the data
# It would put 'Cond' above the element names
# With that extra bit from write.csv, the element names header will be blank and everything else will match

# Finally, we can turn off writing the element names by setting row.names to FALSE
write.csv(sample.data,'Lesson2_Output3.csv',row.names=FALSE)

##############################
# 2.5 - Data cleanliness     #
##############################

# So far all we have learned about reading and using data assumes that it's in good condition
# But this often isn't true in reality - there can be missing or misentered data in your files
# While every situation with unclean data is going to be unique,
#  right now we'll go over three that can be caught relatively easily
# To do this, first we're going to read in some data
dat = read.csv('Lesson2_DataClean.csv')

# Generally, the first thing you'll want to do when reading in data is use the head() command
head(dat)

# From this you can see that there are four fields - two different conditions, and two data fields
# You might also look at this and think that everything looks good, but there are three problems with this data

# Two can be caught easily by using the str() command
str(dat)

# If you have different conditions, those should be factors...
# But Condition2 looks like an 'int'
# Why? Because Condition2 is tagged with '1' '2' and '3', not letters
# So R has no way to understand that Condition2 should be different conditions as opposed to numbers
# This will cause problems later when we start doing analyses where R treats factors and integers differently

# However, it's an easy fix: we just tell R to treat Condition2 as a factor
dat$Condition2 = factor(dat$Condition2)

# Now R will treat Condition2 appropriately
str(dat)

# But there's another issue - Data2 looks like a factor, but should be a number
# This will cause lots of problems if you ever try to do anything with numbers:
dat$Data2 + 3

# Oops...
# So why did this happen?
# Well, this data frame is small enough that we can look through all of Data2
dat$Data2

# If you look carefully, you'll see '91b' in there
# As if someone fat-fingered the data entry
# If you're not sure that it's fat fingered, and want to get rid of it,
#  you can just change it into a character, then a number
dat$Data2Num = as.numeric(as.character(dat$Data2))

# Don't worry about the warning - that's intended because you want to eliminate the 91b
# So why the double transform?
# As we learned in the factors section, factors are stored as numbers with labels
# So if you just use as.numeric, it simply uses the numbers:
as.numeric(dat$Data2)

# By transforming it into characters first, you're making sure that you're using the labels

# But what if you want to correct the fat-finger?
# This isn't easy to do in R...
# But you can easily find where the error is with the which() command
# The which() command returns the indices of a test on a vector
# For instance, if you want to know which rows include Condition1 as 'B', you would write:
which(dat$Condition1 == 'B')

# So if you're testing for where errors occur,
#  you want to know which indices get turned into NA when made into numbers
# For this we use the is.na() command, which return TRUE if the value is NA, FALSE otherwise
is.na(dat$Data2Num)

# So we can just throw a which() question around that call:
which(is.na(dat$Data2Num))

# And now we know that when we edit the data, we use the 14th row

# The last issue is harder to find - missing data
# This gets read in without issue and converted to NA with numbers
# And you never get a warning
# R deals with NAs in different ways depending on the command...
# Sometimes it silently removes NAs, sometimes it throws an error
# So you really want to know where NAs exist

# The way to figure out which columns to worry about is the summary() command
summary(dat)

# You'll note that at the bottom of the Data1 and Data2Num columns, there's an NA's row
# This tells you how many NAs are in each of those fields
# We knew there was one in Data2Num, but the Data1 NA is news
# Which means that if we try to use lots of commands (like mean()), they'll fail:
mean(dat$Data1)

# So how do we deal with this?
# One way is to leave it be and deal with errors as they come up
# However, this isn't advised, since the behavior of NAs is always different

# The other option is to eliminate the rows with NAs
# The easist thing to do here is use the command na.omit()
# This returns a new data frame that eliminates any rows with missing data
dat2 = na.omit(dat)
summary(dat2)

# Note, however, that this eliminates two rows:
nrow(dat)
nrow(dat2)

# That's because there are two rows with NAs...
# The one in Data1, and the one we made in Data2Num

# But what if we didn't care about removing missing data from some columns?
# E.g., we had an 'age' variable that we weren't using but had missing data

# In this case, we can use a more targeted method with subset()
# Here, we can eliminate only the NAs in Data1
# We ask R to return only the parts of dat that are *not* NA
dat3 = subset(dat, !is.na(Data1))

# Note that we now have data that removes the NA in Data1, but keeps the NA in Data2Num
summary(dat3)

# These are some simple ways to make your data acceptable for analysis
# Note that there are some other checks you'll want to run for outliers, etc.
# But we will get to those later when we talk about statistical tests

rm(dat,dat2,dat3)