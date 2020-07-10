########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#                Lesson 1 - Basic R                    #
#                                                      #
########################################################

##############################
# 1.1 - R as a calculator    #
##############################

# The simplest way to use R is for basic arithmetic
# The line below simply adds 3 and 2
# (Press the 'Run' button or ctrl/apple-enter while highlighting the line to run it)

3 + 2

# Note the output: [1] 5
# The number after the [1] is the result of the line
# (We'll discuss why the [1] is there later)

# You can also do other basic arithmetic:

# Subtraction
3 - 2

# Multiplication
3 * 2

# Division
3 / 2

# Square roots (note the parentheses)
sqrt(4)

# Exponentiation
3 ^ 2
# or
3 ** 2
# Note the double asterisk

# Expoentiation with base e
exp(2)

# Logarithms (natural logarithms)
log(3)

# Logarithms in base 10
# Put the number you take the log of first, then the base after a comma
log(100,10)

# And remember PEMDAS - R follows this ordering
# This expression:
2 + 3 ^ 2

# Is not the same as this expression
(2 + 3) ^ 2

# But anything within the parentheses of a function is evaluated before that function is called
# So this expression
sqrt(5 + 11)

# Is equivalent to this expression:
(5 + 11) ^ 0.5

# With exponents, you can get very larger numbers very quickly - for these R uses scientific notation
# The output of this is equivalent to 9.53... * 10^13 - everything after the 'e' tells you what power to raise the 10 to
5 ^ 20

# The number after the 'e' can also be negative for very small numbers - in this case it is 10 to a negative power
# For instance, this is equivalent to 1.024 * (1/10^7)
5 ^ -10

# Modulo arithmetic
# This gives you the remainder when you divide the first number by the second
# Note that you must type the % symbol twice
5 %% 2

# And finally, there's the choose and factorial functions:
choose(5,2)

factorial(5)

# You might also notice that there's a lot of words after #s that doesn't do anything
# That's because # tells R to ignore everything after it (as a 'comment')
# So running this line won't cause an error
But this will

2 + 2 # You can also put comments 'in-line' - in this case, the line evaluates 2+2 but ignores everything else

##############################
# 1.2 - Defining variables   #
##############################

# You can store things in a variable using '<-'
# Here we'll store the value 2 into the variable a
a <- 2

# Note that you didn't see the 2 appear as a result - it just stored the value instead of printing it
# Now enter the variable name to see what is stored in it
a

# Now go to the 'Workspace' tab in RStudio
# You should see that under 'values' is a with 2 attached to it - this is another way of seeing what is stored in a variable

# There are other ways of storing in variables - '=' is the same as '<-'
a = 4
a

# You can also make the assignment at the end using '->'
6 -> a
a

# So which to use?
# R purists will say always use '<-' since '=' has other uses and assigning at the end is bad form
# However, you'll see me use '=' since that's the way of defining variables in many other programming languages

# More important than storing simple numbers is the ability to store the results of calculations
# Below we are storing 9 in a, since 9 = 3 * 3

a = 3 * 3
a

# Likewise, you can use variables in calculations to store into another variable
b = a * 4
b

# You can even do a calculation on a variable and store it back as itself
# For instance, since a = 9, if we say 'a = a - 5', then a will equal 9 - 5 = 4
a = a - 5
a

# You should note that whenever you store a value, it overwrites anything that was stored there before
# So instead, you can store in many different variable names

var1 = 5 + 5
var2 = log(10)
var.new = var1 * var2

# So what can't you use as a variable name?
# First, you can't start a variable name with a number (the line below should cause R to yell at you)
2var = 5

# You also can't use mathematical symbols
V*V = 2

# Case is also important - even though you have a variable 'a' you won't be able to find 'A'
A

# And you really should avoid using basic functions as names
# R will let you, but it can cause confusion
# (Are you trying to reference the variable 'log' or take the log of a number?)

# Now, you can look in the 'Workspace' tab to see what variables you have assigned
# Or you can use the ls() function
ls()

# And we have a lot of junk - you can delete variables with the rm() function
rm(a)

# And you can delete a number of variables at once
rm(b, var.new,var1,var2)

# We will do that often at the end of sections to keep our variable space clean

##############################
# 1.3 - Types of data        #
##############################

# There are a number of types of data that you can store - so far we've only worked with numbers ('numeric' data)
# You can determine the type of data using the class() function
num = 5
class(num)

# But we can also store words ('character' data)
# Note that characters are surrounded by quotes
char = 'Hello!'
char
class(char)

# You can use single or double quotes
char.2 = "Hello again"
char.2

# There are also special 'logical' data values to specify is something is TRUE or FALSE
logic = TRUE
class(logic)

# Note that TRUE and FALSE must be typed in all caps - they are special values in R
# Typing in lower-case will not work (R doesn't know what 'true' is)
logic = true

# However, they can be appreviated to 'T' or 'F'
logic.2 = F
logic.2

# But it's often better to write out the full TRUE/FALSE in your code, since it makes it more readable

# So what's special about logical data? You can do tests, and the results are returned as TRUE/FALSE values
# For instance, is 'num' equal to 3?
# Note here we have to use double-equals - AKA the equality operator
# (This is why R purists don't like the assigment equals - it can get confused with the equality operator)
num == 3

# Well, no, we made it equal to five, so that should return false
# But does it equal 5?
num == 5

# We can also put variables in an expression to test
# Does num-2 equal 3? It does!
(num - 2) == 3

# There are lots of other tests you can run:
# Is num greater than 3?
num > 3

# Is num less than 7?
num < 7

# Is num NOT equal to 5?
num != 5

# Is num less than or equal to 5?
num <= 5

# You can then assign logical values to variables
lg1 = (num > 3) # TRUE
lg2 = (num > 7) # FALSE

# You can also combine logical values
# The '&' (and) operator returns TRUE only if everything is TRUE (so will be FALSE below)
lg1 & lg2

# The '|' (or) operator returns TRUE if any one argument is TRUE (so will be TRUE below)
lg1 | lg2

# The '!' (not) operator returns the opposite of what is in the variable
!lg1 # FALSE
!lg2 # TRUE

# Now let's clean up the variables
rm(num,char,char.2,logic,lg1,lg2)

##############################
# 1.4 - Vectors              #
##############################

# You can also store vectors of information, which contains a number of data points
# You build vectors with the c() function, like so:
c(1,2,3,4)

# You can store these vectors in a variable
a = c(1,2,3)

# You can even combine two vectors with c()
b = c(4,5,6)
c(a,b)

# In RStudio you can edit vectors by double-clicking on them in the 'Workspace' tab
# Try it with 'a'
# You'll note that the console fills with fix(a) when you do that - this is the same as entering that command
fix(a)

# You can only store one type of data in a vector
# If you try to mix types, it will force the data to get along
# In the case below, it will make the numbers into characters (note the quotes marking these as characters)
c(1,2,'a')

# Let's say you want to make a vectors of the number 1 to 10 - you could type this out the long way
c(1,2,3,4,5,6,7,8,9,10)

# But that's a lot of work - there's an easier way
# The x:y operator returns a vector of values from x to y
1:10

# You can also use the seq() function to do the same thing
seq(1,10, by = 1)

# Note the use of 'by = 1'
# Sometimes functions will have a lot of different inputs, some of which don't need to be used
# By 'naming' the arguments, you make sure you're giving input correctly (and making your code easier to read)
# In this case, 'by' doesn't need to be used at all (the default is 1 if nothing is entered):
seq(1,10)

# So why use 'seq' instead of ':'? Because it's more flexible
# Let's say we wanted all of the one digit odd numbers...
# That's just the numbers from 1 to 9, incrimenting by 2 each time:
seq(1,9,by = 2)

# Or let's say we want all of the numbers divisible by 5 from 0 to 100
seq(0,100,by = 5)

# Or we want to count backwards by 2 from 20
seq(20,0,by = -2)

# But let's say we don't want to count, we just want a vector with 1 repeated 10 times
# There's a function for that - rep()
rep(1,times = 10)

# You can also repeat vectors
rep(c(1,2),times = 5)

# Note that this gives you 1,2,1,2,...
# If we want 1,1,1,1,1,2,2,2,2,2 there's the 'each' argument
rep(c(1,2),each = 5)

# You might still be curious why the [1] is at the start of all output
# It's because R is a 'vectorized' language - all variables are stored in vectors
# Even single numbers are vectors of length 1
# The [1] tells you where in the vector you are starting on that line

# Sometimes you might have a longer vector - this helps to orient where in the vector you are
# In the example below, note that if the second line starts with [28], that line starts with the 28th entry in the vector
1:100

# Now let's assign a few vectors to play around with
v.1 = 1:5
v.2 = seq(2,10, by = 2)
v.3 = c(3,7,9,1)

# If you want to access individual vector items, you can do that too if you know the number
# For instance, to get the first element of v.1, you would type:
v.1[1]

# For the third, type:
v.1[3]

# Note that to do this, you put square brackets after the vector, and give the index of the value you want to get
# Indices start at 1 in R, and go through the length of the vector
# If you try to access a number with an index that's out of bounds, it will return an NA value:
v.1[10]

# There are also a few basic functions you should know on vectors:

# First, the length() function tells you how long a vector is (e.g., how many data points it holds)
length(v.1)

# Next, you can add everything in a vector together with sum()
v.2
sum(v.2)
2 + 4 + 6 + 8 + 10

# You can also find the average value in a vector with mean()
mean(v.2)

# Or find the lowest value with min() or highest with max()
min(v.2)
max(v.2)

# You can also sort a list from smallest to largest with sort()
v.3
sort(v.3)

# Or sort from largest to smallest by telling the function that decreasing = TRUE
sort(v.3, decreasing = TRUE)

# If you have two vectors of the same length, you can add all of the elements together in a piecewise function with +
v.1
v.2
v.2 + v.1

# Other mathematical operators work the same way:
v.2 - v.1
v.2 * v.1
v.2 / v.1

# You can also add/subtract/multiply/etc. a single number to each entry in the vector
v.1 + 2
v.1 * 2

# Or test each element of a vector - this returns a vector of logicals
v.1 >= 3

# Want to know how many of the vector elements pass the conditional? Use sum()!
sum(v.1 >= 3)

# It's also possible to do arithmetic on vectors of unequal lengths - but THIS IS BAD!
# R will 'recycle' the shorter vector, and repeat it so that it matches the longer one
v.3 + c(1,2)

# This will be the same as:
v.3 + c(1,2,1,2)

# I'm going to reiterate: THIS IS BAD!
# While it can save space in some instances, it makes it harder to see what your code is doing

# In the rare case that you actually want to recycle vectors, you should use the rep() function
# For instance:
v.3 + rep(c(1,2),times = 2)

# This makes it easier to see what is going on and thus makes it more difficult for unwanted errors to slip in
# So don't let R recycle your vectors - ever!

# Now let's clean up our variables
rm(a,b,v.1,v.2,v.3)

##############################
# 1.5 - Vector access        #
##############################

# We just learned how to get individual pieces out of vectors:
junk.vector = c(2,6,4,10,8)
junk.vector[3] # Should be 4 - the item in the third position

# But lets say you wanted a larger slice of the vector - perhaps the first three values...
# You can get these in one swoop using the range 1:3
junk.vector[1:3] # 2, 6, 4

# Or the last three items in the vector
junk.vector[3:5] # 4, 10, 8

# But if we don't know exactly how long the vector is, we using the length function
# This tells it to get back all items from index (length-2) to the end (length)
junk.vector[(length(junk.vector)-2):length(junk.vector)] # 4, 10, 8

# Note the parentheses... if you don't use them, this will solve as length - (2:length) which becomes 3:0...
junk.vector[length(junk.vector)-2:length(junk.vector)]
junk.vector[3:0] # 4, 6, 2 (position 0 is ignored)

# Likewise, you can get all items EXCEPT certain indices using negative values
# For instance, if you want everything but the first entry, you would write
junk.vector[-1] # 6, 4, 10, 8

# And if you want everything except entries 2 through 4, you would write:
junk.vector[-2:-4] # 2, 8

# You can also 'name' vector entries using the names() command
# By default all names are null... you can check this:
names(junk.vector)

# But you can then set the names by assigning a vector of names to that command:
names(junk.vector) = c("Alan","Barbara","Cathryn","Dylan","Eric")

# Now the vector prints with names
junk.vector

# You can also check the names directly again
names(junk.vector)

# And now you can pull from the vector by name
junk.vector["Dylan"]

# Or multiple names
junk.vector[c('Alan','Eric')]

# Or erase the names
names(junk.vector) = NULL
junk.vector

# You can also get only values that satisfy a conditional by placing that conditional in the brackets
# For instance, say you wanted only the values in junk.vector that are greater than 5:
junk.vector[junk.vector > 5]

# This works because you get a vector of logicals out of junk.vector > 5
junk.vector > 5 # F T F T T

# And when you access a vector with a vector of equal length logicals, it returns just the TRUE ones
junk.vector[c(FALSE,TRUE,FALSE,TRUE,TRUE)]

# This works just as well for conditionals from other vectors
cond.vector = c("Red","Blue","Red","Blue","Green")
cond.vector == "Red" # T F T F F
junk.vector[cond.vector == "Red"]

# On top of just indexing data in a vector, you might want to change it
# If you want to change a single index, this is easy - you just assign to that index
junk.vector
junk.vector[3] = 7
junk.vector

# You can also assign to a range of indices
junk.vector[2:4] = c(8,10,2)
junk.vector

# Note that you must be careful that the length of the sub-vector you are overwriting is the same length as what you feed it
# Recycling will occur otherwise, and I hope I don't have you remind you how bad that is
# Seriously... don't do it

# You might also want to assign a value to everything that satisfies a given condition
# This might happen if you want to enact a ceiling/floor, or code for error data
# For instance, what if we want anything greater than 9 in junk.vector to be set to 9 (a ceiling)?
junk.vector[junk.vector > 9] = 9
junk.vector

# That's it for vector access... now let's clean up
rm(junk.vector,cond.vector)

##############################
# 1.6 - Custom functions     #
##############################

# We've encountered a number of functions before, from log() to class() to seq() to sort() - these are built-in functions
# You can also build your own functions too - like the one below that takes a number and doubles it:
times2 = function(x) {
  y = x * 2
  return(y)
}

# Now you can call it and give it the argument you want to double
times2(5)
times2(10)

# The way to define a function is FUNCTION.NAME = function(ARGS) {FUNCTION.CODE}
# FUNCTION.NAME is just any variable name
# ARGS is any number of arguments you want to pass to the function (here we just used one - x)
# FUNCTION.CODE is all the stuff you want the function to do - note that it must be within curly brackets

# Let's break it down even further, by starting with the function call
# When you enter times2(10), it runs the code in times2 and sets the variable x to 10
# It knows that x is what you want to set because we defined x in the arguments: times2 = function(x) {...}

# It then starts running the code - the first thing it does is assign x * 2 to the variable y
# Note that y is a temporary variable that only exists while the function is running
# Since the function has now stopped, if you try to see what is in y, you'll get an error
y

# The last line of the function tells it to give you the value that is stored in y
# The return() command ends the function and returns whatever the stuff in the parentheses evaluates to
# This is your way of getting information out of the function and back to the console, since all variables are temporary

# Let's try another trivial example that adds two items:
add = function(x,y) {
  return(x + y)
}

add(1,2)
add(5,5)

# This function takes two arguments (x & y)
# You can define multiple arguments by putting commas between them as above
# Also, note that return() first evaluates x + y, then gives it back to you

# Since functions return a value, you can call functions on the output from other functions
# For instance, the code below gives 10: 2*(4+1)
times2(add(4,1))

# If you want to edit functions, you can do it the same way as vectors:
# 1) Double click on the function on the 'Workspace' tab
# 2) Type fix(FUNCTION.NAME)

# You can also view function code by typing in the function name without any arguments
times2

# This also works with some built-in functions (in case you're curious)
ls

# However, some built-ins are 'primitives' - they aren't written in R code so you can't see their guts
# You will know a function is a primitive if there is .Primitive() when you try to view the function
sum

##############################
# 1.7 - Packages             #
##############################

# R has tons of functionality built in, but there's a lot missing from what's currently on your computer
# Let's say we want to find the skewness of a distribution (we can worry what that means later)
# Now by default there isn't a function called 'skewness' that will do this
skewdat = c(3,6,1,4,7,1,4,1,2,1,3,1,2,6,2,9,1)
skewness(skewdat) # ERROR!

# So if we want to find the skewness, we need to write our own function, right?

# Wrong!
# There is a function of this type, it's just not on your computer
# R is so powerful because it has 'packages' - modular collections of functions and data
# If there's something you want to do with statistics that's not in the base R distribution,
# more likely than not, someone somewhere has written that function into a package

# But we need to get those packages onto our computer in order to use them
# For this, we have the install.packages() function
# We give it the name of the package we want, and it downloads and installs it for us

# I'll tell you that the skewness() function is in the 'moments' package
# So to get it, we type:
install.packages('moments')

# R may ask you to 'choose a mirror' - this is asking where to download the package from
# I typically choose one of the CA ones since they're closest, but it's the same package no matter where you get it

# So we can use the skewness() function now, right?
skewness(skewdat)

# Wrong!
# The 'moments' package is now on our computers, but we still have to tell R to use it
# For this, we use the library() command
# This command tells R that we want to open up the package and use it for the session
# Then we can call the functions inside
library(moments)
skewness(skewdat)

# Whew! It works!

# Note that you only have to call install.packages() once, but library() every time you use R
# Once you've installed the package, it's on your computer
# But every time you start up R, you need to tell it which packages to use

# You can check which packages are currently being used as well with the 'Packages' tab in RStudio
# This lists every package you have installed on your computer
# If there's a check mark next to the package, it's currently in use
# (Note that if you don't want to type library() you can just check the box here as well to do the same thing)

# This should also shed some light on the 'PSYC201Setup.R' script you should have run
# I wanted to make sure that you have all the packages I know you'll need for the course on your computer
# Note that it also contains a custom PSYC201 package that will make your life (and mine) easier throughout the course

##############################
# 1.8 - Loading data         #
##############################

# There are many ways of getting data into and out of R, but here we will focus on save() and load()
# These functions speak in R's "native language":
#  they keep variables as is, but don't play nicely outside of R (as some other functions next lecture will)

# save() is used to store variables between R sessions, or to transfer between computers
# It takes a number of variables and puts them in a file
# You give the function a number of variables, then the named 'file' which tells you where to store it
# For instance:
s1 = 1
s2 = 'abc'
s3 = 50
save(s1,s2,s3,file = 'testsave.RData')

# Note that you will usually want to end your file with .RData to keep these apart
# You can now go to the 'Files' tab in RStudio, and you will likely see testsave.RData appear there
# (If not, you may have changed the directory already...
#  click on the 'More' gear and select 'Set As Working Directory', then try again)

# Now that we have that file, we can clear all of the variables in R
rm(s1,s2,s3)

# But if we want the variables again, all we have to do is load() the file we saved them to
load('testsave.RData')

# And now we can see that those variables exist once again
s1
s2
s3

# One note about the load() command - it depends on where your 'working directory' is
# We will go over this in a bit more detail in a later lesson
# However, you will need to load() data for the homework
# So make sure that when you are writing your homework scripts, you:
#  (a) Save the .RData file you download to the same folder that your homework script is in
#  (b) When you are writing your script, go to the Session menu and select
#        Set Working Directory -> To Source File Location
# If you don't do this, it won't load correctly

##############################
# 1.9 - So you're stuck...   #
##############################

# Now you have enough R knowledge to do a lot, but you still might get stuck trying to figure out a function
# In that case, you don't need to panic, you just need the help() function
# You can call help() on any built-in function and an explanation will show up in the 'Help' window
help(sum)

# This tells you what arguments the function takes, and what it does

# You can also write this shorthand by starting with ?
?sum

# That works if you know the function but don't know how it works
# But in many cases, you won't even know what you're looking for
# For those times, there's '??'
# This searches through all functions and gives you a list of functions that have help pages containing your search
# For instance, if you want to know how to do trigonometry:
??trigonometry

# From that you see base::Trig is a package - this means that Trig is part of the base R functions
# So we move on:
?Trig

# And now we know all about cos, sin, tan, acos, asin, atan, and atan2 - huzzah!