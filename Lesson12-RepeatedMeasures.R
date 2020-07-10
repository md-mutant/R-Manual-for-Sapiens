########################################################
#                                                      #
#                   PSYC201 R Lab                      #
#            Lesson 12 - Repeated Measures             #
#                                                      #
########################################################

library(PSYC201)
library(ggplot2)
library(plyr)

##############################
# 12.1 - Repeated measures   #
##############################

# Let's start with a simple repeated measures example: the wine dataset in PSYC201
wine

# In this dataset we have a wine tasting where six judges each gave a rating to four different wines
# We want to know - is there a difference between the wines?
# Well, we can look at the mean scores:
with(wine,tapply(Score,Wine,mean))

# Looks like Wine 3 is worse... but is it significant?
summary(aov(Score ~ Wine, wine))

# Doesn't look like it... why not?
# We might expect judges to differ in how strict they are with their ratings
# And in fact, we can look at this data in tabular format:
wine.alt

# You'll note that the judges differ wildly - some really like wine (Judge 1), and some seem to hate it (Judge 3)
# Note that this is a within-subject design, with the Judge as subjects
# With a simple design like this, you can just add Judge first and test whether Wine has a significant effect on Score
summary(aov(Score ~ Judge + Wine, wine))

# Now it does!
# But why?

# Note that now the SS for Wine is the same in both summaries, but the F score is higher with Judge in the model
# But the SSE is very different...
# Here, the Judge factor has soaked up a large portion of the SSE, at a cost of 5 df
# Therefore, the MSE is about a third as small, with a corresponding increase in F

# But how does this fit with what we know about blocking and repeated measures?
# Recall in the conceptual lesson, with blocking we wanted to separate the SSE into two sources:
# 1) Error due to the subjects, and 2) error around the subjects
# Any within-subjects factors should use error around the subjects for the F-statistic
# While between-subjects factors should use the subject error

# In this example, Wine is a within-subject factor
# Thus we don't care about the subject error, just the error within each Judge
# But by adding Judge as a factor, the Residual error is the error around the subjects

# There's another way of adding blocking terms to aov objects as well - through Error()
# You can add an Error() term to aov formulas to tell R what you want to use as a blocking factor
# If you are using a fully within-subject design, you can just put the blocking factor in the Error()
# For instance:
summary(aov(Score ~ Wine + Error(Judge),wine))

# Now note that this looks a bit different than the typical anova table
# This is because aov is now explicitly separating the error terms
# In the top field, you have the error around the Judge factor
# Because this is fully within-subjects, the only Sums of Squares is the Residuals
# These are the SS-Subject due to Judge

# In the bottom field, you have both Wine and Residuals
# Here Wine is the same Sums of Squares as before
# And Residuals is the same Sums of Squares once Judge is taken out
# Thus, for Wine, you see the same F as the ANOVA with Judge as another term

# Now, note that the way we've written the Error term was a shortcut
# Technically, that should be Error(Judge / Wine)
# In this case, the '/' operator means 'within'
# Which is telling R that the Wine factor is within-subject
# Thus you could write:
summary(aov(Score ~ Wine + Error(Judge / Wine),wine))

# But if you could get this same information by just adding Judge as a factor, why do we care?
# Because often you do need to tell R how to divvy up error
# In this case (and very simple cases) it didn't matter, but we will see how it does later

# Also, why are we using aov? Why not lm?
# Simply because lm doesn't support an Error term
# So if you're doing repeated measures, you need to use aov()
# (Or mle based models... but we'll learn about that later)

# So let's move onto a more complicated design...
# For this example, let's go back to the puzzles data
head(puzzles)

# Recall from earlier lessons that people got puzzles in three different types of conditions:
#  (1) Difficulty (easy vs difficult)
#  (2) Type of puzzle (visual vs word)
#  (3) Emotional state (happy vs sad)
# We then measured how long it took them to solve each puzzle, and wanted to know if solution time differed by condition

# We could visualize this data quickly by:
puzzagg = ddply(puzzles,c('PuzzleType','Emotion','Difficulty'),summarize,AvgTime=mean(Time),SETime=se(Time))
ggplot(puzzagg,aes(x=PuzzleType,y=AvgTime,ymin=AvgTime-2*SETime,ymax=AvgTime+2*SETime,group=Difficulty,color=Difficulty)) +
  geom_line() + geom_pointrange() + facet_grid(.~Emotion)

# But before we ignored one of the data fields: SubjID
# We didn't test 160 different subjects.. instead we gave 40 subjects different puzzle types
levels(puzzles$SubjID)

# Now - emotional state isn't easily switched, so each subject was assigned to either the happy or sad state
# (The between-subjects manipulation)
# But we could give each subject easy word, hard word, easy visual, and hard visual puzzle types (within-subjects manipulations)

##########################
# One between-subject factor

# Let's start with a very simple question (between subject factors): does the emotion affect solution time?
# The model for this is quite simple
summary(aov(Time ~ Emotion, data=puzzles))

# Well this is clearly significant... but we are also treating all observations as independent
# But some people might be better at solving puzzles than others, and we can account for this
# And if we're just looking at emotion, there will be multiple observations per person

# So we write the model using using SubjID in the error term
# We don't need to use any slash because there are no within-subject effects
summary(aov(Time ~ Emotion + Error(SubjID),data=puzzles))

# So why is this different from the original test we used?
# In the original test, we treated all observations as independent
# Thus we assumed testing a person four different times is like getting four different tests
# This is clearly not true

# Instead, when you have multiple observations, R will segment the error into within subject, and between subject
# The within subject error is just the natural variability that people will have from task to task
# The between subject error measures the differences across people
# When you build the model correctly, R understands that Emotion is a between subjects factor and treats it appropriately
# Note that this is identical to averaging peoples' times and running an ANOVA on that - the way Ed mentioned you should do it

emo.sub = ddply(puzzles,c('SubjID','Emotion'),summarize,AvgScore = mean(Time))
summary(aov(AvgScore~Emotion,data=emo.sub))

##########################
# One within-subject factor

# Next we might ask, does the difficulty affect solution time?
# Here we can again try our naive model without subjects:
summary(aov(Time ~ Difficulty, data = puzzles))

# So there's certainly an effect...
# But again we're treating all observations as independent
# And this is an even worse assumption now, because subjects differ, and you get easy and hard puzzles for each subject
# Thus observations are clearly not independent

# But never fear: appropriate model building in R can save you!
# Here we DO use the slash, because Difficulty is within subjectss
summary(aov(Time ~ Difficulty + Error(SubjID / Difficulty), data = puzzles))

# Note here that our F-value has increased from 190 to 357...
# As we accounted for variability between subjects, we got more power

# But why did we have to tell R that Difficulty is nested in SubjID?
# What would happen if we didn't?
summary(aov(Time ~ Difficulty + Error(SubjID),data=puzzles))

# Note that we no longer see the SubjID:Difficulty error... and Difficulty is assigned to Within error
# If we ignore the nesting call, then R assumes you just want a single error term
# But we're still getting multiple observations of each difficulty type per person... (one visual, one word)
# If we don't include the nesting term, then R treats the observations as independent
# But they're not
# If we do include the term, R understands that you mean to say that people will vary even if you give them the same test multiple times
# And it averages over that appropriately

# So big note: Always build your aov error models appropriately to get the right answer!

######################################################
# One within-subject factor, one between-subject factor

# What if we ignore subject?
summary(aov(Time ~ Difficulty * Emotion, data=puzzles))

# Well, the interaction is not quite significant, but the other two clearly are...
# But we're ignoring the effect of subject here
# So let's include it

# However, it's a bit more tricky here... each subject was assigned to one of the two emotional states
# So SubjID is nested withing Emotion, and Difficulty is nested within SubjID
# Thus we are dealing with a mixed design

# Writing out the model is still easy...
# We just write the fixed factors as normal, then nest the subject effects appropriately
summary(aov(Time ~ Difficulty * Emotion + Error(SubjID / Difficulty), data=puzzles))

# Here we have the three same error levels as we saw above, but now three fixed items:
#  Difficulty, Emotion, and the interaction

# But note where they have been assigned:
#  Emotion has been assigned to the between-subject error (as it is a between-subject factor)
#  Difficulty and the interaction have been assigned to the interaction error appropriately

# So long as you build your model correctly, R will assign factors to their appropriate error

######################################################
# One within-subject factor, two between-subject factors

# And you can even get more complex...
# What about the full three-way ANOVA?
# We did this easily in Lesson 12
summary(aov(Time ~ PuzzleType * Emotion * Difficulty, data=puzzles))

# Again, we can build in Subject effects
# But note, now PuzzleType, Difficulty, and that interaction are all nested within SubjID
# So our Error term becomes a bit more complex
summary(aov(Time ~ PuzzleType * Emotion * Difficulty + Error(SubjID / (PuzzleType * Difficulty)), data=puzzles))

# Now there are all sorts of different types of errors to account for...
# Explaining why all of this happens is is very difficult to explain without the math
# But note that R assigns the factors appropriately to each error type if you've built the model correctly
# And so you just need to find the factor you care about in the table to determine whether it's correct (accounting for Subject)

# Finally, note that PSYC201 functions like get.p and get.stat work on these complex aov objects as well
full.mod = aov(Time ~ PuzzleType * Emotion * Difficulty + Error(SubjID / (PuzzleType * Difficulty)), data=puzzles)
get.p(full.mod)
get.stat(full.mod)

# However, also note that it collapses across all error types
# So be careful, and just pull it out when you know what you are grabbing for the homework

rm(full.mod,puzzagg)

###################################
# 12.2 - Data cleaning practicum  #
###################################

# For this part of the lesson, we'll be doing group practice with data cleaning
# There are two data sets that come in raw form from two popular programs (EPrime and Qualtrix)
# Your job (as a small group) is to take one of those files and answer a few questions about them
# However, the big challenge will be getting the data into an R-usable format
# Note that your analyses should include both graphs and statistical tests

#######################
# E-Prime (courtesy of Evan Carr)
#   http://vulstats.ucsd.edu/RAssignData/DataCleaning_EPrime.csv

# This experiment is measuring how people process assymmetric facial expresions
# People watched four different faces (avatars) that made various expressions:
#  1) Their emotions could be angry or happy
#  2) The emotion assymmetry could start on the left or right side of the face
#  3) The assymmetry started either 20 or 400ms into the video
# Thus there were effectively 32 different videos they could watch

# The experiment was split into two phases
# In phase 1, subjects saw each video three times, and each time would rate the face on EITHER intensity, authenticity, or trustworthiness
# This rating was a Likert scale (1-7) and was recorded along with the time it took to respond
# In phase 2, subjects saw each video once, and judged which emotion was being expressed and which side of the face it started on
# The program recorded whether they were right for each (0,1) and their reaction time

# (Hint: you may need to use the dcast() function for this data set)
# (Hint2: the 'anger' and 'angry' emotions should be the same)

# The main analysis is looking at how emotion, side of assymmetry, and time of assymmetry affect various ratings and reaction times
# So you should always include those factors in your analysis

# You'll need to answer the following questions:
# a) Is the speed (RT) with which people make the emotional judgment in part 2 correlated with any of the three
#    speeds of rating from part 1?
# b) I want to make sure that the emotional manipulation worked - do people rate angry faces as more intense?
# c) Are they faster to rate angry faces as intense than happy faces?

#######################
# Qualtrix (courtesy of Jess Thierman, from prior work with Kevin & Ed)
#   http://vulstats.ucsd.edu/RAssignData/DataCleaning_Qualtrix.csv

# This data came from an experiment investigating whether the Fundamental Attribution Error was moderated by ratings of intelligence
# The question is whether people are less likely to make attributions (or make them in the opposite direction)
#  if they thought that people were "tanking" the question
# For instance, if a smart person argues something with weak evidence, then they might not really hold that opinion

# Subjects read 8 'test responses' that people supposedly made as part of a debate class
# All of the 'test questions' were politically charged topics
# Subjects were told that the test takers had their side assigned to them (e.g., not their own political leanings)
# Answers were either Democratic or Republican, but also split by two other factors:
# They could be high or low support ('death penalty is bad since it is not shown to reduce crime' vs 'killing is just bad')
# And written intelligently or dumbly (lots of grammatical mistakes, etc.)
# For each of these 'tests', subjects rated both how intelligent they thought the test taker to be, and the test takers political leaning
# These ratings were on a scale from 0-100 (higher is smarter / more conservative)

# So, the conditions are marked by a three letter code, where each letter stands for:
#  1) D(emocratic) or R(epublican) argument
#  2) H(igh) or L(ow) support
#  3) S(mart) or D(umb) writing

# So RHS would be a Republican argument with good support, written intelligently
# And thus RHS_Int_1 is the rating of the RHS test taker's intelligence
# And RHS_Pol_1 is the rating of that test taker's political leanings
# But oops! Someone made a typo in Qualtrix and instead of DLS, they put in DHL
# So something is mislabeled here... and you need to fix it
# And if you look at the data, there are two header lines... and the second one seems worthless

# The main analysis will look at how intelligence and support of the argument affect percieved political attitudes 
# and intelligence
# So your analyses should generally include those and political leaning where appropriate

# Finally, you will need to answer the following questions:
# a) Did our intelligence manipulation work? E.g., do people rate "smart" exams higher on intelligence than "dumb" exams?
# b) Did our political maipulation work? E.g., are "Republican" arguments rated more conservative?
# c) Do we have evidence that political attributions are specifically attenuated when smart people give low support arguments?