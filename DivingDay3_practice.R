#
# Diving Exploration of February 26 (see below for new work)
#
# This data set is for Olympic Diving from the 2000 Olympic Games
# which took place in Sydney, Australia.

x <- read.csv("Diving2000.csv", stringsAsFactors = FALSE)
dim(x)

head(x)
names(x)

# QUESTION: How many events are there in this data set?
table(x$Event)
unique(x$Event)
length(unique(x$Event))

# QUESTION: How many countries sent divers to Sydney Australia in 2000?
table(x$Country)
unique(x$Country)
length(unique(x$Country))

# QUESTION: What is the maximum and minimum score we have in this data set?
min(x$JScore)
max(x$JScore)
summary(x$JScore)
range(x$JScore)

# QUESTION: Which countries had divers who earned perfect scores of 10?
x$Country[x$JScore == 10]
unique(x$Country[x$JScore == 10])

# Observe: For each DIVE (and I use the word carefully), there are seven (7)
# judges.  And these are reported in blocks of 7 rows.  So rows 1-7 are all
# for one dive.
x[1:7, ]

# Let's look at the first two dives:
x[1:14, ]

# Observe.  How many dives are there in this data set?
dim(x)
# Incorrect answer: 10787
# Correct answer: 10787 / 7 = 1541 dives!
# There are 10787 scores present in this data set, in groups of 7 for each
# dive.  Keep the vocabulary clear in your mind.

################################################################################
# New February 19, 2021

# Qianyao Zhu: Which judge gives the highest score?
# NOTE: 10 is a perfect score.  All the following judges award perfect 10
# scores in this competition.
unique(paste(x$Judge, x$JCountry)[x$JScore == 10])

# Which judge gives the highest scores to each diver?
# (skip for now, maybe come back)

# Which judges are "not so strict" (i.e. they give higher scores on average)?
sort(tapply(x$JScore, x$Judge, mean))
# Comment: This may actually not mean that these judges are more or less
# strict, because of something we haven't yet discussed.  We'll return
# to this later.

# Lejing Li: Every diver has six chances to dive (maybe) and every dive
# has 7 judges.  Simplify: So every diver has 6 rows of data rather than 42.
# This would be like calculating dive averages, so that there is only one
# row for each diver.

# For the above two questions, we'd like to have a new data structure
# with one row per dive, and the dive average and the degree of difficulty,
# along with the diver name and nationality.

# First, I'm going to propose something a little different.  Once we do this,
# satisfying the request above will become easy.

### See below the challenge for an answer to the request above.

###
### Challenge: For each dive (each block of 7 scores), calculate the dive
### average.  Then save it in the new column columns, repeated on each of
### the associated rows.
###

x$avg <- 0
x$avg[1:7] <- 8.5
x$avg[8:14] <- mean(x$JScore[8:14])

# There are nrow(x) / 7 = 10787 / 7 = 1541 dives
for (i in 1:1541) {
  # Example check: i = 2 ?
  x$avg[(7*i-6):(7*i)] <- mean(x$JScore[(7*i-6):(7*i)])
}

x$DiveID <- rep(1:1541, each = 7)   # Useful!
for (i in 1:1541) {
  x$avg2[x$DiveID == i] <- mean(x$JScore[x$DiveID == i])
}

x$avg3 <- rep(tapply(x$JScore, x$DiveID, mean), each = 7)    # NICE!

### End the little challenge

### ASIDE on duplicated:
duplicated( c(1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,4,4,4) )
!duplicated( c(1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,4,4,4) )

### Now to answer the requst up above (2 different ways)
s <- x[!duplicated(x$DiveID), c("Diver", "Country", "Difficulty", "avg")]
s1 <- x[seq(1, 10787, by = 7), c("Diver", "Country", "Difficulty", "avg")]

###
###


# Which countries divers have the highest scores in the same
# difficulty level?
for (d in sort(unique(s$Difficulty))) {
  cat("Studying difficulty level", d, "\n")
  temp <- s[s$Difficulty == d,]
  ans <- temp$Country[temp$avg == max(temp$avg)]
  #if (length(ans) > 1) stop("multiple divers found.  Fix code.")
  cat("Country", paste(ans, collapse = ":"),
      "had the best score at this difficulty level.\n\n")
}

#  What is the average difficult level for different rounds?

# Who won?  This could related to the scoring system,
# which has changed over the years.  A project could examine several different
# ways of calculating the final scores, to see if a change might award
# the medals in a different way.

# Which countries tend to choose higher difficulties?

# Can we do a comparison by gender of scores at different difficulty
# levels?

# Can we find out relationship between difficulty and scores
# for each diver?  Sure!

# How often do we find dives where the nationality of the diver and the
# judge are the same?  And when this happens, are you concerned about
# fairness?

################################################################################
################################################################################
################################################################################
# New February 26, 2021
#
# For your homework, I asked you to try to answer the un-answered questions,
# above.  Here, though, I start from the beginning with only the code
# necessary to add a few critical variables to this data set.

x <- read.csv("Diving2000.csv", stringsAsFactors = FALSE)
dim(x)

x$match <- x$Country == x$JCountry      # See the quiz!
x$DiveID <- rep(1:1541, each = 7)       # Convenient.... 1541 unique dives
x$avg <- ave(x$JScore, x$DiveID)        # Super-easy way of getting diver avg's.

# Kyle: Which countries tend to choose higher difficulties?
tapply(x$Difficulty, x$Country, mean)
sort(tapply(x$Difficulty, x$Country, mean))


###
# Guo Zhengyang: What is the relationship between the difficulty and the
# average score of each dive?  Graphics and maybe calculations.

plot(JScore ~ Difficulty, data = x)       # Not great -- overplotting!

# "jitter" the data points to help avoid overplotting while preserving
# the general patterns that we wish to explore.
plot(jitter(JScore) ~ jitter(Difficulty), data = x)

# Let's add color based on the round of the competition:
plot(jitter(JScore) ~ jitter(Difficulty), data = x,
     col = factor(x$Round))
# Red = Prelim    all divers compete here
# Green = Semi    Only pretty good divers here, and MUST do easy dives.
# Black = Final   Only the best divers, and can do any dive they want

# QUESTION: Why do ZIM, CZE, THA, FIN, VEN, PER, ARM... and a few
# others, attempt such difficult dives on average?

# ANSWER: These countries are pretty bad!  The only compete in the
# prelimary round, and don't make the semi-final, so they only attempt
# hard dives!  In contrast, the Chinese and other ALWAYS make the semi-final,
# and thus always have to do at least a few easy dives, pulling down their
# average.

# YES, Yiheng is correct: in the semi-finals, all dives MUST be easy
# dives.

table(x$Country, x$Round)

# Correlation is the strength of the linear association between
# two variables.  Here, the association is negative... higher
cor(x$JScore, x$Difficulty)

###
###
# Wenqi Yang: What is the average difficulty level for different rounds?
# (( Please try to answer this on your own right now... do not type in chat ))

# GREAT question, and important to know.  Relating to the plot above!
tapply(x$Difficulty, x$Round, mean)


###
# Tingying Yan: Who won?  This could related to the scoring system,
# which has changed over the years.  A project could examine several different
# ways of calculating the final scores, to see if a change might award
# the medals in a different way.


###
# Kyle: Which countries tend to choose higher difficulties?  This may 
# relate to a topic we'll explore from Guo Zhengyang, above.

# Countries could have higher difficulties because:
# (i) Their divers are good!
# (ii) Their divers are bad and only compete in the preliminary round.
#      Why is this?  Recall our discussion above!


###
# Can we do a comparison by gender of scores at different difficulty
# levels?  Yes, I suppose we could!

x$Gender <- substring(x$Event, 1, 1)
for (d in sort(unique(x$Difficulty))) {
  cat("Studying difficulty level", d, "average scores by by gender:\n")
  print(tapply(x$JScore[x$Difficulty == d],
               x$Gender[x$Difficulty == d],
               mean))
  cat("\n\n")
}





###
# How often do we find dives where the nationality of the diver and the
# judge are the same?  And when this happens, are you concerned about
# fairness?  Ah -- we had this on the quiz, at least a starting point...

table(x$match)

# 314 cases where the nationalities match up!  For China:

sum(x$match[x$Country == "CHN"])
sum(x$Country == "CHN" & x$JCountry == "CHN")

################################################################################
################################################################################
# Let's study Chinese judge "WANG Facheng"

thisjudge <- "WANG Facheng"
thiscountry <- "CHN"

mean(x$JScore)    # All scores

# Scores from Judge Wang when the divers are Chinese (i.e. matching cases):
mean(x$JScore[x$Judge == thisjudge & x$match])     # 8.45

# IS THIS EVIDENCE THAT JUDGE WANG IS BIASED AND HELPING CHINESE DIVERS?
# NO -- It isn't necessarily bias, because Chinese divers are
# really good and get higher scores!!

# Instead, consider averages for Chinese divers from non-Chinese judges:
mean(x$JScore[x$Country == thiscountry & !x$match])    # 8.14

# 8.45 seems high, higher than 8.14.  So is Judge Wang biased?




