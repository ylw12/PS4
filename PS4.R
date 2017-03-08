############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 4
## Author: Luwei Ying
############################################

rm(list=ls())
library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

# Use html_table(temp) alone first and notice that the second table contains useful information
PresElection <- html_table(temp)[[2]]

# Subset the data, delete the first two rows and rename the columns.
PresElection <- PresElection[-c(1,2), ]
colnames(PresElection) <- c('Election', 'year', 'Winner', 'Party', 'Electoral College Vote', 
                            'Electoral College Percentage', 'Popular Vote Percentage', 
                            'Popular Vote Percentage Margin', 'Popular Votes', 'Pop Vote Margin', 
                            'Runner Up', 'Runner Up Party', 'Turnout')

# Clean the data for later use. Here I use Patrick's idea to first define a function for 
# the names of "winner" and "runner-up", then a function for numbers.

# For names:
cleanname <- function(x){
  x <- gsub(".*,", "", x)
  x <- gsub("[A-Z]. ", "", x) 
}

# For numbers:
cleannumber <- function(x){
  x <- gsub("\\[a\\]", "", x)
  x <- gsub("\\%", "", x)
  x <- gsub("−", "-", x)
  x <- gsub(",", "", x)
  x <- as.numeric(x)
}

# Apply the functions to the dataset
PresElection[, c(3, 11)] <- apply(PresElection[, c(3, 11)], 2, cleanname)
PresElection[, c(1, 2, 6, 7, 8, 9, 10, 13)] <- apply(PresElection[, c(1, 2, 6, 7, 8, 9, 10, 13)],
                                                     2, cleannumber)

############################################
## Plot
############################################

# Before I actually do the plotting, define a pdf first. Latter the two plots wil go to
# that file.
pdf("PresElectionTrends.pdf")

# My first plot is going to depict the difference between Electoral Vote and Popular Vote.
# First, I bould a NULL plot with proper scales.
plot(NULL, main = "Electoral Vote vs Popular Vote in Percentages",
     ylim = c(min(PresElection[,c(6,7)]), max(PresElection[,c(6,7)])+2),
     xlim = c(min(PresElection[,2]), max(PresElection[,2])),
     ylab = "Vote Received by Winning Parties", 
     xlab = "Election Year")

# Then, I add two lines, which depict the trends of Electoral College Vote and Popular Vote
# over time. Since the total number of votes is not fixed, I use percentage rather than the
# absolute votes.
lines(x=sort(PresElection$year), y=PresElection$`Electoral College Percentage`, lty=1)
lines(x=sort(PresElection$year), y=PresElection$`Popular Vote Percentage`, lty=4)

# Finally, I add a legend.
legend("topleft",
       legend=c("Electoral College", "Popular Vote"),
       lty = c(1,4),
       cex = .9)

# Explanation: The percentage of winner's electoral college vote is increasing overtime, 
# but the percentage of winner's popular vote stays almost the same. This may indicate 
# an increasing used strategy of gerrymandering.


# My second plot is intended to make a contrast between the Electoral College Vote win by
# the two parties. In order to do this, we would first split the column ``Electoral College 
# Vote" into two.

PresElection$Lose_EC <- as.numeric(sub(".*\\/", "",PresElection$`Electoral College Vote`))-
  as.numeric(sub("*\\/.*", "",PresElection$`Electoral College Vote`))
PresElection$Win_EC <- as.numeric(sub("*\\/.*", "",PresElection$`Electoral College Vote`))

# Before plotting, use "layout()" to set some space for my legend.
layout(matrix(c(1,2), ncol=1, byrow=FALSE), heights = c(0.85,0.15))

plot(NULL,
     main="The Electoral College Votes for two parties",
     ylab="Vote Received by the two Parties", xlab="Election Year", 
     ylim=c(min(PresElection[, c(14, 15)]), max(PresElection[, c(14, 15)])), 
     xlim = c(min(PresElection[,2]), max(PresElection[,2])))

# In order to differentiate all parties, I difine a unique color for each of them.
PartyColors <- as.character(PresElection$Party)
PartyColors[PartyColors == 'Dem.'] <- 'blue'
PartyColors[PartyColors == 'Rep.'] <- 'red'
PartyColors[PartyColors == 'Whig'] <- 'yellow'
PartyColors[PartyColors == 'D.-R.'] <- 'green'

# Add points to the NULL plot. They are the electoral college votes obtained by both 
# candidates.

# Identify the winners with solid dots.
points(x=PresElection$year,
       y=PresElection$Win_EC,
       pch=19, col=PartyColors)

# Identify the losers with empty dots.
points(x=PresElection$year,
       y=PresElection$Lose_EC,
       pch=1, col=PartyColors)

# Add segments between them so we can match the candidates in the same year.
segments(x0=PresElection$year,
         y0=PresElection$Win_EC,
         y1=PresElection$Lose_EC,
         lty=4, col="black")

# Add the legend in a seperate space.
par(mar=c(1,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend("topleft",legend=c("Democratic","Republican","Whig","Democratic-Republican"), 
       col=c("blue","red","yellow","green"), 
       pch=c(19,19,19,19),
       horiz=TRUE,
       cex=.6)
legend("bottomleft",legend=c("Winner", "Loser"), 
       col=c("black","black"), 
       pch=c(19,1),
       horiz=TRUE,
       cex=.6)
# Form this plot of trend, we can see that last centray has witnessed a huge gap between
# the electoral college votes obtained by winning and run-up candidates. In this centray, 
# the votes become close again. Also, we see that no single party in history has won more
# than 5 elections in a row.

dev.off()

############################################
## Merge the data
############################################

# I have already gotten the electoral college votes for both candidates by splitting column 
# 5 in the original data frame. For practice purpose, I would remove them first, and then
# add them by merging a new table.
PresElection <- PresElection[, 1:13]

# library a new package for the second URL.
library(htmltab)

wikiURL2 <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

# Create a second dataframe on the same topic. I only include the useful information,
# namely year, candidates and electoral college votes. The first two columns are to be 
# used as indicators and the third contains the electoral votes we care about.
PresElection2 <- htmltab(wikiURL2, which = 3)
PresElection2 <- PresElection2[ , c(1, 3, 7)]
colnames(PresElection2) <- c('year', 'candidate', 'ECvotes')

# Clean the "vote" column and only leave the actual votes the candidate gets.
PresElection2$ECvotes <- sapply(strsplit(PresElection2$ECvotes, ' / '),
                           function(x) as.numeric(x[1]))

# Aggregate electoral college votes by year and candidate:
AggregateList <- list(year=PresElection2$year, candidate=PresElection2$candidate)
PresElection2 <- aggregate(PresElection2$ECvotes, by=AggregateList, FUN=sum)

# Since all candidates are included in this dataframe, we need to delete the candidates 
# from thirdparties and only keep the major candidates.
PresElection2 <- PresElection2[order(PresElection2$year, PresElection2$x, decreasing=TRUE), ]
PresElection2 <- Reduce(rbind, by(PresElection2, PresElection2['year'], head, n=2))

# For each election, define who is the winner and who is not.
winner<-data.frame()
run_up<-data.frame()
for (i in 1:nrow(PresElection2)){
  winner <- PresElection2[seq(1, nrow(PresElection2), by=2),]
  run_up <- PresElection2[seq(2, nrow(PresElection2), by=2),]
}

# Merge the two data frame.
FinalTable <- merge(PresElection, winner, by='year', all.x=TRUE)
FinalTable <- merge(FinalTable, run_up, by='year', all.x=TRUE)

# Remove the columns of candidates.
FinalTable <- FinalTable[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,17)]
# Name the electoral college votes.
colnames(FinalTable)[14:15] <- c("Win_EC","Lose_EC")

# Save the results in a Rdata file.
save(FinalTable, file='PresElection.Rdata')
