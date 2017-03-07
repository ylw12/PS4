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

# My first plot is intended to make a contrast between the Electoral College Vote win by
# the two parties. In order to do this, we would first split the column ``Electoral College 
# Vote" into two.
PresElection$Lose_EC <- as.numeric(sub(".*\\/", "",PresElection$`Electoral College Vote`))-
  as.numeric(sub("*\\/.*", "",PresElection$`Electoral College Vote`))
PresElection$Win_EC <- as.numeric(sub("*\\/.*", "",PresElection$`Electoral College Vote`))

layout(matrix(c(1,2), ncol=1, byrow=FALSE), heights = c(0.85,0.15))
# setting the layout
plot(NULL,
     main="The Electoral College Votes for two parties",
     ylab="Vote Received by the two Parties", xlab="Election Year", 
     ylim=c(min(PresElection[, c(14, 15)]), max(PresElection[, c(14, 15)])), 
     xlim = c(min(PresElection[,2]), max(PresElection[,2])))

# Difine the colors of the points
PartyColors <- as.character(PresElection$Party)
PartyColors[PartyColors == 'Dem.'] <- 'blue'
PartyColors[PartyColors == 'Rep.'] <- 'red'
PartyColors[PartyColors == 'Whig'] <- 'yellow'
PartyColors[PartyColors == 'D.-R.'] <- 'green'
points(x=PresElection$year,
       y=PresElection$Win_EC,
       pch=19, col=PartyColors)

points(x=PresElection$year,
       y=PresElection$Lose_EC,
       pch=1, col=PartyColors)

segments(x0=PresElection$year,
         y0=PresElection$Win_EC,
         y1=PresElection$Lose_EC,
         lty=4, col="black")

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

dev.off()

# My second plot is going to depict the difference between Electoral Vote vs Popular Vote.
plot(NULL, main = "Electoral Vote vs Popular Vote in Percentages",
     ylim = c(min(PresElection[,c(6,7)]), max(PresElection[,c(6,7)])+2),
     xlim = c(min(PresElection[,2]), max(PresElection[,2])),
     ylab = "Vote Received by Winning Parties", 
     xlab = "Election Year")
lines(x=sort(PresElection$year), y=PresElection$`Electoral College Percentage`, lty=1)
lines(x=sort(PresElection$year), y=PresElection$`Popular Vote Percentage`, lty=4)
legend("topleft",
       legend=c("Electoral College", "Popular Vote"),
       lty = c(1,4),
       cex = .9)

# Explanation: The percentage of winner's electoral college vote is increasing overtime, 
# but the percentage of winner's popular vote stays almost the same. This may indicate 
# an increasing used strategy of gerrymandering.


############################################
## Merge the data
############################################

library(htmltab)

wikiURL2 <- 'https://en.wikipedia.org/wiki/United_States_presidential_election'

PresElection2 <- htmltab(wikiURL2, which = 3)

PresElection2[, 7] <- gsub( " .*$", "", PresElection2[, 7])

PresElection2 <- aggregate(as.numeric(PresElection2[, 7]) ~ PresElection2[, 1] + PresElection2[, 3], FUN = sum) 
colnames(PresElection2) <- c('Year', 'Candidate', 'Votes')

# Clean the names in our new data, so they can be matched to the previous one.
PresElection2[, 2] <- cleanname(PresElection2[, 2])

# The codes to merge do not work currently.