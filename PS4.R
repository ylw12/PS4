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
plot(NULL, main = "Electoral Vote vs Popular Vote\n Percentages",
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