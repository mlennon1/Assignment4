##       Mike Lennon  Assignment 4 #

##### Assignment 4 #####
#I used the algorithm that to be a best popular movie the rating has to be above 7.5
# I then took the count of above average movies for each year
# I also took the average rating of movies in that year.
#75% count 25% average add together for score
# In 2004 there was an 8.518 average movie rating
# In 2004 there was also the highest total of hit movies. The total was 619 highly rated movies.



#install.packages("ggplot2")
library(ggplot2)
library(plyr)



assign4 <- read.table("C:\\Users\\mike\\Desktop\\Data Aquisition\\movies.tab", 
                    sep="\t", header=TRUE, quote="", comment="")

goodMoviesOnly<- subset(assign4, rating > 7.5)
goodMoviesOnly1 <- goodMoviesOnly[order(goodMoviesOnly$year, goodMoviesOnly$rating),]
row.names(goodMoviesOnly1) <- NULL
goodMovieAvg <- aggregate(x=goodMoviesOnly1$rating,by=list(goodMoviesOnly1$year),FUN=mean)
 colnames(goodMovieAvg) <- c("year", "avgRating") #need to rename the cols of above 


meanVector <- colMeans(goodMovieAvg)
#Average rating was8.15

(meanMovieRating <- meanVector[2] )
#is.numeric(meanMovieRating)
#(meanMovieRating)


meanSD <- function(input){
  result <- data.frame(mean= apply(input,2,mean), stdDev = apply(input,2,sd))
  
  
}
(allStats <- alply(goodMovieAvg, 2,meanSD ))


columnOfOnes <- ifelse(goodMoviesOnly1$rating==1000,0,1)
goodMoviesOnly1$ones <- columnOfOnes

sumYears <- function(yearID){
  
  df<- goodMoviesOnly1[goodMoviesOnly1$year ==yearID,  ]
  return (sum(df$ones))
}

yearsUnique <- unique(goodMoviesOnly1$year)
str(yearsUnique)
yearCount <- sapply(yearsUnique, sumYears)
yearAndCount <- data.frame(yearsUnique,yearCount )
row.names(yearAndCount) <- NULL



#ggplot(data= yearAndCount) + geom_histogram(aes(x=yearsUnique))

ggplot(yearAndCount, aes(x=yearsUnique, y= yearCount)) + geom_point()
ggplot(goodMovieAvg, aes(x=year, y= avgRating)) + geom_point()
ggplot(data= yearAndCount) + geom_density(aes(x=yearsUnique))