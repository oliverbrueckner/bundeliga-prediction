###
# Import the libraries
###

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

if(!require(jsonlite)) install.packages("jsonlite")



###
# Gather the Data
###
years<-seq(2000:2019)
url <- 'https://www.openligadb.de/api/getmatchdata/bl1/2019'
data<- fromJSON(url, flatten = TRUE)
data<- unnest(data = data, cols = c(MatchResults))


###
# Prepare the Data
###

matches<-data%>%filter(ResultName=="Endergebnis")%>%mutate(year=as.integer(substr(MatchDateTime,1,4)),result=as.factor(ifelse(PointsTeam1>PointsTeam2,"1",ifelse(PointsTeam1==PointsTeam2,"X","2"))))%>%select(year, Team1.TeamName,Team1.TeamId,Team2.TeamName,Team2.TeamId,PointsTeam1,PointsTeam2,result)
x<-matches%>%select(year,Team1.TeamId,Team2.TeamId)
y<-as.factor(matches%>%select(result)%>%.$result)
###
# Build the Model
###


###
# Evaluation
###