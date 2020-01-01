###
# Import the libraries
###

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite")



###
# Gather the Data
###
years_loaded<-2012:2018
years_inspected<-2014:2018
#url <- 'https://www.openligadb.de/api/getmatchdata/bl1/2019'

match_list<-lapply(years_loaded, function(y){
  match_url <- paste('https://www.openligadb.de/api/getmatchdata/bl1/', y, sep="")
  temp<-fromJSON(match_url,flatten=TRUE)%>%mutate(year=y)
  unnest(data = temp, cols = c(MatchResults))
})

ranking_list<-lapply(years_loaded, function(y){
  ranking_url<- paste('https://www.openligadb.de/api/getbltable/bl1/', y, sep="")
  temp<-fromJSON(ranking_url,flatten=TRUE)%>%mutate(year=y)%>%select(TeamName, Points_This_Year=Points, year)
#unnest(data = temp, cols = c(MatchResults))
  temp
  })


match_data<-bind_rows(match_list)
ranking_data<-bind_rows(ranking_list)

###
# Prepare the Data
###

matches<-match_data%>%filter(ResultName=="Endergebnis")%>%mutate(Begin=as.Date(MatchDateTime,"%Y-%m-%dT%H:%M:%S",tz="CET"),result=ifelse(PointsTeam1>PointsTeam2,1,ifelse(PointsTeam1==PointsTeam2,0,-1)))%>%select(Begin, Team1.TeamName,Team2.TeamName,PointsTeam1,PointsTeam2,year,result)
x<-matches%>%select(Begin,Team1.TeamName,Team2.TeamName,year)

###
# Calculate the Features
###

# Some Teams have a higher probability of winning than others.

matches%>%group_by(Team1.TeamName)%>%summarize(r=mean(result))%>%top_n(3,r)
matches%>%group_by(Team1.TeamName)%>%summarize(r=mean(result))%>%top_n(3,-r)

# To take account into this we calculate some features for each match, based on the histroical data we have.

## LongTermSignals: These Signals are calculated by subtracting the points, that the opponent gained in the last n years.
get_points_last_n_years<-function(n,team,y){
  temp<-ranking_data%>%filter(year<y & (TeamName==team))%>%top_n(n,year)
  sum(temp$Points_This_Year)/max(ranking_data$Points_This_Year)
}
get_long_term_signals<-function(n,team1,team2,y){
  (get_points_last_n_years(n,team1,y)-get_points_last_n_years(n,team2,y))
}

x<-x%>%rowwise()%>%mutate(LongTermSignal1=get_long_term_signals(1,Team1.TeamName,Team2.TeamName,year))
x<-x%>%rowwise()%>%mutate(LongTermSignal3=get_long_term_signals(3,Team1.TeamName,Team2.TeamName,year))
x<-x%>%rowwise()%>%mutate(LongTermSignal5=get_long_term_signals(5,Team1.TeamName,Team2.TeamName,year))
x[is.na(x)] <- 0


## ShortTermSignals: These Signals are calculated by subtracting the points, that the opponent gained in the last n matches.

get_last_n_games<-function(n,team, begin){
  matches%>%filter(Begin<begin & (Team1.TeamName==team | Team2.TeamName==team))%>%top_n(n,Begin)
}

get_points_from_last_n_games<-function(n,team,begin){
   a<-get_last_n_games(n,team,begin)%>%mutate(points=ifelse(result==0,1,ifelse(Team1.TeamName==team,ifelse(result==1,3,0),ifelse(result==-1,3,0))))
   sum(a$points)
}

get_short_term_signals<-function(n,team1, team2, begin){
  (get_points_from_last_n_games(n,team1,begin)-get_points_from_last_n_games(n,team2,begin))/(n*3)
}

x<-x%>%rowwise()%>%mutate(ShortTermSignal10=get_short_term_signals(10,Team1.TeamName,Team2.TeamName,Begin))
x<-x%>%rowwise()%>%mutate(ShortTermSignal5=get_short_term_signals(5,Team1.TeamName,Team2.TeamName,Begin))
x<-x%>%rowwise()%>%mutate(ShortTermSignal3=get_short_term_signals(3,Team1.TeamName,Team2.TeamName,Begin))
matches$PointsTeam1<-NULL
matches$PointsTeam2<-NULL

## DirectSignals: These Signals are calculated by subtracting the points, that the opponent gained in the last n matches against each other.

get_last_n_direct_games<-function(n,team1,team2,begin){
  matches%>%filter(Begin<begin & Team1.TeamName %in% c(team1,team2) & Team2.TeamName %in% c(team1,team2))%>%top_n(n,Begin)
}
get_direct_signals<-function(n,team1, team2, begin){
  a<-get_last_n_direct_games(n,team1,team2,begin)
  points_team1<-a%>%mutate(points_team1=ifelse(result==0,1,ifelse(Team1.TeamName==team1,ifelse(result==1,3,0),ifelse(result==-1,3,0))))%>%.$points_team1
  points_team2<-a%>%mutate(points_team2=ifelse(result==0,1,ifelse(Team1.TeamName==team2,ifelse(result==1,3,0),ifelse(result==-1,3,0))))%>%.$points_team2
  (sum(points_team1)-sum(points_team2))/(n*3)
}
x<-x%>%rowwise()%>%mutate(DirectSignal10=get_direct_signals(10,Team1.TeamName,Team2.TeamName,Begin))
x<-x%>%rowwise()%>%mutate(DirectSignal5=get_direct_signals(5,Team1.TeamName,Team2.TeamName,Begin))
x<-x%>%rowwise()%>%mutate(DirectSignal3=get_direct_signals(3,Team1.TeamName,Team2.TeamName,Begin))


# Deleting duplicated columns in x and matches and filtering by years that will be inspected

x$Begin<-NULL
x$Team1.TeamName<-NULL
x$Team2.TeamName<-NULL

x$Team1.PointsLastYear<-NULL
x$Team2.PointsLastYear<-NULL

matches<-matches%>%filter(year %in% years_inspected)
x<-x%>%filter(year %in% years_inspected)
matches$year<-NULL
x$year<-NULL

matches<-add_column(matches,x=as_tibble(x))

## Split the Matches into traing and test set

test_index <- createDataPartition(y = matches$result, times = 1, p = 0.2, list = FALSE)
test_set<-matches[test_index,]
train_set<-matches[-test_index,]

###
# Build the Model
###

add_row_to_rmse_table<-function(method_name,rmse_value){
  if(!exists("rmse_results")){
    data.frame(method = method_name, rmse_value)
  } 
  else {
    bind_rows(rmse_results, data.frame(method = method_name, rmse_value))
  }
}
rmse_results<-NULL

## Naive Approachs
# guessing always the most comon outcome
which.max(train_set$result)
RMSE(which.max(train_set$result),test_set$result)
rmse_results<-add_row_to_rmse_table("Most common result",RMSE(which.max(train_set$result),train_set$result))

# guessing the mean of the three possible outcomes
mean(c(1,0,-1))
RMSE(mean(c(1,0,-1)),test_set$result)
rmse_results<-add_row_to_rmse_table("Mean of the possible results",RMSE(mean(c(1,0,-1)),train_set$result))

# guessing allways the mean of the results
mean(train_set$result)
RMSE(mean(train_set$result),test_set$result)
rmse_results<-add_row_to_rmse_table("Average of the training results",RMSE(mean(train_set$result),train_set$result))
rmse_results

## Examine the Features in the Trainig set

# LongTermSignals: These Signals are calculated by subtracting the points, that the opponent gained in the last n years.
train_set%>%ggplot(aes(as.factor(result),x$LongTermSignal1))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last Year")
train_set%>%ggplot(aes(as.factor(result),x$LongTermSignal3))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 3 Years")
train_set%>%ggplot(aes(as.factor(result),x$LongTermSignal5))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 5 Years")

# ShortTermSignals: These Signals are calculated by subtracting the points, that the opponent gained in the last n matches.
train_set%>%ggplot(aes(as.factor(result),x$ShortTermSignal3))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 3 Matches")
train_set%>%ggplot(aes(as.factor(result),x$ShortTermSignal5))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 5 Matches")
train_set%>%ggplot(aes(as.factor(result),x$ShortTermSignal10))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 10 Matches")

# DirectSignals: These Signals are calculated by subtracting the points, that the opponent gained in the last n matches against each other.
train_set%>%ggplot(aes(as.factor(result),x$DirectSignal3))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 3 Direct Matches")
train_set%>%ggplot(aes(as.factor(result),x$DirectSignal5))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 5 Direct Matches")
train_set%>%ggplot(aes(as.factor(result),x$DirectSignal10))+geom_boxplot()+ylim(-1,1)+ggtitle("Effects of the Results in the Last 10 Direct Matches")

# Applying the Featrures to some ML-Models

control <- trainControl(method = "cv", number = 5)
models <- c( "lm","glm","knn","gamLoess","svmLinear","rpart","rf")
fits <- lapply(models, function(model){train(train_set$x,train_set$result,model,trControl = control)} )
model_rsme<-sapply(fits, function(o){
       min(o[["results"]][["RMSE"]])
   })
rmse_results<-add_row_to_rmse_table(models,model_rsme)

## Bulding an Ensamble

pred_train <- sapply(fits, function(object) 
  predict(object, newdata = train_set$x))

RMSE(rowMeans(pred_train),train_set$result)
rmse_results<-add_row_to_rmse_table("Model-Ensemble",RMSE(rowMeans(pred_train),train_set$result))
rmse_results

###
# Evaluation
###

# Use the Ensembled Model to predict the Reults of the Test Set
pred_test <- sapply(fits, function(object) 
  predict(object, newdata = test_set$x))
RMSE(rowMeans(pred_test),test_set$result)

