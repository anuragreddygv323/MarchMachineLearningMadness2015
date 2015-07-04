#March Machine Learning Madness
#Ver 0.16 #Local Model Evaluation (LogLoss) included

#Init & Directories------------------------------------------
rm(list=ls(all=TRUE))

#Libraries
require("rjson")
require("parallel")
require("data.table")
require("doParallel")
require("glmnet")
require("leaps")
require("ggplot2")
require("Metrics")

#Read Settings file
directories <- fromJSON(file = "SETTINGS.json")

#Set Working Directory
workingDirectory <- directories$workingDirectory
setwd(workingDirectory)
dataDirectory <- directories$dataDirectory
edaDirectory <- directories$EDALoc
#h2o location
h2o.jarLoc <- directories$h2o.jarLoc

#Detect available cores
numCores <- detectCores()

#Define helper mining functions
source(file.path(workingDirectory, "getSeedDivision.R"))
source(file.path(workingDirectory, "getScoreAndWins.R"))
source(file.path(workingDirectory, "getForAndAgainstPoints.R"))
source(file.path(workingDirectory, "resultsPredictionMatrix.R"))

#Load Data----------------------
seasonCompact <- fread(file.path(dataDirectory, "regular_season_compact_results.csv"))
seasonDetailed <- fread(file.path(dataDirectory, "regular_season_detailed_results.csv"))
seasons <- fread(file.path(dataDirectory, "seasons.csv"))
teams <- fread(file.path(dataDirectory, "teams.csv"))
tourneyCompact <- fread(file.path(dataDirectory, "tourney_compact_results.csv"))
tourneyDetailed <- fread(file.path(dataDirectory, "tourney_detailed_results.csv"))
tourneySeeds <- fread(file.path(dataDirectory, "tourney_seeds.csv"))
tourneySlots <- fread(file.path(dataDirectory, "tourney_slots.csv"))
#Append 2015 data
seasonCompact2015 <- fread(file.path(dataDirectory, "regular_season_compact_results_2015.csv"))
seasonDetailed2015 <- fread(file.path(dataDirectory, "regular_season_detailed_results_2015.csv"))
seeds2015 <- fread(file.path(dataDirectory, "tourney_seeds_2015.csv"))
seasonCompact <- rbind(seasonCompact, seasonCompact2015)
seasonDetailed <- rbind(seasonDetailed, seasonDetailed2015)
tourneySeeds <- rbind(tourneySeeds, seeds2015)

#Tourney Locations' Data
tourneyCities <- fread(file.path(dataDirectory, "seasons_with_location_001.csv"))
teamsCoordinates <- fread(file.path(dataDirectory, "team_conf_and_geog.csv"))
venuesCoordinates <- fread(file.path(dataDirectory, "hosts.csv"))
  
#Extra Data
MasseyOrdinals <- fread(file.path(dataDirectory, "massey_ordinals.csv"))

#Write .csv
sampleSubmission <- fread(file.path(dataDirectory, "sample_submission.csv"))

#Generated Data--------------------------------------
#teamsBySeason since 2003
allSeasons <- seq(2003, 2014)

#Average points past tournaments and winning percentage
averagePointsList <- lapply(allSeasons, function(marchSeason){
  seasonIdx <- which(tourneyCompact$season == marchSeason)
  teamsInTourney <- union(unique(tourneyCompact$wteam[seasonIdx]), unique(tourneyCompact$lteam[seasonIdx]))
  teamsScores <- t(sapply(teamsInTourney, function(marchTeam){
    winningScores <- tourneyCompact$wscore[seasonIdx][tourneyCompact$wteam[seasonIdx] == marchTeam]
    losingScores <- tourneyCompact$lscore[seasonIdx][tourneyCompact$lteam[seasonIdx] == marchTeam]
    #Winning Percentage
    totalGamesPlayed <- length(winningScores) + length(losingScores)
    winningPercentage <- length(winningScores) / totalGamesPlayed
    return(c(marchTeam, mean(c(winningScores, losingScores)), winningPercentage))
  }))
  return(teamsScores)
})
names(averagePointsList) <- allSeasons

#teamsBySeason since 2003
allSeasons <- seq(2003, 2015)

#Average points scored and recieved during the season and averages
pointsSeasonList <- lapply(allSeasons, function(marchSeason){
  seasonIdx <- which(seasonCompact$season == marchSeason)
  teamsInTourney <- tourneySeeds$team[which(tourneySeeds$season == marchSeason)]
  teamsScores <- t(sapply(teamsInTourney, function(marchTeam){
    #For points
    winningPointsMade <- seasonCompact$wscore[seasonIdx][seasonCompact$wteam[seasonIdx] == marchTeam]
    losingPointsMade <- seasonCompact$lscore[seasonIdx][seasonCompact$lteam[seasonIdx] == marchTeam]
    #Against points
    winningPointsAgainst <- seasonCompact$lscore[seasonIdx][seasonCompact$wteam[seasonIdx] == marchTeam]
    losingPointsAgainst <- seasonCompact$wscore[seasonIdx][seasonCompact$lteam[seasonIdx] == marchTeam]
    
    return(c(marchTeam,
             mean(c(winningPointsMade, losingPointsMade)), 
             mean(c(winningPointsAgainst, losingPointsAgainst))))
  }))
  return(teamsScores)
})
names(pointsSeasonList) <- allSeasons

#Data Mining (Functions)------------------------
#Obtain Massey Rankings
rankingsColNames <- unique(MasseyOrdinals$sys_name)
getExtraRankings <- function(seasonMatch, teamMatch){
  masseyData <- MasseyOrdinals[MasseyOrdinals$team == teamMatch & MasseyOrdinals$season == seasonMatch] #it only takes 0.19s, better than SQL
  rankingSys <- match(masseyData$sys_name, rankingsColNames)
  extraRankingsTeam <- sapply(1:130, function(rankColNum){
    IdxSys <- which(rankingSys == rankColNum)
    mostRecentIdxSys <- IdxSys[which.max(masseyData$rating_day_num[IdxSys])]
    colRank <- ifelse(length(mostRecentIdxSys) != 0, masseyData$orank[mostRecentIdxSys], NA)
    return(colRank)
  })
  return(extraRankingsTeam)
}

#Shuffle Winning teams with their corresponding features
makeTrainTable <- function(gamesIdx, shufIdxs, returnPointspread = TRUE){
  wTeamSeed <- getSeedDivision(tourneyCompact$season[gamesIdx], tourneyCompact$wteam[gamesIdx])
  lTeamSeed <- getSeedDivision(tourneyCompact$season[gamesIdx], tourneyCompact$lteam[gamesIdx]) 
  #Ordinal Ranks
  wTeamOrdinalRanks <- getExtraRankings(tourneyCompact$season[gamesIdx], tourneyCompact$wteam[gamesIdx])[1:33]
  lTeamOrdinalRanks <- getExtraRankings(tourneyCompact$season[gamesIdx], tourneyCompact$lteam[gamesIdx])[1:33]
  #Transform Ordinals to power ratings
  wTeamPowerRatings <- 100 - 4* log(wTeamOrdinalRanks + 1) - wTeamOrdinalRanks / 22
  lTeamPowerRatings <- 100 - 4* log(lTeamOrdinalRanks + 1) - lTeamOrdinalRanks / 22  
  #Seeds Power Ranking
  wPowerSeeds <- 100 - 4 * log(as.numeric(wTeamSeed[1]) + 1) - as.numeric(wTeamSeed[1]) / 22
  lPowerSeeds <- 100 - 4 * log(as.numeric(lTeamSeed[1]) + 1) - as.numeric(lTeamSeed[1]) / 22
  #Average Scores & Winning percentages
  wTeamScoreAndWins <- getScoreAndWins(tourneyCompact$season[gamesIdx] - 1, tourneyCompact$wteam[gamesIdx])
  lTeamScoreAndWins <- getScoreAndWins(tourneyCompact$season[gamesIdx] - 1, tourneyCompact$lteam[gamesIdx])  
  wTeamScoreAndWins2Years <- getScoreAndWins(tourneyCompact$season[gamesIdx] - 2, tourneyCompact$wteam[gamesIdx])
  lTeamScoreAndWins2Years <- getScoreAndWins(tourneyCompact$season[gamesIdx] - 2, tourneyCompact$lteam[gamesIdx])   
  wTeamScoreAndWins3Years <- getScoreAndWins(tourneyCompact$season[gamesIdx] - 3, tourneyCompact$wteam[gamesIdx])
  lTeamScoreAndWins3Years <- getScoreAndWins(tourneyCompact$season[gamesIdx] - 3, tourneyCompact$lteam[gamesIdx])  
  #Average Points for and against during regular season
  wForAndAgainstPoints <- getForAndAgainstPoints(tourneyCompact$season[gamesIdx], tourneyCompact$wteam[gamesIdx])
  lForAndAgainstPoints <- getForAndAgainstPoints(tourneyCompact$season[gamesIdx], tourneyCompact$lteam[gamesIdx])  
  
  if (shufIdxs[gamesIdx] == 1){    
    #Seed Based Benchmark
    seedBasedBenchmark <- 0.5 + (as.numeric(lTeamSeed[1]) - as.numeric(wTeamSeed[1])) * 0.03    
        
    shuffledTeams <- c(tourneyCompact$wteam[gamesIdx], tourneyCompact$lteam[gamesIdx], 
                       wTeamSeed, lTeamSeed, seedBasedBenchmark,
                       (wPowerSeeds - lPowerSeeds),
                       (wTeamPowerRatings - lTeamPowerRatings), mean((wTeamPowerRatings - lTeamPowerRatings), na.rm = TRUE),
                       wTeamScoreAndWins - lTeamScoreAndWins,
                       wTeamScoreAndWins2Years - lTeamScoreAndWins2Years,
                       wTeamScoreAndWins3Years - lTeamScoreAndWins3Years,
                       wForAndAgainstPoints, lForAndAgainstPoints,
                       tourneyCompact$wscore[gamesIdx] - tourneyCompact$lscore[gamesIdx])
  }else{
    #Seed Based Benchmark
    seedBasedBenchmark <- 0.5 + (as.numeric(wTeamSeed[1]) - as.numeric(lTeamSeed[1])) * 0.03
        
    shuffledTeams <- c(tourneyCompact$lteam[gamesIdx], tourneyCompact$wteam[gamesIdx], 
                       lTeamSeed, wTeamSeed, seedBasedBenchmark,
                       (lPowerSeeds - wPowerSeeds),
                       (lTeamPowerRatings - wTeamPowerRatings),  mean((lTeamPowerRatings - wTeamPowerRatings), na.rm = TRUE),
                       lTeamScoreAndWins - wTeamScoreAndWins,
                       lTeamScoreAndWins2Years - wTeamScoreAndWins2Years,
                       lTeamScoreAndWins3Years - wTeamScoreAndWins3Years,
                       lForAndAgainstPoints, wForAndAgainstPoints,
                       tourneyCompact$lscore[gamesIdx] - tourneyCompact$wscore[gamesIdx])
  }  
  return(shuffledTeams)
}

#Create Test Table
makeTestTable <- function(testIdx, team1Vector, team2Vector, season){
  #Get seeds from both teams
  team1Seed <- getSeedDivision(season, team1Vector[testIdx])
  team2Seed <- getSeedDivision(season, team2Vector[testIdx])
  #Ordinal Rankings
  team1OrdinalRanks <- getExtraRankings(season, team1Vector[testIdx])[1:33]
  team2OrdinalRanks <- getExtraRankings(season, team2Vector[testIdx])[1:33]
  #Transform Ordinals to power ratings
  team1PowerRatings <- 100 - 4* log(team1OrdinalRanks + 1) - team1OrdinalRanks / 22
  team2PowerRatings <- 100 - 4* log(team2OrdinalRanks + 1) - team2OrdinalRanks / 22  
  #Seeds Power Ranking
  team1PowerSeeds <- 100 - 4* log(as.numeric(team1Seed[1]) + 1) - as.numeric(team1Seed[1]) / 22
  team2PowerSeeds <- 100 - 4* log(as.numeric(team2Seed[1]) + 1) - as.numeric(team2Seed[1]) / 22
  #Average Scores & Winning percentages
  team1ScoreAndWins <- getScoreAndWins(season - 1, team1Vector[testIdx])
  team2ScoreAndWins <- getScoreAndWins(season - 1, team2Vector[testIdx])
  team1ScoreAndWins2Years <- getScoreAndWins(season - 2, team1Vector[testIdx])
  team2ScoreAndWins2Years <- getScoreAndWins(season - 2, team2Vector[testIdx])   
  team1ScoreAndWins3Years <- getScoreAndWins(season - 3, team1Vector[testIdx])
  team2ScoreAndWins3Years <- getScoreAndWins(season - 3, team2Vector[testIdx])
  #Average Points for and against during regular season
  team1ForAndAgainstPoints <- getForAndAgainstPoints(season, team1Vector[testIdx])
  team2ForAndAgainstPoints <- getForAndAgainstPoints(season, team2Vector[testIdx])  
    
  #Seed Based Benchmark
  seedBasedBenchmark <- 0.5 + (as.numeric(team2Seed[1]) - as.numeric(team1Seed[1])) * 0.03
  
  #Make a vector containing the features
  matchTeams <- c(team1Vector[testIdx], team2Vector[testIdx], 
                  team1Seed, team2Seed, seedBasedBenchmark, 
                  (team1PowerSeeds - team2PowerSeeds),
                  (team1PowerRatings - team2PowerRatings), mean((team1PowerRatings - team2PowerRatings), na.rm = TRUE),
                  team1ScoreAndWins - team2ScoreAndWins, 
                  team1ScoreAndWins2Years - team2ScoreAndWins2Years, 
                  team1ScoreAndWins3Years - team2ScoreAndWins3Years, 
                  team1ForAndAgainstPoints, team2ForAndAgainstPoints)
  
  return(matchTeams)
}

#Build the full training data.frame (2003-2015)
#Training Data 2003 - 2015
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
seasonDate <- 2015
set.seed(1001010)
positionShuffles <- rbinom(nrow(tourneyCompact), 1, 0.5)
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 

teamsGamesUnlisted <- unlist(mclapply(seq(first2003Idx, lastIdx), makeTrainTable, mc.cores = numCores,
                                      shufIdxs = positionShuffles))
teamsShuffledMatrixFull <- as.data.frame(matrix(teamsGamesUnlisted, nrow = length(seq(first2003Idx, lastIdx)), byrow = TRUE), 
                                         stringsAsFactors = FALSE)

validColTrain <- sapply(names(teamsShuffledMatrixFull)[-length(names(teamsShuffledMatrixFull))], function(nam){
  return(sum(is.na(teamsShuffledMatrixFull[, nam])))
})

for (i in seq(9, 42)){
  teamsShuffledMatrixFull[, i] <- as.numeric(teamsShuffledMatrixFull[, i])
}

#Select Best Rankings to Predict with--------------------------
#EDA 0.5; 2014 Season
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 

#Slice the training dataframe
teamsShuffledMatrix2014 <- teamsShuffledMatrixFull[1:(lastIdx - first2003Idx + 1), ]

#Linear Model Selection for Rankings
validRankings <- intersect(seq(9, 42), which(validColTrain == 0))
linearBestModels <- regsubsets(x = as.matrix(teamsShuffledMatrix2014[, validRankings]),
                               y = as.numeric(teamsShuffledMatrix2014[, ncol(teamsShuffledMatrix2014)]), 
                               method = "forward")

#Plot the best number of predictors
bestMods <- summary(linearBestModels)
bestNumberOfPredictors <- which.min(bestMods$cp)
plot(bestMods$cp, xlab="Number of Variables", ylab="CP Error", main ="Best Number of Rankings")
points(bestNumberOfPredictors, bestMods$cp[bestNumberOfPredictors], pch=20, col="red")

#Name of the most predictive rankings
predictors1 <- as.data.frame(bestMods$which)
bestRankings <- names(sort(apply(predictors1[, -1], 2, sum), decreasing = TRUE)[1:bestNumberOfPredictors])

#EDA Algorithms-------------------------------    
#EDA 1; 2011 Season
#Training Data 2003 - 2010
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
seasonDate <- 2011
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 

teamsShuffledMatrix2010 <- teamsShuffledMatrixFull[1:(lastIdx - first2003Idx + 1), ]

validColTrain <- sapply(names(teamsShuffledMatrix2010)[-length(names(teamsShuffledMatrix2010))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2010[, nam])))
})

#Test Data; 2011 Season
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1_2011 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2_2011 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1_2011)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1_2011, team2Vector = teams2_2011,
                                          season = seasonDate))
teamsTestMatrix2011 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1_2011), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2011), function(nam){
  return(sum(is.na(teamsTestMatrix2011[, nam])))
  })

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2010) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#GLMNET
registerDoParallel(numCores)

pointSpreadsTrain <- as.numeric(teamsShuffledMatrix2010[, ncol(teamsShuffledMatrix2010)])
dataFrameData <- rbind(teamsShuffledMatrix2010[, validCols], 
                       teamsTestMatrix2011[, validCols])

for (i in c(1, 2, 3, 5, seq(7, ncol(dataFrameData)))){
  dataFrameData[, i] <- as.numeric(dataFrameData[, i])
}

#transform train Dataframe to model matrix as glmnet only accepts matrices as input
trainTestMatrix <- model.matrix(~ . , data = dataFrameData)

trainMatrix <- trainTestMatrix[1:nrow(teamsShuffledMatrix2010), ]
testMatrix <- trainTestMatrix[(nrow(teamsShuffledMatrix2010) + 1):nrow(trainTestMatrix), ]

#Elastic Net alpha values validation
alphaValues2Test <- c(0, 1)
numberOfRepeatedModels <- 5

holdoutMSEScores <- sapply(alphaValues2Test, function(alphaValue){
  cvErrors <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber){    
    
    #Set seed for sampling
    set.seed(1001000 + modelNumber)
    #Shuffle indexes
    randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))
    
    #10 fold CV with glmnet
    GLMNETModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = alphaValue)
    
    plot(GLMNETModelCV)
    cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]
    
    print(paste0("MSE score of : ", cvError, " with an alpha value of: ", alphaValue))
    
    #Clear out memory
    rm(GLMNETModelCV)
    
    return(cvError)
  })
  
  return(c(mean(cvErrors), alphaValue))
})

#Alpha plot
holdoutMSEScores <- as.data.frame(t(holdoutMSEScores))
names(holdoutMSEScores) <- c("MSEcv", "Alpha")
ggplot(data = holdoutMSEScores, aes(x = Alpha, y = MSEcv)) + geom_line()

#Best Alpha
bestAlpha <- holdoutMSEScores[which.min(holdoutMSEScores$MSEcv), 2]

#TODO: h2o is not easy to work with right now, this code will be reimplemented in h2o if h2o is mpre accurate than glmnet and when automatic cross validation becomes available again
#Cross Validation
#Set seed for sampling
set.seed(1001000)
#Shuffle indexes  
randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))

#10 fold CV with glmnet
NCAA2011RFModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = bestAlpha)

plot(NCAA2011RFModelCV)
cvError <- NCAA2011RFModelCV$cvm[which(NCAA2011RFModelCV$lambda == NCAA2011RFModelCV$lambda.min)]  

NCAA2011RFPrediction <- signif(predict(NCAA2011RFModelCV, newx =  testMatrix, s = "lambda.min"), digits = 6)

print(paste0("MSE score of : ", cvError, " with an alpha value of: ", bestAlpha,
             " and a lambda value of: ", NCAA2011RFModelCV$lambda.min))

#EDA 2; 2012 Season
#Training Data 2003 - 2012
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
seasonDate <- 2012
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 

teamsShuffledMatrix2011 <- teamsShuffledMatrixFull[1:(lastIdx - first2003Idx + 1), ]

validColTrain <- sapply(names(teamsShuffledMatrix2011)[-length(names(teamsShuffledMatrix2011))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2011[, nam])))
})

#Test Data; 2012 Season
season2012Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1_2012 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2012Indexes])
teams2_2012 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2012Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1_2012)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1_2012, team2Vector = teams2_2012,
                                          season = seasonDate))
teamsTestMatrix2012 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1_2012), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2012), function(nam){
  return(sum(is.na(teamsTestMatrix2012[, nam])))
})

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2011) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#GLMNET
registerDoParallel(numCores)

pointSpreadsTrain <- as.numeric(teamsShuffledMatrix2011[, ncol(teamsShuffledMatrix2011)])
dataFrameData <- rbind(teamsShuffledMatrix2011[, validCols], 
                       teamsTestMatrix2012[, validCols])

for (i in c(1, 2, 3, 5, seq(7, ncol(dataFrameData)))){
  dataFrameData[, i] <- as.numeric(dataFrameData[, i])
}

#transform train Dataframe to model matrix as glmnet only accepts matrices as input
trainTestMatrix <- model.matrix(~ . , data = dataFrameData)

trainMatrix <- trainTestMatrix[1:nrow(teamsShuffledMatrix2011), ]
testMatrix <- trainTestMatrix[(nrow(teamsShuffledMatrix2011) + 1):nrow(trainTestMatrix), ]

#Elastic Net alpha values validation
alphaValues2Test <- c(0, 1)
numberOfRepeatedModels <- 5

holdoutMSEScores <- sapply(alphaValues2Test, function(alphaValue){
  cvErrors <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber){    
    
    #Set seed for sampling
    set.seed(1001000 + modelNumber)
    #Shuffle indexes
    randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))
    
    #10 fold CV with glmnet
    GLMNETModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = alphaValue)
    
    plot(GLMNETModelCV)
    cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]
    
    print(paste0("MSE score of : ", cvError, " with an alpha value of: ", alphaValue))
    
    #Clear out memory
    rm(GLMNETModelCV)
    
    return(cvError)
  })
  
  return(c(mean(cvErrors), alphaValue))
})

#Alpha plot
holdoutMSEScores <- as.data.frame(t(holdoutMSEScores))
names(holdoutMSEScores) <- c("MSEcv", "Alpha")
ggplot(data = holdoutMSEScores, aes(x = Alpha, y = MSEcv)) + geom_line()

#Best Alpha
bestAlpha <- holdoutMSEScores[which.min(holdoutMSEScores$MSEcv), 2]

#TODO: h2o is not easy to work with right now, this code will be reimplemented in h2o if h2o is mpre accurate than glmnet and when automatic cross validation becomes available again
#Cross Validation
#Set seed for sampling
set.seed(1001000)
#Shuffle indexes  
randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))

#10 fold CV with glmnet
NCAA2012RFModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = bestAlpha)

plot(NCAA2012RFModelCV)
cvError <- NCAA2012RFModelCV$cvm[which(NCAA2012RFModelCV$lambda == NCAA2012RFModelCV$lambda.min)]  

NCAA2012RFPrediction <- signif(predict(NCAA2012RFModelCV, newx =  testMatrix, s = "lambda.min"), digits = 6)

print(paste0("MSE score of : ", cvError, " with an alpha value of: ", bestAlpha,
             " and a lambda value of: ", NCAA2012RFModelCV$lambda.min))

#EDA 3; 2013 Season
#Training Data 2003 - 2013
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
seasonDate <- 2013
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 

teamsShuffledMatrix2012 <- teamsShuffledMatrixFull[1:(lastIdx - first2003Idx + 1), ]

validColTrain <- sapply(names(teamsShuffledMatrix2012)[-length(names(teamsShuffledMatrix2012))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2012[, nam])))
})

#Test Data; 2013 Season
season2013Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1_2013 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2013Indexes])
teams2_2013 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2013Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1_2013)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1_2013, team2Vector = teams2_2013,
                                          season = seasonDate))
teamsTestMatrix2013 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1_2013), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2013), function(nam){
  return(sum(is.na(teamsTestMatrix2013[, nam])))
})

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2012) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#GLMNET
registerDoParallel(numCores)

pointSpreadsTrain <- as.numeric(teamsShuffledMatrix2012[, ncol(teamsShuffledMatrix2012)])
dataFrameData <- rbind(teamsShuffledMatrix2012[, validCols], 
                       teamsTestMatrix2013[, validCols])

for (i in c(1, 2, 3, 5, seq(7, ncol(dataFrameData)))){
  dataFrameData[, i] <- as.numeric(dataFrameData[, i])
}

#transform train Dataframe to model matrix as glmnet only accepts matrices as input
trainTestMatrix <- model.matrix(~ . , data = dataFrameData)

trainMatrix <- trainTestMatrix[1:nrow(teamsShuffledMatrix2012), ]
testMatrix <- trainTestMatrix[(nrow(teamsShuffledMatrix2012) + 1):nrow(trainTestMatrix), ]

#Elastic Net alpha values validation
alphaValues2Test <- c(0, 1)
numberOfRepeatedModels <- 5

holdoutMSEScores <- sapply(alphaValues2Test, function(alphaValue){
  cvErrors <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber){    
    
    #Set seed for sampling
    set.seed(1001000 + modelNumber)
    #Shuffle indexes
    randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))
    
    #10 fold CV with glmnet
    GLMNETModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = alphaValue)
    
    plot(GLMNETModelCV)
    cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]
    
    print(paste0("MSE score of : ", cvError, " with an alpha value of: ", alphaValue))
    
    #Clear out memory
    rm(GLMNETModelCV)
    
    return(cvError)
  })
  
  return(c(mean(cvErrors), alphaValue))
})

#Alpha plot
holdoutMSEScores <- as.data.frame(t(holdoutMSEScores))
names(holdoutMSEScores) <- c("MSEcv", "Alpha")
ggplot(data = holdoutMSEScores, aes(x = Alpha, y = MSEcv)) + geom_line()

#Best Alpha
bestAlpha <- holdoutMSEScores[which.min(holdoutMSEScores$MSEcv), 2]

#TODO: h2o is not easy to work with right now, this code will be reimplemented in h2o if h2o is mpre accurate than glmnet and when automatic cross validation becomes available again
#Cross Validation
#Set seed for sampling
set.seed(1001000)
#Shuffle indexes  
randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))

#10 fold CV with glmnet
NCAA2013RFModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = bestAlpha)

plot(NCAA2013RFModelCV)
cvError <- NCAA2013RFModelCV$cvm[which(NCAA2013RFModelCV$lambda == NCAA2013RFModelCV$lambda.min)]  

NCAA2013RFPrediction <- signif(predict(NCAA2013RFModelCV, newx =  testMatrix, s = "lambda.min"), digits = 6)

print(paste0("MSE score of : ", cvError, " with an alpha value of: ", bestAlpha,
             " and a lambda value of: ", NCAA2013RFModelCV$lambda.min))

#EDA 4; 2014 Season
#Training Data 2003 - 2014
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
seasonDate <- 2014
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 

teamsShuffledMatrix2013 <- teamsShuffledMatrixFull[1:(lastIdx - first2003Idx + 1), ]

validColTrain <- sapply(names(teamsShuffledMatrix2013)[-length(names(teamsShuffledMatrix2013))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2013[, nam])))
})

#Test Data; 2014 Season
season2014Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1_2014 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2014Indexes])
teams2_2014 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2014Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1_2014)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1_2014, team2Vector = teams2_2014,
                                          season = seasonDate))
teamsTestMatrix2014 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1_2014), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2014), function(nam){
  return(sum(is.na(teamsTestMatrix2014[, nam])))
})

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2013) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#GLMNET
registerDoParallel(numCores)

pointSpreadsTrain <- as.numeric(teamsShuffledMatrix2013[, ncol(teamsShuffledMatrix2013)])
dataFrameData <- rbind(teamsShuffledMatrix2013[, validCols], 
                       teamsTestMatrix2014[, validCols])

for (i in c(1, 2, 3, 5, seq(7, ncol(dataFrameData)))){
  dataFrameData[, i] <- as.numeric(dataFrameData[, i])
}

#transform train Dataframe to model matrix as glmnet only accepts matrices as input
trainTestMatrix <- model.matrix(~ . , data = dataFrameData)

trainMatrix <- trainTestMatrix[1:nrow(teamsShuffledMatrix2013), ]
testMatrix <- trainTestMatrix[(nrow(teamsShuffledMatrix2013) + 1):nrow(trainTestMatrix), ]

#Elastic Net alpha values validation
alphaValues2Test <- c(0, 1)
numberOfRepeatedModels <- 5

holdoutMSEScores <- sapply(alphaValues2Test, function(alphaValue){
  cvErrors <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber){    
    
    #Set seed for sampling
    set.seed(1001000 + modelNumber)
    #Shuffle indexes
    randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))
    
    #10 fold CV with glmnet
    GLMNETModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = alphaValue)
    
    plot(GLMNETModelCV)
    cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]
    
    print(paste0("MSE score of : ", cvError, " with an alpha value of: ", alphaValue))
    
    #Clear out memory
    rm(GLMNETModelCV)
    
    return(cvError)
  })
  
  return(c(mean(cvErrors), alphaValue))
})

#Alpha plot
holdoutMSEScores <- as.data.frame(t(holdoutMSEScores))
names(holdoutMSEScores) <- c("MSEcv", "Alpha")
ggplot(data = holdoutMSEScores, aes(x = Alpha, y = MSEcv)) + geom_line()

#Best Alpha
bestAlpha <- holdoutMSEScores[which.min(holdoutMSEScores$MSEcv), 2]

#TODO: h2o is not easy to work with right now, this code will be reimplemented in h2o if h2o is mpre accurate than glmnet and when automatic cross validation becomes available again
#Cross Validation
#Set seed for sampling
set.seed(1001000)
#Shuffle indexes  
randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))

#10 fold CV with glmnet
NCAA2014RFModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = bestAlpha)

plot(NCAA2014RFModelCV)
cvError <- NCAA2014RFModelCV$cvm[which(NCAA2014RFModelCV$lambda == NCAA2014RFModelCV$lambda.min)]  

NCAA2014RFPrediction <- signif(predict(NCAA2014RFModelCV, newx =  testMatrix, s = "lambda.min"), digits = 6)

print(paste0("MSE score of : ", cvError, " with an alpha value of: ", bestAlpha,
             " and a lambda value of: ", NCAA2014RFModelCV$lambda.min))

#Make a Kaggle Submission file with the predictions
sampleSubmission$pred <- c(NCAA2011RFPrediction, 
                           NCAA2012RFPrediction,
                           NCAA2013RFPrediction, 
                           NCAA2014RFPrediction)

sampleSubmission$pred <- 1 / (1 + 10 ^ (-(sampleSubmission$pred)/15))

write.csv(sampleSubmission, file = "RFXVI.csv", row.names = FALSE)
system('zip RFXVI.zip RFXVI.csv')

#Evaluate the models against the known results--------------------------
predictedActual2011 <- resultsPredictionMatrix(teams1 = teams1_2011, teams2 = teams2_2011, 
                                               year2Evaluate = 2011, yearsPrediction = NCAA2011RFPrediction)
predictedActual2012 <- resultsPredictionMatrix(teams1 = teams1_2012, teams2 = teams2_2012, 
                                               year2Evaluate = 2012, yearsPrediction = NCAA2012RFPrediction)
predictedActual2013 <- resultsPredictionMatrix(teams1 = teams1_2013, teams2 = teams2_2013, 
                                               year2Evaluate = 2013, yearsPrediction = NCAA2013RFPrediction)
predictedActual2014 <- resultsPredictionMatrix(teams1 = teams1_2014, teams2 = teams2_2014, 
                                               year2Evaluate = 2014, yearsPrediction = NCAA2014RFPrediction)

predActual <- rbind(predictedActual2011, predictedActual2012, predictedActual2013, predictedActual2014)

print(paste0("LogLoss error for all four years of: ", logLoss(predActual[, 2], predActual[, 1])))

#MARCH MACHINE LEARNING 2015------------------------
#Append Rankings to Old Ones
masseyOrdinals2015 <- fread(file.path(dataDirectory, "massey_ordinals_2015_Tuesday_54systems.csv"))
MasseyOrdinals <- rbind(MasseyOrdinals, masseyOrdinals2015)

#Read the 2015 test matches .csv
sampleSubmission <- fread(file.path(dataDirectory, "sample_submission_2015.csv"))

#Training Data 2003 - 2015
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
seasonDate <- 2015
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 

teamsShuffledMatrix2014 <- teamsShuffledMatrixFull[1:(lastIdx - first2003Idx + 1), ]

validColTrain <- sapply(names(teamsShuffledMatrix2014)[-length(names(teamsShuffledMatrix2014))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2014[, nam])))
})

#Test Data; 2015 Season
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9))
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14))

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2015 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2015), function(nam){
  return(sum(is.na(teamsTestMatrix2015[, nam])))
})

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2014) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#GLMNET
registerDoParallel(numCores)

pointSpreadsTrain <- as.numeric(teamsShuffledMatrix2014[, ncol(teamsShuffledMatrix2014)])
dataFrameData <- rbind(teamsShuffledMatrix2014[, validCols], 
                       teamsTestMatrix2015[, validCols])

for (i in c(1, 2, 3, 5, seq(7, ncol(dataFrameData)))){
  dataFrameData[, i] <- as.numeric(dataFrameData[, i])
}

#transform train Dataframe to model matrix as glmnet only accepts matrices as input
trainTestMatrix <- model.matrix(~ . , data = dataFrameData)

trainMatrix <- trainTestMatrix[1:nrow(teamsShuffledMatrix2014), ]
testMatrix <- trainTestMatrix[(nrow(teamsShuffledMatrix2014) + 1):nrow(trainTestMatrix), ]

#Elastic Net alpha values validation
alphaValues2Test <- c(0, 1)
numberOfRepeatedModels <- 5

holdoutMSEScores <- sapply(alphaValues2Test, function(alphaValue){
  cvErrors <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber){    
    
    #Set seed for sampling
    set.seed(1001000 + modelNumber)
    #Shuffle indexes
    randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))
    
    #10 fold CV with glmnet
    GLMNETModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = alphaValue)
    
    plot(GLMNETModelCV)
    cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]
    
    print(paste0("MSE score of : ", cvError, " with an alpha value of: ", alphaValue))
    
    #Clear out memory
    rm(GLMNETModelCV)
    
    return(cvError)
  })
  
  return(c(mean(cvErrors), alphaValue))
})

#Alpha plot
holdoutMSEScores <- as.data.frame(t(holdoutMSEScores))
names(holdoutMSEScores) <- c("MSEcv", "Alpha")
ggplot(data = holdoutMSEScores, aes(x = Alpha, y = MSEcv)) + geom_line()

#Best Alpha
bestAlpha <- holdoutMSEScores[which.min(holdoutMSEScores$MSEcv), 2]

#TODO: h2o is not easy to work with right now, this code will be reimplemented in h2o if h2o is mpre accurate than glmnet and when automatic cross validation becomes available again
#Cross Validation
#Set seed for sampling
set.seed(1001000)
#Shuffle indexes  
randomIndexOrder <- sample(seq(1, nrow(trainMatrix)), nrow(trainMatrix))

#10 fold CV with glmnet
NCAA2015RFModelCV <- cv.glmnet(x = trainMatrix[randomIndexOrder, ], 
                               y = pointSpreadsTrain[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "gaussian",
                               alpha = bestAlpha)

plot(NCAA2015RFModelCV)
cvError <- NCAA2015RFModelCV$cvm[which(NCAA2015RFModelCV$lambda == NCAA2015RFModelCV$lambda.min)]  

NCAA2015RFPrediction <- signif(predict(NCAA2015RFModelCV, newx = testMatrix, s = "lambda.min"), digits = 6)

print(paste0("MSE score of : ", cvError, " with an alpha value of: ", bestAlpha,
             " and a lambda value of: ", NCAA2015RFModelCV$lambda.min))

#Make a Kaggle Submission file with the predictions
sampleSubmission$pred <- NCAA2015RFPrediction

sampleSubmission$pred <- 1 / (1 + 10 ^ (-(sampleSubmission$pred)/15))

write.csv(sampleSubmission, file = "GLM2015VII.csv", row.names = FALSE)
system('zip GLM2015VII.zip GLM2015VII.csv')

##Steal this submission data ensemble
#Read steal this submission file
submissionStolen <- fread(file.path(dataDirectory, "kaggle_submission_public.csv"))

sampleSubmission$pred <- rowMeans(cbind(sampleSubmission$pred, submissionStolen$pred))
write.csv(sampleSubmission, file = "GLM2015VIII.csv", row.names = FALSE)
system('zip GLM2015VIII.zip GLM2015VIII.csv')
