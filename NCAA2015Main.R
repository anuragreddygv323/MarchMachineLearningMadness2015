#March Machine Learning Madness
#Ver 0.11 #bugs fixed

#Init-----------------------------------------------
rm(list=ls(all=TRUE))

#Libraries, directories, options and extra functions----------------------
require("parallel")
require("data.table")
require("h2o")
require("leaps")

#Set Working Directory
workingDirectory <- "/home/wacax/Wacax/Kaggle/March-Machine-Learning-Madness-2015/"
setwd(workingDirectory)
dataDirectory <- "/home/wacax/Wacax/Kaggle/March-Machine-Learning-Madness-2015/Data/"
#h2o location
h2o.jarLoc <- "/home/wacax/R/x86_64-pc-linux-gnu-library/3.1/h2o/java/h2o.jar"

#Detect available cores
numCores <- detectCores()

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
seasonCompact2015 <- fread(file.path(dataDirectory, "regular_season_compact_results_2015_prelim.csv"))
seasonDetailed2015 <- fread(file.path(dataDirectory, "regular_season_detailed_results_2015_prelim.csv"))
seeds2015 <- fread(file.path(dataDirectory, "tourney_seeds_2015_prelim.csv"))
seasonCompact <- rbind(seasonCompact, seasonCompact2015)
seasonDetailed <- rbind(seasonDetailed, seasonDetailed2015)
tourneySeeds <- rbind(tourneySeeds, seeds2015)


#Extra Data
MasseyOrdinals <- fread(file.path(dataDirectory, "massey_ordinals.csv"))
tourneyVenues <- fread(file.path(dataDirectory, "seasons_with_locations.csv"))

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
#Seed & Division 
getSeedDivision <- function(seasonFromData, teamFromData, trimmedSouce = TRUE){
  #Reduce the search space 
  if (trimmedSouce == TRUE){
    #Only data from 2003
    seedsSource <- tourneySeeds[1155:nrow(tourneySeeds)]
  }else{
    #Full Data (from 1985)
    seedsSource <- tourneySeeds
  }
  seedIndex <- which(seedsSource$season == seasonFromData & seedsSource$team == teamFromData)
  seed <- seedsSource$seed[seedIndex]
  seedTeam <- gsub(pattern = "[A-Z+a-z]", replacement = "", x = seed)
  divisionTeam <- gsub(pattern = "[0-9]", replacement = "", x = seed)
  #clean the extra letters
  divisionTeam <- gsub(pattern = "[a-z]", replacement = "", x = divisionTeam)  
  
  return(c(seedTeam, divisionTeam))
}

#get Team's average score and winning percentage
getScoreAndWins <- function(seasonMatch, teamMatch){
  table <- averagePointsList[[as.character(seasonMatch)]]
  if (sum(table[, 1] == teamMatch) > 0){
    scoreAndWins <- table[table[, 1] == teamMatch, c(2, 3)]
  }else{
    scoreAndWins <- c(0, 0)
  }  
  return(scoreAndWins)
}

#get Team's average for and against points during regular season
getForAndAgainstPoints <- function(seasonMatch, teamMatch){
  table <- pointsSeasonList[[as.character(seasonMatch)]]
  forAndAgainstPoints <- table[table[, 1] == teamMatch, c(2, 3)]  
  return(forAndAgainstPoints)
}

#Obtain Massey Rankings
rankingsColNames <- unique(MasseyOrdinals$sys_name)
getExtraRankings <- function(seasonMatch, teamMatch){
  masseyData <- MasseyOrdinals[MasseyOrdinals$team == teamMatch & MasseyOrdinals$season == seasonMatch] #it only takes 0.19s
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
                       #1 / (1 + 10 ^ (-(wPowerSeeds - lPowerSeeds)/15)),
                       (wTeamPowerRatings - lTeamPowerRatings), mean((wTeamPowerRatings - lTeamPowerRatings), na.rm = TRUE),
                       #1 / (1 + 10 ^ (-(wTeamPowerRatings - lTeamPowerRatings)/15)),
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
                       #1 / (1 + 10 ^ (-(lPowerSeeds - wPowerSeeds)/15)),
                       (lTeamPowerRatings - wTeamPowerRatings),  mean((lTeamPowerRatings - wTeamPowerRatings), na.rm = TRUE),
                       #1 / (1 + 10 ^ (-(lTeamPowerRatings - wTeamPowerRatings)/15)),
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
                  #1 / (1 + 10 ^ (-(team1PowerSeeds - team2PowerSeeds)/15)),                  
                  (team1PowerRatings - team2PowerRatings), mean((team1PowerRatings - team2PowerRatings), na.rm = TRUE),
                  #1 / (1 + 10 ^ (-(team1PowerRatings - team2PowerRatings)/15)),
                  team1ScoreAndWins - team2ScoreAndWins, 
                  team1ScoreAndWins2Years - team2ScoreAndWins2Years, 
                  team1ScoreAndWins3Years - team2ScoreAndWins3Years, 
                  team1ForAndAgainstPoints, team2ForAndAgainstPoints)
  
  return(matchTeams)
}

#Select Best Rankings to Predict with--------------------------
#EDA 0.5; 2014 Season
#Training Data 2003 - 2013
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
seasonDate <- 2015
positionShuffles <- rbinom(nrow(tourneyCompact), 1, 0.5)

lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 
teamsGamesUnlisted <- unlist(mclapply(seq(first2003Idx, lastIdx), makeTrainTable, mc.cores = numCores,
                                      shufIdxs = positionShuffles))
teamsShuffledMatrix2014 <- as.data.frame(matrix(teamsGamesUnlisted, nrow = length(seq(first2003Idx, lastIdx)), byrow = TRUE), 
                                         stringsAsFactors = FALSE)

validColTrain <- sapply(names(teamsShuffledMatrix2014)[-length(names(teamsShuffledMatrix2014))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2014[, nam])))
})

for (i in seq(9, 42)){
  teamsShuffledMatrix2014[, i] <- as.numeric(teamsShuffledMatrix2014[, i])
}

#Linear Model Selection for Rankings
validRankings <- intersect(seq(9, 42), which(validColTrain == 0))
linearBestModels <- regsubsets(x = as.matrix(teamsShuffledMatrix2014[, validRankings]),
                               y = as.numeric(teamsShuffledMatrix2014[, ncol(teamsShuffledMatrix2014)]), 
                               method = "backward")

#Plot the best number of predictors
bestMods <- summary(linearBestModels)
bestNumberOfPredictors <- which.min(bestMods$cp)
plot(bestMods$cp, xlab="Number of Variables", ylab="CP Error", main ="Best Number of Rankings")
points(bestNumberOfPredictors, bestMods$cp[bestNumberOfPredictors],pch=20,col="red")

#Name of the most predictive rankings
predictors1 <- as.data.frame(bestMods$which)
bestRankings <- names(sort(apply(predictors1[, -1], 2, sum), decreasing = TRUE)[1:bestNumberOfPredictors])

#EDA Algorithms-------------------------------    
#Set up training parameters
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
positionShuffles <- rbinom(nrow(tourneyCompact), 1, 0.5)

#EDA 1; 2011 Season
#Training Data 2003 - 2010
seasonDate <- 2011
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 
teamsGamesUnlisted <- unlist(mclapply(seq(first2003Idx, lastIdx), makeTrainTable, mc.cores = numCores,
                                      shufIdxs = positionShuffles))
teamsShuffledMatrix2010 <- as.data.frame(matrix(teamsGamesUnlisted, nrow = length(seq(first2003Idx, lastIdx)), byrow = TRUE), 
                                         stringsAsFactors = FALSE)

validColTrain <- sapply(names(teamsShuffledMatrix2010)[-length(names(teamsShuffledMatrix2010))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2010[, nam])))
})

#Test Data; 2011 Season
seasonDate <- 2011
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2011 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2011), function(nam){
  return(sum(is.na(teamsTestMatrix2011[, nam])))
  })

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2010) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2011 <- as.h2o(h2oServer, signif(teamsShuffledMatrix2010[, c(validCols, ncol(teamsShuffledMatrix2010))], digits = 6))
#h2o.ai Test
h2oTest2011 <- as.h2o(h2oServer, signif(teamsTestMatrix2011[, validCols], digits = 6))
#Remove Data
rm(teamsShuffledMatrix2010, teamsTestMatrix2011)

#h2o.ai Cross Validation
NCAA2011RFModelCV <- h2o.glm(x = seq(3, ncol(h2oTrain2011) - 1), y = ncol(h2oTrain2011),
                             data = h2oTrain2011,
                             nfolds = 5,
                             family = "gaussian", 
                             alpha = c(0, 1),
                             lambda_search = TRUE,
                             nlambda = 100)

print(paste0("There is an error of: ", NCAA2011RFModelCV@model[[1]]@model$deviance))

#Model Training
NCAA2011RFModel <- h2o.glm(x = seq(3, ncol(h2oTrain2011) - 1), y = ncol(h2oTrain2011),
                           data = h2oTrain2011,
                           family = "gaussian", 
                           alpha = NCAA2011RFModelCV@model[[1]]@model$params$alpha,
                           lambda = NCAA2011RFModelCV@model[[1]]@model$params$lambda)

#probability Predictions on all 2011 NCAA Games
NCAA2011RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2011RFModel, newdata = h2oTest2011)), digits = 8)[, 1]

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#EDA 2; 2012 Season
#Training Data 2003 - 2011
seasonDate <- 2012
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 
teamsGamesUnlisted <- unlist(mclapply(seq(first2003Idx, lastIdx), makeTrainTable, mc.cores = numCores,
                                      shufIdxs = positionShuffles))
teamsShuffledMatrix2011 <- as.data.frame(matrix(teamsGamesUnlisted, nrow = length(seq(first2003Idx, lastIdx)), byrow = TRUE), 
                                         stringsAsFactors = FALSE)

validColTrain <- sapply(names(teamsShuffledMatrix2011)[-length(names(teamsShuffledMatrix2011))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2011[, nam])))
})

#Test Data; 2012 Season
seasonDate <- 2012
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2012 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2012), function(nam){
  return(sum(is.na(teamsTestMatrix2012[, nam])))
})

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2011) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2012 <- as.h2o(h2oServer, signif(teamsShuffledMatrix2011[, c(validCols, ncol(teamsShuffledMatrix2011))], digits = 6))
#h2o.ai Test
h2oTest2012 <- as.h2o(h2oServer, signif(teamsTestMatrix2012[, validCols], digits = 6))
#Remove Data
rm(teamsShuffledMatrix2011, teamsTestMatrix2012)

#h2o.ai Cross Validation
NCAA2012RFModelCV <- h2o.glm(x = seq(3, ncol(h2oTrain2012) - 1), y = ncol(h2oTrain2012),
                             data = h2oTrain2012,
                             nfolds = 5,
                             family = "gaussian", 
                             alpha = c(0, 1),
                             lambda_search = TRUE,
                             nlambda = 100)

print(paste0("There is an error of: ", NCAA2012RFModelCV@model[[1]]@model$deviance))

#Model Training
NCAA2012RFModel <- h2o.glm(x = seq(3, ncol(h2oTrain2012) - 1), y = ncol(h2oTrain2012),
                           data = h2oTrain2012,
                           family = "gaussian", 
                           alpha = NCAA2011RFModelCV@model[[1]]@model$params$alpha,
                           lambda = NCAA2011RFModelCV@model[[1]]@model$params$lambda)

#probability Predictions on all 2012 NCAA Games
NCAA2012RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2012RFModel, newdata = h2oTest2012)), digits = 8)[, 1]

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#EDA 3; 2013 Season
#Training Data 2003 - 2012
seasonDate <- 2013
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 
teamsGamesUnlisted <- unlist(mclapply(seq(first2003Idx, lastIdx), makeTrainTable, mc.cores = numCores,
                                      shufIdxs = positionShuffles))
teamsShuffledMatrix2012 <- as.data.frame(matrix(teamsGamesUnlisted, nrow = length(seq(first2003Idx, lastIdx)), byrow = TRUE), 
                                         stringsAsFactors = FALSE)

validColTrain <- sapply(names(teamsShuffledMatrix2012)[-length(names(teamsShuffledMatrix2012))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2012[, nam])))
})

#Test Data; 2013 Season
seasonDate <- 2013
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2013 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2013), function(nam){
  return(sum(is.na(teamsTestMatrix2013[, nam])))
})

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2012) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2013 <- as.h2o(h2oServer, signif(teamsShuffledMatrix2012[, c(validCols, ncol(teamsShuffledMatrix2012))], digits = 6))
#h2o.ai Test
h2oTest2013 <- as.h2o(h2oServer, signif(teamsTestMatrix2013[, validCols], digits = 6))
#Remove Data
rm(teamsShuffledMatrix2012, teamsTestMatrix2013)

#h2o.ai Cross Validation
NCAA2013RFModelCV <- h2o.glm(x = seq(3, ncol(h2oTrain2013) - 1), y = ncol(h2oTrain2013),
                             data = h2oTrain2013,
                             nfolds = 5,
                             family = "gaussian", 
                             alpha = c(0, 1),
                             lambda_search = TRUE,
                             nlambda = 100)

print(paste0("There is an error of: ", NCAA2013RFModelCV@model[[1]]@model$deviance))

#Model Training
NCAA2013RFModel <- h2o.glm(x = seq(3, ncol(h2oTrain2013) - 1), y = ncol(h2oTrain2013),
                           data = h2oTrain2013,
                           family = "gaussian", 
                           alpha = NCAA2011RFModelCV@model[[1]]@model$params$alpha,
                           lambda = NCAA2011RFModelCV@model[[1]]@model$params$lambda)

#probability Predictions on all 2013 NCAA Games
NCAA2013RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2013RFModel, newdata = h2oTest2013)), digits = 8)[, 1]

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#EDA 4; 2014 Season
#Training Data 2003 - 2013
seasonDate <- 2014
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 
teamsGamesUnlisted <- unlist(mclapply(seq(first2003Idx, lastIdx), makeTrainTable, mc.cores = numCores,
                                      shufIdxs = positionShuffles))
teamsShuffledMatrix2013 <- as.data.frame(matrix(teamsGamesUnlisted, nrow = length(seq(first2003Idx, lastIdx)), byrow = TRUE), 
                                         stringsAsFactors = FALSE)

validColTrain <- sapply(names(teamsShuffledMatrix2013)[-length(names(teamsShuffledMatrix2013))], function(nam){
  return(sum(is.na(teamsShuffledMatrix2013[, nam])))
})

#Test Data; 2013 Season
seasonDate <- 2014
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2014 <- as.data.frame(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE), 
                                     stringsAsFactors = FALSE)

validColTest <- sapply(names(teamsTestMatrix2014), function(nam){
  return(sum(is.na(teamsTestMatrix2014[, nam])))
})

validCols <- intersect(which(validColTrain == 0), which(validColTest == 0))
bestRankingsIdxs <- which(names(teamsShuffledMatrix2013) %in% bestRankings)
validCols <- c(validCols[seq(1, 8)], bestRankingsIdxs, validCols[seq(length(validCols) - 9, length(validCols))])

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2014 <- as.h2o(h2oServer, signif(teamsShuffledMatrix2013[, c(validCols, ncol(teamsShuffledMatrix2013))], digits = 6))
#h2o.ai Test
h2oTest2014 <- as.h2o(h2oServer, signif(teamsTestMatrix2014[, validCols], digits = 6))
#Remove Data
rm(teamsShuffledMatrix2013, teamsTestMatrix2014)

#h2o.ai Cross Validation
NCAA2014RFModelCV <- h2o.glm(x = seq(3, ncol(h2oTrain2014) - 1), y = ncol(h2oTrain2014),
                             data = h2oTrain2014,
                             nfolds = 5,
                             family = "gaussian", 
                             alpha = c(0, 1),
                             lambda_search = TRUE,
                             nlambda = 100)

print(paste0("There is an error of: ", NCAA2014RFModelCV@model[[1]]@model$deviance))

#Model Training
NCAA2014RFModel <- h2o.glm(x = seq(3, ncol(h2oTrain2014) - 1), y = ncol(h2oTrain2014),
                           data = h2oTrain2014,
                           family = "gaussian", 
                           alpha = NCAA2011RFModelCV@model[[1]]@model$params$alpha,
                           lambda = NCAA2011RFModelCV@model[[1]]@model$params$lambda)

#probability Predictions on all 2014 NCAA Games
NCAA2014RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2014RFModel, newdata = h2oTest2014)), digits = 8)[, 1]

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#Make a Kaggle Submission file with the predictions
sampleSubmission$pred <- c(NCAA2011RFPrediction, 
                           NCAA2012RFPrediction,
                           NCAA2013RFPrediction, 
                           NCAA2014RFPrediction)

sampleSubmission$pred <- 1 / (1 + 10 ^ (-(sampleSubmission$pred)/15))

write.csv(sampleSubmission, file = "RFXV.csv", row.names = FALSE)
system('zip RFXV.zip RFXV.csv')

#Evaluate the models against the known results--------------------------
#Season 2011
seasonDate <- 2011
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

derp <- sapply(1:length(tourneyCompact$wteam[tourneyCompact$season == seasonDate]), function(matchIdx){
  predictionIdx <- union(which(teams1 %in% tourneyCompact$wteam[tourneyCompact$season == seasonDate][matchIdx] &
                                 teams2 %in% tourneyCompact$lteam[tourneyCompact$season == seasonDate][matchIdx]),
                         which(teams2 %in% tourneyCompact$wteam[tourneyCompact$season == seasonDate][matchIdx] &
                                 teams1 %in% tourneyCompact$lteam[tourneyCompact$season == seasonDate][matchIdx]))  
  return(predictionIdx)
})


#Season 2012
season11Idx <- tourneyCompact$season == 2012
#Season 2013
season11Idx <- tourneyCompact$season == 2013
#Season 2014
season11Idx <- tourneyCompact$season == 2014


#MARCH MACHINE LEARNING 2015------------------------
#Append Rankings New Data to Old Data
masseyOrdinals2015 <- fread(file.path(dataDirectory, "massey_ordinals_2015_prelim.csv"))
MasseyOrdinals <- rbind(MasseyOrdinals, masseyOrdinals2015)

#Read the 2015 test matches .csv
sampleSubmission <- fread(file.path(dataDirectory, "sample_submission_2015_prelim_all50pct.csv"))

#Training Data 2003 - 2014
#Set up training parameters
first2003Idx <- min(which(tourneyCompact$season == 2003)) 
positionShuffles <- rbinom(nrow(tourneyCompact), 1, 0.5)

seasonDate <- 2015
lastIdx <- max(which(tourneyCompact$season == seasonDate - 1)) 
teamsGamesUnlisted <- unlist(mclapply(seq(first2003Idx, lastIdx), makeTrainTable, mc.cores = numCores,
                                      shufIdxs = positionShuffles))
teamsShuffledMatrix2014 <- as.data.frame(matrix(teamsGamesUnlisted, nrow = length(seq(first2003Idx, lastIdx)), byrow = TRUE), 
                                         stringsAsFactors = FALSE)

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

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2015 <- as.h2o(h2oServer, signif(teamsShuffledMatrix2014[, c(validCols, ncol(teamsShuffledMatrix2014))], digits = 6))
#h2o.ai Test
h2oTest2015 <- as.h2o(h2oServer, signif(teamsTestMatrix2015[, validCols], digits = 6))
#Remove Data
rm(teamsShuffledMatrix2014, teamsTestMatrix2015)

#h2o.ai Cross Validation
NCAA2015RFModelCV <- h2o.glm(x = seq(3, ncol(h2oTrain2015) - 1), y = ncol(h2oTrain2015),
                             data = h2oTrain2015,
                             nfolds = 5,
                             family = "gaussian", 
                             alpha = c(0, 1),
                             lambda_search = TRUE,
                             nlambda = 100)

print(paste0("There is an error of: ", NCAA2015RFModelCV@model[[1]]@model$deviance))

#Model Training
NCAA2015RFModel <- h2o.glm(x = seq(3, ncol(h2oTrain2015) - 1), y = ncol(h2oTrain2015),
                           data = h2oTrain2015,
                           family = "gaussian", 
                           alpha = NCAA2015RFModelCV@model[[1]]@model$params$alpha,
                           lambda = NCAA2015RFModelCV@model[[1]]@model$params$lambda)

#probability Predictions on all 2014 NCAA Games
NCAA2015RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2015RFModel, newdata = h2oTest2015)), digits = 8)[, 1]

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#Make a Kaggle Submission file with the predictions
sampleSubmission$pred <- NCAA2015RFPrediction

sampleSubmission$pred <- 1 / (1 + 10 ^ (-(sampleSubmission$pred)/15))

write.csv(sampleSubmission, file = "GLM2015I.csv", row.names = FALSE)
system('zip GLM2015I.zip GLM2015I.csv')

