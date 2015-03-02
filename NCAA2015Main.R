#March Machine Learning Madness
#Ver 0.7 #Back to MOV outcomes, Power rankings included

#Init-----------------------------------------------
rm(list=ls(all=TRUE))

#Libraries, directories, options and extra functions----------------------
require("parallel")
require("data.table")
require("h2o")

#Set Working Directory
workingDirectory <- "/home/wacax/Wacax/Kaggle/March-Machine-Learning-Madness-2015/"
setwd(workingDirectory)
dataDirectory <- "/home/wacax/Wacax/Kaggle/March-Machine-Learning-Madness-2015/Data/"
outputDirectory <- "/home/wacax/Wacax/Kaggle/March-Machine-Learning-Madness-2015/Data/Output"
vw77Dir = "/home/wacax/vowpal_wabbit-7.7/vowpalwabbit/"
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

#Extra Data
MasseyOrdinals <- fread(file.path(dataDirectory, "massey_ordinals.csv"))
tourneyVenues <- fread(file.path(dataDirectory, "seasons_with_locations.csv"))

#Write .csv
sampleSubmission <- fread(file.path(dataDirectory, "sample_submission.csv"))

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
  
  if (shufIdxs[gamesIdx] == 1){    
    #Seed Based Benchmark
    seedBasedBenchmark <- 0.5 + (as.numeric(lTeamSeed[1]) - as.numeric(wTeamSeed[1])) * 0.03    
        
    shuffledTeams <- c(tourneyCompact$wteam[gamesIdx], tourneyCompact$lteam[gamesIdx], 
                       wTeamSeed, lTeamSeed, seedBasedBenchmark,
                       (wPowerSeeds - lPowerSeeds),
                       1 / (1 + 10 ^ (-(wPowerSeeds - lPowerSeeds)/15)),
                       #wTeamOrdinalRanks, lTeamOrdinalRanks,
                       #wTeamPowerRatings, lTeamPowerRatings,
                       (wTeamPowerRatings - lTeamPowerRatings),
                       1 / (1 + 10 ^ (-(wTeamPowerRatings - lTeamPowerRatings)/15)),
                       tourneyCompact$wscore[gamesIdx] - tourneyCompact$lscore[gamesIdx]
                       )
  }else{
    #Seed Based Benchmark
    seedBasedBenchmark <- 0.5 + (as.numeric(wTeamSeed[1]) - as.numeric(lTeamSeed[1])) * 0.03
        
    shuffledTeams <- c(tourneyCompact$lteam[gamesIdx], tourneyCompact$wteam[gamesIdx], 
                       lTeamSeed, wTeamSeed, seedBasedBenchmark,
                       (lPowerSeeds - wPowerSeeds),
                       1 / (1 + 10 ^ (-(lPowerSeeds - wPowerSeeds)/15)),
                       #lTeamOrdinalRanks, wTeamOrdinalRanks,
                       #lTeamPowerRatings, wTeamPowerRatings, 
                       (lTeamPowerRatings - wTeamPowerRatings),
                       1 / (1 + 10 ^ (-(lTeamPowerRatings - wTeamPowerRatings)/15)),
                       tourneyCompact$lscore[gamesIdx] - tourneyCompact$wscore[gamesIdx]
                       )
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
    
  #Seed Based Benchmark
  seedBasedBenchmark <- 0.5 + (as.numeric(team2Seed[1]) - as.numeric(team1Seed[1])) * 0.03
  
  #Make a vector containing the features
  matchTeams <- c(team1Vector[testIdx], team2Vector[testIdx], 
                  team1Seed, team2Seed, seedBasedBenchmark, 
                  (team1PowerSeeds - team2PowerSeeds),
                  1 / (1 + 10 ^ (-(team1PowerSeeds - team2PowerSeeds)/15)),                  
                  #team1OrdinalRanks, team2OrdinalRanks, 
                  #team1PowerRatings, team2PowerRatings, 
                  (team1PowerRatings - team2PowerRatings),
                  1 / (1 + 10 ^ (-(team1PowerRatings - team2PowerRatings)/15))
                  )
  
  return(matchTeams)
}

#EDA-------------------------------                  
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

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2011 <- as.h2o(h2oServer, teamsShuffledMatrix2010[, c(validCols, ncol(teamsShuffledMatrix2010))])
#h2o.ai Test
h2oTest2011 <- as.h2o(h2oServer, teamsTestMatrix2011[, validCols])
#Remove Data
rm(teamsShuffledMatrix2010, teamsTestMatrix2011)

#h2o.ai Cross Validation
NCAA2011RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2011) - 1), y = ncol(h2oTrain2011),
                                      data = h2oTrain2011,
                                      nfolds = 5,
                                      #classification = TRUE,
                                      classification = FALSE,
                                      type = "BigData",                                      
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", min(NCAA2011RFModelCV@model[[1]]@model$mse, na.rm = TRUE)))

#Model Training
NCAA2011RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2011) - 1), y = ncol(h2oTrain2011),
                                    data = h2oTrain2011,
                                    #classification = TRUE,
                                    classification = FALSE,                                                                        
                                    type = "BigData",
                                    ntree = NCAA2011RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2011RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

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

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2012 <- as.h2o(h2oServer, teamsShuffledMatrix2011[, c(validCols, ncol(teamsShuffledMatrix2011))])
#h2o.ai Test
h2oTest2012 <- as.h2o(h2oServer, teamsTestMatrix2012[, validCols])
#Remove Data
rm(teamsShuffledMatrix2011, teamsTestMatrix2012)

#h2o.ai Cross Validation
NCAA2012RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2012) - 1), y = ncol(h2oTrain2012),
                                      data = h2oTrain2012,
                                      nfolds = 5,
                                      #classification = TRUE,
                                      classification = FALSE,
                                      type = "BigData",                                      
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", min(NCAA2012RFModelCV@model[[1]]@model$mse, na.rm = TRUE)))

#Model Training
NCAA2012RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2012) - 1), y = ncol(h2oTrain2012),
                                    data = h2oTrain2012,
                                    #classification = TRUE,
                                    classification = FALSE,
                                    type = "BigData", 
                                    ntree = NCAA2012RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2012RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

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

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2013 <- as.h2o(h2oServer, teamsShuffledMatrix2012[, c(validCols, ncol(teamsShuffledMatrix2012))])
#h2o.ai Test
h2oTest2013 <- as.h2o(h2oServer, teamsTestMatrix2013[, validCols])
#Remove Data
rm(teamsShuffledMatrix2012, teamsTestMatrix2013)

#h2o.ai Cross Validation
NCAA2013RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2013) - 1), y = ncol(h2oTrain2013),
                                      data = h2oTrain2013,
                                      nfolds = 5,
                                      #classification = TRUE,
                                      classification = FALSE,
                                      type = "BigData", 
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", min(NCAA2013RFModelCV@model[[1]]@model$mse, na.rm = TRUE)))

#Model Training
NCAA2013RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2013) - 1), y = ncol(h2oTrain2013),
                                    data = h2oTrain2013,
                                    #classification = TRUE,
                                    classification = FALSE,
                                    type = "BigData", 
                                    ntree = NCAA2013RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2013RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

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

#h2o.ai
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name NCAA2015 &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2014 <- as.h2o(h2oServer, teamsShuffledMatrix2013[, c(validCols, ncol(teamsShuffledMatrix2013))])
#h2o.ai Test
h2oTest2014 <- as.h2o(h2oServer, teamsTestMatrix2014[, validCols])
#Remove Data
rm(teamsShuffledMatrix2013, teamsTestMatrix2014)

#h2o.ai Cross Validation
NCAA2014RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2014) - 1), y = ncol(h2oTrain2014),
                                      data = h2oTrain2014,
                                      nfolds = 5,
                                      #classification = TRUE,
                                      classification = FALSE,
                                      type = "BigData", 
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", min(NCAA2014RFModelCV@model[[1]]@model$mse, na.rm = TRUE)))

#Model Training
NCAA2014RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2014) - 1), y = ncol(h2oTrain2014),
                                    data = h2oTrain2014,
                                    #classification = TRUE,
                                    classification = FALSE,
                                    type = "BigData", 
                                    ntree = NCAA2014RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2014RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

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

write.csv(sampleSubmission, file = "RFVII.csv", row.names = FALSE)
system('zip RFVII.zip RFVII.csv')

#Evaluate the models against the known results

#MARCH MACHINE LEARNING 2015------------------------
#TODO

#Make a Kaggle Submission file with the predictions


