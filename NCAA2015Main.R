#March Machine Learning Madness
#Ver 0.3 #Added seed based benchmark

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
resultsCompact <- fread(file.path(dataDirectory, "regular_season_compact_results.csv"))
resultsDetailed <- fread(file.path(dataDirectory, "regular_season_detailed_results.csv"))
seasons <- fread(file.path(dataDirectory, "seasons.csv"))
teams <- fread(file.path(dataDirectory, "teams.csv"))
tourneyCompact <- fread(file.path(dataDirectory, "tourney_compact_results.csv"))
tourneyDetailed <- fread(file.path(dataDirectory, "tourney_detailed_results.csv"))
tourneySeeds <- fread(file.path(dataDirectory, "tourney_seeds.csv"))
tourneySlots <- fread(file.path(dataDirectory, "tourney_slots.csv"))

#Write .csv
sampleSubmission <- fread(file.path(dataDirectory, "sample_submission.csv"))

#Data Mining (Functions)------------------------
#Seed & Division 
getSeedDivision <- function(seasonFromData, teamFromData, trimmedSouce = FALSE){
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

#Shuffle Winning teams with its corresponding features
makeTrainTable <- function(gamesIdx, shufIdxs){
  wTeamSeed <- getSeedDivision(tourneyCompact$season[gamesIdx], tourneyCompact$wteam[gamesIdx])
  lTeamSeed <- getSeedDivision(tourneyCompact$season[gamesIdx], tourneyCompact$lteam[gamesIdx]) 
  
  if (shufIdxs[gamesIdx] == 1){    
    #Seed Based Benchmark
    seedBasedBenchmark <- 0.5 + (as.numeric(lTeamSeed[1]) - as.numeric(wTeamSeed[1])) * 0.03
    
    shuffledTeams <- c(tourneyCompact$wteam[gamesIdx], tourneyCompact$lteam[gamesIdx], 
                       wTeamSeed, lTeamSeed, seedBasedBenchmark)
  }else{
    #Seed Based Benchmark
    seedBasedBenchmark <- 0.5 + (as.numeric(wTeamSeed[1]) - as.numeric(lTeamSeed[1])) * 0.03
    
    shuffledTeams <- c(tourneyCompact$lteam[gamesIdx], tourneyCompact$wteam[gamesIdx], 
                       lTeamSeed, wTeamSeed, seedBasedBenchmark)
  }  
  return(shuffledTeams)
}

#Create Test Table
makeTestTable <- function(testIdx, team1Vector, team2Vector, season){
  #Get seeds from both teams
  team1Seed <- getSeedDivision(season, team1Vector[testIdx])
  team2Seed <- getSeedDivision(season, team2Vector[testIdx])
  
  #Seed Based Benchmark
  seedBasedBenchmark <- 0.5 + (as.numeric(team2Seed[1]) - as.numeric(team1Seed[1])) * 0.03
  
  #Make a vector containing the features
  matchTeams <- c(team1Vector[testIdx], team2Vector[testIdx], 
                  team1Seed, team2Seed, seedBasedBenchmark)
  
  return(matchTeams)
}

#EDA-------------------------------
#EDA 1; 2011 Season
#Training Data 2003 - 2010
seasonDate <- 2011
lastIdx <- min(which(tourneyCompact$season == seasonDate)) - 1
positionShuffles <- rbinom(lastIdx, 1, 0.5)
teamsGamesUnlisted <- unlist(mclapply(seq(1, lastIdx), makeTrainTable, mc.cores = numCores, shufIdxs = positionShuffles))
teamsShuffledMatrix2010 <- as.data.table(cbind(matrix(teamsGamesUnlisted, nrow = lastIdx, byrow = TRUE), positionShuffles))

#Test Data; 2011 Season
seasonDate <- 2011
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2011 <- as.data.table(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE))

#h2o Init
#Start h2o directly from R
h2oServer <- h2o.init(ip = "localhost", max_mem_size = "5g", nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2011 <- as.h2o(h2oServer, as.data.frame(teamsShuffledMatrix2010))
#h2o.ai Test
h2oTest2011 <- as.h2o(h2oServer, as.data.frame(teamsTestMatrix2011))
#Remove Data
rm(teamsShuffledMatrix2010, teamsTestMatrix2011)

#h2o.ai Cross Validation
NCAA2011RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2011) - 1), y = ncol(h2oTrain2011),
                                      data = h2oTrain2011,
                                      nfolds = 5,
                                      classification = TRUE,
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", NCAA2011RFModelCV@model[[1]]@model$auc))

#Model Training
NCAA2011RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2011) - 1), y = ncol(h2oTrain2011),
                                    data = h2oTrain2011,
                                    classification = TRUE,
                                    type = "BigData",
                                    ntree = NCAA2011RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2011RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

#probability Predictions on all 2011 NCAA Games
NCAA2011RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2011RFModel, newdata = h2oTest2011)[, 3]), digits = 8)

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#EDA 2; 2012 Season
#Training Data 2003 - 2011
seasonDate <- 2012
lastIdx <- min(which(tourneyCompact$season == seasonDate)) - 1
positionShuffles <- rbinom(lastIdx, 1, 0.5)
teamsGamesUnlisted <- unlist(mclapply(seq(1, lastIdx), makeTrainTable, mc.cores = numCores, shufIdxs = positionShuffles))
teamsShuffledMatrix2011 <- as.data.table(cbind(matrix(teamsGamesUnlisted, nrow = lastIdx, byrow = TRUE), positionShuffles))

#Test Data; 2012 Season
seasonDate <- 2012
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2012 <- as.data.table(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE))

#h2o Init
#Start h2o directly from R
h2oServer <- h2o.init(ip = "localhost", max_mem_size = "5g", nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2012 <- as.h2o(h2oServer, as.data.frame(teamsShuffledMatrix2011))
#h2o.ai Test
h2oTest2012 <- as.h2o(h2oServer, as.data.frame(teamsTestMatrix2012))
#Remove Data
rm(teamsShuffledMatrix2011, teamsTestMatrix2012)

#h2o.ai Cross Validation
NCAA2012RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2012) - 1), y = ncol(h2oTrain2012),
                                      data = h2oTrain2012,
                                      nfolds = 5,
                                      classification = TRUE,
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", NCAA2012RFModelCV@model[[1]]@model$auc))

#Model Training
NCAA2012RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2012) - 1), y = ncol(h2oTrain2012),
                                    data = h2oTrain2012,
                                    classification = TRUE,
                                    type = "BigData",
                                    ntree = NCAA2012RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2012RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

#probability Predictions on all 2011 NCAA Games
NCAA2012RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2012RFModel, newdata = h2oTest2012)[, 3]), digits = 8)

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#EDA 3; 2013 Season
#Training Data 2003 - 2012
seasonDate <- 2013
lastIdx <- min(which(tourneyCompact$season == seasonDate)) - 1
positionShuffles <- rbinom(lastIdx, 1, 0.5)
teamsGamesUnlisted <- unlist(mclapply(seq(1, lastIdx), makeTrainTable, mc.cores = numCores, shufIdxs = positionShuffles))
teamsShuffledMatrix2012 <- as.data.table(cbind(matrix(teamsGamesUnlisted, nrow = lastIdx, byrow = TRUE), positionShuffles))

#Test Data; 2013 Season
seasonDate <- 2013
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2013 <- as.data.table(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE))

#h2o Init
#Start h2o directly from R
h2oServer <- h2o.init(ip = "localhost", max_mem_size = "5g", nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2013 <- as.h2o(h2oServer, as.data.frame(teamsShuffledMatrix2012))
#h2o.ai Test
h2oTest2013 <- as.h2o(h2oServer, as.data.frame(teamsTestMatrix2013))
#Remove Data
rm(teamsShuffledMatrix2012, teamsTestMatrix2013)

#h2o.ai Cross Validation
NCAA2013RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2013) - 1), y = ncol(h2oTrain2013),
                                      data = h2oTrain2013,
                                      nfolds = 5,
                                      classification = TRUE,
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", NCAA2013RFModelCV@model[[1]]@model$auc))

#Model Training
NCAA2013RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2013) - 1), y = ncol(h2oTrain2013),
                                    data = h2oTrain2013,
                                    classification = TRUE,
                                    type = "BigData",
                                    ntree = NCAA2013RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2013RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

#probability Predictions on all 2011 NCAA Games
NCAA2013RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2013RFModel, newdata = h2oTest2013)[, 3]), digits = 8)

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#EDA 4; 2014 Season
#Training Data 2003 - 2013
seasonDate <- 2014
lastIdx <- min(which(tourneyCompact$season == seasonDate)) - 1
positionShuffles <- rbinom(lastIdx, 1, 0.5)
teamsGamesUnlisted <- unlist(mclapply(seq(1, lastIdx), makeTrainTable, mc.cores = numCores, shufIdxs = positionShuffles))
teamsShuffledMatrix2013 <- as.data.table(cbind(matrix(teamsGamesUnlisted, nrow = lastIdx, byrow = TRUE), positionShuffles))

#Test Data; 2013 Season
seasonDate <- 2014
season2011Indexes <- which(substr(sampleSubmission$id, 1, 4) == seasonDate)
teams1 <- as.numeric(substr(sampleSubmission$id, 6, 9)[season2011Indexes])
teams2 <- as.numeric(substr(sampleSubmission$id, 11, 14)[season2011Indexes])

teamsGamesTestUnlisted <- unlist(mclapply(seq(1, length(teams1)), makeTestTable, mc.cores = numCores,
                                          team1Vector = teams1, team2Vector = teams2,
                                          season = seasonDate))
teamsTestMatrix2014 <- as.data.table(matrix(teamsGamesTestUnlisted, nrow = length(teams1), byrow = TRUE))

#h2o Init
#Start h2o directly from R
h2oServer <- h2o.init(ip = "localhost", max_mem_size = "5g", nthreads = -1)

#Load Data to h2o
#h2o.ai Train
h2oTrain2014 <- as.h2o(h2oServer, as.data.frame(teamsShuffledMatrix2013))
#h2o.ai Test
h2oTest2014 <- as.h2o(h2oServer, as.data.frame(teamsTestMatrix2014))
#Remove Data
rm(teamsShuffledMatrix2013, teamsTestMatrix2014)

#h2o.ai Cross Validation
NCAA2014RFModelCV <- h2o.randomForest(x = seq(3, ncol(h2oTrain2014) - 1), y = ncol(h2oTrain2014),
                                      data = h2oTrain2014,
                                      nfolds = 5,
                                      classification = TRUE,
                                      ntree = c(50, 75, 100),
                                      depth = c(20, 50, 75), 
                                      verbose = TRUE)

print(paste0("There is an AUC error of: ", NCAA2014RFModelCV@model[[1]]@model$auc))

#Model Training
NCAA2014RFModel <- h2o.randomForest(x = seq(3, ncol(h2oTrain2014) - 1), y = ncol(h2oTrain2014),
                                    data = h2oTrain2014,
                                    classification = TRUE,
                                    type = "BigData",
                                    ntree = NCAA2014RFModelCV@model[[1]]@model$params$ntree,
                                    depth = NCAA2014RFModelCV@model[[1]]@model$params$depth, 
                                    verbose = TRUE)

#probability Predictions on all 2014 NCAA Games
NCAA2014RFPrediction <- signif(as.data.frame(h2o.predict(NCAA2014RFModel, newdata = h2oTest2014)[, 3]), digits = 8)

#Shutdown h20 instance
h2o.shutdown(h2oServer, prompt = FALSE)

#Make a Kaggle Submission file with the predictions
sampleSubmission$pred <- c(NCAA2011RFPrediction[, 1], 
                           NCAA2012RFPrediction[, 1],
                           NCAA2013RFPrediction[, 1], 
                           NCAA2014RFPrediction[, 1])
write.csv(sampleSubmission, file = "RFII.csv", row.names = FALSE)
system('zip RFII.zip RFII.csv')

#Evaluate the models against the known results

#MARCH MACHINE LEARNING 2015------------------------
#TODO

#Make a Kaggle Submission file with the predictions


