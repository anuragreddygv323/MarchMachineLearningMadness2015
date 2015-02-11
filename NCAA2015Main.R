#March Machine Learning Madness
#Ver 0.1 #Init Script

#Init-----------------------------------------------
rm(list=ls(all=TRUE))

#Libraries, directories, options and extra functions----------------------
require("parallel")
require("data.table")

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
#Shuffle Wins and Losses
ShuffleFun <- function(gamesIdx, shufIdxs){
  if (shufIdxs[gamesIdx] == 1){
    shuffledTeams <- c(tourneyCompact$wteam[gamesIdx], tourneyCompact$lteam[gamesIdx])
  }else{
    shuffledTeams <- c(tourneyCompact$lteam[gamesIdx], tourneyCompact$wteam[gamesIdx])
  }  
  return(shuffledTeams)
}

#String Generator for training data
StringMaker <- function(seasonDate){
  lastIdx <- min(which(tourneyCompact$season == seasonDate)) - 1
  positionShuffles <- rbinom(lastIdx, 1, 0.5)
  teamsGamesUnlisted <- unlist(mclapply(seq(1, lastIdx), ShuffleFun, mc.cores = numCores, shufIdxs = positionShuffles))
  teamsShuffled <- signif(matrix(teamsGamesUnlisted, nrow = lastIdx, byrow = TRUE), digits = 3)
  paste(tourneyCompact$season, tourneyCompact$wteam, sep = "_")
}
#Define function to be passed as parallel
VectorMining <- function(gameString){
  season, team1, team2
  return(gameVector)
}
