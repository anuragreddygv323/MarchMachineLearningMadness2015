resultsPredictionMatrix <- function(teams1, teams2, year2Evaluate, yearsPrediction){
  
  #This function returns a matrix with a column with the predictions of the model and a 
  #second column with the actual result in the match
  
  listPossibleMatches <- function(teamsPair){
    return(paste(teamsPair[1], teamsPair[2], sep = "_"))
  }
  matches1 <- apply(tourneyCompact[season == year2Evaluate, .(wteam, lteam)], 1, listPossibleMatches)
  matches2 <- apply(tourneyCompact[season == year2Evaluate, .(lteam, wteam)], 1, listPossibleMatches)
  
  possibleMatches <- apply(cbind(teams1, teams2), 1, listPossibleMatches)
  
  matchesIdxs_1 <- match(possibleMatches, matches1, nomatch = 0)
  
  #possibleMatches[as.logical(matchesIdxs_1)]
  #tourneyCompact[season == year2Evaluate, .(wteam, lteam)][matchesIdxs_1[matchesIdxs_1 > 0]]
  
  numericPrediction_1 <- yearsPrediction[as.logical(matchesIdxs_1)]
  probabilityPrediction_1 <- 1 / (1 + 10 ^ (-(numericPrediction_1)/15))
  predictedActual_1 <- cbind(probabilityPrediction_1, rep(1, length(probabilityPrediction_1)))
  
  matchesIdxs_2 <- match(possibleMatches, matches2, nomatch = 0)
  
  #possibleMatches[as.logical(matchesIdxs_2)]
  #tourneyCompact[season == year2Evaluate, .(lteam, wteam)][matchesIdxs_2[matchesIdxs_2 > 0]]
  
  numericPrediction_2 <- yearsPrediction[as.logical(matchesIdxs_2)]
  probabilityPrediction_2 <- 1 / (1 + 10 ^ (-(numericPrediction_2)/15))
  predictedActual_2 <- cbind(probabilityPrediction_2, rep(0, length(probabilityPrediction_2)))
  
  predictedActual <- rbind(predictedActual_1, predictedActual_2)
  
  print(paste0("LogLoss error in year ", year2Evaluate, " of: ", logLoss(predictedActual[, 2], predictedActual[, 1])))
  
  return(predictedActual)
}