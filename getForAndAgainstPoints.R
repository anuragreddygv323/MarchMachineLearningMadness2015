getForAndAgainstPoints <- function(seasonMatch, teamMatch){
  
  #This function gets the team's average of for and against points during regular season
  
  table <- pointsSeasonList[[as.character(seasonMatch)]]
  forAndAgainstPoints <- table[table[, 1] == teamMatch, c(2, 3)]  
  return(forAndAgainstPoints)
}