getScoreAndWins <- function(seasonMatch, teamMatch){
  
  #this function gets the team's average score and winning percentage
  
  table <- averagePointsList[[as.character(seasonMatch)]]
  if (sum(table[, 1] == teamMatch) > 0){
    scoreAndWins <- table[table[, 1] == teamMatch, c(2, 3)]
  }else{
    scoreAndWins <- c(0, 0)
  }  
  return(scoreAndWins)
}