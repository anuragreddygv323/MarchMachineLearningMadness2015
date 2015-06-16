getSeedDivision <- function(seasonFromData, teamFromData, trimmedSouce = TRUE){
  
  #Seed & Division 
  #THis function gets the seed and division of a team in a given season
  
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
