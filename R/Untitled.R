#' @title Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title Contestatnt Selects a Door
#'
#' @description
#' `select_door()` has contestant makes their first selection at random of
#' 3 available doors with equal probability.
#'
#' @details
#' The function creates a numeric vector containing the values 1,2, and 3 to 
#' represent the available doors which it randomly selects exactly one door 
#' with equal probability of being chosen.
#' 
#' @param ... no arguments are used by the function.
#'
#' @return 
#' A single integer (1, 2, or 3) representing the randomly selected door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Host Opens Goat Door
#'
#' @description
#' `open_goat_door()` simulates the host's opening one door that was not
#' selected by the contestant and does not contain the car.
#'
#' @details
#' The function shows the doors the host is allowed to open which are the
#' doors that were not selected by the contesant and do not contain the car.
#' The host randomly selects one door to open. If the contestant selects the 
#' car on the first guess the host can open either door, but if the contestant
#' selects a goaat the host only has one option. 
#'
#' @param game A vector indicating what is behind each door (e.g., "car" or 
#' goat").
#' @param a.pick An integer (1,2, or 3) representing the door initially
#' selected by the contestant.
#'
#' @return 
#' A single integer (1,2, or 3) indicating the door opened by the host.
#'
#' @examples
#' open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Change Doors
#'
#' @description
#' `change_door()` determines the contestant's final door selection in the 
#' game based on whether the contestant chooses to stay with their initial 
#' selection or switch to the remaining unopened door.
#'
#' @details
#' The function gives the contestant the option to change from their initial
#' selection to the other door that is still closed. The fucntion will 
#' represent the game-playing strategy as the arguement stay=TRUE or
#' stay=FALSE.
#'
#' @param stay Logical value indicating whether the contestant stays with
#' their original choice. If TRUE, the contestant keeps their initial pick. If
#' FALSE, the contestant switches to the remaining door.
#' @param opened.door An integer (1,2, or 3) representing the door opened by 
#' the host.
#' @param a.pick An integer (1, 2, or 3) representing the door initially
#' selected by the contestant.
#'
#' @return 
#' A single integer (1,2, or 3) inidicating the contestant's final door
#' selection.
#'
#' @examples
#' change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Determine if Contestant has Won
#'
#' @description
#' `determine_winner()` determines whether the contestant wins or loses
#' based on their final door selection.
#'
#' @details
#' The function determines if the door selected by the contestant contains
#' a goat or the car. If the selected door contains the car, the function
#' returns "WIN". If it contains a goat, the function returns "LOSE".
#'
#' @param final.pick An integer (1,2, or 3) representing the contestant's
#' final door choice.
#' @param game A vector indicating what is behind each door (e.g., "car" or
#' "goat").
#'
#' @return 
#' A character string depending on the final result. "WIN" if the contestant's
#' final pick is the car, or "LOSE" if it is a goat.
#'
#' @examples
#' determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Simulation Set-Up
#'
#' @description
#' `play_game()` simulates a single round of the Monty Hall game and 
#' evaluates the outcomes of both possible strategies of staying with the 
#' inital door selection or switching to the remaining unopened door.
#'
#' @details
#' The function begins by creating a new game which randomly assigns the car
#' and goats to the three doors. The contestant then makes their inital
#' selection at random. The host opens one door that the contestant did not
#' initially select and does not contain the car. The function computes the 
#' contestants final door selection with them either staying with their
#' inital pick or switching to the remaining unopened door then evaluates
#' if it is a "WIN" or "LOSE".
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#' A data frame with two rows and two columns stating the strategy ("stay" or
#' "switch"), and the outcome: the result of the game ("WIN" or "LOSE").
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Adding the Game to a Loop
#'
#' @description
#' `play_n_games()` runs multiple independent simulations of the Monty Hall 
#' game and summarizes the outcomes for the "stay" and "switch" strategies.
#'
#' @details
#' The function repeatedly plays the game (n) amount of times and stores the 
#' outcomes from each simulation in a list. The results are combined in a 
#' single data frame.
#'
#' @param n An integer specifying the number of games to simulate.
#'
#' @return 
#' A data frame contaning the results of all simulated games, with one row per
#' strategy ("stay" or "switch") per game and the columns showing the outcome
#' ("WIN" or "LOSE").
#'
#' @examples
#' play_n_games()
#' play_n_games(n = 100)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
