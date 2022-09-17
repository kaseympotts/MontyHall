#' @title
#'   Create a new Monty Hall Problem game.
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



#' @title
#' Player selects first door
#' @description
#' Choose randomly from 1, 2 or 3
#' @details
#' using a sample function, we pick an initial random door
#' @param 
#' none used
#' @return 
#' Return the first chosen door
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' The hosts opens the first door
#' @description
#' The host opens another door - NOT the one chosen by the contestant, and NOT the car door
#' @details
#' If the car door was chosen previously, host opens either goat. If a goat was chosen, the host opens the OTHER goat. 
#' @param 
#' The randomized three doors created in create_game &
#' The first selection made by the player
#' @return 
#' The goat door number that corresponds to what the host will open

#' @examples
#' open_goat_door -> 2
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



#' @title
#' Player chooses to switch of stay
#' @description
#' The player is presented two options and can choose to switch or stay.
#' @details
#' If the player switches, the function returns the final unchosen and unopened door. If they stay, no cahge is made. 
#' @param 
#' The first door chosen by the player, the goat door opened by the host, and a boolean if/then option
#' @return 
#' if stay, keep same door chosen initially. If switch, chose unopened, unchosen door
#' @examples
#' change_door(stay=F, opened.door, a.pick)
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



#' @title
#' Determine Win or Lose
#' @description
#' Dependent upon the choices made previously
#' 
#' @details
#' if final choice = car, WIN
#' if final choice  = goat, LOSE
#' @param 
#' The choices made by the plaer and the position of the goat doors
#' @return 
#' WIN or LOSE
#' @examples
#' determine_winner(1, (car, goat, goat)) --> win
#' determine_winner(2, (car, goat, goat)) --> lose
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



#' @title
#' Playing the game through
#' @description
#' call all functions in the game
#' @details
#' create a game, chose an initial door, open a goat door, chose stay or switch, determine win or lose
#' @param 
#' none
#' @return
#' the steps of the game from start to resulting win or lose 
#' @examples
#' play_game()
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






#' @title
#' Putting the game into a Loop
#' @description 
#' Creating a loop to play the game enough times to compare the strategies of staying vs switching
#' @details
#' The game is played n times the results reported as total wins and total loses
#' @param 
#' n = number of games played
#' @return 
#' table with wins and loses
#' @examples
#' play_n_games (n=1000)
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
