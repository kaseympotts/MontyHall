% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem (1).R
\name{change_door}
\alias{change_door}
\title{Player chooses to switch of stay}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{The}{first door chosen by the player, the goat door opened by the host, and a boolean if/then option}
}
\value{
if stay, keep same door chosen initially. If switch, chose unopened, unchosen door
}
\description{
The player is presented two options and can choose to switch or stay.
}
\details{
If the player switches, the function returns the final unchosen and unopened door. If they stay, no cahge is made.
}
\examples{
change_door(stay=F, opened.door, a.pick)
}
