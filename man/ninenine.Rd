\name{ninenine}
\alias{ninenine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Playing the game "ninenine"
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ninenine(n_people = 4, n_computer = 0, block = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_people}{
  Number of players.
}
  \item{n_computer}{
  Add how many computers.
}
  \item{block}{
  Prevent other to look.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n_people = 4, n_computer = 0, block = 10) 
{
    n = n_people + n_computer
    pointsum = 0
    poker = paste(c(rep(c("spade", "heart", "diamond", "club"), 
        13)), c(as.character(rep(1:10, each = 4)), rep(c("J", 
        "Q", "K"), each = 4)))
    card = sample(1:52, size = 52, replace = F)
    u_card = c()
    if (!(n <= 4 && n >= 2)) {
        cat("game limit 2~4 people", "\n")
    }
    loser = c()
    if (n <= 4 && n >= 2) {
        cat("number of player:", n, "game start", "\n")
        playerlist = vector("list")
        for (i in 1:n) {
            playerlist[[i]] = sort(card[(1 + 5 * (i - 1)):(5 + 
                5 * (i - 1))])
        }
        if (n_computer == 0) {
            np = as.character(c(1:n_people))
        }
        else if (n_people == 0) {
            np = paste0("c", as.character(c(1:n_computer)))
        }
        else {
            np = c(as.character(c(1:n_people)), paste0("c", as.character(c(1:n_computer))))
        }
        names(playerlist) = sample(np, size = length(np), replace = F)
        card = card[-c(1:(5 * n))]
        i = 1
        cat("The current order of cards is:", names(playerlist), 
            "\n")
        cat("player", names(playerlist)[i], "start", "\n")
        st = readline("game start")
        while (n == 4) {
            cat("The current sum of the deck = ", pointsum, "\n")
            cat("player", names(playerlist)[i], "\n")
            cat(poker[playerlist[[i]]], "\n")
            pl = names(playerlist)[i]
            if (grepl("c", names(playerlist)[i])) {
                if (pointsum > 90) {
                  pc = playerlist[[i]][1:5]
                  pc = pc \%in\% as.character(c(1, 13:20, 37:52))
                  if (sum(pc) == 0) {
                    a = sample(as.character(c(1:5)), size = 1, 
                      replace = F)
                  }
                  else {
                    pc = which(pc)
                    a = sample(as.character(pc), size = 1, replace = F)
                  }
                }
                else {
                  a = sample(as.character(c(1:5)), size = 1, 
                    replace = F)
                }
            }
            else {
                a = readline("Which card to play?")
            }
            if (!(a \%in\% c("Q", "1", "2", "3", "4", "5"))) {
                cat("Input error, leave the game", "\n")
                break
            }
            if (a == "Q") {
                cat("leave the game", "\n")
                break
            }
            if (playerlist[[i]][as.integer(a)] \%in\% c(1)) {
                pointsum = 0
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 4, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(13:16)) {
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(4, 3, 2, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(17:20)) {
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                if (grepl("c", names(playerlist)[i])) {
                  a5 = sample(names(playerlist), size = 1, replace = F)
                }
                else {
                  a5 = readline("Specify which player?")
                }
                pa5 = which(names(playerlist) == a5)
                playerlist = playerlist[c(pa5, ifelse((pa5 + 
                  1) <= 4, pa5 + 1, pa5 - 3), ifelse((pa5 + 2) <= 
                  4, pa5 + 2, pa5 - 2), ifelse((pa5 + 3) <= 4, 
                  pa5 + 3, pa5 - 1))]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(41:44)) {
                pointsum = pointsum
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 4, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(37:40)) {
                if (grepl("c", names(playerlist)[i])) {
                  if (pointsum > 89) {
                    a10 = "B"
                  }
                  else if (pointsum < 10) {
                    a10 = "A"
                  }
                  else {
                    a10 = sample(c("A", "B"), size = 1, replace = F)
                  }
                }
                else {
                  a10 = readline("A:+10  B:-10 ?")
                }
                if (a10 == "A") 
                  a10 = 10
                if (a10 == "B") 
                  a10 = -10
                if ((pointsum + as.integer(a10)) > 99 | (pointsum + 
                  as.integer(a10)) < 0) {
                  break
                }
                pointsum = pointsum + as.integer(a10)
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 4, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(45:48)) {
                if (grepl("c", names(playerlist)[i])) {
                  if (pointsum > 79) {
                    a20 = "B"
                  }
                  else if (pointsum < 20) {
                    a20 = "A"
                  }
                  else {
                    a20 = sample(c("A", "B"), size = 1, replace = F)
                  }
                }
                else {
                  a20 = readline("A:+20  B:-20 ?")
                }
                if (a20 == "A") 
                  a20 = 20
                if (a20 == "B") 
                  a20 = -20
                if ((pointsum + as.integer(a20)) > 99 | (pointsum + 
                  as.integer(a20)) < 0) {
                  break
                }
                pointsum = pointsum + as.integer(a20)
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 4, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(49:52)) {
                pointsum = 99
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 4, 1)]
            }
            else {
                ae = playerlist[[i]][as.integer(a)]
                ae = as.integer((ae + 3)/4)
                if ((pointsum + ae) > 99 | (pointsum + ae) < 
                  0) {
                  break
                }
                pointsum = pointsum + ae
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 4, 1)]
            }
            if (length(u_card) > 15) {
                card = c(card, sample(u_card, size = length(u_card), 
                  replace = F))
                u_card = c()
            }
            cat("player", pl, "End of cards", "\n")
            cat("The current sum of the deck = ", pointsum, "\n")
            cat("Change the next one,player", names(playerlist)[i], 
                "\n")
            if (grepl("c", pl)) {
                ch = "Y"
            }
            else {
                ch = readline("Hand it over to the next player")
            }
            for (e in c(1:block)) {
                cat("=====================================================", 
                  "\n")
            }
            cat("player", names(playerlist)[i], "\n")
            if (grepl("c", names(playerlist)[i])) {
                ch = "Y"
            }
            else {
                ch = readline("Handed over")
            }
        }
        if (n == 4) {
            n = n - 1
            cat("player", names(playerlist)[i], "Make the sum of the deck not between 0~99", 
                "\n")
            cat("player", names(playerlist)[i], "Weed out,", 
                n, "players continue to play", "\n")
            loser = c(names(playerlist)[i], loser)
            playerlist[names(playerlist)[i]] = NULL
        }
        while (n == 3) {
            cat("The current sum of the deck = ", pointsum, "\n")
            cat("player", names(playerlist)[i], "\n")
            cat(poker[playerlist[[i]]], "\n")
            pl = names(playerlist)[i]
            if (grepl("c", names(playerlist)[i])) {
                if (pointsum > 90) {
                  pc = playerlist[[i]][1:5]
                  pc = pc \%in\% as.character(c(1, 13:20, 37:52))
                  if (sum(pc) == 0) {
                    a = sample(as.character(c(1:5)), size = 1, 
                      replace = F)
                  }
                  else {
                    pc = which(pc)
                    a = sample(as.character(pc), size = 1, replace = F)
                  }
                }
                else {
                  a = sample(as.character(c(1:5)), size = 1, 
                    replace = F)
                }
            }
            else {
                a = readline("Which card to play?")
            }
            if (!(a \%in\% c("Q", "1", "2", "3", "4", "5"))) {
                cat("Input error, leave the game", "\n")
                break
            }
            if (a == "Q") {
                cat("leave the game", "\n")
                break
            }
            if (playerlist[[i]][as.integer(a)] \%in\% c(1)) {
                pointsum = 0
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(13:16)) {
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(3, 2, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(17:20)) {
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                if (grepl("c", names(playerlist)[i])) {
                  a5 = sample(names(playerlist), size = 1, replace = F)
                }
                else {
                  a5 = readline("Specify which player?")
                }
                pa5 = which(names(playerlist) == a5)
                playerlist = playerlist[c(pa5, ifelse((pa5 + 
                  1) <= 3, pa5 + 1, pa5 - 2), ifelse((pa5 + 2) <= 
                  3, pa5 + 2, pa5 - 1))]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(41:44)) {
                pointsum = pointsum
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(37:40)) {
                if (grepl("c", names(playerlist)[i])) {
                  if (pointsum > 89) {
                    a10 = "B"
                  }
                  else if (pointsum < 10) {
                    a10 = "A"
                  }
                  else {
                    a10 = sample(c("A", "B"), size = 1, replace = F)
                  }
                }
                else {
                  a10 = readline("A:+10  B:-10 ?")
                }
                if (a10 == "A") 
                  a10 = 10
                if (a10 == "B") 
                  a10 = -10
                if ((pointsum + as.integer(a10)) > 99 | (pointsum + 
                  as.integer(a10)) < 0) {
                  break
                }
                pointsum = pointsum + as.integer(a10)
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(45:48)) {
                if (grepl("c", names(playerlist)[i])) {
                  if (pointsum > 79) {
                    a20 = "B"
                  }
                  else if (pointsum < 20) {
                    a20 = "A"
                  }
                  else {
                    a20 = sample(c("A", "B"), size = 1, replace = F)
                  }
                }
                else {
                  a20 = readline("A:+20  B:-20 ?")
                }
                if (a20 == "A") 
                  a20 = 20
                if (a20 == "B") 
                  a20 = -20
                if ((pointsum + as.integer(a20)) > 99 | (pointsum + 
                  as.integer(a20)) < 0) {
                  break
                }
                pointsum = pointsum + as.integer(a20)
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(49:52)) {
                pointsum = 99
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 1)]
            }
            else {
                ae = playerlist[[i]][as.integer(a)]
                ae = as.integer((ae + 3)/4)
                if ((pointsum + ae) > 99) {
                  break
                }
                pointsum = pointsum + ae
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 3, 1)]
            }
            if (length(u_card) > 15) {
                card = c(card, sample(u_card, size = length(u_card), 
                  replace = F))
                u_card = c()
            }
            cat("player", pl, "End of cards", "\n")
            cat("player = ", pointsum, "\n")
            cat("Change the next one,player", names(playerlist)[i], 
                "\n")
            if (grepl("c", pl)) {
                ch = "Y"
            }
            else {
                ch = readline("Hand it over to the next player")
            }
            for (e in c(1:block)) {
                cat("=====================================================", 
                  "\n")
            }
            cat("player", names(playerlist)[i], "\n")
            if (grepl("c", names(playerlist)[i])) {
                ch = "Y"
            }
            else {
                ch = readline("Handed over")
            }
        }
        if (n == 3) {
            n = n - 1
            cat("player", names(playerlist)[i], "Make the sum of the deck not between 0~99", 
                "\n")
            cat("player", names(playerlist)[i], "Weed out", n, 
                "players continue to play", "\n")
            loser = c(names(playerlist)[i], loser)
            playerlist[names(playerlist)[i]] = NULL
        }
        while (n == 2) {
            cat("The current sum of the deck = ", pointsum, "\n")
            cat("player", names(playerlist)[i], "\n")
            cat(poker[playerlist[[i]]], "\n")
            pl = names(playerlist)[i]
            if (grepl("c", names(playerlist)[i])) {
                if (pointsum > 90) {
                  pc = playerlist[[i]][1:5]
                  pc = pc \%in\% as.character(c(1, 13:20, 37:52))
                  if (sum(pc) == 0) {
                    a = sample(as.character(c(1:5)), size = 1, 
                      replace = F)
                  }
                  else {
                    pc = which(pc)
                    a = sample(as.character(pc), size = 1, replace = F)
                  }
                }
                else {
                  a = sample(as.character(c(1:5)), size = 1, 
                    replace = F)
                }
            }
            else {
                a = readline("Which card to play?")
            }
            if (!(a \%in\% c("Q", "1", "2", "3", "4", "5"))) {
                cat("Input error, leave the game", "\n")
                break
            }
            if (a == "Q") {
                cat("leave the game", "\n")
                break
            }
            if (playerlist[[i]][as.integer(a)] \%in\% c(1)) {
                pointsum = 0
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(13:16)) {
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(17:20)) {
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                if (grepl("c", names(playerlist)[i])) {
                  a5 = sample(names(playerlist), size = 1, replace = F)
                }
                else {
                  a5 = readline("Specify which player??")
                }
                pa5 = which(names(playerlist) == a5)
                playerlist = playerlist[c(pa5, ifelse((pa5 + 
                  1) <= 2, pa5 + 1, pa5 - 1))]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(41:44)) {
                pointsum = pointsum
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(37:40)) {
                if (grepl("c", names(playerlist)[i])) {
                  if (pointsum > 89) {
                    a10 = "B"
                  }
                  else if (pointsum < 10) {
                    a10 = "A"
                  }
                  else {
                    a10 = sample(c("A", "B"), size = 1, replace = F)
                  }
                }
                else {
                  a10 = readline("A:+10  B:-10 ?")
                }
                if (a10 == "A") 
                  a10 = 10
                if (a10 == "B") 
                  a10 = -10
                if ((pointsum + as.integer(a10)) > 99 | (pointsum + 
                  as.integer(a10)) < 0) {
                  break
                }
                pointsum = pointsum + as.integer(a10)
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(45:48)) {
                if (grepl("c", names(playerlist)[i])) {
                  if (pointsum > 79) {
                    a20 = "B"
                  }
                  else if (pointsum < 20) {
                    a20 = "A"
                  }
                  else {
                    a20 = sample(c("A", "B"), size = 1, replace = F)
                  }
                }
                else {
                  a20 = readline("A:+20  B:-20 ?")
                }
                if (a20 == "A") 
                  a20 = 20
                if (a20 == "B") 
                  a20 = -20
                if ((pointsum + as.integer(a20)) > 99 | (pointsum + 
                  as.integer(a20)) < 0) {
                  break
                }
                pointsum = pointsum + as.integer(a20)
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 1)]
            }
            else if (playerlist[[i]][as.integer(a)] \%in\% c(49:52)) {
                pointsum = 99
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 1)]
            }
            else {
                ae = playerlist[[i]][as.integer(a)]
                ae = as.integer((ae + 3)/4)
                if ((pointsum + ae) > 99 | (pointsum + ae) < 
                  0) {
                  break
                }
                pointsum = pointsum + ae
                u_card = c(u_card, playerlist[[i]][c(as.integer(a))])
                playerlist[[i]] = playerlist[[i]][-c(as.integer(a))]
                playerlist[[i]] = sort(c(playerlist[[i]], card[1]))
                card = card[-c(1)]
                playerlist = playerlist[c(2, 1)]
            }
            if (length(u_card) > 15) {
                card = c(card, sample(u_card, size = length(u_card), 
                  replace = F))
                u_card = c()
            }
            cat("player", pl, "End of cards", "\n")
            cat("The current sum of the deck = ", pointsum, "\n")
            cat("Change the next one,player", names(playerlist)[i], 
                "\n")
            if (grepl("c", pl)) {
                ch = "Y"
            }
            else {
                ch = readline("Hand it over to the next player")
            }
            for (e in c(1:block)) {
                cat("=====================================================", 
                  "\n")
            }
            cat("player", names(playerlist)[i], "\n")
            if (grepl("c", names(playerlist)[i])) {
                ch = "Y"
            }
            else {
                ch = readline("Handed over")
            }
        }
        if (n == 2) {
            n = n - 1
            cat("player", names(playerlist)[i], "Make the sum of the deck not between 0~99", 
                "\n")
            cat("player", names(playerlist)[i], "Weed out", "\n")
            loser = c(names(playerlist)[i], loser)
            playerlist[names(playerlist)[i]] = NULL
            loser = c(names(playerlist)[i], loser)
        }
        cat("=======game over=======", "\n")
        cat("Last place:", "\n")
        for (w in c(1:length(loser))) {
            cat(w, "place : player", loser[w], "\n")
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
