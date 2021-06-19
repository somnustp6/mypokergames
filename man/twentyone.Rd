\name{twentyone}
\alias{twentyone}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Playing the game "twentyone"
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twentyone(n_people = 2, counter = 100, block = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n_people}{
  Number of players.
}
  \item{counter}{
  How many chips.
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
function (n_people = 2, counter = 100, block = 10) 
{
    n = n_people
    if (!(n <= 4 && n >= 2)) {
        cat("game limit 2~4 people", "\n")
    }
    if (n <= 4 && n >= 2) {
        counter = rep(counter, n)
        poker = paste0(c(rep(c("spade", "heart", "diamond", "club"), 
            13)), c(as.character(rep(1:10, each = 4)), rep(c("J", 
            "Q", "K"), each = 4)))
        point = c(rep(1:10, each = 4), rep(10, each = 12))
        while (sum(counter <= 0) < 1) {
            co = c()
            card = sample(1:52, size = 52, replace = F)
            player = c(paste0("player", as.character(c(1:n_people))), 
                "Bank")
            card_1 = card[1:length(player)]
            card = card[-c(1:length(player))]
            card_2 = card[1:length(player)]
            card = card[-c(1:length(player))]
            df = data.frame(player = player, counter = c(counter, 
                Inf), Hidden = poker[card_1], card1 = poker[card_2], 
                card2 = "NA", card3 = "NA", card4 = "NA", card5 = "NA", 
                card6 = "NA", card7 = "NA", card8 = "NA", card9 = "NA", 
                card10 = "NA")
            cat("Current situation on the table", "\n")
            w = 9
            sign_pad = stringr::str_pad(colnames(df), width = w, 
                side = c("both"), pad = c("_"))
            cat(sign_pad, "\n")
            w = 9
            for (i in c(1:(n + 1))) {
                d = c(c(df[i, ])[1:2], "*", c(df[i, ])[4:13])
                sign_pad = stringr::str_pad(d, width = w, side = c("both"), 
                  pad = c("_"))
                cat(sign_pad, "\n")
            }
            cat("Now every player will check their hidden cards", 
                "\n")
            for (i in c(1:length(player))) {
                ch = readline(player[i])
                cat(player[i], "Hidden is", df[i, 3], "\n")
                cat(player[i], "card1 is", df[i, 4], "\n")
                if (i == length(player)) {
                  cat("After watching, start the game")
                }
                else {
                  co = c(co, readline("How much to bet"))
                  ch = readline("After watching, change to the next one")
                }
                for (e in c(1:block)) {
                  cat("=====================================================", 
                    "\n")
                }
            }
            cat("Now the game starts", "\n")
            for (p in c(1:length(player))) {
                if (p == length(player)) {
                  cat(player[p], "Start to get cards", "\n")
                  cat("Your have", c("*", df[p, 4]), "\n")
                  ta = 0
                  tac = c()
                  po = which(poker \%in\% c(df[p, 3], df[p, 4]))
                  s = sum(point[po])
                  while (s < 17) {
                    y = "Y"
                    if (y == "Y") {
                      cat(player[p], "You got", poker[card[1]], 
                        "\n")
                      s = s + point[card[1]]
                      tac = c(tac, poker[card[1]])
                      card = card[-c(1)]
                      ta = ta + 1
                    }
                    else {
                      break
                    }
                  }
                  if (ta != 0) {
                    df[p, 5:(5 - 1 + ta)] = tac
                  }
                  sign_pad = stringr::str_pad(colnames(df), width = w, 
                    side = c("both"), pad = c("_"))
                  cat(sign_pad, "\n")
                  for (i in c(1:(n + 1))) {
                    d = c(c(df[i, ])[1:2], "*", c(df[i, ])[4:13])
                    sign_pad = stringr::str_pad(d, width = w, 
                      side = c("both"), pad = c("_"))
                    cat(sign_pad, "\n")
                  }
                }
                else {
                  cat(player[p], "Start to get cards", "\n")
                  cat("Your have", c("*", df[p, 4]), "\n")
                  ta = 0
                  tac = c()
                  while (ta < 10) {
                    y = readline("Do you want to get another card?? Y:Yes N:No")
                    if (y == "Y") {
                      cat(player[p], "You got", poker[card[1]], 
                        "\n")
                      tac = c(tac, poker[card[1]])
                      card = card[-c(1)]
                      ta = ta + 1
                    }
                    else {
                      break
                    }
                  }
                  if (ta != 0) {
                    df[p, 5:(5 - 1 + ta)] = tac
                  }
                  sign_pad = stringr::str_pad(colnames(df), width = w, 
                    side = c("both"), pad = c("_"))
                  cat(sign_pad, "\n")
                  for (i in c(1:(n + 1))) {
                    d = c(c(df[i, ])[1:2], "*", c(df[i, ])[4:13])
                    sign_pad = stringr::str_pad(d, width = w, 
                      side = c("both"), pad = c("_"))
                    cat(sign_pad, "\n")
                  }
                }
            }
            cat("Show hidden cards and count points", "\n")
            cat("Current situation on the table", "\n")
            sign_pad = stringr::str_pad(colnames(df), width = w, 
                side = c("both"), pad = c("_"))
            cat(sign_pad, "\n")
            for (i in c(1:(n + 1))) {
                d = c(c(df[i, ])[1:13])
                sign_pad = stringr::str_pad(d, width = w, side = c("both"), 
                  pad = c("_"))
                cat(sign_pad, "\n")
            }
            cat("The sum of the Bank's points is", s, "\n")
            sc = c()
            for (p in c(1:(length(player) - 1))) {
                cat(player[p], "has:", c(df[p, 3], df[p, 4], 
                  df[p, 5], df[p, 6], df[p, 7], df[p, 8], df[p, 
                    9], df[p, 10], df[p, 11], df[p, 12], df[p, 
                    13]), "\n")
                ss = readline("The sum of points is")
                sc = c(sc, ss)
            }
            if (sum(sc \%in\% c("21")) > 0) {
                for (p in c(1:length(sc))) {
                  if (sc[p] == "21" && df[p, 5] == "NA") {
                    cat(player[p], "achieve BlackJack", "\n")
                    cat("gave", player[p], "double the bet", 
                      "\n")
                    df[p, 2] = df[p, 2] + 2 * as.numeric(co[p])
                    sc[p] = "50"
                  }
                }
            }
            if (s <= 21) {
                cat("The Bank has not Bust", "\n")
                cat("Confiscate the bets of the Bust players and point smaller than Bank", 
                  "\n")
                cat("Pay one bet to players who have not Bust and points bigger than Bank", 
                  "\n")
                cat("If the points are the same as Bank, Tie, not confiscate the bets", 
                  "\n")
                for (p in c(1:length(sc))) {
                  ss = as.numeric(sc[p])
                  if (ss == 50) {
                    next
                  }
                  if (ss <= 21 && ss > s) {
                    df[p, 2] = df[p, 2] + as.numeric(co[p])
                  }
                  else if (ss > 21 | ss < s) {
                    df[p, 2] = df[p, 2] - as.numeric(co[p])
                  }
                  else {
                    df[p, 2] = df[p, 2]
                  }
                }
            }
            else if (s > 21) {
                cat("The Bank Bust", "\n")
                cat("Confiscate the bets of the Bust players", 
                  "\n")
                cat("Pay one bet to players who have not Bust", 
                  "\n")
                for (p in c(1:length(sc))) {
                  ss = as.numeric(sc[p])
                  if (ss <= 21) {
                    df[p, 2] = df[p, 2] + as.numeric(co[p])
                  }
                  else {
                    df[p, 2] = df[p, 2] - as.numeric(co[p])
                  }
                }
            }
            cat("Current situation on the table", "\n")
            sign_pad = stringr::str_pad(colnames(df), width = w, 
                side = c("both"), pad = c("_"))
            cat(sign_pad, "\n")
            for (i in c(1:(n + 1))) {
                d = c(c(df[i, ])[1:13])
                sign_pad = stringr::str_pad(d, width = w, side = c("both"), 
                  pad = c("_"))
                cat(sign_pad, "\n")
            }
            cat("This round of the game is over", "\n")
            y = readline("Whether to start the next round? Y:Yes N:No")
            if (y == "Y") {
                for (p in c(1:length(sc))) {
                  counter[p] = df[p, 2]
                  if (counter[p] <= 0) {
                    cat(player[p], "bankrupted", "\n")
                  }
                }
                if (sum(counter <= 0) >= 1) {
                  cat("End Game", "\n")
                  break
                }
            }
            else {
                cat("End Game", "\n")
                break
            }
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
